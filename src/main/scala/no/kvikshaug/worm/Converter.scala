package no.kvikshaug.worm

import java.util.Collection
import java.lang.reflect.Field
import java.lang.reflect.Constructor
import java.lang.reflect.ParameterizedType

import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._

case class Table(name: String, rows: List[Row], obj: Worm)
case class Row(columns: List[Column])
case class Column(name: String, value: Any, depends: Option[Dependency])

class Dependency(val parent: Worm, val tableName: String, val parentName: String, val childName: String)
case class SingleWormDependency(override val parent: Worm, val child: Worm, override val tableName: String, override val parentName: String, override val childName: String) extends Dependency(parent, tableName, parentName, childName)
case class WormDependency(override val parent: Worm, children: Iterable[Worm], override val tableName: String, override val parentName: String, override val childName: String) extends Dependency(parent, tableName, parentName, childName)
case class PrimitiveDependency(override val parent: Worm, children: Iterable[AnyRef], override val tableName: String, override val parentName: String, override val childName: String) extends Dependency(parent, tableName, parentName, childName)

case class TableStructure(name: String, columns: List[ColumnStructure])
case class ColumnStructure(name: String, typeName: String)

/** The Converter class converts data objects into a datastructure
    that is simple for our SQL class to use when executing statements.

    The Table and Row classes represents data to be inserted or updated,
    and the TableStructure and ColumnStructure classes represent a data
    structure (without the data) for creating tables. */
object Converter {
  var db = ""
  def setDb(db: String) = {
    db.toLowerCase match {
      case "sqlite" => Converter.db = db.toLowerCase
      case _        => throw new UnsupportedDatabaseException("Worm doesn't support the '"+db+
                         "' DB engine yet.")
    }
  }

  /** Take an object and create a representation of it using the
      Table and Row classes which can be used to insert or update
      that object. */
  def objectToTables(obj: Worm): Tuple2[List[Table], List[Dependency]] = {
    // Traverse all its fields, and figure out what columns to save
    var deps = ListBuffer[Dependency]()
    var tables = ListBuffer[Table]()
    var columns = obj.getClass.getDeclaredFields.map { f =>
      f.setAccessible(true)
      if(classOf[Worm].isAssignableFrom(f.getType)) {
        // This field is another Worm. It will depend on this one
        val that = f.get(obj).asInstanceOf[Worm]
        deps += SingleWormDependency(obj, that, obj.getClass.getSimpleName + f.getType.getSimpleName,
          fieldName(obj.getClass.getSimpleName), f.getName)
        val (table, dep) = objectToTables(that)
        tables = tables ++ table
        deps = deps ++ dep
        // No column, the dependency column will be in a separate table
        None
      } else if(classOf[java.util.Collection[_]].isAssignableFrom(f.getType) ||
                classOf[Seq[_]].isAssignableFrom(f.getType) ||
                classOf[Set[_]].isAssignableFrom(f.getType)) {
        // This field is a sequence/collection
        val seqType = f.getGenericType.asInstanceOf[ParameterizedType]
                       .getActualTypeArguments()(0).asInstanceOf[java.lang.Class[_]]
        if(classOf[Worm].isAssignableFrom(seqType)) {
          // A sequence of Worms. Make tables for each Worm, based on the collection type
          val collectionType = f.getGenericType.asInstanceOf[ParameterizedType]
                                .getRawType.asInstanceOf[java.lang.Class[_]]
          val list = if(classOf[Collection[_]].isAssignableFrom(collectionType)) {
            f.get(obj).asInstanceOf[Collection[Worm]].asScala
          } else if(classOf[Iterable[_]].isAssignableFrom(collectionType)) {
            f.get(obj).asInstanceOf[Iterable[Worm]]
          } else {
            throw new UnsupportedTypeException("I don't know how to create a " +
              "generic collection of type '" + f.getType + "'.")
          }
          deps += WormDependency(obj, list, obj.getClass.getSimpleName + seqType.getSimpleName,
            fieldName(obj.getClass.getSimpleName), f.getName)
          list.foreach { worm =>
            val (table, dep) = objectToTables(worm)
            tables = tables ++ table
            deps = deps ++ dep
          }
        } else {
          // A sequence of assumed primitives
          deps += PrimitiveDependency(obj, f.get(obj).asInstanceOf[Iterable[AnyRef]], obj.getClass.getSimpleName + seqType.getSimpleName, fieldName(obj.getClass.getSimpleName), f.getName)
        }
        // No column needed for sequences
        None
      } else {
        // Assume this field is a primitive, create a column of it
        Some(Column(f.getName, f.get(obj), None))
      }
    }.toList.flatten
    tables = Table(obj.getClass.getSimpleName, List(Row(columns)), obj) +: tables
    return (tables.toList, deps.toList)
  }

  /** Take a list of rows from the database, the type they belong to and create
      objects out of the data in the rows */
  def tableToObject[T <: Worm: ClassManifest](rows: List[List[AnyRef]]): (List[T], List[ID]) = {
    var allTableIds = ListBuffer[ID]()
    val constructors = classManifest[T].erasure.getConstructors()
    if(constructors.isEmpty) {
      throw new IllegalArgumentException(classManifest[T].erasure.getSimpleName + " has no public " +
        "constructor that can be used to create it.")
    } else if(constructors.size > 1) {
      throw new IllegalArgumentException(classManifest[T].erasure.getSimpleName + " has more than " +
        "one constructor, how am I supposed to know which one to use to create it?")
    }
    val constructor = constructors(0)
    val objects = rows.map { originalRow =>
      var tableIds = ListBuffer[ID]()
      // We'll iterate the constructor, and create objects on the fly.
      // We use an iterator for when we need the next value from the original row
      val it = originalRow.tail.iterator
      // We need some extra field, so zip the construtor with the fields
      val rows = classManifest[T].erasure.getDeclaredFields.zip(constructor.getGenericParameterTypes).map { x =>
        val (f, t) = x
        if(t.isInstanceOf[java.lang.Class[_]]) {
          val classType = t.asInstanceOf[java.lang.Class[_]]
          if(classOf[Worm].isAssignableFrom(classType)) {
            // It's a Worm - select and create the Worm first
            val id = Worm.sql.get.select(
              classManifest[T].erasure.getSimpleName + classType.getSimpleName,
              "where `" + fieldName(classManifest[T].erasure.getSimpleName) + "`='" + originalRow.head + "'")
            tableIds += ID(classManifest[T].erasure.getSimpleName + classType.getSimpleName,
              id(0)(3).asInstanceOf[Int].toLong)
            // Select the first object. We will (should) always just select one
            val row = Worm.sql.get.select(classType.getSimpleName, "where `id`='" + id(0)(3) + "'")
            if(row.isEmpty) {
              throw new InconsistentStateException("The relation from '" +
                classManifest[T].erasure.getSimpleName + "' to '" + classType.getSimpleName + "' "+
                "contains a reference which doesn't exist in the DB. " +
                "Did you call 'delete' on the related object?")
            }
            val (obj, thoseIds) = tableToObject(row)(Manifest.classType(classType))
            tableIds = tableIds ++ thoseIds
            obj(0)
          } else {
            // A primitive
            jvmType(it.next, classType)
          }
        } else if(t.isInstanceOf[ParameterizedType]) {
          // Assume this field is a sequence/collection
          val seqType = t.asInstanceOf[ParameterizedType]
                         .getActualTypeArguments()(0).asInstanceOf[java.lang.Class[_]]
          if(classOf[Worm].isAssignableFrom(seqType)) {
            // A sequence of Worms. Select each worm
            val ids = Worm.sql.get.select(
              classManifest[T].erasure.getSimpleName + seqType.getSimpleName,
              "where `" + fieldName(classManifest[T].erasure.getSimpleName) + "`='" + originalRow.head + "'" +
              "order by `order` desc")
            if(ids.isEmpty) {
              Iterable()
            } else {
              // We have the order in the ids list, but we can't select the right order based on it.
              // For that reason, do one select query for each item in the right order.
              // This is probably very much slower than being able to select all items in one statement.
              val objs = ids.map { id =>
                tableIds += ID(classManifest[T].erasure.getSimpleName + seqType.getSimpleName,
                  id(0).asInstanceOf[Int].toLong)
                val row = Worm.sql.get.select(seqType.getSimpleName, "where `id`='" + id(3) + "'")
                val (inner, thoseIds) = tableToObject(row)(Manifest.classType(seqType))
                tableIds = tableIds ++ thoseIds
                inner
              }.asInstanceOf[List[Iterable[T]]].map(_.head)
              // Return the collection as its appropriate type
              // We'll have to check for each type manually (do YOU know of a better way?)
              val collectionType = t.asInstanceOf[ParameterizedType].getRawType.asInstanceOf[java.lang.Class[_]]
              if(classOf[Seq[_]].isAssignableFrom(collectionType)) {
                objs.toSeq
              } else if(classOf[Set[_]].isAssignableFrom(collectionType)) {
                objs.toSet
              } else if(classOf[Collection[_]].isAssignableFrom(collectionType)) {
                objs.asJava
              } else {
                objs
              }
            }
          } else {
            // A sequence of assumed primitives
            // Potential optimization: Only need to select fieldName, not *
            val rows = Worm.sql.get.select(
              classManifest[T].erasure.getSimpleName + seqType.getSimpleName,
              "where `" + fieldName(classManifest[T].erasure.getSimpleName) + "`='" + originalRow.head + "'" +
              "order by `order` desc")
            rows foreach(r => tableIds += ID(classManifest[T].erasure.getSimpleName + seqType.getSimpleName, r(0).asInstanceOf[Int].toLong))
            rows.map(r => jvmType(r(3), seqType))
          }
        }
      }.toList.asInstanceOf[List[AnyRef]]
      tableIds += ID(classManifest[T].erasure.getSimpleName, originalRow.head.asInstanceOf[Int].toLong)
      val obj = constructor.newInstance(rows: _*).asInstanceOf[T]
      obj.wormDbId = Some(originalRow.head.asInstanceOf[Int].toLong)
      obj.wormDbIds = Some(tableIds.toList)
      allTableIds = allTableIds ++ tableIds
      obj
    }.asInstanceOf[List[T]]
    return (objects, allTableIds.toList)
  }

  /** Create a list of TableStructures corresponding to the given class type,
      which can be used to create the tables for its structure */
  def classToStructure[T <: Worm: ClassManifest](): List[TableStructure] = {
    def unwrapSeq(containerName: String, f: Field): List[TableStructure] = {
      val seqType = f.getGenericType.asInstanceOf[ParameterizedType]
                     .getActualTypeArguments()(0).asInstanceOf[java.lang.Class[_]]
      if(classOf[Worm].isAssignableFrom(seqType)) {
        // It's a list of objects that extends Worm - create a separate table and a join table
        return TableStructure(containerName + seqType.getSimpleName, List(
          ColumnStructure("id", pkType),
          ColumnStructure("order", columnType("long")),
          ColumnStructure(fieldName(containerName), fkType),
          ColumnStructure(f.getName, fkType))) ::
            classToStructure()(Manifest.classType(seqType))
      } else {
        // Assume it's a list of primitives - create a separate table for them
        return List(TableStructure(containerName + seqType.getSimpleName, List(
          ColumnStructure("id", pkType),
          ColumnStructure("order", columnType("long")),
          ColumnStructure(fieldName(containerName), fkType),
          ColumnStructure(f.getName, columnType(commonName(seqType.getSimpleName))))))
      }
    }
    var tables = List[TableStructure]()
    val columns = ColumnStructure("id", pkType) :: classManifest[T].erasure.getDeclaredFields.map { f =>
      f.setAccessible(true)
      // We don't support inner classes of classes
      if(classManifest[T].erasure.getDeclaringClass != null &&
        (f.getName == "$outer" || f.getName == "$this$0")) {
        throw new UnsupportedOperationException("Worm does not support inner classes. It does support " +
          "static nested classes (classes defined in companion objects), read more about why in the " +
          "documentation.")
      }
      if(classOf[Worm].isAssignableFrom(f.getType)) {
        // Relation, create a join table
        val relTable = TableStructure(
          classManifest[T].erasure.getSimpleName + f.getType.getSimpleName, List(
            ColumnStructure("id", pkType),
            ColumnStructure("order", columnType("long")),
            ColumnStructure(fieldName(classManifest[T].erasure.getSimpleName), fkType),
            ColumnStructure(f.getName, fkType)))
        tables = tables ++ (relTable :: classToStructure()(Manifest.classType(f.getType)))
        None
      } else if(classOf[java.util.Collection[_]].isAssignableFrom(f.getType) ||
                classOf[Seq[_]].isAssignableFrom(f.getType) ||
                classOf[Set[_]].isAssignableFrom(f.getType)) {
        // Sequence collection
        tables = tables ++ unwrapSeq(classManifest[T].erasure.getSimpleName, f)
        None
      } else {
        Some(ColumnStructure(f.getName, columnType(commonName(f.getType.getSimpleName))))
      }
    }.flatten.toList
    TableStructure(classManifest[T].erasure.getSimpleName, columns) :: tables
  }

  private def fieldName(name: String) = name.head.toLower + name.tail

  private def commonName(name: String) =
    name.replaceAll("(?i)integer", "int").replaceAll("(?i)character", "char").toLowerCase

  /* DB-engine specific functions */

  // Cast and if necessary convert objects to their applicable types
  private def jvmType(obj: Any, t: Class[_]) = db match {
    case "sqlite" => jvmTypeSQLite(obj, t)
  }

  private def jvmTypeSQLite(obj: Any, t: Class[_]) = commonName(t.getSimpleName) match {
    case "double"  => obj.asInstanceOf[java.lang.Double]
    case "float"   => obj.asInstanceOf[java.lang.Double].floatValue
    case "long"    =>
      if(obj.isInstanceOf[java.lang.Long]) {
        obj.asInstanceOf[java.lang.Long]
      } else {
        // If the value is below the max int value, it will be an instance of
        // Int and uncastable to Long, so we need to call longValue
        obj.asInstanceOf[java.lang.Integer].longValue
      }
    case "int"     => obj.asInstanceOf[java.lang.Integer]
    case "short"   => obj.asInstanceOf[java.lang.Integer].shortValue
    case "byte"    => obj.asInstanceOf[java.lang.Integer].byteValue
    case "boolean" => java.lang.Boolean.parseBoolean(obj.asInstanceOf[java.lang.String])
    case "char"    => obj.asInstanceOf[java.lang.String].charAt(0)
    case "string"  => obj.asInstanceOf[java.lang.String]
    case _         => throw new UnsupportedTypeException("Cannot retrieve an object of " +
      "unknown type '" + t.getName + "'")
  }

  // Primary key type
  private def pkType = db match {
    case "sqlite" => "INTEGER PRIMARY KEY"
  }

  // Foreign key type
  private def fkType = db match {
    case "sqlite" => "INTEGER"
  }

  // Column types
  private def columnType(fieldType: String) = db match {
    case "sqlite" => columnTypeSQLite(fieldType)
  }

  private def columnTypeSQLite(fieldType: String) = fieldType match {
    case "double"  => "NUMERIC"
    case "float"   => "NUMERIC"
    case "long"    => "NUMERIC"
    case "int"     => "NUMERIC"
    case "short"   => "NUMERIC"
    case "byte"    => "NUMERIC"
    case "boolean" => "TEXT"
    case "char"    => "TEXT"
    case "string"  => "TEXT"
    case _         => throw new UnsupportedTypeException("Can't create DB column with unknown type '" +
      fieldType + "'")
  }
}
