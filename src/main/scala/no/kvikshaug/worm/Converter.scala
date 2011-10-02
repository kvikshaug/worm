package no.kvikshaug.worm

import java.lang.reflect.Field
import java.lang.reflect.Constructor
import java.lang.reflect.ParameterizedType

import scala.collection.mutable.ListBuffer

case class Table(name: String, rows: List[Row], obj: Worm)
case class Row(columns: List[Column])
case class Column(name: String, value: Any, depends: Option[Dependency])

class Dependency(val parent: Worm, val tableName: String, val parentName: String, val childName: String)
case class SingleWormDependency(override val parent: Worm, val child: Worm, override val tableName: String, override val parentName: String, override val childName: String) extends Dependency(parent, tableName, parentName, childName)
case class WormDependency(override val parent: Worm, children: Seq[Worm], override val tableName: String, override val parentName: String, override val childName: String) extends Dependency(parent, tableName, parentName, childName)
case class PrimitiveDependency(override val parent: Worm, children: Seq[AnyRef], override val tableName: String, override val parentName: String, override val childName: String) extends Dependency(parent, tableName, parentName, childName)

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
        deps += SingleWormDependency(obj, that, obj.getClass.getSimpleName + f.getType.getSimpleName + "s",
          fieldName(obj.getClass.getSimpleName), f.getName)
        val (table, dep) = objectToTables(that)
        tables = tables ++ table
        deps = deps ++ deps
        // No column, the dependency column will be in a separate table
        None
      } else if(classOf[java.util.Collection[_]].isAssignableFrom(f.getType) ||
                classOf[Seq[_]].isAssignableFrom(f.getType)) {
        // This field is a sequence/collection
        val seqType = f.getGenericType.asInstanceOf[ParameterizedType]
                       .getActualTypeArguments()(0).asInstanceOf[java.lang.Class[_]]
        if(classOf[Worm].isAssignableFrom(seqType)) {
          // A sequence of Worms. Make tables for each Worm
          val list = f.get(obj).asInstanceOf[Seq[Worm]] // i suspect this won't work with java.util.List
          deps += WormDependency(obj, list, obj.getClass.getSimpleName + seqType.getSimpleName + "s",
            fieldName(obj.getClass.getSimpleName), f.getName)
          list.foreach { worm =>
            val (table, dep) = objectToTables(worm)
            tables = tables ++ table
            deps = deps ++ dep
          }
        } else {
          // A sequence of assumed primitives
          deps += PrimitiveDependency(obj, f.get(obj).asInstanceOf[Seq[AnyRef]], obj.getClass.getSimpleName + seqType.getSimpleName + "s", fieldName(obj.getClass.getSimpleName), f.getName)
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
  def tableToObject[T <: Worm: ClassManifest](rows: List[List[AnyRef]]): List[T] = {
    val constructor = classManifest[T].erasure.getConstructors()(0)
    val objects = rows.map { originalRow =>
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
              classManifest[T].erasure.getSimpleName + classType.getSimpleName + "s",
              "where " + fieldName(classManifest[T].erasure.getSimpleName) + "='" + originalRow.head + "'")
            // Select the first object. We will (should) always just select one
            val row = Worm.sql.get.select(classType.getSimpleName, "where `id`='" + id(0)(2) + "'")
            tableToObject(row)(Manifest.classType(classType))(0)
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
              classManifest[T].erasure.getSimpleName + seqType.getSimpleName + "s",
              "where `" + fieldName(classManifest[T].erasure.getSimpleName) + "`='" + originalRow.head + "'")
            if(ids.isEmpty) {
              List()
            } else {
              val clause = new StringBuilder
              clause.append("`id`='").append(ids.head(2)).append("'")
              ids.tail.map(id => " or `id`='" + id(2) + "'").foreach(str => clause.append(str))
              val rows = Worm.sql.get.select(seqType.getSimpleName, "where " + clause.toString)
              tableToObject(rows)(Manifest.classType(seqType))
            }
          } else {
            // A sequence of assumed primitives
            // Potential optimization: Only need to select fieldName, not *
            val rows = Worm.sql.get.select(
              classManifest[T].erasure.getSimpleName + seqType.getSimpleName + "s",
              "where `" + fieldName(classManifest[T].erasure.getSimpleName) + "`='" + originalRow.head + "'")
            rows.map(r => r(2))
          }
        }
      }.toList.asInstanceOf[List[AnyRef]]
      val obj = constructor.newInstance(rows: _*).asInstanceOf[T]
      obj.wormDbId = Some(originalRow.head.asInstanceOf[Int].toLong)
      obj
    }.asInstanceOf[List[T]]
    return objects
  }

  /** Create a list of TableStructures corresponding to the given class type,
      which can be used to create the tables for its structure */
  def classToStructure[T <: Worm: ClassManifest](): List[TableStructure] = {
    def unwrapSeq(containerName: String, f: Field): List[TableStructure] = {
      val seqType = f.getGenericType.asInstanceOf[ParameterizedType]
                     .getActualTypeArguments()(0).asInstanceOf[java.lang.Class[_]]
      if(classOf[Worm].isAssignableFrom(seqType)) {
        // It's a list of objects that extends Worm - create a separate table and a join table
        return TableStructure(containerName + seqType.getSimpleName + "s", List(
          ColumnStructure("id", pkType),
          ColumnStructure(fieldName(containerName), fkType),
          ColumnStructure(f.getName, fkType))) ::
            classToStructure()(Manifest.classType(seqType))
      } else {
        // Assume it's a list of primitives - create a separate table for them
        return List(TableStructure(containerName + seqType.getSimpleName + "s", List(
          ColumnStructure("id", pkType),
          ColumnStructure(fieldName(containerName), fkType),
          ColumnStructure(f.getName, columnType(commonName(seqType.getSimpleName))))))
      }
    }
    var tables = List[TableStructure]()
    val columns = ColumnStructure("id", pkType) :: classManifest[T].erasure.getDeclaredFields.map { f =>
      f.setAccessible(true)
      if(classOf[Worm].isAssignableFrom(f.getType)) {
        // Relation, create a join table
        val relTable = TableStructure(
          classManifest[T].erasure.getSimpleName + f.getType.getSimpleName + "s", List(
            ColumnStructure("id", pkType),
            ColumnStructure(fieldName(classManifest[T].erasure.getSimpleName), fkType),
            ColumnStructure(f.getName, fkType)))
        tables = tables ++ (relTable :: classToStructure()(Manifest.classType(f.getType)))
        None
      } else if(classOf[java.util.Collection[_]].isAssignableFrom(f.getType) ||
                classOf[Seq[_]].isAssignableFrom(f.getType)) {
        // Sequence collection
        tables = tables ++ unwrapSeq(classManifest[T].erasure.getSimpleName, f)
        None
      } else {
        Some(ColumnStructure(f.getName, columnType(commonName(f.getType.getSimpleName))))
      }
    }.flatten.toList
    TableStructure(classManifest[T].erasure.getSimpleName, columns) :: tables
  }

  /* DB-engine specific functions */

  private def fieldName(name: String) = name.head.toLower + name.tail + 's'

  private def commonName(name: String) =
    name.replaceAll("(?i)integer", "int").replaceAll("(?i)character", "char").toLowerCase

  // Cast and if necessary convert objects to their applicable types
  private def jvmType(obj: Any, t: Class[_]) = db match {
    case "sqlite" => jvmTypeSQLite(obj, t)
  }

  private def jvmTypeSQLite(obj: Any, t: Class[_]) = {
    if(classOf[Worm].isAssignableFrom(t)) {
      // Relation
      val rows = Worm.sql.get.select(t.getSimpleName, "where `id`='" + obj.toString + "'")
      tableToObject(rows)(Manifest.classType(t))(0)
    } else {
      t.getSimpleName.replaceAll("(?i)integer", "int").replaceAll("(?i)character", "char").toLowerCase match {
        case "double"  => obj.asInstanceOf[java.lang.Double]
        case "float"   => obj.asInstanceOf[java.lang.Double].floatValue
        case "long"    => obj.asInstanceOf[java.lang.Integer].longValue
        case "int"     => obj.asInstanceOf[java.lang.Integer]
        case "short"   => obj.asInstanceOf[java.lang.Integer].shortValue
        case "byte"    => obj.asInstanceOf[java.lang.Integer].byteValue
        case "boolean" => java.lang.Boolean.parseBoolean(obj.asInstanceOf[java.lang.String])
        case "char"    => obj.asInstanceOf[java.lang.String].charAt(0)
        case "string"  => obj.asInstanceOf[java.lang.String]
        case _         => throw new UnsupportedTypeException("Cannot create an object of type '" +
          t.getName + "'")
      }
    }
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
