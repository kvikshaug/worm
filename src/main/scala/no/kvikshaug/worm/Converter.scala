package no.kvikshaug.worm

import java.lang.reflect.Field
import java.lang.reflect.Constructor
import java.lang.reflect.ParameterizedType

import scala.collection.mutable.ListBuffer

class Attribute
case class ForeignKey() extends Attribute // Refactor to ForeignKey when that class is removed
case class Primitive() extends Attribute

case class Table(name: String, var rows: List[Row], obj: Option[Worm])
case class Row(columns: List[Column])
case class Column(name: String, value: AnyRef, attribute: Attribute = Primitive())

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
  def objectToTables(obj: Worm): List[Table] = {
    // Traverse all the fields of the class
    var prependTables = ListBuffer[Table]() // To make sure inserts are unrwapped in the right order
    var tables = ListBuffer[Table]()
    val thisTable = Table(obj.getClass.getSimpleName, null, Some(obj))
    val columns = obj.getClass.getDeclaredFields.map { f =>
      f.setAccessible(true)
      if(classOf[Worm].isAssignableFrom(f.getType)) {
        // It's another custom class that extends Worm
        // We know that the table for that class will be the first
        // in the list, since it's prepended at the end of this method
        val innerTables = objectToTables(f.get(obj).asInstanceOf[Worm])
        prependTables = prependTables ++ innerTables
        Some(Column(f.getName, innerTables(0), ForeignKey()))
      } else if(classOf[java.util.Collection[_]].isAssignableFrom(f.getType) ||
                classOf[Seq[_]].isAssignableFrom(f.getType)) {
        // Sequence collection
        val seqType = f.getGenericType.asInstanceOf[ParameterizedType]
                       .getActualTypeArguments()(0).asInstanceOf[java.lang.Class[_]]
        if(classOf[Worm].isAssignableFrom(seqType)) {
          // A sequence of Worms
          val rows = f.get(obj).asInstanceOf[List[Worm]].map { worm =>
            val thatTable = objectToTables(worm)
            tables = tables ++ thatTable
            Row(List(
              Column(fieldName(obj.getClass.getSimpleName), thisTable, ForeignKey()),
              Column(f.getName, thatTable(0), ForeignKey())
            ))
          }
          tables = tables += Table(obj.getClass.getSimpleName + seqType.getSimpleName + "s", rows, None)
        //} else if(classOf[java.util.Collection[_]].isAssignableFrom(seqType) ||
        //          classOf[Seq[_]].isAssignableFrom(seqType)) {
          // A list of lists (not yet supported)
        } else {
          // Something else, assume primitive
          val rows = f.get(obj).asInstanceOf[List[AnyRef]].map { item =>
            Row(List(
              Column(fieldName(obj.getClass.getSimpleName), thisTable, ForeignKey()),
              Column(f.getName, item, Primitive())
            ))
          }
          tables = tables += Table(obj.getClass.getSimpleName + seqType.getSimpleName + "s", rows, None)
        }
        None
      } else {
        // It's something else, assume primitive
        Some((Column(f.getName, f.get(obj))))
      }
    }.toList.flatten
    thisTable.rows = List(Row(columns))
    tables = prependTables ++ (thisTable +: tables)
    tables.toList
  }

  /** Take a list of rows from the database, the type they belong to and create
      objects out of the data in the rows */
  def tableToObject[T <: Worm: ClassManifest](rows: List[List[AnyRef]]): List[T] = {
    val constructor = classManifest[T].erasure.getConstructors()(0)
    val objects = rows.map { originalRow =>
      val row = originalRow.tail.zip(constructor.getParameterTypes).map { tuple =>
        jvmType(tuple._1, tuple._2.asInstanceOf[Class[_]])
      }.asInstanceOf[List[AnyRef]]
      val obj = constructor.newInstance(row: _*).asInstanceOf[T]
      obj.wormDbId = Some(originalRow.head.asInstanceOf[Int].toLong)
      obj
    }
    return objects
  }

  /** Create a list of TableStructures corresponding to the given class type,
      which can be used to create the tables for its structure */
  def classToStructure[T <: Worm: ClassManifest]: List[TableStructure] = {
    def unwrapSeq(containerName: String, f: Field): List[TableStructure] = {
      val seqType = f.getGenericType.asInstanceOf[ParameterizedType]
                     .getActualTypeArguments()(0).asInstanceOf[java.lang.Class[_]]
      if(classOf[Worm].isAssignableFrom(seqType)) {
        // It's a list of objects that extends Worm - create a separate table and a join table
        return TableStructure(containerName + seqType.getSimpleName + "s", List(
          ColumnStructure("id", pkType),
          ColumnStructure(fieldName(containerName), fkType),
          ColumnStructure(f.getName, fkType))) ::
            classToStructure(Manifest.classType(seqType))
      } else {
        // Assume it's a list of primitives - create a separate table for them
        return List(TableStructure(containerName + seqType.getSimpleName + "s", List(
          ColumnStructure("id", pkType),
          ColumnStructure(fieldName(containerName), fkType),
          ColumnStructure(fieldName(seqType.getSimpleName), columnType(commonName(seqType.getSimpleName))))))
      }
    }
    var tables = List[TableStructure]()
    val columns = ColumnStructure("id", pkType) :: classManifest[T].erasure.getDeclaredFields.map { f =>
      f.setAccessible(true)
      if(classOf[Worm].isAssignableFrom(f.getType)) {
        // Relation
        tables = tables ++ classToStructure(Manifest.classType(f.getType))
        Some(ColumnStructure(f.getName, fkType))
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
      val rows = Worm.sql.get.select(t.getSimpleName, "where id='" + obj.toString + "'")
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
