package no.kvikshaug.worm

import java.lang.reflect.Field
import java.lang.reflect.Constructor
import java.lang.reflect.ParameterizedType

class Attribute
case class ForeignKey() extends Attribute // Refactor to ForeignKey when that class is removed
case class Primitive() extends Attribute

case class Table(name: String, rows: List[Row], obj: Worm)
case class Row(name: String, value: AnyRef, attribute: Attribute = Primitive())

case class TableStructure(name: String, rows: List[RowStructure])
case class RowStructure(name: String, typeName: String)

/** The Converter class converts data objects into a datastructure
    that is simple for our SQL class to use when executing statements.

    The Table and Row classes represents data to be inserted or updated,
    and the TableStructure and RowStructure classes represent a data
    structure (without the data) for creating tables.

    This class does not verify that a connection to SQL has been
    performed, so do that before calling methods here */
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
  def objectToTable(obj: Worm): Table = {
    // Traverse all the fields of the class
    val rows = obj.getClass.getDeclaredFields.map { f =>
      f.setAccessible(true)
      if(classOf[Worm].isAssignableFrom(f.getType)) {
        // It's another custom class that extends Worm
        Row(f.getName, objectToTable(f.get(obj).asInstanceOf[Worm]), ForeignKey())
      //} else if() {
        // It's a collection
        
      } else {
        // It's something else, assume primitive
        Row(f.getName, f.get(obj))
      }
    }.toList
    Table(obj.getClass.getSimpleName, rows, obj)
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
          RowStructure("id", pkType),
          RowStructure(fieldName(containerName), fkType),
          RowStructure(f.getName, fkType))) ::
            classToStructure(Manifest.classType(seqType))
      } else {
        // Assume it's a list of primitives - create a separate table for them
        return List(TableStructure(containerName + seqType.getSimpleName + "s", List(
          RowStructure("id", pkType),
          RowStructure(fieldName(containerName), fkType),
          RowStructure(fieldName(seqType.getSimpleName), columnType(commonName(seqType.getSimpleName))))))
      }
    }
    var tables = List[TableStructure]()
    val rows = RowStructure("id", pkType) :: classManifest[T].erasure.getDeclaredFields.map { f =>
      f.setAccessible(true)
      if(classOf[Worm].isAssignableFrom(f.getType)) {
        // Relation
        tables = tables ++ classToStructure(Manifest.classType(f.getType))
        Some(RowStructure(f.getName, fkType))
      } else if(classOf[java.util.Collection[_]].isAssignableFrom(f.getType) ||
                classOf[Seq[_]].isAssignableFrom(f.getType)) {
        // Sequence collection
        tables = tables ++ unwrapSeq(classManifest[T].erasure.getSimpleName, f)
        None
      } else {
        Some(RowStructure(f.getName, columnType(commonName(f.getType.getSimpleName))))
      }
    }.flatten.toList
    TableStructure(classManifest[T].erasure.getSimpleName, rows) :: tables
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
