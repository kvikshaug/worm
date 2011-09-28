package no.kvikshaug.worm

import java.lang.reflect.Constructor

class Attribute
case class ForeignKeyNew() extends Attribute // Refactor to ForeignKey when that class is removed
case class Primitive() extends Attribute

case class Table(name: String, rows: List[Row], obj: Worm)
case class Row(name: String, value: AnyRef, attribute: Attribute = Primitive())

case class TableStructure(name: String, rows: List[RowStructure])
case class RowStructure(name: String, typeName: String)

/* This class does not verify that a connection to SQL has been
   performed, so do that before calling using methods here */
object Converter {

  def objectToTable(obj: Worm): Table = {
    // Traverse all the fields of the class
    val rows = obj.getClass.getDeclaredFields.map { f =>
      f.setAccessible(true)
      if(classOf[Worm].isAssignableFrom(f.getType)) {
        // It's another custom class that extends Worm
        Row(f.getName, objectToTable(f.get(obj).asInstanceOf[Worm]), ForeignKeyNew())
      //} else if() {
        // It's a collection
        
      } else {
        // It's something else, assume primitive
        Row(f.getName, f.get(obj))
      }
    }.toList
    Table(obj.getClass.getSimpleName, rows, obj)
  }

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

  def classToStructure[T <: Worm: ClassManifest]: List[TableStructure] = {
    var tables = List[TableStructure]()
    val rows = RowStructure("id", pkType) :: classManifest[T].erasure.getDeclaredFields.map { f =>
      f.setAccessible(true)
      if(classOf[Worm].isAssignableFrom(f.getType)) {
        // Relation
        tables = tables ++ classToStructure(Manifest.classType(f.getType))
        RowStructure(f.getName, fkType)
      //} else if() {
        // Collection
          
      } else {
        RowStructure(f.getName, columnType(
          f.getType.getSimpleName.replaceAll("(?i)integer", "int")
           .replaceAll("(?i)character", "char").toLowerCase))
      }
    }.toList
    TableStructure(classManifest[T].erasure.getSimpleName, rows) :: tables
  }

  /* DB-engine specific functions */

  // Cast and if necessary convert objects to their applicable types
  private def jvmType(obj: Any, t: Class[_]) = Worm.sql.get.db match {
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
  private def pkType = Worm.sql.get.db match {
    case "sqlite" => "INTEGER PRIMARY KEY"
  }

  // Foreign key type
  private def fkType = Worm.sql.get.db match {
    case "sqlite" => "INTEGER"
  }

  // Column types
  private def columnType(fieldType: String) = Worm.sql.get.db match {
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
