package no.kvikshaug.worm

import java.lang.reflect.{Field => JVMField}
//import java.lang.reflect.Field

class Attribute
case class ForeignKeyNew() extends Attribute // Refactor to ForeignKey when that class is removed
case class Primitive() extends Attribute

case class Table(name: String, rows: List[Row], obj: Worm)
case class Row(name: String, value: AnyRef, attribute: Attribute = Primitive())

object Transformation {

  /* Does not verify that a connection to SQL has been
     performed, so do that before calling this method */
  def objectToSql(obj: Worm): Table = {
    // Traverse all the fields of the class
    val rows = obj.getClass.getDeclaredFields.map { f =>
      f.setAccessible(true)
      if(classOf[Worm].isAssignableFrom(f.getType)) {
        // It's another custom class that extends Worm
        Row(f.getName, objectToSql(f.get(obj).asInstanceOf[Worm]), ForeignKeyNew())
      //} else if() {
        // It's a collection
        
      } else {
        // It's something else, assume primitive
        Row(f.getName, f.get(obj))
      }
    }.toList
    Table(obj.getClass.getSimpleName, rows, obj)
  }
}
