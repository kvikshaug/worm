package no.kvikshaug.worm

import java.lang.reflect.{Field => JVMField}
import java.sql.SQLException

import scala.collection.JavaConverters._

case class Field(name: String, value: Any)

object Worm {
  var sql: Option[SQL] = None
  def connect(driver: String, jdbcURL: String) {
    sql = Some(new SQL(driver, jdbcURL))
  }

  def disconnect { if(sql isDefined) { sql.get.disconnect; sql = None } }

  def getJavaWhere[T <: Worm](c: Class[_ <: Worm], whereClause: String): Option[T] =
    getWhere[T](whereClause)(Manifest.classType(c))

  def getWhere[T <: Worm: ClassManifest](whereClause: String): Option[T] = {
    if(sql isEmpty) {
      throw new NotConnectedException("You need to connect to the database before using it.")
    }
    val constructor = classManifest[T].erasure.getConstructors()(0)
    val row = sql.get.selectWhere(classManifest[T].erasure.getSimpleName, whereClause, constructor)
    if(row isEmpty) {
      return None
    }
    val obj = constructor.newInstance(row.get.values: _*).asInstanceOf[T]
    obj.__setid__(row.get.id)
    Some(obj)
  }

  def getJava[T <: Worm](c: Class[_ <: Worm]): java.util.List[T] = get[T](Manifest.classType(c)).asJava
  def get[T <: Worm: ClassManifest]: List[T] = {
    if(sql isEmpty) {
      throw new NotConnectedException("You need to connect to the database before using it.")
    }
    val constructor = classManifest[T].erasure.getConstructors()(0)
    val rows = sql.get.selectAll(classManifest[T].erasure.getSimpleName, constructor)
    val objects = rows.map { row =>
      val obj = constructor.newInstance(row.values: _*).asInstanceOf[T]
      obj.__setid__(row.id)
      obj
    }
    return objects
  }
}

class Worm {
  private val c = this.getClass
  private var id: Option[Long] = None
  private def fields = c.getDeclaredFields.map(f => retrieveField(f)).flatten.toList

  // the id needs to be set by the Worm companion object, so this needs to be a public
  // method. hence, it can clash with names from the superclass namespace. :(
  // any ideas for improvements?
  def __setid__(id: Long) = this.id = Some(id)

  def insert() = {
    if(Worm.sql isEmpty) {
      throw new NotConnectedException("You need to connect to the database before using it.")
    }
    if(id.isDefined) {
      throw new IllegalStateException("This object already exists in the database, its ID is: " +
        id.get + ".")
    }
    val fields = this.fields
    val key = Worm.sql.get.insert(c.getSimpleName, fields)
    if(key isEmpty) {
      throw new SQLException("The SQL driver didn't throw any exception, but it also said that no keys were inserted!\n" +
      "Not really sure how that happened, or what I (the ORM) can do about it.")
    } else {
      id = Some(key.get)
      id
    }
  }

  def update() = {
    if(Worm.sql isEmpty) {
      throw new NotConnectedException("You need to connect to the database before using it.")
    }
    if(id.isEmpty) {
      throw new IllegalStateException("This object doesn't exist in the database!")
    }
    Worm.sql.get.update(c.getSimpleName, id.get, fields)
  }

  @throws(classOf[IllegalStateException])
  def delete() = {
    if(Worm.sql isEmpty) {
      throw new NotConnectedException("You need to connect to the database before using it.")
    }
    if(id isEmpty) {
      throw new IllegalStateException("This object doesn't exist in the database!")
    }
    Worm.sql.get.delete(c.getSimpleName, id.get)
  }

  /* This is based on conventions.
     For java classes, we assume that a field 'foo' will have a 'getFoo' method.
     For scala classes, we assume that each field will have a corresponding method with
     the same name. We don't know which is which, we just test for both. */
  private def retrieveField(field: JVMField): Option[Field] = {
    def asGetter(s: String) = "get" + s(0).toUpper + s.tail
    // check for getter
    val getter = c.getMethods.find(_.getName == asGetter(field.getName))
    if(getter isDefined) {
      return Some(Field(field.getName, getter.get.invoke(this)))
    }

    // check for method with same name
    val method = c.getMethods.find(_.getName == field.getName)
    if(method isDefined) {
      return Some(Field(field.getName, method.get.invoke(this)))
    }
    return None
  }
}
