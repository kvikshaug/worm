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
    obj.wormDbId = Some(row.get.id)
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
      obj.wormDbId = Some(row.id)
      obj
    }
    return objects
  }
}

class Worm {
  private val c = this.getClass
  private def fields = c.getDeclaredFields.map { f =>
      f.setAccessible(true)
      Field(f.getName, f.get(this))
  }.toList

  // the id needs to be set by some other classes, so this needs to be publicly
  // accessible. hence, it can clash with names from the subclass namespace. :(
  // any ideas for improvements?
  var wormDbId: Option[Long] = None

  def insert() = {
    if(Worm.sql isEmpty) {
      throw new NotConnectedException("You need to connect to the database before using it.")
    }
    if(wormDbId.isDefined) {
      throw new IllegalStateException("This object already exists in the database, its ID is: " +
        wormDbId.get + ".")
    }
    val key = Worm.sql.get.insert(c.getSimpleName, fields)
    if(key isEmpty) {
      throw new SQLException("The SQL driver didn't throw any exception, but it also said that no keys were inserted!\n" +
      "Not really sure how that happened, or what I (the ORM) can do about it.")
    } else {
      wormDbId = Some(key.get)
      wormDbId.get
    }
  }

  def update() = {
    if(Worm.sql isEmpty) {
      throw new NotConnectedException("You need to connect to the database before using it.")
    }
    if(wormDbId.isEmpty) {
      throw new IllegalStateException("This object doesn't exist in the database!")
    }
    Worm.sql.get.update(c.getSimpleName, wormDbId.get, fields)
  }

  def delete() = {
    if(Worm.sql isEmpty) {
      throw new NotConnectedException("You need to connect to the database before using it.")
    }
    if(wormDbId isEmpty) {
      throw new IllegalStateException("This object doesn't exist in the database!")
    }
    Worm.sql.get.delete(c.getSimpleName, wormDbId.get)
  }
}
