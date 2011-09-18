package no.kvikshaug.worm

import java.lang.reflect.{Field => JVMField}
import java.sql.SQLException

import scala.collection.JavaConverters._

case class Field(name: String, value: Any)
case class Column(name: String, fieldType: String, fk: Option[ForeignKey])
case class ForeignKey(otherTable: String)

object Worm {
  var sql: Option[SQL] = None
  def connect(driver: String, jdbcURL: String) {
    sql = Some(new SQL(driver, jdbcURL))
  }

  def disconnect { if(sql isDefined) { sql.get.disconnect; sql = None } }

  def createJava[T <: Worm](c: Class[_ <: Worm]): Unit = { create(Manifest.classType(c)) }

  def create[T <: Worm: ClassManifest]: Unit = {
    if(sql isEmpty) {
      throw new NotConnectedException("You need to connect to the database before using it.")
    }
    val columns = classManifest[T].erasure.getDeclaredFields.map { f =>
      f.setAccessible(true)
      if(classOf[Worm].isAssignableFrom(f.getType)) {
        // Relation
        Worm.create(Manifest.classType(f.getType))
        Column(f.getName, "int", Some(ForeignKey(f.getType.getSimpleName)))
      } else {
        Column(f.getName,
          f.getType.getSimpleName.replaceAll("(?i)integer", "int").replaceAll("(?i)character", "char").toLowerCase,
          None)
      }
    }.toList
    sql.get.create(classManifest[T].erasure.getSimpleName, columns)
  }

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

  // the id needs to be set by some other classes, so this needs to be publicly
  // accessible. hence, it can clash with names from the subclass namespace. :(
  // any ideas for improvements?
  var wormDbId: Option[Long] = None

  def insert(): Long = {
    if(Worm.sql isEmpty) {
      throw new NotConnectedException("You need to connect to the database before using it.")
    }
    if(wormDbId.isDefined) {
      throw new IllegalStateException("This object already exists in the database, its ID is: " +
        wormDbId.get + ".")
    }
    val fields = c.getDeclaredFields.map { f =>
      f.setAccessible(true)
      if(classOf[Worm].isAssignableFrom(f.getType)) {
        Field(f.getName, f.get(this).asInstanceOf[Worm].insert)
      } else {
        Field(f.getName, f.get(this))
      }
    }.toList
    val key = Worm.sql.get.insert(c.getSimpleName, fields)
    if(key isEmpty) {
      throw new SQLException("The SQL driver didn't throw any exception, but it also said that no keys were inserted!\n" +
      "Not really sure how that happened, or what I (the ORM) can do about it.")
    } else {
      wormDbId = Some(key.get)
      wormDbId.get
    }
  }

  def update(): Unit = {
    if(Worm.sql isEmpty) {
      throw new NotConnectedException("You need to connect to the database before using it.")
    }
    if(wormDbId.isEmpty) {
      throw new IllegalStateException("This object doesn't exist in the database!")
    }
    val fields = c.getDeclaredFields.map { f =>
      f.setAccessible(true)
      if(classOf[Worm].isAssignableFrom(f.getType)) {
        // Relation
        val instance = f.get(this).asInstanceOf[Worm]
        if(instance.wormDbId.isDefined) {
          // The related object is already inserted, so update it
          instance.update
          Some(Field(f.getName, instance.wormDbId.get))
        } else {
          // The related object isn't defined, it was changed after insertion, so re-insert it
          Some(Field(f.getName, instance.insert))
        }
      } else {
        Some(Field(f.getName, f.get(this)))
      }
    }.flatten.toList
    Worm.sql.get.update(c.getSimpleName, wormDbId.get, fields)
  }

  def delete(): Unit = {
    if(Worm.sql isEmpty) {
      throw new NotConnectedException("You need to connect to the database before using it.")
    }
    if(wormDbId isEmpty) {
      throw new IllegalStateException("This object doesn't exist in the database!")
    }
    val fields = c.getDeclaredFields.foreach { f =>
      f.setAccessible(true)
      if(classOf[Worm].isAssignableFrom(f.getType)) {
        // Relation
        val instance = f.get(this).asInstanceOf[Worm]
        if(instance.wormDbId.isDefined) {
          f.get(this).asInstanceOf[Worm].delete
        }
      }
    }
    Worm.sql.get.delete(c.getSimpleName, wormDbId.get)
  }
}
