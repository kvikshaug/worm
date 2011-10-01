package no.kvikshaug.worm

import java.lang.reflect.{Field => JVMField}
import java.sql.SQLException

import scala.collection.JavaConverters._

case class ID(tableName: String, id: Long)

object Worm {
  var sql: Option[SQL] = None
  def connect(db: String, driver: String, jdbcURL: String) {
    Converter.setDb(db)
    sql = Some(new SQL(driver, jdbcURL))
  }
  def disconnect {
    if(sql isDefined) { sql.get.disconnect; sql = None }
  }

  def create[T <: Worm: ClassManifest]: Unit = {
    if(sql isEmpty) {
      throw new NotConnectedException("You need to connect to the database before using it.")
    }
    val structure = Converter.classToStructure[T]()
    sql.get.create(structure)
  }

  def get[T <: Worm: ClassManifest]: List[T] = getWith[T]("")

  def getWith[T <: Worm: ClassManifest](sqlString: String): List[T] = {
    if(sql isEmpty) {
      throw new NotConnectedException("You need to connect to the database before using it.")
    }
    val rows = sql.get.select(classManifest[T].erasure.getSimpleName, sqlString)
    Converter.tableToObject[T](rows)
  }
}

object JWorm {
  def create[T <: Worm](c: Class[_ <: Worm]): Unit = { Worm.create(Manifest.classType(c)) }    
  def getWith[T <: Worm](c: Class[_ <: Worm], sql: String): java.util.List[T] =
    Worm.getWith[T](sql)(Manifest.classType(c)).asJava
  def get[T <: Worm](c: Class[_ <: Worm]): java.util.List[T] = Worm.get[T](Manifest.classType(c)).asJava
}

class Worm {
  private val c = this.getClass

  // the id needs to be set by some other classes, so this needs to be publicly
  // accessible. hence, it can clash with names from the subclass namespace. :(
  // any ideas for improvements?
  var wormDbId: Option[Long] = None
  var wormDbIds: Option[List[ID]] = None

  def insert(): Unit = {
    if(Worm.sql isEmpty) {
      throw new NotConnectedException("You need to connect to the database before using it.")
    }
    if(wormDbId.isDefined) {
      throw new IllegalStateException("This object already exists in the database, its ID is: " +
        wormDbId.get + ".")
    }
    val (tables, deps) = Converter.objectToTables(this)
    val ids = Worm.sql.get.insert(tables, deps)
    wormDbIds = Some(ids)
  }

  def update(): Unit = {
    if(Worm.sql isEmpty) {
      throw new NotConnectedException("You need to connect to the database before using it.")
    }
    if(wormDbId.isEmpty) {
      throw new IllegalStateException("This object doesn't exist in the database!")
    }
    val (tables, deps) = Converter.objectToTables(this)
    val ids = Worm.sql.get.update(wormDbIds.get, tables, deps)
    wormDbIds = Some(ids)
  }

  def delete(): Unit = {
    if(Worm.sql isEmpty) {
      throw new NotConnectedException("You need to connect to the database before using it.")
    }
    if(wormDbId isEmpty) {
      throw new IllegalStateException("This object doesn't exist in the database!")
    }
    val (tables, deps) = Converter.objectToTables(this)
    Worm.sql.get.delete(tables, deps)
  }
}
