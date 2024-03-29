package no.kvikshaug.worm

import java.util.concurrent.locks.ReentrantLock
import scala.collection.JavaConverters._

case class ID(tableName: String, id: Long)

object Worm {
  val lock = new ReentrantLock
  var sql: Option[SQL] = None
  def connect(db: String, driver: String, jdbcURL: String) {
    Converter.setDb(db)
    sql = Some(new SQL(driver, jdbcURL))
  }
  def disconnect {
    if(sql isDefined) { sql.get.disconnect; sql = None }
  }

  def create[T <: Worm: ClassManifest]: Unit = createTable[T](false)
  def printSchema[T <: Worm: ClassManifest]: Unit = createTable[T](true)

  private def createTable[T <: Worm: ClassManifest](print: Boolean): Unit = {
    if(sql isEmpty) {
      throw new NotConnectedException("You need to connect to the database before using it.")
    }
    lock.lock
    try {
      val structure = Converter.classToStructure[T]()
      sql.get.create(structure, print)
    } finally {
      lock.unlock
    }
  }

  def get[T <: Worm: ClassManifest]: List[T] = getWith[T]("")

  def getWith[T <: Worm: ClassManifest](sqlString: String): List[T] = {
    if(sql isEmpty) {
      throw new NotConnectedException("You need to connect to the database before using it.")
    }
    lock.lock
    try {
      val rows = sql.get.select(classManifest[T].erasure.getSimpleName, sqlString)
      Converter.tableToObject[T](rows)._1
    } finally {
      lock.unlock
    }
  }
}

object JWorm {
  def create[T <: Worm](c: Class[_ <: Worm]): Unit = { Worm.create(Manifest.classType(c)) }
  def printSchema[T <: Worm](c: Class[_ <: Worm]): Unit = { Worm.printSchema(Manifest.classType(c)) }
  def getWith[T <: Worm](c: Class[_ <: Worm], sql: String): java.util.List[T] =
    Worm.getWith[T](sql)(Manifest.classType(c)).toBuffer.asJava
  def get[T <: Worm](c: Class[_ <: Worm]): java.util.List[T] = getWith(c, "")
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
    Worm.lock.lock
    try {
      val (tables, deps) = Converter.objectToTables(this)
      val ids = Worm.sql.get.insert(tables, deps)
      wormDbIds = Some(ids)
    } finally {
      Worm.lock.unlock
    }
  }

  def update(): Unit = {
    if(Worm.sql isEmpty) {
      throw new NotConnectedException("You need to connect to the database before using it.")
    }
    if(wormDbId.isEmpty) {
      throw new IllegalStateException("This object doesn't exist in the database!")
    }
    Worm.lock.lock
    try {
      val (tables, deps) = Converter.objectToTables(this)
      val ids = Worm.sql.get.update(wormDbIds.get, tables, deps)
      wormDbIds = Some(ids)
    } finally {
      Worm.lock.unlock
    }
  }

  def delete(): Unit = {
    if(Worm.sql isEmpty) {
      throw new NotConnectedException("You need to connect to the database before using it.")
    }
    if(wormDbId isEmpty) {
      throw new IllegalStateException("This object doesn't exist in the database!")
    }
    Worm.lock.lock
    try {
      val (tables, deps) = Converter.objectToTables(this)
      Worm.sql.get.delete(tables, deps)
    } finally {
      Worm.lock.unlock
    }
  }
}
