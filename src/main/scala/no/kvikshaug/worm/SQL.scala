package no.kvikshaug.worm

import java.sql._
import java.lang.reflect.Constructor

class SQL(val dbRaw: String, val driver: String, val jdbcURL: String) {

  val db = dbRaw.toLowerCase

  db match {
    case "sqlite" =>
    case _        => throw new UnsupportedDatabaseException("Worm doesn't support the '"+db+"' DB engine yet.")
  }
  Class.forName(driver)
  private var connection = DriverManager.getConnection(jdbcURL)

  def disconnect = connection.close

  def create(table: String, columns: List[Column]) {
    // Format the columns into a string
    val sb = new StringBuilder
    sb.append("id " + pkType + ", ")
    for(i <- 0 until columns.size) {
      sb.append(columns(i).name).append(" ").append(columnType(columns(i).fieldType))
      if(i != columns.size - 1) {
        sb.append(", ")
      }
    }
    for(i <- 0 until columns.size) {
      if(columns(i).fk.isDefined) {
        sb.append(", FOREIGN KEY(").append(columns(i).name).append(") REFERENCES ")
          .append(columns(i).fk.get.otherTable).append("(id)")
      }
    }

    val query = String.format("CREATE TABLE IF NOT EXISTS %s (%s);", table, sb.toString)
    val statement = connection.prepareStatement(query)
    statement.execute
  }

  def select[T](table: String, sql: String, constructor: Constructor[T]): List[List[AnyRef]] = {
    // todo - sanitize table String AND whereClause String - SQL injection
    val statement = connection.prepareStatement(String.format("select * from '%s' %s;", table, sql))
    statement.execute
    val resultset = statement.getResultSet()
    var rows = List[List[AnyRef]]()
    while(resultset.next()) {
      val types = constructor.getParameterTypes
      val values = for(i <- 2 to resultset.getMetaData().getColumnCount())
        yield jvmType(types(i-2), resultset.getObject(i))
      rows = (resultset.getLong(1).asInstanceOf[java.lang.Long] :: values.toList.asInstanceOf[List[AnyRef]]) :: rows
    }
    rows
  }

  def insert(table: Table): Long = {
    // todo - sanitize table String AND all fields - SQL injection
    val inserts = table.rows.map { row =>
      row.attribute match {
        case ForeignKeyNew() => Row(row.name, insert(row.value.asInstanceOf[Table]).asInstanceOf[java.lang.Long], Primitive())
        case Primitive()  => row
      }
    }
    val query = String.format("insert into '%s' (%s) values (%s);",
        table.name,
        commaize(inserts.map("'" + _.name + "'")),
        commaize(inserts.map("'" + _.value + "'")))
    val statement = connection.prepareStatement(query)
    statement.execute
    val key = statement.getGeneratedKeys
    if(!key.next) {
      throw new SQLException("The SQL driver didn't throw any exception, but it also said that no " +
        "keys were inserted!\nNot really sure how that happened, or what I (the ORM) can do about it.")
    }
    table.obj.wormDbId = Some(key.getLong(1))
    table.obj.wormDbId.get
  }

  def update(table: Table): Unit = {
    val rows = table.rows.map { row =>
      row.attribute match {
        case ForeignKeyNew() =>
          // Check if the object has an ID
          if(row.value.asInstanceOf[Table].obj.wormDbId.isDefined) {
            update(row.value.asInstanceOf[Table])
            None
          } else {
            Some(Row(row.name, insert(row.value.asInstanceOf[Table]).asInstanceOf[java.lang.Long], Primitive()))
          }
        case Primitive() => Some(row)
      }
    }.flatten
    val sb = new StringBuilder
    for(i <- 0 until rows.size) {
      sb.append("'").append(rows(i).name).append("'='").append(rows(i).value).append("'")
      if(i != rows.size - 1) {
        sb.append(",")
      }
    }
    val query = String.format("update '%s' set %s where id='%s';", table.name, sb.toString, table.rows(0).value)
    connection.prepareStatement(query).execute
  }

  def delete(table: Table): Unit = {
    table.rows.foreach { row =>
      row.attribute match {
        case ForeignKeyNew() =>
          // If the object has an ID, delete that too
          if(row.value.asInstanceOf[Table].obj.wormDbId.isDefined) {
            delete(row.value.asInstanceOf[Table])
          }
        case _ =>
      }
    }
    val query = String.format("delete from '%s' where id='%s';", table.name,
      table.obj.wormDbId.get.toString)
    connection.prepareStatement(query).execute
    table.obj.wormDbId = None
  }

  private def commaize(list: List[_ <: Any]): String = list match {
    case List()  => ""
    case List(x) => x.toString
    case _       => list(0) + ", " + commaize(list.tail)
  }

  /* DB-engine specific functions */

  // Cast and if necessary conevrt objects to their applicable types
  private def jvmType(t: Class[_], obj: Any) = db match {
    case "sqlite" => jvmTypeSQLite(t, obj)
  }

  private def jvmTypeSQLite(t: Class[_], obj: Any) = {
    if(classOf[Worm].isAssignableFrom(t)) {
      // Relation
      val constructor = t.getConstructors()(0)
      val rows = select(t.getSimpleName, "where id='" + obj.toString + "'", constructor)
      val inner = constructor.newInstance(rows(0).tail: _*).asInstanceOf[Worm]
      inner.wormDbId = Some(rows(0).head.asInstanceOf[Long])
      inner
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
