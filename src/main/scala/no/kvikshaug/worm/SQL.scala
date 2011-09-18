package no.kvikshaug.worm

import java.sql._
import java.lang.reflect.Constructor

case class Row(id: Long, values: List[AnyRef])

class SQL(val driver: String, val jdbcURL: String) {

  Class.forName(driver)
  private var connection = DriverManager.getConnection(jdbcURL)

  def disconnect = connection.close

  def selectAll[T](table: String, constructor: Constructor[T]) = {
    // todo - sanitize table String - SQL injection
    executeSelect(connection.prepareStatement(String.format("select * from '%s';", table)), constructor)
  }

  def selectID[T](table: String, id: Long, constructor: Constructor[T]) = {
    // todo - sanitize table String - SQL injection (also, it's not used)
    returnQuery(String.format("select * from '%s' where id='%s';", table, id.toString), constructor)
  }

  def selectWhere[T](table: String, whereClause: String, constructor: Constructor[T]) = {
    // todo - sanitize table String AND whereClause String - SQL injection
    returnQuery(String.format("select * from '%s' where %s;", table, whereClause), constructor)
  }

  def insert(table: String, fields: List[Field]) = {
    // todo - sanitize table String AND all fields - SQL injection
    val query = String.format("insert into '%s' (%s) values (%s);",
        table,
        commaize(fields.map("'" + _.name + "'")),
        commaize(fields.map("'" + _.value + "'")))
    val statement = connection.prepareStatement(query)
    statement.execute
    val key = statement.getGeneratedKeys
    if(key.next) {
      Some(key.getObject(1).asInstanceOf[Int].toLong)
    } else {
      None
    }
  }

  def update(table: String, id: Long, fields: List[Field]) = {
    val sb = new StringBuilder
    for(i <- 0 until fields.size) {
      sb.append("'").append(fields(i).name).append("'='").append(fields(i).value).append("'")
      if(i != fields.size - 1) {
        sb.append(",")
      }
    }
    val pairs = sb.toString

    val query = String.format("update '%s' set %s where id='%s';", table, pairs, id.toString)
    val statement = connection.prepareStatement(query)
    statement.execute
    statement.getUpdateCount
  }

  def delete(table: String, id: Long) = {
    val query = String.format("delete from '%s' where id='%s';",
      table,
      id.toString)
    val statement = connection.prepareStatement(query)
    statement.execute
    statement.getUpdateCount
  }

  private def returnQuery[T](query: String, constructor: Constructor[T]) = {
    val rows = executeSelect(connection.prepareStatement(query), constructor)
    if(rows.size == 1)
      Some(rows(0))
    else
      None
  }

  private def executeSelect[T](statement: PreparedStatement, constructor: Constructor[T]): List[Row] = {
    statement.execute
    val resultset = statement.getResultSet()
    var rows = List[Row]()
    while(resultset.next()) {
      val types = constructor.getParameterTypes
      val values = for(i <- 2 to resultset.getMetaData().getColumnCount())
        yield cast(types(i-2), resultset.getObject(i))
      rows = Row(resultset.getObject(1).asInstanceOf[Int].toLong, values.toList.asInstanceOf[List[AnyRef]]) :: rows
    }
    rows
  }

  // Cast and if necessary conevrt objects to their applicable types
  private def cast(t: Class[_], obj: Any) = {
    if(classOf[Worm].isAssignableFrom(t)) {
      // Relation
      val constructor = t.getConstructors()(0)
      val row = selectWhere(t.getSimpleName, "id='" + obj.toString + "'", constructor)
      val inner = constructor.newInstance(row.get.values: _*).asInstanceOf[Worm]
      inner.wormDbId = Some(row.get.id)
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

  private def commaize(list: List[_ <: Any]): String = list match {
    case List()  => ""
    case List(x) => x.toString
    case _       => list(0) + ", " + commaize(list.tail)
  }
}
