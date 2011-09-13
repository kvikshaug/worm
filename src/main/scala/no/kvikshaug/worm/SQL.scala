package no.kvikshaug.worm

import java.sql._
import java.lang.reflect.Constructor

case class Row(id: Long, values: List[AnyRef])

class SQL(val driver: String, val jdbcURL: String) {

  Class.forName(driver)
  private var connection = DriverManager.getConnection(jdbcURL)

  def disconnect = connection.close

  // The time in seconds to wait for the database operation used to validate the connection to complete.
  private val timeout = 10

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
    val query = String.format("update '%s' set %s where id='%s';",
      table,
      dualize(fields),
      id.toString)
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

  private def executeSelect[T](statement: PreparedStatement, constructor: Constructor[T]) = {
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
    if(t == classOf[Double] || t == classOf[java.lang.Double]) {
      obj.asInstanceOf[java.lang.Double]
    } else if(t == classOf[Float] || t == classOf[java.lang.Float]) {
      obj.asInstanceOf[java.lang.Double].floatValue
    } else if(t == classOf[Long] || t == classOf[java.lang.Long]) {
      obj.asInstanceOf[java.lang.Integer].longValue
    } else if(t == classOf[Int] || t == classOf[java.lang.Integer]) {
      obj.asInstanceOf[java.lang.Integer]
    } else if(t == classOf[Short] || t == classOf[java.lang.Short]) {
      obj.asInstanceOf[java.lang.Integer].shortValue
    } else if(t == classOf[Byte] || t == classOf[java.lang.Byte]) {
      obj.asInstanceOf[java.lang.Integer].byteValue
    } else if(t == classOf[Boolean] || t == classOf[java.lang.Boolean]) {
      java.lang.Boolean.parseBoolean(obj.asInstanceOf[java.lang.String])
    } else if(t == classOf[Char] || t == classOf[java.lang.Character]) {
      obj.asInstanceOf[java.lang.String].charAt(0)
    } else if(t == classOf[String]) {
      obj.asInstanceOf[java.lang.String]
    }
  }

  private def commaize(list: List[_ <: Any]): String = list match {
    case List()  => ""
    case List(x) => x.toString
    case _       => list(0) + ", " + commaize(list.tail)
  }

  private def dualize(fields: List[Field]): String = {
    val sb = new StringBuilder
    for(i <- 0 until fields.size) {
      if(i == fields.size - 1) {
        sb.append("'").append(fields(i).name).append("'='").append(fields(i).value).append("'")
      } else {
        sb.append("'").append(fields(i).name).append("'='").append(fields(i).value).append("',")
      }
    }
    sb.toString
  }
}
