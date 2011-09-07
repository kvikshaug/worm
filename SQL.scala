import java.sql._

case class Row(id: Long, values: List[AnyRef])

class SQL(val driver: String, val jdbcURL: String) {

  Class.forName(driver)
  private var connection = DriverManager.getConnection(jdbcURL)

  def disconnect = connection.close

  // The time in seconds to wait for the database operation used to validate the connection to complete.
  private val timeout = 10

  def selectAll(table: String) = {
    // todo - sanitize table String - SQL injection
    executeSelect(connection.prepareStatement(String.format("select * from '%s';", table)))
  }

  def selectID(table: String, id: Long) = {
    // todo - sanitize table String - SQL injection (also, it's not used)
    returnQuery(String.format("select * from '%s' where id='%s';", table, id.toString))
  }

  def selectWhere(table: String, whereClause: String) = {
    // todo - sanitize table String AND whereClause String - SQL injection
    returnQuery(String.format("select * from '%s' where %s;", table, whereClause))
  }

  def insert(table: String, fields: List[Field]) {
    // todo - sanitize table String AND all fields - SQL injection
    val query = String.format("insert into '%s' (%s) values (%s);",
        table,
        commaize(fields.map("'" + _.name + "'")),
        commaize(fields.map("'" + _.value + "'")))
    val statement = connection.prepareStatement(query)
    statement.execute
  }

  private def returnQuery(query: String) = {
    val rows = executeSelect(connection.prepareStatement(query))
    if(rows.size == 1)
      Some(rows(0))
    else
      None
  }

  private def executeSelect(statement: PreparedStatement) = {
    statement.execute
    val resultset = statement.getResultSet()
    var rows = List[Row]()
    while(resultset.next()) {
      val values = for(i <- 2 to resultset.getMetaData().getColumnCount())
        yield resultset.getObject(i).asInstanceOf[AnyRef]
      rows = Row(resultset.getObject(1).asInstanceOf[Int].toLong, values.toList) :: rows
    }
    rows
  }

  private def commaize(list: List[_ <: Any]): String = list match {
    case List()  => ""
    case List(x) => x.toString
    case _       => list(0) + ", " + commaize(list.tail)
  }
}
