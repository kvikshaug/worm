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
    executeSelect(connection.prepareStatement("select * from `" + table + "`"))
  }

  def selectID(table: String, id: Long) = {
    // todo - sanitize table String - SQL injection
    executeSelect(connection.prepareStatement("select * from `" + table + "` where id=`" + id + "`"))
  }

  private def executeSelect(statement: PreparedStatement) = {
    statement.execute
    val resultset = statement.getResultSet()
    println(resultset.getMetaData().getColumnCount())
    var rows = List[Row]()
    while(resultset.next()) {
      val values = for(i <- 2 to resultset.getMetaData().getColumnCount())
        yield resultset.getObject(i).asInstanceOf[AnyRef]
      rows = Row(resultset.getObject(1).asInstanceOf[Int].toLong, values.toList) :: rows
    }
    rows
  }

}
