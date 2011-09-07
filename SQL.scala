import java.sql._

case class Row(id: Long, values: List[AnyRef])

object SQL {

  var connection: Option[Connection] = None

  def connect(driver: String, jdbcURL: String) = {
    Class.forName("org.sqlite.JDBC")
    connection = Some(DriverManager.getConnection(jdbcURL))
  }

  def select(table: String) = {
    // todo - sanitize table String - SQL injection
    if(connection isEmpty) {
      throw new Exception("TODO some better exception")
    }
    val statement = connection.get.prepareStatement("select * from `" + table + "`")
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
