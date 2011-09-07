import java.sql._

case class Row(id: Long, values: List[AnyRef])

object SQL {

  var connection: Option[Connection] = None

  def connect(driver: String, jdbcURL: String) = {
    Class.forName(driver)
    connection = Some(DriverManager.getConnection(jdbcURL))
  }

  def selectAll(table: String) = {
    // todo - sanitize table String - SQL injection
    executeSelect(connection.get.prepareStatement("select * from `" + table + "`"))
  }

  def selectID(table: String, id: Long) = {
    // todo - sanitize table String - SQL injection
    executeSelect(connection.get.prepareStatement("select * from `" + table + "` where id=`" + id + "`"))
  }

  def executeSelect(statement: PreparedStatement) = {
    if(connection isEmpty) {
      throw new Exception("TODO some better exception")
    }
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
