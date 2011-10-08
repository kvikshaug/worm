package no.kvikshaug.worm

import java.sql._
import scala.collection.mutable.ListBuffer

class SQL(val driver: String, val jdbcURL: String) {

  Class.forName(driver)
  private val connection = DriverManager.getConnection(jdbcURL)

  def disconnect = connection.close

  def create(tables: List[TableStructure], print: Boolean) = {
    tables foreach { table =>
      val query = String.format("create table if not exists %s (%s);",
        table.name, commaize(table.columns.map(r => "`" + r.name + "` " + r.typeName)))
      print match {
        case true  => println(query)
        case false => connection.prepareStatement(query).execute
      }
    }
  }

  def select[T](table: String, sql: String): List[List[AnyRef]] = {
    val query = String.format("select * from '%s' %s;", table, sql)
    val statement = connection.prepareStatement(query)
    statement.execute
    val resultset = statement.getResultSet()
    var rows = ListBuffer[List[AnyRef]]()
    while(resultset.next()) {
      val row = for(i <- 1 to resultset.getMetaData().getColumnCount())
        yield resultset.getObject(i)
      rows += row.toList
    }
    rows.toList
  }

  def insert(tables: List[Table], deps: List[Dependency]): List[ID] = {
    val ids = ListBuffer[ID]()
    tables foreach { table =>
      // For each column dependency, replace the value with the parent ID
      table.rows foreach { row =>
        val columns = row.columns.map { column =>
          if(column.depends.isDefined) {
            Column(column.name, column.depends.get.parent.wormDbId.get, None)
          } else {
            column
          }
        }
        val key = performInsert(table.name, columns.map(_.name), columns.map(_.value))
        ids += ID(table.name, key)
        table.obj.wormDbId = Some(key)
      }
    }

    // After all tables are inserted, IDs will be filled out, so insert dependency data
    def insertDep(dep: Dependency, values: List[Any]) = {
      var orderCount = 0
      values foreach { value =>
        ids += ID(dep.tableName, performInsert(
          dep.tableName, List("order", dep.parentName, dep.childName), List(orderCount, dep.parent.wormDbId.get, value)
        ))
        orderCount += 1
      }
    }

    deps foreach { dep => dep match {
      case SingleWormDependency(_, child, _, _, _) => insertDep(dep, List(child.wormDbId.get))
      case WormDependency(_, children, _, _, _) => insertDep(dep, children.map(c => c.wormDbId.get).toList)
      case PrimitiveDependency(_, children, _, _, _) => insertDep(dep, children.toList)
      }
    }
    ids.toList
  }

  def update(ids: List[ID], tables: List[Table], deps: List[Dependency]): List[ID] = {
    // Just delete everything, and reinsert it
    ids.foreach(id => performDelete(id.tableName, id.id))
    insert(tables, deps)
  }

  def delete(tables: List[Table], deps: List[Dependency]): Unit = {
    // Delete all the dependencies
    deps foreach { dep =>
      if(dep.parent.wormDbId.isDefined) {
        // It may not be defined if the user updated the field without calling update().
        // (Which they shouldn't.)
        performDelete(dep.tableName, dep.parent.wormDbId.get, dep.parentName)
      }
    }
    // Delete all the tables
    tables foreach { table =>
      if(table.obj.wormDbId.isDefined) {
        // It may not be defined if the user updated the field without calling update().
        // (Which they shouldn't.)
        performDelete(table.name, table.obj.wormDbId.get)
        table.obj.wormDbId = None
      }
    }
  }

  private def performInsert(table: String, names: Seq[Any], values: Seq[Any]): Long = {
    val query = String.format("insert into '%s' (%s) values (%s);",
      table,
      commaize("`id`" :: names.map("`" + _ + "`").toList),
      commaize("NULL" :: values.map("'" + _ + "'").toList)
    )
    val statement = connection.prepareStatement(query)
    statement.execute
    val key = statement.getGeneratedKeys
    if(!key.next) {
      throw new SQLException("The SQL driver didn't throw any exception, but it also said that no " +
        "keys were inserted!\nNot really sure how that happened, or what I (the ORM) can do about it.")
    }
    key.getLong(1)
  }

  private def performDelete(tableName: String, id: Long, columnName: String = "id") = {
    val query = String.format("delete from '%s' where `%s`='%s';", tableName, columnName, id.toString)
    connection.prepareStatement(query).execute
  }

  private def commaize(list: List[_ <: Any]): String = list match {
    case List()  => ""
    case List(x) => x.toString
    case _       => list(0) + ", " + commaize(list.tail)
  }
}
