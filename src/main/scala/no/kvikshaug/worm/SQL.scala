package no.kvikshaug.worm

import java.sql._
import scala.collection.mutable.ListBuffer

class SQL(val driver: String, val jdbcURL: String) {

  Class.forName(driver)
  private val connection = DriverManager.getConnection(jdbcURL)

  def disconnect = connection.close

  def create(tables: List[TableStructure]) = {
    tables foreach { table =>
      val query = String.format("create table if not exists %s (%s);",
        table.name, commaize(table.columns.map(r => r.name + " " + r.typeName)))
      val statement = connection.prepareStatement(query)
      statement.execute
    }
  }

  def select[T](table: String, sql: String): List[List[AnyRef]] = {
    // todo - sanitize table String AND whereClause String - SQL injection
    val statement = connection.prepareStatement(String.format("select * from '%s' %s;", table, sql))
    statement.execute
    val resultset = statement.getResultSet()
    var rows = List[List[AnyRef]]()
    while(resultset.next()) {
      val row = for(i <- 1 to resultset.getMetaData().getColumnCount())
        yield resultset.getObject(i)
      rows = row.toList :: rows
    }
    rows
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
    deps foreach { dep => dep match {
      case SingleWormDependency(_, _) =>
      case WormDependency(parent, children, tableName, parentName, childName) =>
        children foreach { child =>
          val key = performInsert(tableName,
            List(parentName, childName),
            List(parent.wormDbId.get, child.wormDbId.get))
          ids += ID(tableName, key)
        }
      case PrimitiveDependency(parent, children, tableName, parentName, childName) =>
        children foreach { child =>
          val key = performInsert(tableName,
            List(parentName, childName),
            List(parent.wormDbId.get, child))
          ids += ID(tableName, key)
        }
      }
    }
    ids.toList
  }

  private def performInsert(table: String, names: Seq[Any], values: Seq[Any]): Long = {
    val query = String.format("insert into '%s' (%s) values (%s);",
      table,
      commaize("'id'" :: names.map("'" + _ + "'").toList),
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

  def update(ids: List[ID], tables: List[Table], deps: List[Dependency]): List[ID] = {
    // Just delete everything, and reinsert it
    ids foreach { id =>
      val query = String.format("delete from '%s' where id='%s';", id.tableName, id.id.toString)
      connection.prepareStatement(query).execute
    }
    insert(tables, deps)
  }

  def delete(table: Table): Unit = {
    table.rows.foreach { row =>
      row.attribute match {
        case ForeignKey() =>
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
}
