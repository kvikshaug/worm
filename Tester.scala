case class Tester(var foo: String, hmm: Int) extends ORM

object Runner {
  def main(args: Array[String]) {
    ORM.connect("org.sqlite.JDBC", "jdbc:sqlite:test.db")
    val orm = Tester("Ny dude", 9000)
    println("Should be an insert ID: " + orm.insert)
    val test2 = ORM.getWhere[Tester]("foo='OMG'")
    println("Should be OMG: " + test2)
    val list = ORM.get[Tester]
    list foreach println
    ORM.disconnect
  }
}
