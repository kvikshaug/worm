case class Tester(foo: String, hmm: Int) extends ORM

object Runner {
  def main(args: Array[String]) {
    ORM.connect("org.sqlite.JDBC", "jdbc:sqlite:test.db")
    val orm = Tester("hello", 2)
    orm.insert
    val test1 = ORM.get[Tester](1)
    println("Should be row 1: " + test1)
    val list = ORM.get[Tester]
    list foreach println
    ORM.disconnect
  }
}
