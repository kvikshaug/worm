case class Tester(var foo: String, hmm: Int) extends Worm

object Runner {
  def main(args: Array[String]) {
    Worm.connect("org.sqlite.JDBC", "jdbc:sqlite:test.db")
    val orm = Tester("Ny dude", 9000)
    println("Should be an insert ID: " + orm.insert)
    val test2 = Worm.getWhere[Tester]("foo='OMG'")
    println("Should be OMG: " + test2)
    val list = Worm.get[Tester]
    list foreach println
    Worm.disconnect
  }
}
