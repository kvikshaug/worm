case class Tester(foo: String, hmm: Int) extends ORM

object Runner {
  def main(args: Array[String]) {
    val orm = Tester("hello", 2)
    orm.insert
    val t = ORM.get[Tester]
    println(t)
    println(t.getClass)
  }
}
