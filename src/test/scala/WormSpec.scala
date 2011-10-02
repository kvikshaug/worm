import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import no.kvikshaug.worm._

case class Foo(var bar: Bar, var d: Double, f: Float, l: Long, i: Int, s: Short, byte: Byte, boolean: Boolean, c: Char, str: String) extends Worm
case class Bar(str: String) extends Worm

case class Fob(bab: Bab) extends Worm
case class Bab(fob: Fob) extends Worm

class WormSpec extends Spec with ShouldMatchers {

  describe("A Worm, when initially disconnected, should") {

    describe("throw NotConnectedException") {

      it("when calling get") {
        evaluating { Worm.get[Worm] } should produce [NotConnectedException]
      }

      it("when calling create") {
        evaluating { Worm.create[Worm] } should produce [NotConnectedException]
      }
    }

    it("successfully connect") {
      Worm.connect("SQLite", "org.sqlite.JDBC", "jdbc:sqlite:test.db")
    }

    describe("when connected") {
      it("create a table") {
        Worm.create[Foo]
      }

      it("stack overflow when creating two classes referencing each other") {
        evaluating { Worm.create[Fob] } should produce [StackOverflowError]
      }

      val foo = Foo(Bar("Bar1"), 0.3529, 0.155f, 149l, 42, 13, 5.toByte, true, 'å', "Hello world")

      describe("throw IllegalStateException") {
        it("when updating an uninserted new Foo") {
          evaluating { foo.update } should produce [IllegalStateException]
        }

        it("when deleting an uninserted new Foo") {
          evaluating { foo.delete } should produce [IllegalStateException]
        }
      }

      it("insert a new Foo") {
        foo.insert
      }

      it("not insert an already inserted Foo or its Bar") {
        evaluating { foo.insert } should produce [IllegalStateException]
        evaluating { foo.bar.insert } should produce [IllegalStateException]
      }

      it("get back that Foo") {
        val list = Worm.get[Foo]
        list.size should be === 1
        list(0) should be === foo
      }

      it("update the Foo") {
        foo.d = 3.141592653589
        foo.update
      }

      it("delete and update the Bar") {
        foo.bar.delete
        foo.bar = Bar("Bar2")
        foo.update
      }

      it("get only that Bar back") {
        val list = Worm.get[Bar]
        list.size should be === 1
        list(0) should be === Bar("Bar2")
      }

      it("get Foos when searching with constraints") {
        val list = Worm.getWith[Foo]("where d > '3'")
        list.size should be === 1
        list(0) should be === foo
        list(0).bar should be === Bar("Bar2")
      }

      it("delete the foo") {
        foo.delete
      }
    }

    it("successfully disconnect") {
      Worm.disconnect
    }

  }
}
