import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import no.kvikshaug.worm._

case class Foo(var text: String) extends Worm

class WormSpec extends Spec with ShouldMatchers {

  describe("A Worm, when initially disconnected") {

    describe("should throw NotConnectedException") {

      it("when calling get") {
        evaluating { Worm.get[Worm] } should produce [NotConnectedException]
      }

      it("when calling create") {
        evaluating { Worm.create[Worm] } should produce [NotConnectedException]
      }
    }

    it("should successfully connect") {
      Worm.connect("SQLite", "org.sqlite.JDBC", "jdbc:sqlite:test.db")
    }

    describe("when connected") {
      it("should create a table") {
        Worm.create[Foo]
      }

      val foo = new Foo("hello world")
      it("should save a new Foo") {
        foo.insert
      }

      it("should get back that Foo") {
        val list = Worm.get[Foo]
        list.size should be === 1
        list(0) should be === foo
      }

      it("should update the Foo") {
        foo.text = "goodbye world"
        foo.update
      }

      it("should get a Foo when searching for 'goodbye world'") {
        val res = Worm.getWith[Foo]("where text='goodbye world'")
        res.isDefined should be === true
        res.get should be === foo
      }

      it("should delete the foo") {
        foo.delete
      }
    }

    it("should successfully disconnect") {
      Worm.disconnect
    }

  }
}
