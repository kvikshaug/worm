import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import no.kvikshaug.worm._

case class Foo(var bar: Bar, var d: Double, f: Float, l: Long, i: Int, s: Short, byte: Byte, boolean: Boolean, c: Char, str: String) extends Worm
case class Bar(str: String) extends Worm

case class Fob(bab: Bab) extends Worm
case class Bab(fob: Fob) extends Worm

// Lists
case class LFoo(var bars: List[LBar], val bazs: List[LBaz], var omg: LOmg, strs: List[String]) extends Worm
case class LBar(val bazs: List[LBaz]) extends Worm
case class LBaz(string: String) extends Worm
case class LOmg(int: Int) extends Worm

case class A(b: B) extends Worm
case class B(c: C) extends Worm
case class C() extends Worm

case class UPPERSEQ(var LOWER: Seq[lowerseq]) extends Worm
case class lowerseq(val I: Int) extends Worm

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
        val list = Worm.get[Foo]
        list.size should be === 1
        list(0).bar should be === Bar("Bar2")
      }

      it("just update the Bar without deleting") {
        foo.bar = Bar("Bar3")
        foo.update
      }

      it("still get only that Bar back") {
        val list = Worm.get[Foo]
        list.size should be === 1
        list(0).bar should be === Bar("Bar3")
      }

      it("get Foos when searching with constraints") {
        val list = Worm.getWith[Foo]("where d > '3'")
        list.size should be === 1
        list(0) should be === foo
        list(0).bar should be === Bar("Bar3")
      }

      it("delete the foo") {
        foo.delete
        val list = Worm.get[Foo]
        list.size should be === 0
      }

      /* Lists */

      it("create an LFoo (of lists)") {
        Worm.create[LFoo]
      }

      val lomg = LOmg(42)
      val lbar1 = LBar(List(LBaz("en"), LBaz("to"), LBaz("tre")))
      val lbar2 = LBar(List(LBaz("fire"), LBaz("fem"), LBaz("seks")))
      val lfoo = LFoo(List(lbar1, lbar2), List(LBaz("direkte baz")), lomg, List("hello", "world"))

      it("insert a new LFoo") {
        lfoo.insert
      }

      val newLBar = LBar(List(LBaz("ny bar"), LBaz("med bazs")))
      val newOmg = LOmg(15)

      it("update the LFoo") {
        lfoo.bars = List(newLBar)
        lfoo.omg = newOmg
        lfoo.update
      }

      it("get the updated LFoo back") {
        val list = Worm.get[LFoo]
        list.size should be === 1
        list(0) should be === lfoo
        list(0).bars should be === List(newLBar)
        list(0).omg should be === newOmg
      }

      it("only get the new LBar back") {
        val list = Worm.get[LBar]
        list.size should be === 1
        list(0) should be === newLBar
      }

      it("delete the LFoo") {
        lfoo.delete
        val list = Worm.get[LFoo]
        list.size should be === 0
      }

      /* Empty lists */

      val elomg = LOmg(0)
      val newElomg = LOmg(2)
      val elfoo = LFoo(List(), List(), lomg, List())

      it("insert an LFoo with empty lists") {
        elfoo.insert
      }

      it("update an LFoo with empty lists") {
        elfoo.omg = newElomg
        elfoo.update
      }

      it("get that updated empty-listed LFoo back") {
        val list = Worm.get[LFoo]
        list.size should be === 1
        list(0) should be === elfoo
        list(0).bars should be === List()
      }

      it("delete the empty-listed LFoo") {
        elfoo.delete
      }

      /* Deleting relation */

      val c = C()
      val b = B(c)
      val a = A(b)

      it("create and insert a new ABC") {
        Worm.create[A]
        a.insert
      }

      it("delete the B in ABC and getting A; throws exception") {
        b.delete
        evaluating { val aList1 = Worm.get[A] } should produce [InconsistentStateException]
        val bList1 = Worm.get[B]
        val cList1 = Worm.get[C]
        bList1.size should be === 0
        cList1.size should be === 0
        a.wormDbId.isEmpty should be === false
        b.wormDbId.isEmpty should be === true
        c.wormDbId.isEmpty should be === true
      }

      it("but still update the A, and have it intact with no ghost rows") {
        a.update
        val aList2 = Worm.get[A]
        val bList2 = Worm.get[B]
        val cList2 = Worm.get[C]
        aList2.size should be === 1
        bList2.size should be === 1
        cList2.size should be === 1
        a.wormDbId.isEmpty should be === false
        b.wormDbId.isEmpty should be === false
        c.wormDbId.isEmpty should be === false
      }

      it("delete the ABC") {
        a.delete
        val aList = Worm.get[A]
        val bList = Worm.get[B]
        val cList = Worm.get[C]
        aList.size should be === 0
        bList.size should be === 0
        cList.size should be === 0
        a.wormDbId.isEmpty should be === true
        b.wormDbId.isEmpty should be === true
        c.wormDbId.isEmpty should be === true
      }

      it("create, insert, update, get, delete with wrong cases") {
        Worm.create[UPPERSEQ]
        val l1 = Seq(lowerseq(5), lowerseq(6))
        val l2 = Seq(lowerseq(40), lowerseq(42))
        val u = UPPERSEQ(l1)
        u.insert
        u.LOWER = l2
        u.update
        val list = Worm.get[UPPERSEQ]
        list.size should be === 1
        list(0).LOWER should be === l2
        u.delete
      }
    }

    it("successfully disconnect") {
      Worm.disconnect
    }

  }
}
