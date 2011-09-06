import java.lang.Class
import java.lang.reflect.{Field => JVMField}

case class Tester(foo: String, hmm: Int) extends ORM

case class Field(name: String, value: String)

object ORM {
  def get = println("you got!")
}

class ORM {
  class State(saved: Boolean)

  val __ormstate__ = new State(false)

  def create() = {
    val fields = this.getClass.getDeclaredFields.map(f => retrieveField(f)).filter(_.isDefined).map(_.get)
    println(String.format("insert into `%s` (%s) values (%s)",
            this.getClass.getSimpleName,
            commaize(fields.map(_.name)),
            commaize(fields.map(_.value))))
  }

  // mulig jeg må omstrukturere denne, er ikke 100% sikker på alle use cases.
  // men for now returnerer vi bare en tuple med strings; navn og value.toString.
  /* This is based on conventions.
     For java classes, we assume that a field 'foo' will have use a 'getFoo' method.
     For scala classes, we assume that each field will have a corresponding method with
     the same name. */
  def retrieveField(field: JVMField): Option[Field] = {
    def asGetter(s: String) = "get" + s(0).toUpper + s.tail
    // check for getter
    val getter = this.getClass.getMethods.find(_.getName == asGetter(field.getName))
    if(getter isDefined) {
      return Some(Field(field.getName, getter.get.invoke(this).toString))
    }

    // check for method with same name
    val method = this.getClass.getMethods.find(_.getName == field.getName)
    if(method isDefined) {
      return Some(Field(field.getName, method.get.invoke(this).toString))
    }
    return None
  }

  def commaize(array: Array[_ <: Any]): String = commaize(array.toList)
  def commaize(list: List[_ <: Any]): String = list match {
    case List()  => ""
    case List(x) => x.toString
    case _       => list(0) + ", " + commaize(list.tail)
  }
}

object Runner {
  def main(args: Array[String]) {
    val orm = Tester("hello", 2)
    orm.create
    ORM.get
  }
}
