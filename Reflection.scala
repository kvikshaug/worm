import java.lang.Class
import java.lang.reflect.{Field => JVMField}

case class Tester(foo: String, hmm: Int) extends ORM

case class Field(name: String, value: String)

object ORM {
  def get = println("you got!")
}

class ORM {
  private var id: Option[Long] = None
  private def fields = this.getClass.getDeclaredFields.map(f => retrieveField(f)).flatten.toList

  @throws(classOf[IllegalStateException])
  def insert() = {
    if(id.isDefined) {
      throw new IllegalStateException("This object already exists in the database, its ID is: " +
        id.get + ".")
    }
    val fields = this.fields
    println(String.format("insert into `%s` (%s) values (%s)",
            this.getClass.getSimpleName,
            commaize(fields.map(_.name)),
            commaize(fields.map(_.value))))
  }

  @throws(classOf[IllegalStateException])
  def update() = {
    if(id.isEmpty) {
      throw new IllegalStateException("This object doesn't exist in the database!")
    }
    val fields = this.fields
    println(String.format("update `%s` set (%s) where id=`" + id.get + "`",
            this.getClass.getSimpleName,
            fields.map(f => f.name + "=" + f.value + ", ")))
  }

  /* This is based on conventions.
     For java classes, we assume that a field 'foo' will have use a 'getFoo' method.
     For scala classes, we assume that each field will have a corresponding method with
     the same name. We don't know which is which, we just test for both. */
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

  def commaize(list: List[_ <: Any]): String = list match {
    case List()  => ""
    case List(x) => x.toString
    case _       => list(0) + ", " + commaize(list.tail)
  }
}

object Runner {
  def main(args: Array[String]) {
    val orm = Tester("hello", 2)
    orm.insert
    ORM.get
  }
}
