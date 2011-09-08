import java.util.List;
import scala.Option;

public class JRunner {
	public static void main(String[] args) {
		//Tester t = new Tester("hello world", 42);
		//t.create();

		ORM.connect("org.sqlite.JDBC", "jdbc:sqlite:test.db");
		//JTester jt = new JTester("goodbye world", 1337);
		//jt.insert();
		//jt.update();
		List<JTester> list = ORM.getJava(JTester.class);
		for(JTester jt : list) {
			System.out.println(jt.toString());
		}
		Option<JTester> test = ORM.getJavaWhere(JTester.class, "far='goodbye world'");
		if(test.isDefined()) {
			System.out.println("JTester(" + test.get().getFar() + ", " + test.get().getBaz() + ")");
		} else {
			System.out.println("Not defined.");
		}
	}
}
