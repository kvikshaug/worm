package no.kvikshaug.worm.examples;

import no.kvikshaug.worm.Worm;

import java.util.List;
import scala.Option;

public class JRunner {
	public static void main(String[] args) {
		//Tester t = new Tester("hello world", 42);
		//t.create();

		Worm.connect("org.sqlite.JDBC", "jdbc:sqlite:test.db");
		//JTester jt = new JTester("goodbye world", 1337);
		//jt.insert();
		//jt.update();
		List<JTester> list = Worm.getJava(JTester.class);
		for(JTester jt : list) {
			System.out.println(jt.toString());
		}
		Option<JTester> test = Worm.getJavaWhere(JTester.class, "far='goodbye world'");
		if(test.isDefined()) {
			System.out.println("JTester(" + test.get().getFar() + ", " + test.get().getBaz() + ")");
		} else {
			System.out.println("Not defined.");
		}
	}
}
