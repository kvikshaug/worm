public class JTester extends ORM {
	private String far;
	private int baz;

	public JTester(String far, int baz) {
		this.far = far;
		this.baz = baz;
	}

	public String far() {
		return far;
	}

	public int getBaz() {
		return baz;
	}
}
