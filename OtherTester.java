public class OtherTester extends ORM {
	private String far;
	private int baz;

	public OtherTester(String far, int baz) {
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
