package no.kvikshaug.worm.examples;

import no.kvikshaug.worm.Worm;

public class JTester extends Worm {
	private String far;
	private int baz;

	public JTester(String far, int baz) {
		this.far = far;
		this.baz = baz;
	}

	public String getFar() {
		return far;
	}

	public int getBaz() {
		return baz;
	}
}
