import no.kvikshaug.worm.Worm;

public class JFoo extends Worm {
    private JBar jbar;
    private double d;
    private Float f;
    private long l;
    private int i;
    private short s;
    private byte b;
    private boolean bool;
    private char c;
    private String str;

    public JFoo(JBar jbar, Double d, Float f, Long l, Integer i, Short s, Byte b, Boolean bool, Character c, String str) {
        this.jbar = jbar;
        this.d = d;
        this.f = f;
        this.l = l;
        this.i = i;
        this.s = s;
        this.b = b;
        this.bool = bool;
        this.c = c;
        this.str = str;
    }
    public JBar getJBar() {
        return jbar;
    }
    public void setJBar(JBar jbar) {
        this.jbar = jbar;
    }
    public double getD() {
        return d;
    }
    public Float getF() {
        return f;
    }
    public long getL() {
        return l;
    }
    public int getI() {
        return i;
    }
    public short getS() {
        return s;
    }
    public byte getB() {
        return b;
    }
    public boolean getBool() {
        return bool;
    }
    public char getC() {
        return c;
    }
    public String getStr() {
        return str;
    }
}
