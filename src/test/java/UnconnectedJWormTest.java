import static org.junit.Assert.*;
import org.junit.*;

import no.kvikshaug.worm.*;

public class UnconnectedJWormTest {

    JFoo jfoo = new JFoo(new JBar(42), 0.3, 0.155F, 149L, 42, (short)49,
        (byte)5, true, 'å', "hello world");

    @Test(expected=NotConnectedException.class) public void getBeforeConnect() {
        JWorm.get(JFoo.class);
    }

    @Test(expected=NotConnectedException.class) public void insertBeforeConnect() {
        jfoo.insert();
    }

    @Test(expected=NotConnectedException.class) public void updateBeforeConnect() {
        jfoo.update();
    }

    @Test(expected=NotConnectedException.class) public void deleteBeforeConnect() {
        jfoo.delete();
    }
}
