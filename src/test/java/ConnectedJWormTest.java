import static org.junit.Assert.*;
import org.junit.*;

import java.util.List;

import no.kvikshaug.worm.*;

public class ConnectedJWormTest {

    @Before public void connect() {
        Worm.connect("SQLite", "org.sqlite.JDBC", "jdbc:sqlite:test.db");
        // TODO clean the db? so we're sure it's empty incase some test failed at some point?
    }

    @Test(expected=IllegalStateException.class) public void updateBeforeInsert() {
        JFoo jfoo = new JFoo(new JBar(42), 0.3, 0.155F, 149L, 42, (short)49, (byte)5,
            true, 'å', "hello world");
        jfoo.update();
    }

    @Test public void createInsertUpdateGetDelete() {
        JFoo jfoo = new JFoo(new JBar(42), 0.3, 0.155F, 149L, 42, (short)49, (byte)5,
            true, 'å', "hello world");
        // Create the table
        JWorm.create(JFoo.class);

        // Insert the object
        jfoo.insert();

        // Delete and update the related object
        jfoo.getJBar().delete();
        jfoo.setJBar(new JBar(15));
        jfoo.update();

        // Retrieve the stored object
        List<JFoo> list = JWorm.get(JFoo.class);

        // Check that it's the only one and that it has the same ID
        assertTrue(list.size() == 1);
        assertEquals(list.get(0).wormDbId().get(), jfoo.wormDbId().get());

        // Retrieve the stored object with a check
        //Option<JFoo> t = JWorm.getWith(JFoo.class, "where l < 150");

        // Check that it's the only one and that it has the same ID
        //assertTrue(list.size() == 1);
        //assertEquals(list.get(0).wormDbId().get(), jfoo.wormDbId().get());

        // Delete the object
        jfoo.delete();
    }

    @After public void disconnect() {
        Worm.disconnect();
    }
}
