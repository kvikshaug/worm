import static org.junit.Assert.*;
import org.junit.*;

import java.util.List;
import java.util.ArrayList;

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
        list = JWorm.getWith(JFoo.class, "where l < 150");

        // Check that it's the only one and that it has the same ID
        assertTrue(list.size() == 1);
        assertEquals(list.get(0).wormDbId().get(), jfoo.wormDbId().get());

        // Delete the object
        jfoo.delete();
    }

    @Test public void createInsertUpdateDeleteList() {
        List<JBar> jbarList1 = new ArrayList<JBar>();
        jbarList1.add(new JBar(1));
        jbarList1.add(new JBar(2));

        List<JBar> jbarList2 = new ArrayList<JBar>();
        jbarList2.add(new JBar(4));
        jbarList2.add(new JBar(5));

        List<JListBar> jListBars = new ArrayList<JListBar>();
        jListBars.add(new JListBar(jbarList1));
        jListBars.add(new JListBar(jbarList2));

        List<JBar> jbarList3 = new ArrayList<JBar>();
        jbarList3.add(new JBar(50));
        jbarList3.add(new JBar(60));

        List<JListBar> newJListBars = new ArrayList<JListBar>();
        newJListBars.add(new JListBar(jbarList3));

        JListFoo j = new JListFoo(jListBars);

        // Create and insert a 3-depth list
        JWorm.create(JListFoo.class);
        j.insert();

        // Update the 2nd relation
        j.setJListBars(newJListBars);
        j.update();

        // Get a list, verify its contents
        List<JListFoo> list = JWorm.get(JListFoo.class);
        assertTrue(list.size() == 1);
        assertEquals(list.get(0).getJListBars().get(0).getJBars().get(0).getI(), 50);

        // Delete the object
        j.delete();
    }

    @After public void disconnect() {
        Worm.disconnect();
    }
}
