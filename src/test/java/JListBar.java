import no.kvikshaug.worm.Worm;

import java.util.List;

public class JListBar extends Worm {
   private List<JBar> jbars;

   public JListBar(List<JBar> jbars) {
      this.jbars = jbars;
   }

   public List<JBar> getJBars() {
       return this.jbars;
   }
}
