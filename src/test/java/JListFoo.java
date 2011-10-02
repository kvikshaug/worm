import no.kvikshaug.worm.Worm;

import java.util.List;

public class JListFoo extends Worm {
   private List<JListBar> jListBars;

   public JListFoo(List<JListBar> jListBars) {
      this.jListBars = jListBars;
   }

   public List<JListBar> getJListBars() {
       return this.jListBars;
   }

   public void setJListBars(List<JListBar> jListBars) {
       this.jListBars = jListBars;
   }
}
