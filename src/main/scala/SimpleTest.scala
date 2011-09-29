import java.util.ArrayList
import org.junit.Test
  
class SimpleTest {
  @Test
  def testEmptyCollection() {
    val collection = new ArrayList()
    org.junit.Assert.assertTrue(collection.isEmpty())
  }
}
