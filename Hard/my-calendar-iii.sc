import java.util
import scala.collection.JavaConverters._

class MyCalendarThree() {

  val map = new util.TreeMap[Int, Int]

  def book(start: Int, end: Int): Int = {
    map.put(start, map.getOrDefault(start, 0) + 1)
    map.put(end, map.getOrDefault(end, 0) - 1)

    map.values.asScala.foldLeft((0, 0)){
      case ((a, b), c) => (Math.max(a, b + c), b + c)
    }._1
  }

}
