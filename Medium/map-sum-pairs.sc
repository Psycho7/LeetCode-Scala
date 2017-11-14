import scala.collection.mutable

class MapSum() {

  /** Initialize your data structure here. */
  var m = mutable.Map.empty[String, Int]

  def insert(key: String, `val`: Int) {
    m(key) = `val`
  }

  def sum(prefix: String): Int = {
    m.filterKeys(_.startsWith(prefix)).values.sum
  }

}

/**
  * Your MapSum object will be instantiated and called as such:
  * var obj = new MapSum()
  * obj.insert(key,`val`)
  * var param_2 = obj.sum(prefix)
  */