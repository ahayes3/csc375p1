import scala.collection.mutable.ArrayBuffer
import scala.util.Random
class Factory(val x: Int,val y:Int,val flavors: Int,stations:List[Station]) { //Chromosome for purpose of GA
  val floor = Array.fill[Option[Station]](x, y)(None)
  placeRandom(stations)

  def swap(x1: Int, y1: Int, x2: Int, y2: Int): Unit = { //swap mutation
    val a = floor(x1)(y1)
    floor(x1)(y1) = floor(x2)(y2)
    floor(x2)(y2) = a
  }

  def find(s: Station): (Int, Int) = {
    for (i <- floor.indices) {
      for (j <- floor(i).indices) {
        if (floor(i)(j).nonEmpty && floor(i)(j).get == s)
          return (i, j)
      }
    }
    return (-1, -1)
  }

  def distance(s1: Station, s2: Station): Double = {
    val pos1 = find(s1)
    val pos2 = find(s2)

    Factory.distFormula(pos1, pos2)
  }

  def closestThing(s1: Station, flavor: Int): Station = { //can't think of a fitting name
    val pos = find(s1)
    val stations = flavorPositions(flavor)
    if (stations.isEmpty)
      throw new IndexOutOfBoundsException(s"Flavor $flavor not found")
    val closest = stations.reduceLeft((a, b) => if (Factory.distFormula(pos, a) < Factory.distFormula(pos, b)) a else b) //if there are multiple at same distance picks later in the array
    floor(closest._1)(closest._2).get
  }


  def flavorPositions(flavor: Int): Seq[(Int, Int)] = {
    var positions = Seq[(Int, Int)]()
    for (i <- floor.indices) {
      for (j <- floor(i).indices) {
        if (floor(i)(j).nonEmpty && floor(i)(j).get.flavor == flavor)
          positions :+= (i, j)
      }
    }
    positions
  }


  private def placeRandom(stations: List[Station]): Unit = {
    val positions = (for(i <- 0 until x) yield {
      (for(j <- 0 until y) yield {
        j
      }).map(p => (i,p))
    }).flatten.toBuffer
    for (s <- stations) {
      val index = Random.between(0,positions.length)
      val pos = positions(index)
      positions.remove(index)
      floor(pos._1)(pos._2) = Some(s)
    }
  }
}
object Factory {
  def distFormula(a:(Int,Int),b:(Int,Int)): Double = {
    math.hypot(a._1.toDouble - b._1.toDouble,a._2.toDouble - b._2.toDouble)
  }
}