import scala.collection.mutable.ArrayBuffer
import scala.util.Random
class Factory(val x: Int,val y:Int,val floor: Array[Array[Option[Station]]],val stations:IndexedSeq[Station]) { //Chromosome for purpose of GA
  var affinity:Double = -1
  override def clone():Factory = {
    val newFloor = Array.fill[Option[Station]](x,y)(None)
    var newStations = IndexedSeq[Station]()
    for(i <- floor.indices) {
      for(j <-floor(i).indices) {
        floor(i)(j) match {
          case Some(value) => newFloor(i)(j) = Some({
            val cpy = value.copy()
            cpy.position = value.position.copy()
            newStations :+= cpy
            cpy
          })
          case None =>
        }
      }
    }
    val out = new Factory(x,y,newFloor,newStations)
    out.affinity = this.affinity
    out
  }



  def swap(p1:Position, p2:Position): Unit = { //swap mutation
    val a = floor(p1.x)(p1.y)
    val b = floor(p2.x)(p2.y)
    if(p1.x == -1 || p1.y == -1 || p2.x == -1 || p2.y == -1)
      throw new IndexOutOfBoundsException()
    floor(p1.x)(p1.y) = floor(p2.x)(p2.y)
    floor(p2.x)(p2.y) = a
    if(a.nonEmpty)
      a.get.position = Position(p2.x,p2.y)
    if(b.nonEmpty)
      b.get.position = Position(p1.x,p1.y)
  }

  def find(s: Station): Position = {
    for (i <- floor.indices) {
      for (j <- floor(i).indices) {
        if (floor(i)(j).nonEmpty && floor(i)(j).get == s)
          return Position(i, j)
      }
    }
    throw new NoSuchElementException()
  }

  def distance(s1: Station, s2: Station): Double = {
    val pos1 = s1.position
    val pos2 = s2.position

    Factory.distFormula(pos1, pos2)
  }

  def closestThing(s1: Station, flavor: Int): Station = { //can't think of a fitting name
    val pos = s1.position
    val stations = flavorPositions(flavor)
    if (stations.isEmpty)
      throw new IndexOutOfBoundsException(s"Flavor $flavor not found")
    val closest = stations.reduceLeft((a, b) => if (Factory.distFormula(pos, a) < Factory.distFormula(pos, b)) a else b) //if there are multiple at same distance picks later in the array
    floor(closest._1)(closest._2).get
  }


  def flavorPositions(flavor: Int): Seq[Position] = {
    var positions = Seq[Position]()
    for (i <- floor.indices) {
      for (j <- floor(i).indices) {
        if (floor(i)(j).nonEmpty && floor(i)(j).get.flavor == flavor)
          positions :+= Position(i, j)
      }
    }
    positions
  }

  def similarity(f:Factory): Double = {
    stations.map((p) => {
      val p1 = p.position
      val p2 = f.find(p)
      Factory.distFormula(p1,Position(p2._1,p2._2))
    }).sum
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case fact:Factory => {
        val a = obj.asInstanceOf[Factory]
        for (i <- floor.indices) {
          for (j <- floor(i).indices) {
            if (floor(i)(j) != a.floor(i)(j))
              return false
          }
        }
        true
      }
      case x:AnyVal => false
    }
  }

  override def toString: String = {
    val a = new StringBuilder("")
    for(i <- 0 until y) {
      for(j <- 0 until x) {
        a ++= s"[${if(floor(j)(i).isEmpty) " " else floor(j)(i).get.flavor}]"
      }
      a++="\n"
    }
    a.toString()
  }
}
object Factory {
  def distFormula(a:(Int,Int),b:(Int,Int)): Double = {
    math.hypot(a._1.toDouble - b._1.toDouble,a._2.toDouble - b._2.toDouble)
  }
  def distFormula(a:Position,b:Position): Double = {
    math.hypot(a.x.toDouble - b.x.toDouble,a.y.toDouble - b.y.toDouble)
  }
  def randomFloor(st: List[Station],x:Int,y:Int): (Array[Array[Option[Station]]],IndexedSeq[Station]) = {
    val arr = Array.fill[Option[Station]](x,y)(None)
    var stations = IndexedSeq[Station]()
    val positions = (for(i <- 0 until x) yield {
      (for(j <- 0 until y) yield {
        j
      }).map(p => (i,p))
    }).flatten.toBuffer
    for (s <- st) {
      val index = Random.between(0,positions.length)
      val pos = positions(index)
      positions.remove(index)
      val cpy = s.copy()
      cpy.position = Position(pos._1,pos._2)
      stations :+= cpy
      arr(pos._1)(pos._2) = Some(cpy)
    }
    (arr,stations)
  }
}