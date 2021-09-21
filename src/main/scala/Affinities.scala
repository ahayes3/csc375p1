import scala.collection.mutable

object Affinities {
  val takesFrom = new mutable.HashMap[Int,Int]()

  def apply(a: Array[Array[Int]],f:Int,f2:Int): Unit = {
    for(i <- a) {
      takesFrom.put(i(0),i(1))
    }

  }

  def getTotal(factory:Factory):Double = { //affinity from station s to its input
    val remaining = new mutable.HashMap[Station,Boolean]
    val stations = factory.floor.flatten.filter(_.nonEmpty).map(_.get).toList//.foreach(p => capRemaining.addOne(p,p.capacity))
    stations.foreach(remaining.addOne(_,true))
    (for(i <- stations) yield {
      val input = closestThing(i,factory,remaining)
      if(input.isEmpty)
        0
      else {
        val dist = factory.distance(i, input.get)
        val capRatio = if (input.get.capacity / i.capacity > 1) 1 else input.get.capacity / i.capacity
        //println(s"Dist: $dist\n")
        (i.capacity.toFloat * capRatio) * (1 / dist)
      }
    }).sum
  }

  def closestThing(s:Station,factory:Factory,remaining: mutable.HashMap[Station,Boolean]):Option[Station] = {
    val pos = factory.find(s)
    if(!takesFrom.keySet.contains(s.flavor)) {
      return None
    }
    var fMatch = factory.flavorPositions(takesFrom(s.flavor)).to(mutable.ArrayBuffer)
    var fStations = fMatch.map(p =>factory.floor(p._1)(p._2).get)
    if(fMatch.isEmpty || fStations.filter(remaining(_)).isEmpty)
      return None

    val removeIndices = new mutable.ArrayBuffer[Int]()
    for(i <- fStations.indices) {
      if(!remaining(fStations(i)))
        removeIndices.addOne(i)
    }
    removeIndices.sorted(Ordering.Int.reverse).foreach(p => {fMatch.remove(p);fStations.remove(p)})

    val closest = fMatch.reduceLeft((a,b) => if(Factory.distFormula(pos,a) < Factory.distFormula(pos,b)) a else b) //finds closest
    val st = factory.floor(closest._1)(closest._2)
    remaining(factory.floor(closest._1)(closest._2).get) = false
    st
  }


}