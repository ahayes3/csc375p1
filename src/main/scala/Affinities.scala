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
    val stations = factory.floor.flatten.filterNot(p => p==null).toList//.foreach(p => capRemaining.addOne(p,p.capacity))
    stations.foreach(remaining.addOne(_,true))
    (for(i <- stations) yield {
      val input = closestThing(i,factory,remaining)
      val dist = factory.distance(i,input)
      val capRatio = if(i.capacity/input.capacity > 1) 1 else i.capacity/input.capacity
      (input.capacity.toFloat*capRatio) * (1/dist)
    }).sum
  }

  def closestThing(s:Station,factory:Factory,remaining: mutable.HashMap[Station,Boolean]):Station = {
    val pos = factory.find(s)
    var fMatch = factory.flavorPositions(takesFrom(s.flavor)).to(mutable.ArrayBuffer)
    var fStations = fMatch.map(p => factory.floor(p._1)(p._2))
    if(fMatch.isEmpty || fStations.filter(remaining(_)).isEmpty)
      throw new IndexOutOfBoundsException(s"Flavor ${takesFrom(s.flavor)} not found")

    val removeIndices = new mutable.ArrayBuffer[Int]()
    for(i <- fStations.indices) {
      if(!remaining(fStations(i)))
        removeIndices.addOne(i)
    }
    removeIndices.sorted(Ordering.Int.reverse).foreach(p => {fMatch.remove(p);fStations.remove(p)})

    val closest = fMatch.reduceLeft((a,b) => if(Factory.distFormula(pos,a) < Factory.distFormula(pos,b)) a else b)
    val st = factory.floor(closest._1)(closest._2)
    remaining(factory.floor(closest._1)(closest._2)) = false
    st
  }


}