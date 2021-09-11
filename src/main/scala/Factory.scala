class Factory(val x: Int,val y:Int,val flavors: Int,stations:List[Station]) { //Chromosome for purpose of GA
  val floor = Array.fill[Station](x,y)(null)

  placeRandom(stations)

  def swap(x1:Int,y1:Int,x2:Int,y2:Int): Unit = { //swap mutation
    val a = floor(x1)(y1)
    floor(x1)(y1) = floor(x2)(y2)
    floor(x2)(y2) = a
  }

  def affinity():Float = { //will need adjusting
    var aff:Float = 0
    for(i <- floor.indices) {
      for(j <- floor(i).indices) {
        if(i > 0) {
          aff += affBetween(floor(i)(j),floor(i-1)(j))//add affinity to left to total
        }
        if(i < floor.length-1)
          aff += affBetween(floor(i)(j),floor(i+1)(j))//add right affinity
        if(j> 0)
          aff += affBetween(floor(i)(j),floor(i)(j-1))//add above affinity
        if(j < floor(i).length - 1)
          aff += affBetween(floor(i)(j),floor(i)(j+1))//add below affinity
      }
    }
    aff
  }
  
  def find(s:Station): (Int,Int) = {
    for(i <- floor.indices) {
      for(j <- floor(i).indices) {
        if(floor(i)(j) == s)
          return (i,j)
      }
    }
    return (-1,-1)
  }

  def distance(s1:Station,s2:Station): Double = {
    val pos1 = find(s1)
    val pos2 = find(s2)

    Factory.distFormula(pos1,pos2)
  }

  def closestThing(s1:Station,flavor:Int):Station = { //can't think of a fitting name
    val pos = find(s1)
    val stations = flavorPositions(flavor)
    if(stations.isEmpty)
      throw new IndexOutOfBoundsException(s"Flavor $flavor not found")
    val closest = stations.reduceLeft((a,b) => if(Factory.distFormula(pos,a) < Factory.distFormula(pos,b)) a else b) //if there are multiple at same distance picks later in the array
    floor(closest._1)(closest._2)
  }


  def flavorPositions(flavor:Int): Seq[(Int,Int)] = {
    var positions = Seq[(Int,Int)]()
    for(i <- floor.indices) {
      for(j <- floor(i).indices) {
        if(floor(i)(j).flavor == flavor)
          positions :+= (i,j)
      }
    }
    positions
  }

  private def affBetween(s1:Station,s2:Station):Float = {
    ???
  }

  private def placeRandom(stations:List[Station]): Unit = {
    for(s <- stations) {

    }
  }
}

object Factory {
  def distFormula(a:(Int,Int),b:(Int,Int)): Double = {
    math.sqrt((b._1-a._1)^2 + (b._2 - a._2)^2)
  }
}