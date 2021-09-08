class Factory(val x: Int,val y:Int,val flavors: Int,stations:List[Station]) { //Chromosome for purpose of GA
  val floor = Array.ofDim[Station](x,y)
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
  private def affBetween(s1:Station,s2:Station):Float = {
    if(s1 == None || s2 ==None)
      return 0
    var aff:Float = Affinities.get(s1.flavor,s2.flavor).toFloat
    val mod = if(s1.capacity > s2.capacity) s1.capacity/s2.capacity else s2.capacity/s1.capacity //Should never divide by 0 because factories shouldn't have 0 capacity
    (aff * mod)
  }

  private def placeRandom(stations:List[Station]): Unit = {
    for(s <- stations) {

    }
  }
}