import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
class Cell(val pStart:Int, val stations:List[Station],val facX:Int,val facY:Int,val pause:AtomicBoolean) extends Thread {
  var stopMe = false
  //var buff = IndexedSeq[Factory]()
  var affinity:Double = -1
  var active:Factory = _
  var crossover:Factory = _
  private val mutationChance = .01
  override def run(): Unit = {
    println("Started thread "+this.getId)
    active = (for(i <- 0 until pStart) yield {
      val a = Factory.randomFloor(stations,facX,facY)
      new Factory(facX,facY,a._1,a._2)
    }).reduce((a,b) => if(Affinities.getTotal(a) > Affinities.getTotal(b)) a else b) //generate 10 random factories and pick best
    //buff :+= active.clone()
    affinity = Affinities.getTotal(active)
    spin() //selector does crossover
    while(!stopMe) {
      //println("Entered loop")

      //crossover
      doCrossover()

      //mutation
      if(Random.between(0f,1f) > .25)
        mutate()

      //affinity
      affinity = Affinities.getTotal(active)
//      if(!buff.contains(active)) {
//        buff :+= active.clone()
//      }
//      buff = buff.sortWith(sortByAffinity)
//      active = buff(0)
      spin()
    }
    println(s"Thread stopped at time: ${System.currentTimeMillis()}")
  }
  private def sortByAffinity(f1:Factory,f2:Factory) = {
      Affinities.getTotal(f1) > Affinities.getTotal(f2)
  }

  def doCrossover():Unit = {
    var toSwap = Seq[Station]()
    val amt = crossover.stations.length/2
    val list = crossover.stations.map(p => p.copy()).toBuffer
    while(toSwap.length < amt) {
      val index = Random.nextInt(list.length)
      toSwap :+= list(index)
      list.remove(index)
    }
    for(i <- toSwap) {
      val newPos = crossover.find(i)
      val pos = active.find(i)
        active.swap(pos,newPos)
    }
  }

  def mutate(): Unit = {
    val st = active.stations(Random.nextInt(active.stations.length))
    val pos = Position(Random.nextInt(active.x),Random.nextInt(active.y))
    active.swap(st.position,pos)
  }

  def spin(): Unit = {
    pause.set(true)
    while(pause.get()) {Thread.sleep(50)}
  }

}
