import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
class Cell(val pStart:Int, val stations:List[Station],val facX:Int,val facY:Int,val pause:AtomicBoolean) extends Runnable {
  var stop = false
  //var buff = IndexedSeq[Factory]()
  var affinity:Double = -1
  var active:Factory = _
  var crossover:Factory = _
  private val mutationChance = .25
  override def run(): Unit = {
    active = (for(i <- 0 until pStart) yield {
      new Factory(facX,facY,stations)
    }).reduce((a,b) => if(Affinities.getTotal(a) > Affinities.getTotal(b)) a else b) //generate 10 random factories and pick best
    //buff :+= active.clone()
    affinity = Affinities.getTotal(active)
    spin() //selector does crossover
    while(!stop) {
      //crossover
      //Todo

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

  def mutate(): Unit = {
    val st = active.stations(Random.nextInt(active.stations.length))
    val pos = (Random.nextInt(active.x),Random.nextInt(active.y))
    active.swap(active.find(st),pos)
  }

  def spin(): Unit = {
    pause.set(true)
    while(pause.get()) {Thread.sleep(50)}
  }

}
