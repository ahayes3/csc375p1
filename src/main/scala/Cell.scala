import java.util.concurrent.Phaser
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
class Cell(val tPop:Int, val stations:List[Station],val facX:Int,val facY:Int,val ph:Phaser) extends Thread {
  var stopMe = false
  private val mutationChance = .01
  //var buff = IndexedSeq[Factory]()
  //var affinity:Double = -1
  var factories:IndexedSeq[Factory] = (for(i <- 0 until tPop) yield {
    val a = Factory.randomFloor(stations,facX,facY)
    new Factory(facX,facY,a._1,a._2)
  }).toIndexedSeq
  factories.foreach(p => p.affinity = Affinities.getTotal(p))
  //var active:Factory = _
  var crossOver = IndexedSeq[Factory]() //shouldn't be a data race
  override def run(): Unit = {
    ph.register()
    while(!stopMe) {

      //crossover
      doCrossover()

      //mutation
      factories.filter(p => Random.between(0f,1f) < mutationChance).foreach(mutate(_))

      //affinity
      factories.foreach(p => p.affinity = Affinities.getTotal(p))
      ph.arriveAndAwaitAdvance()
    }
    println(s"Thread stopped at time: ${System.currentTimeMillis()}")
    ph.arriveAndDeregister()
  }
  private def sortByAffinity(f1:Factory,f2:Factory) = {
      Affinities.getTotal(f1) > Affinities.getTotal(f2)
  }

  def doCrossover():Unit = {
    while(factories.size < tPop) {
      //use crossover to create new factories
      val i = Random.nextInt(crossOver.size)
      var j:Int = -1
      while({
        j = Random.nextInt(crossOver.size)
        j != i
      })
      factories :+= combine(crossOver(i),crossOver(j))
    }
  }

  private def combine(f1:Factory,f2:Factory):Factory = {
    val out = f1.clone()
    val shuffled = Random.shuffle(f2.stations)
    val stationList = shuffled.take(shuffled.size/2)
    stationList.foreach(p => out.swap(out.find(p),f2.find(p)))
    out
  }

  def mutate(f:Factory): Unit = {
    val st = f.stations(Random.nextInt(f.stations.length))
    val pos = Position(Random.nextInt(f.x),Random.nextInt(f.y))
    f.swap(st.position,pos)
  }

//  def spin(): Unit = {
//    pause.set(true)
//    while(pause.get()) {Thread.sleep(50)}
//  }

}
