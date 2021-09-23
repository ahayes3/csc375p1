import java.util.concurrent.{Executors, Phaser}
import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Random
import scala.concurrent

//TODO add more populator, redo crossover and selection
object Main {
  def main(args: Array[String]): Unit = {
    val a = Station(1,2,3)
    a.position = Position(6,9)
    val b = a.copy()


    val affinityFile = "AffinityChart"
    val affinities:Array[Array[Int]] = (for(line <- Source.fromFile(affinityFile).getLines.filterNot(_.charAt(0)=='#')) yield {
      line.split(' ').map(_.toInt).toArray[Int]
    }).toArray[Array[Int]]

    //Temporary constants, will become arguements
    val factoryX = 5
    val factoryY = 5
    val n = 9
    val flavors = 3
    val pc = .25
    val pm = .25
    val capRange = Range.inclusive(5,10)
    val stations = randomStations(n,flavors,capRange)
    val nThreads = 11
    val pStart = 10
    val tPop = 10 //population in each thread
    //

    Affinities(affinities,factoryX,factoryY) //initializes affinity calculator
    val ph = new Phaser()
    ph.register()
    //val statuses:Array[AtomicBoolean] = (0 until nThreads).map(_ =>new AtomicBoolean(false)).toArray
    val crossOverList = new ArrayBuffer[Factory]()
    val threads = for(i <- 0 until nThreads) yield {
      new Cell(pStart,tPop,stations,factoryX,factoryY,ph)
    }
    threads.foreach(_.start())

    var iter = 0
    var converged = false

    //spinUntilReady(statuses)
    var best:Factory = null

    while({
      if(ph.getPhase % 10 ==0)
        println(s"Phase: ${ph.getPhase}     Best affinity: ${if(best==null) 0 else best.affinity}         Current best: ${threads.flatMap(_.factories).map(_.affinity).reduce((a,b) => if(a>b) a else b)}")
      //body

      while(ph.getArrivedParties != ph.getRegisteredParties-1) //wait until Cells are done
        Thread.sleep(50)

      val affinities = threads.flatMap(p => p.factories.map(_.affinity))
      val avgAffinity = affinities.sum / affinities.size
      threads.foreach(p => p.factories = p.factories.filter(i => i.affinity >= avgAffinity))
      val co = threads.flatMap(p => p.factories)
      threads.foreach(_.crossOver =  co)
      best = threads.flatMap(_.factories).reduce((a,b) => if(a.affinity > b.affinity) a else b)
      //todo check if converged every 50 phases


      ph.arriveAndAwaitAdvance()
      //delete bottom half, create list of possible parents and make available to cells
      //body

      ph.getPhase < 1000 && !converged //condition
    }){}

    threads.foreach(_.stopMe =true)
    println(best)
    println("Total affinity: " + Affinities.getTotal(best))
    
  }

  def spinUntilReady(s:Array[AtomicBoolean]): Unit = {
    while(s.map(p => p.get()).contains(false)) {Thread.sleep(50)}
  }

  def randomStations(amt:Int,flavors: Int, capRange:Range): List[Station] = {
    val stations:mutable.ListBuffer[Station] = mutable.ListBuffer[Station]()
    for(i <- 0 until amt) {
      stations.addOne(new Station(i,Random.between(1,flavors+1),Random.between(capRange.start,capRange.end+1)))
    }
    stations.toList
  }
}