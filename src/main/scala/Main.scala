import java.util.concurrent.{Executors, Phaser}
import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.mutable
import javax.swing.*
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Random
import scala.concurrent


object Main {
  val factoryX = 5
  val factoryY = 5
  val n = 25
  val flavors = 3 //number of possible flavors
  val capRange = Range.inclusive(5,10) //range of possible capacities
  val stations = randomStations(n,flavors,capRange) //list of stations. Constant between factories
  var nThreads = 11
  val tPop = 10 //population in each thread

  def main(args: Array[String]): Unit = {
    if(args.contains("-t") && args.indexOf("-t") < args.length -1) {
      val str = args(args.indexOf("-t")+1)
      val nT = str.toIntOption
      if(nT.isEmpty)
        throw new IllegalArgumentException(s"Arguement ${str} not a valid thread number")
      else
        nThreads = nT.get
    }

    val a = Station(1,2,3)
    a.position = Position(6,9)
    val b = a.copy()


    val affinityFile = "AffinityChart"
    val affinities:Array[Array[Int]] = (for(line <- Source.fromFile(affinityFile).getLines.filterNot(_.charAt(0)=='#')) yield {
      line.split(' ').map(_.toInt).toArray[Int]
    }).toArray[Array[Int]]

    //Temporary constants, will become arguements

    //

    Affinities(affinities,factoryX,factoryY) //initializes affinity calculator
    val ph = new Phaser()
    ph.register()
    //val statuses:Array[AtomicBoolean] = (0 until nThreads).map(_ =>new AtomicBoolean(false)).toArray
    val crossOverList = new ArrayBuffer[Factory]()
    val threads = for(i <- 0 until nThreads) yield {
      new Cell(tPop,stations,factoryX,factoryY,ph)
    }
    threads.foreach(_.start())

    var iter = 0
    var converged = false

    //spinUntilReady(statuses)
    var best:Factory = null

    val frame = new JFrame()
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    val container = frame.getContentPane
    val tl = new MyCanvas()
    container.add(tl)

    var clock = System.currentTimeMillis()
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
      val currBest = threads.flatMap(_.factories).reduce((a,b) => if(a.affinity > b.affinity) a else b)
      if(best == null || currBest.affinity > best.affinity)
        best = currBest.clone()


      if(System.currentTimeMillis() - clock < 500) {
        clock = System.currentTimeMillis()
        val data:Array[Array[Object]] = best.floor.map(p => p.map(j => if(j.nonEmpty) j.get.flavor.toString else " "))
        tl.factory=best
        frame.setSize(25*factoryX,25*(factoryY+1))
        frame.setVisible(true)
        tl.repaint()

      }

      if(ph.getPhase%50 ==0) {//don't do everytime because slow
        converged = checkConverge(threads,best)
      }

      if(!converged)
        ph.arriveAndAwaitAdvance()
      //delete bottom half, create list of possible parents and make available to cells
      //body

      ph.getPhase < 200 && !converged //condition
    }){}


    threads.foreach(_.stopMe =true)
    ph.arriveAndDeregister()
    println(best)
    println("Total affinity: " + Affinities.getTotal(best))
  }

  def checkConverge(cells:IndexedSeq[Cell],best:Factory):Boolean = {
    println("CHecking")
    val someNumber = (cells.map(_.factories.size).sum) + (factoryX * factoryY)
    val a = cells.flatMap(_.factories).map(_.similarity(best)).sum
    if(a < someNumber) {
      println("CONVERGED")
      return true
    }
    false
  }
//
//  def spinUntilReady(s:Array[AtomicBoolean]): Unit = {
//    while(s.map(p => p.get()).contains(false)) {Thread.sleep(50)}
//  }

  def randomStations(amt:Int,flavors: Int, capRange:Range): List[Station] = {
    val stations:mutable.ListBuffer[Station] = mutable.ListBuffer[Station]()
    for(i <- 0 until amt) {
      stations.addOne(new Station(i,Random.between(1,flavors+1),Random.between(capRange.start,capRange.end+1)))
    }
    stations.toList
  }
}