import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.mutable
import scala.io.Source
import scala.util.Random
import scala.concurrent

object Main {
  def main(args: Array[String]): Unit = {
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
    //

    Affinities(affinities,factoryX,factoryY) //initializes affinity calculator
    val statuses:Array[AtomicBoolean] = (0 until nThreads).map(_ =>new AtomicBoolean(false)).toArray
    val threads = for(i <- 0 until nThreads) yield {
      new Cell(pStart,stations,factoryX,factoryY,statuses(i))
    }

    var iter = 0
    var converged = false
    spinUntilReady(statuses)
    
    
    while(iter < 1000 && !converged) {
      //select parents
      val p1 = threads.reduce((a,b) => if(a.affinity > b.affinity) a else b).active
      threads.foreach({p =>
        p.crossover = p1
        p.pause.set(false)
      })
      
      spinUntilReady(statuses)
      iter += 1
      if(threads.map(p => p.active.similarity(threads.head.active)).filter(p => p > (n*(factoryX*factoryY)*n)).isEmpty)
        converged = true
    }
    threads.foreach(_.stop =true)
    
    //TODO: parent selection, anything parallel

    
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