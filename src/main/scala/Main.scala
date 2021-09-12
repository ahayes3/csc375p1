import java.util.concurrent.Executors
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
    //

    Affinities(affinities,factoryX,factoryY)

    val executor = Executors.newFixedThreadPool(11) //i have 12 threads so the main will be the picker
    //val population = Seq.fill(n)(new Factory(factoryX,factoryY,flavors,stations))
    val pop:Seq[Cell] = Seq.fill(n)(new Cell(new Factory(factoryX,factoryY,flavors,stations)))
    val futures = pop.map(executor.submit(_))
    val results = futures.map(_.get())
    //val results:Seq[Double] = futures.map(p => p.get())
    results.foreach(println(_))
    //TODO: parent selection, anything parallel

    
  }
  def randomStations(amt:Int,flavors: Int, capRange:Range): List[Station] = {
    val stations:mutable.ListBuffer[Station] = mutable.ListBuffer[Station]()
    for(i <- 0 until amt) {
      stations.addOne(new Station(i,Random.between(1,flavors+1),Random.between(capRange.start,capRange.end+1)))
    }
    stations.toList
  }
}