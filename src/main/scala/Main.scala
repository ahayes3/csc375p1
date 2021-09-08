import scala.collection.mutable
import scala.io.Source
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    val affinityFile = "AffinityChart"
    val affinities:Array[Array[Int]] = (for(line <- Source.fromFile(affinityFile).getLines) yield {
      line.split(' ').map(_.toInt).toArray[Int]
    }).toArray[Array[Int]]

    val factoryX = 3
    val factoryY = 3
    val n = 9
    val flavors = 2
    val pc = .25
    val pm = .25
    val capRange = Range.inclusive(5,10)
    val stations = randomStations(n,flavors,capRange)
    Affinities(affinities,flavors)

    val population = Seq.fill(n)(new Factory(factoryX,factoryY,flavors,stations))
    //TODO: parent selection, anything parallel



  }
  def randomStations(amt:Int,flavors: Int, capRange:Range): List[Station] = {
    val stations:mutable.ListBuffer[Station] = mutable.ListBuffer[Station]()
    for(i <- 0 to amt) {
      stations.addOne(new Station(Random.between(1,flavors+1),Random.between(capRange.start,capRange.end+1)))
    }
    stations.toList
  }
}