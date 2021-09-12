import java.util.concurrent.Callable
class Cell(val factory:Factory) extends Callable[Double] {
  override def call(): Double = {
    val a = Affinities.getTotal(factory)
    println(a)
    a
  }
}
