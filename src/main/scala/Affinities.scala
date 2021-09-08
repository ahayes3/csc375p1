object Affinities {
  var affinities: Array[Array[Int]] = _

  def apply(a: Array[Array[Int]],f:Int): Unit = {
    affinities = a
    affinities = Array.ofDim(f,f) //maybe defaults to 0, if error this is probably it
    for(l <- a) { //set up array symmetrically along the diagonal
      affinities(l(0))(l(1)) = l(2)
      affinities(l(1))(l(0)) = l(2)
    }
  }

  def get(s1:Int,s2:Int):Int = {
    affinities(s1)(s1)
  }
}
