case class Station(val id:Int,val flavor:Int,val capacity:Int) {
  var position:Position = _
  def copy():Station = {
    val pos = if(this.position == null) null else this.position.copy()
    val out = Station(id,flavor, capacity)
    out.position = pos
    out
  }
}
case class Position(val x:Int,val y:Int)