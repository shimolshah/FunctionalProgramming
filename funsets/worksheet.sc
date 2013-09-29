object worksheet {
  type Set = Int => Boolean
  def contains(s:Set, elem: Int): Boolean = s(elem)
                                                  //> contains: (s: Int => Boolean, elem: Int)Boolean
  
  val ans = (x:Int) => x * x                      //> ans  : Int => Int = <function1>
  println (ans(2))                                //> 4
}