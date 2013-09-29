object worksheet {
  type Set = Int => Boolean;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(98); 
  def contains(s:Set, elem: Int): Boolean = s(elem);System.out.println("""contains: (s: Int => Boolean, elem: Int)Boolean""");$skip(32); 
  
  val ans = (x:Int) => x * x;System.out.println("""ans  : Int => Int = """ + $show(ans ));$skip(19); 
  println (ans(2))}
}
