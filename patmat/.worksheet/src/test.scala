import patmat._

object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(74); 
  println("Welcome to the Scala worksheet");$skip(20); val res$0 = 
  
  List().isEmpty;System.out.println("""res0: Boolean = """ + $show(res$0));$skip(14); val res$1 = 
  Nil.isEmpty;System.out.println("""res1: Boolean = """ + $show(res$1));$skip(16); val res$2 = 
  List() == Nil;System.out.println("""res2: Boolean = """ + $show(res$2));$skip(43); 
  val l = List((1,2), (4,5), (2,3), (3,4));System.out.println("""l  : List[(Int, Int)] = """ + $show(l ));$skip(64); val res$3 = 
  l.exists {
    case (3,_) => true
    case (_,_) => false
  };System.out.println("""res3: Boolean = """ + $show(res$3));$skip(90); val res$4 = 

  l match {
    case tail1 :: (4,_) :: tail2 => true
    case (_,_) :: tail => false
  };System.out.println("""res4: Boolean = """ + $show(res$4));$skip(531); 
  
    def times(chars: List[Char]): List[(Char, Int)] = {
    def helper(char: Char, list: List[(Char, Int)]) : List[(Char, Int)] = {
      list match {
        case Nil => List((char, 1))
        case (x, y) :: tail => if(x == char) (char, y+1) :: tail else (x, y) :: helper(char, tail)
      }
    }
    
    def acc(chars: List[Char], list: List[(Char, Int)]): List[(Char, Int)] = {
      chars match {
        case Nil => list
        case x :: tail => acc(tail, helper(x, list))
      }
    }
    
    acc(chars, List())
  };System.out.println("""times: (chars: List[Char])List[(Char, Int)]""");$skip(46); 
  
  val a = times(List('a','b','a','c','c'));System.out.println("""a  : List[(Char, Int)] = """ + $show(a ));$skip(56); val res$5 = 
  
  Huffman.decode(Huffman.frenchCode, Huffman.secret);System.out.println("""res5: List[Char] = """ + $show(res$5));$skip(21); val res$6 = 
  Huffman.frenchCode;System.out.println("""res6: patmat.Huffman.CodeTree = """ + $show(res$6));$skip(17); val res$7 = 
  Huffman.secret;System.out.println("""res7: List[patmat.Huffman.Bit] = """ + $show(res$7));$skip(218); val res$8 = 

List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1).equals(Huffman.secret);System.out.println("""res8: Boolean = """ + $show(res$8));$skip(84); val res$9 = 

  Huffman.encode(Huffman.frenchCode)(Huffman.decodedSecret).equals(Huffman.secret);System.out.println("""res9: Boolean = """ + $show(res$9));$skip(89); val res$10 = 

  Huffman.quickEncode(Huffman.frenchCode)(Huffman.decodedSecret).equals(Huffman.secret);System.out.println("""res10: Boolean = """ + $show(res$10))}

}
