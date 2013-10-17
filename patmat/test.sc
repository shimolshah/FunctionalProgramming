import patmat._

object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  List().isEmpty                                  //> res0: Boolean = true
  Nil.isEmpty                                     //> res1: Boolean = true
  List() == Nil                                   //> res2: Boolean = true
  val l = List((1,2), (4,5), (2,3), (3,4))        //> l  : List[(Int, Int)] = List((1,2), (4,5), (2,3), (3,4))
  l.exists {
    case (3,_) => true
    case (_,_) => false
  }                                               //> res3: Boolean = true

  l match {
    case tail1 :: (4,_) :: tail2 => true
    case (_,_) :: tail => false
  }                                               //> res4: Boolean = true
  
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
  }                                               //> times: (chars: List[Char])List[(Char, Int)]
  
  val a = times(List('a','b','a','c','c'))        //> a  : List[(Char, Int)] = List((a,2), (b,1), (c,2))
  
  Huffman.decode(Huffman.frenchCode, Huffman.secret)
                                                  //> res5: List[Char] = List(h, u, f, f, m, a, n, e, s, t, c, o, o, l)
  Huffman.frenchCode                              //> res6: patmat.Huffman.CodeTree = Fork(Fork(Fork(Leaf(s,121895),Fork(Leaf(d,56
                                                  //| 269),Fork(Fork(Fork(Leaf(x,5928),Leaf(j,8351),List(x, j),14279),Leaf(f,16351
                                                  //| ),List(x, j, f),30630),Fork(Fork(Fork(Fork(Leaf(z,2093),Fork(Leaf(k,745),Lea
                                                  //| f(w,1747),List(k, w),2492),List(z, k, w),4585),Leaf(y,4725),List(z, k, w, y)
                                                  //| ,9310),Leaf(h,11298),List(z, k, w, y, h),20608),Leaf(q,20889),List(z, k, w, 
                                                  //| y, h, q),41497),List(x, j, f, z, k, w, y, h, q),72127),List(d, x, j, f, z, k
                                                  //| , w, y, h, q),128396),List(s, d, x, j, f, z, k, w, y, h, q),250291),Fork(For
                                                  //| k(Leaf(o,82762),Leaf(l,83668),List(o, l),166430),Fork(Fork(Leaf(m,45521),Lea
                                                  //| f(p,46335),List(m, p),91856),Leaf(u,96785),List(m, p, u),188641),List(o, l, 
                                                  //| m, p, u),355071),List(s, d, x, j, f, z, k, w, y, h, q, o, l, m, p, u),605362
                                                  //| ),Fork(Fork(Fork(Leaf(r,100500),Fork(Leaf(c,50003),Fork(Leaf(v,24975),Fork(L
                                                  //| eaf(g,13288),Leaf(b,13822),List(g, b),27110),List(v, g, b),52085),List(c, v,
                                                  //|  g, b),102088),List(r, c
                                                  //| Output exceeds cutoff limit.
  Huffman.secret                                  //> res7: List[patmat.Huffman.Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0,
                                                  //|  1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 
                                                  //| 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1).equals(Huffman.secret)
                                                  //> res8: Boolean = true

  Huffman.encode(Huffman.frenchCode)(Huffman.decodedSecret).equals(Huffman.secret)
                                                  //> res9: Boolean = true

  Huffman.quickEncode(Huffman.frenchCode)(Huffman.decodedSecret).equals(Huffman.secret)
                                                  //> res10: Boolean = true

}