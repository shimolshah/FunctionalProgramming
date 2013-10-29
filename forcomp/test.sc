import forcomp.Anagrams
import forcomp.Anagrams._
import java.text.DictionaryBasedBreakIterator

object test {
  def anagramsHelp(occ: Occurrences): List[Sentence] = {
    //println ("---" + occ)
    val a = Anagrams.combinations(occ)
    //println ("---" + a)
    val b = for {
      i <- a
      //_ = println("1 " + i)
    } yield {
      (i,Anagrams.dictionaryByOccurrences.get(i) getOrElse Nil)
    }
    val aa = b.filter(x => x._2 != Nil)
    //println ("---" + aa)
    
    val c = for {
      i <- aa
      a = Anagrams.subtract(occ, i._1)
      b = i._2
      j <- b
      _ = println("i " + i)
      //_ = println("1" + List(j))
      ah = anagramsHelp(a)
      //_ = println("2" + ah)
      
    } yield ((List(j) /: ah) ((x,y) => x:::y))
    //println ("c1: " + c.filter(x => x != Nil))
    
    val d = for {
      i <- c
      if Anagrams.sentenceOccurrences(i) == occ
    } yield i
    //val c = b.unzip
    //val d = c._2.filter(x => x!=Nil)
    //println ("d: " + d)
    
    //d
    //c
    //println ("d: " + d)
    d
    //Nil
  }                                               //> anagramsHelp: (occ: forcomp.Anagrams.Occurrences)List[forcomp.Anagrams.Sent
                                                  //| ence]
  
  Anagrams.dictionaryByOccurrences.get(List(('i',1),('o',1))) getOrElse Nil
                                                  //> res0: List[forcomp.Anagrams.Word] = List(Io)
  Anagrams.subtract(List(('e',1), ('i',1), ('l',1), ('o',1), ('v',1)), List(('i',1), ('o',1)))
                                                  //> res1: forcomp.Anagrams.Occurrences = List((e,1), (l,1), (v,1))
  
  //anagramsHelp(sentenceOccurrences(List("I", "Love", "You")))
  
  
  //anagramsHelp(sentenceOccurrences(List("I", "Love")))
  Anagrams.dictionaryByOccurrences.get(List(('e',1),('r',1),('x',1)))
                                                  //> res2: Option[List[forcomp.Anagrams.Word]] = Some(List(Rex))
  
  //anagramsHelp(sentenceOccurrences(List("I", "Love", "You")))
  anagramsHelp(Anagrams.sentenceOccurrences(List("Linux", "rulez")))
                                                  //> i (List((n,1), (u,1)),List(nu))
                                                  //| i (List((i,1), (u,1), (z,1)),List(Uzi))
                                                  //| i (List((e,1), (x,1)),List(ex))
                                                  //| i (List((e,1), (r,1)),List(re))
                                                  //| i (List((e,1), (r,1), (x,1)),List(Rex))
                                                  //| i (List((i,1), (r,1), (u,1)),List(Uri))
                                                  //| i (List((e,1), (x,1)),List(ex))
                                                  //| i (List((i,1), (l,1), (z,1)),List(Liz))
                                                  //| i (List((e,1), (x,1)),List(ex))
                                                  //| i (List((e,1), (r,1)),List(re))
                                                  //| i (List((e,1), (r,1), (x,1)),List(Rex))
                                                  //| i (List((e,1), (r,1), (u,1)),List(rue))
                                                  //| i (List((e,1), (l,1), (r,1), (u,1)),List(lure, rule))
                                                  //| i (List((e,1), (l,1), (r,1), (u,1)),List(lure, rule))
                                                  //| i (List((i,1), (l,2)),List(ill))
                                                  //| i (List((e,1), (x,1)),List(ex))
                                                  //| i (List((e,1), (r,1)),List(re))
                                                  //| i (List((e,1), (r,1), (x,1)),List(Rex))
                                                  //| i (List((e,1), (r,1), (u,1)),List(rue))
                                                  //| i (List((i,1), (l,2), (r,1)),List(rill))
                                                  //| i (List((e,1), (x,1)),List(ex))
                                                  //| i (List((e,1), (x,1)),List(ex))
                                                  //| i (List((i,1), (u,1), (z,1)),List(Uzi))




  "Hello"                                         //> res4: String("Hello") = Hello
  5+8                                             //> res5: Int(13) = 13
  Anagrams.wordOccurrences("wwer")                //> res6: forcomp.Anagrams.Occurrences = List((e,1), (r,1), (w,2))
  "wreewer".groupBy((x:Char) => x).toList.map(x => (x._1, x._2.length))
                                                  //> res7: List[(Char, Int)] = List((e,3), (w,2), (r,2))
  
  "wreewer".groupBy((x:Char) => x).toList.map(x => (x._1, x._2.length)).sortWith((x, y) => x._1 < y._1)
                                                  //> res8: List[(Char, Int)] = List((e,3), (r,2), (w,2))
  
  List(1,2,3).map((x => x*2))                     //> res9: List[Int] = List(2, 4, 6)
    List(('a',1),('b',2),('c',3)).map(x=> (x._1, x._2 * 2))
                                                  //> res10: List[(Char, Int)] = List((a,2), (b,4), (c,6))
  
  def sentenceOccurrences (s :Sentence): Occurrences = {
    s match {
      case Nil => Nil
      case x :: xs =>
        val z = (Anagrams.wordOccurrences(x) ::: sentenceOccurrences(xs))
        val a = z.groupBy((y:(Char, Int)) => y._1)
        val b = a.toList
        val c = consolidateList(b)
        val d = c.sortWith((x, y) => x._1 < y._1)
        d
    }
  }                                               //> sentenceOccurrences: (s: forcomp.Anagrams.Sentence)forcomp.Anagrams.Occurre
                                                  //| nces

  def consolidateList(l: List[(Char, List[(Char, Int)])]):List[(Char, Int)] = {
    //l groupBy
    l match {
      case Nil => Nil
      case head :: tail => {
        List (head._2 reduceRight ((a,b) => {
          (a._1, a._2 + b._2)
        })) ::: consolidateList(tail)
      }
    }
  }                                               //> consolidateList: (l: List[(Char, List[(Char, Int)])])List[(Char, Int)]

  
  sentenceOccurrences(List("werer", "errpo"))     //> res11: forcomp.Anagrams.Occurrences = List((e,3), (o,1), (p,1), (r,4), (w,1
                                                  //| ))
  //Anagrams.dictionary.groupBy((x: Word) => wordOccurrences(x))


  //val l = List(('a', 2), ('b', 2), ('c', 1))
  //val l = List(('a', 2), ('b', 2), ('c', 1))
  val l = List(('a', 2), ('b', 2))                //> l  : List[(Char, Int)] = List((a,2), (b,2))
  l.combinations(2).toList                        //> res12: List[List[(Char, Int)]] = List(List((a,2), (b,2)))

	def combinations(l: Anagrams.Occurrences) : List[Anagrams.Occurrences] = {
    l match {
      case Nil => List(Nil)
      case head :: tail => {
        val (character, freq) = head
        
        val a = for {
          i <- 0 to freq
        } yield (character, i)

        val b:List[Anagrams.Occurrences] = combinations(tail)
        
        val d = for {
          i <- a.toList
          j <- b
          
        } yield i ::j
        //val c:List[Anagrams.Occurrences] = List(a.toList) ::: b
        //println (List(a.toList))
        //println (b)
        //println (c)
        //List(Nil)
        //c
        //println (d)
        //d
        
        val e = for {
          i <- d
          //i.filter(x => x._2!=0)
        } yield i.filter(x => x._2!=0)
        e
      }
    }


//	  val z = for {
//	    s <- l
//	    //println (s)
//	  } yield s
	  //println(z)
//	  Nil
	}                                         //> combinations: (l: forcomp.Anagrams.Occurrences)List[forcomp.Anagrams.Occurr
                                                  //| ences]
   
  val a = combinations(l)                         //> a  : List[forcomp.Anagrams.Occurrences] = List(List(), List((b,1)), List((b
                                                  //| ,2)), List((a,1)), List((a,1), (b,1)), List((a,1), (b,2)), List((a,2)), Lis
                                                  //| t((a,2), (b,1)), List((a,2), (b,2)))
  a.flatten                                       //> res13: List[(Char, Int)] = List((b,1), (b,2), (a,1), (a,1), (b,1), (a,1), (
                                                  //| b,2), (a,2), (a,2), (b,1), (a,2), (b,2))
  a.flatten.combinations(2).toList                //> res14: List[List[(Char, Int)]] = List(List((b,1), (b,1)), List((b,1), (b,2)
                                                  //| ), List((b,1), (a,1)), List((b,1), (a,2)), List((b,2), (b,2)), List((b,2), 
                                                  //| (a,1)), List((b,2), (a,2)), List((a,1), (a,1)), List((a,1), (a,2)), List((a
                                                  //| ,2), (a,2)))

  def subtract(x: Anagrams.Occurrences, y: Anagrams.Occurrences): Anagrams.Occurrences = {
    val map_x = x.toMap
    //println (map_x)
    
    val a = for {
      i <- y
      
    } yield map_x updated (i._1, (map_x apply i._1) - i._2)
    val c = (a.head.map{ case (k,v) => (k,v) } toList)
    println (a)
    println (c)
    val b = c.filter(x => x._2!=0)
    b
    
  }                                               //> subtract: (x: forcomp.Anagrams.Occurrences, y: forcomp.Anagrams.Occurrences
                                                  //| )forcomp.Anagrams.Occurrences
  
  val x = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
                                                  //> x  : List[(Char, Int)] = List((a,1), (d,1), (l,1), (r,1))
  val y = List(('r', 1))                          //> y  : List[(Char, Int)] = List((r,1))
  subtract(x,y)                                   //> List(Map(a -> 1, d -> 1, l -> 1, r -> 0))
                                                  //| List((a,1), (d,1), (l,1), (r,0))
                                                  //| res15: forcomp.Anagrams.Occurrences = List((a,1), (d,1), (l,1))
  
  val input = Map[String, List[Int]]("rk1" -> List(1,2,3), "rk2" -> List(4,5,6))
                                                  //> input  : scala.collection.immutable.Map[String,List[Int]] = Map(rk1 -> List
                                                  //| (1, 2, 3), rk2 -> List(4, 5, 6))
  val output = input.map{ case(k,v) => (k.getBytes, v) } toList
                                                  //> output  : List[(Array[Byte], List[Int])] = List((Array(114, 107, 49),List(1
                                                  //| , 2, 3)), (Array(114, 107, 50),List(4, 5, 6)))



  def sentenceAnagrams(sentence: Anagrams.Sentence): List[Anagrams.Sentence] = {
    sentence match {
      case Nil => Nil
      case head :: tail =>
        val a = sentenceOccurrences(sentence)
        println(a)
        val b = combinations(a)
        //println(b)
        val c = for {
          i <- b
          if i != Nil; if (dictionaryByOccurrences.get(i) getOrElse Nil) != Nil
        } yield (i, dictionaryByOccurrences.get(i) getOrElse Nil)
        println(c)
        
        //for {
        //  i <- c
        //} yield subtract(a, i._1)
        
        
        Nil
    }
    Nil
  }                                               //> sentenceAnagrams: (sentence: forcomp.Anagrams.Sentence)List[forcomp.Anagram
                                                  //| s.Sentence]
  List() map (x => 1)                             //> res16: List[Int] = List()
  (List("Lin") /: List(List("Zulu", "Rex"), List("Rex", "Zulu")))((x, y) => x ::: y)
                                                  //> res17: List[String] = List(Lin, Zulu, Rex, Rex, Zulu)
  
  List(List("Zulu", "Rex"), List("Rex", "Zulu")) map (x => List("Lin") ::: x)
                                                  //> res18: List[List[String]] = List(List(Lin, Zulu, Rex), List(Lin, Rex, Zulu)
                                                  //| )
  
  Anagrams.dictionaryByOccurrences.get(List(('i',1),('o',1))) getOrElse Nil
                                                  //> res19: List[forcomp.Anagrams.Word] = List(Io)
  Anagrams.subtract(List(('e',1), ('i',1), ('l',1), ('o',1), ('v',1)), List(('i',1), ('o',1)))
                                                  //> res20: forcomp.Anagrams.Occurrences = List((e,1), (l,1), (v,1))
  
  //anagramsHelp(sentenceOccurrences(List("I", "Love", "You")))
  
  
  //anagramsHelp(sentenceOccurrences(List("I", "Love")))
  
  
  //anagramsHelp(sentenceOccurrences(List("I", "Love", "You")))
  //anagramsHelp(sentenceOccurrences(List("Linux", "rulez")))
  //sentenceAnagrams(List("I", "Love", "You"))
}