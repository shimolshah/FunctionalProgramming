import forcomp.Anagrams
import forcomp.Anagrams._
import java.text.DictionaryBasedBreakIterator

object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(1051); 
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
  };System.out.println("""anagramsHelp: (occ: forcomp.Anagrams.Occurrences)List[forcomp.Anagrams.Sentence]""");$skip(79); val res$0 = 
  
  Anagrams.dictionaryByOccurrences.get(List(('i',1),('o',1))) getOrElse Nil;System.out.println("""res0: List[forcomp.Anagrams.Word] = """ + $show(res$0));$skip(95); val res$1 = 
  Anagrams.subtract(List(('e',1), ('i',1), ('l',1), ('o',1), ('v',1)), List(('i',1), ('o',1)));System.out.println("""res1: forcomp.Anagrams.Occurrences = """ + $show(res$1));$skip(200); val res$2 = 
  
  //anagramsHelp(sentenceOccurrences(List("I", "Love", "You")))
  
  
  //anagramsHelp(sentenceOccurrences(List("I", "Love")))
  Anagrams.dictionaryByOccurrences.get(List(('e',1),('r',1),('x',1)));System.out.println("""res2: Option[List[forcomp.Anagrams.Word]] = """ + $show(res$2));$skip(136); val res$3 = 
  
  //anagramsHelp(sentenceOccurrences(List("I", "Love", "You")))
  anagramsHelp(Anagrams.sentenceOccurrences(List("Linux", "rulez")));System.out.println("""res3: List[forcomp.Anagrams.Sentence] = """ + $show(res$3));$skip(14); val res$4 = 




  "Hello";System.out.println("""res4: String("Hello") = """ + $show(res$4));$skip(6); val res$5 = 
  5+8;System.out.println("""res5: Int(13) = """ + $show(res$5));$skip(35); val res$6 = 
  Anagrams.wordOccurrences("wwer");System.out.println("""res6: forcomp.Anagrams.Occurrences = """ + $show(res$6));$skip(72); val res$7 = 
  "wreewer".groupBy((x:Char) => x).toList.map(x => (x._1, x._2.length));System.out.println("""res7: List[(Char, Int)] = """ + $show(res$7));$skip(107); val res$8 = 
  
  "wreewer".groupBy((x:Char) => x).toList.map(x => (x._1, x._2.length)).sortWith((x, y) => x._1 < y._1);System.out.println("""res8: List[(Char, Int)] = """ + $show(res$8));$skip(33); val res$9 = 
  
  List(1,2,3).map((x => x*2));System.out.println("""res9: List[Int] = """ + $show(res$9));$skip(60); val res$10 = 
    List(('a',1),('b',2),('c',3)).map(x=> (x._1, x._2 * 2));System.out.println("""res10: List[(Char, Int)] = """ + $show(res$10));$skip(373); 
  
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
  };System.out.println("""sentenceOccurrences: (s: forcomp.Anagrams.Sentence)forcomp.Anagrams.Occurrences""");$skip(294); 

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
  };System.out.println("""consolidateList: (l: List[(Char, List[(Char, Int)])])List[(Char, Int)]""");$skip(50); val res$11 = 

  
  sentenceOccurrences(List("werer", "errpo"));System.out.println("""res11: forcomp.Anagrams.Occurrences = """ + $show(res$11));$skip(196); 
  //Anagrams.dictionary.groupBy((x: Word) => wordOccurrences(x))


  //val l = List(('a', 2), ('b', 2), ('c', 1))
  //val l = List(('a', 2), ('b', 2), ('c', 1))
  val l = List(('a', 2), ('b', 2));System.out.println("""l  : List[(Char, Int)] = """ + $show(l ));$skip(27); val res$12 = 
  l.combinations(2).toList;System.out.println("""res12: List[List[(Char, Int)]] = """ + $show(res$12));$skip(896); 

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
	};System.out.println("""combinations: (l: forcomp.Anagrams.Occurrences)List[forcomp.Anagrams.Occurrences]""");$skip(30); 
   
  val a = combinations(l);System.out.println("""a  : List[forcomp.Anagrams.Occurrences] = """ + $show(a ));$skip(12); val res$13 = 
  a.flatten;System.out.println("""res13: List[(Char, Int)] = """ + $show(res$13));$skip(35); val res$14 = 
  a.flatten.combinations(2).toList;System.out.println("""res14: List[List[(Char, Int)]] = """ + $show(res$14));$skip(378); 

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
    
  };System.out.println("""subtract: (x: forcomp.Anagrams.Occurrences, y: forcomp.Anagrams.Occurrences)forcomp.Anagrams.Occurrences""");$skip(58); 
  
  val x = List(('a', 1), ('d', 1), ('l', 1), ('r', 1));System.out.println("""x  : List[(Char, Int)] = """ + $show(x ));$skip(25); 
  val y = List(('r', 1));System.out.println("""y  : List[(Char, Int)] = """ + $show(y ));$skip(16); val res$15 = 
  subtract(x,y);System.out.println("""res15: forcomp.Anagrams.Occurrences = """ + $show(res$15));$skip(84); 
  
  val input = Map[String, List[Int]]("rk1" -> List(1,2,3), "rk2" -> List(4,5,6));System.out.println("""input  : scala.collection.immutable.Map[String,List[Int]] = """ + $show(input ));$skip(64); 
  val output = input.map{ case(k,v) => (k.getBytes, v) } toList;System.out.println("""output  : List[(Array[Byte], List[Int])] = """ + $show(output ));$skip(604); 



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
  };System.out.println("""sentenceAnagrams: (sentence: forcomp.Anagrams.Sentence)List[forcomp.Anagrams.Sentence]""");$skip(22); val res$16 = 
  List() map (x => 1);System.out.println("""res16: List[Int] = """ + $show(res$16));$skip(85); val res$17 = 
  (List("Lin") /: List(List("Zulu", "Rex"), List("Rex", "Zulu")))((x, y) => x ::: y);System.out.println("""res17: List[String] = """ + $show(res$17));$skip(81); val res$18 = 
  
  List(List("Zulu", "Rex"), List("Rex", "Zulu")) map (x => List("Lin") ::: x);System.out.println("""res18: List[List[String]] = """ + $show(res$18));$skip(80); val res$19 = 
  
  Anagrams.dictionaryByOccurrences.get(List(('i',1),('o',1))) getOrElse Nil;System.out.println("""res19: List[forcomp.Anagrams.Word] = """ + $show(res$19));$skip(95); val res$20 = 
  Anagrams.subtract(List(('e',1), ('i',1), ('l',1), ('o',1), ('v',1)), List(('i',1), ('o',1)));System.out.println("""res20: forcomp.Anagrams.Occurrences = """ + $show(res$20))}
  
  //anagramsHelp(sentenceOccurrences(List("I", "Love", "You")))
  
  
  //anagramsHelp(sentenceOccurrences(List("I", "Love")))
  
  
  //anagramsHelp(sentenceOccurrences(List("I", "Love", "You")))
  //anagramsHelp(sentenceOccurrences(List("Linux", "rulez")))
  //sentenceAnagrams(List("I", "Love", "You"))
}
