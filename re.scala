object M3 {

abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALTs(rs: List[Rexp]) extends Rexp  // alternatives 
case class SEQs(rs: List[Rexp]) extends Rexp  // sequences
case class STAR(r: Rexp) extends Rexp         // star


//the usual binary choice and binary sequence can be defined 
//in terms of ALTs and SEQs
def ALT(r1: Rexp, r2: Rexp) = ALTs(List(r1, r2))
def SEQ(r1: Rexp, r2: Rexp) = SEQs(List(r1, r2))

// some convenience for typing regular expressions
def charlist2rexp(s: List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}

import scala.language.implicitConversions

given Conversion[String, Rexp] = (s => charlist2rexp(s.toList))

extension (r: Rexp) {
  def | (s: Rexp) = ALT(r, s)
  def % = STAR(r)
  def ~ (s: Rexp) = SEQ(r, s)
}

// some examples for the conversion and extension:

// val areg : Rexp = "a" | "b"
//  => ALTs(List(CHAR('a'), CHAR('b')))
//
// val sreg : Rexp = "a" ~ "b"
//  => SEQs(List(CHAR('a'), CHAR('b'))) 
//
// val star_reg : Rexp = ("a" ~ "b").%
//  => STAR(SEQs(List(CHAR('a'), CHAR('b')))) 

// Tests whether a regular expression can match the empty string.
def nullable(r: Rexp): Boolean = {
  r match {
    case ZERO => false
    case ONE => true
    case CHAR(_) => false
    case ALTs(rs) => rs.exists(nullable)
    case SEQs(rs) => rs.forall(nullable)  // Check if all elements in the sequence are nullable
    case STAR(_) => true
  }
}

/* Takes a character and a regular expression as arguments and 
calculates the derivative of a regular expression */
def der(c: Char, r: Rexp): Rexp = {
  r match {
    case ZERO => ZERO
    case ONE => ZERO
    case CHAR(d) => if (c == d) ONE else ZERO
    case ALTs(rs) => ALTs(rs.map(x => der(c, x)))
    case SEQs(List()) => ZERO
    case SEQs(r :: rs) =>
      if (nullable(r)) {
        ALT(SEQs(der(c, r) :: rs), der(c, SEQs(rs)))
      } else {
        SEQs(der(c, r) :: rs)
      }
    case STAR(r) => SEQ(der(c, r), STAR(r))
  }
}

// Deletes 0s and “spills out” nested ALT's.
def denest(rs: List[Rexp]) : List[Rexp] = {
  rs match {
    case List() => List()
    case ZERO :: rest => denest(rest)
    case ALTs(rs) :: rest => rs ::: denest(rest)
    case r :: rest => r :: denest(rest)
  }
}

// Flattens n-ary sequence regular expression
def flts(rs: List[Rexp], acc: List[Rexp] = Nil) : List[Rexp] = {
  rs match {
    case List() => acc
    case ZERO :: rest => List(ZERO)
    case ONE :: rest => flts(rest, acc)
    case SEQs(rs) :: rest => flts(rest, acc ::: rs)
    case r :: rest => flts(rest, acc ::: List(r))
  }
}


def ALTs_smart(rs: List[Rexp]) : Rexp = {
  rs match {
    case List() => ZERO
    case List(r) => r
    case _ => ALTs(rs)
  }
}


def SEQs_smart(rs: List[Rexp]) : Rexp = {
  rs match {
    case List() => ONE
    case List(ZERO) => ZERO
    case List(r) => r
    case _ => SEQs(rs)
  }
}

/*  Traverses a regular ex- pression, and on the way up simplifies 
every regular expression */
def simp(r: Rexp) : Rexp = {
  r match{
    case ALTs(rs) => ALTs_smart(denest(rs.map(simp)).distinct)
    case SEQs(rs) => SEQs_smart(flts(rs.map(simp)))
    case r => r
  }
}

// Takes a list of characters and a regular expression as arguments, and builds the derivative
def ders (s: List[Char], r: Rexp) : Rexp = {
  s match{
    case Nil => r
    case c :: cs => ders(cs, simp(der(c, r)))
  }
}

/*  Builds first the derivatives and tests whether the resulting derivative regular 
expression can match the empty string */
def matcher(r: Rexp, s: String): Boolean = {
  nullable(ders(s.toList, r))
}

/* If a regular expression is seen as a tree, then size should return the number 
of nodes in such a tree. */
def size(r: Rexp): Int = {
  r match{
    case ZERO => 1
    case ONE => 1
    case CHAR(c) => 1
    case ALTs(rs) => 1 + rs.map(size).sum
    case SEQs(rs) => 1 + rs.map(size).sum
    case STAR(r) => 1 + size(r)
  }
}



// Some testing data
/*

simp(ALT(ONE | CHAR('a'), CHAR('a') | ONE))   // => ALTs(List(ONE, CHAR(a)))
simp(((CHAR('a') | ZERO) ~ ONE) | 
     (((ONE | CHAR('b')) | CHAR('c')) ~ (CHAR('d') ~ ZERO)))   // => CHAR(a)

matcher(("a" ~ "b") ~ "c", "ab")   // => false
matcher(("a" ~ "b") ~ "c", "abc")  // => true


// the supposedly 'evil' regular expression (a*)* b
val EVIL = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))

matcher(EVIL, "a" * 1000)          // => false
matcher(EVIL, "a" * 1000 ++ "b")   // => true


// size without simplifications
size(der('a', der('a', EVIL)))             // => 36
size(der('a', der('a', der('a', EVIL))))   // => 83

// size with simplification
size(simp(der('a', der('a', EVIL))))           // => 7
size(simp(der('a', der('a', der('a', EVIL))))) // => 7

// Python needs around 30 seconds for matching 28 a's with EVIL. 
// Java 9 and later increase this to an "astonishing" 40000 a's in
// 30 seconds.
//
// Lets see how long it really takes to match strings with 
// 5 Million a's...it should be in the range of a few
// of seconds.

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  "%.5f".format((end - start)/(i * 1.0e9))
}

for (i <- 0 to 5000000 by 500000) {
  println(s"$i ${time_needed(2, matcher(EVIL, "a" * i))} secs.") 
}

// another "power" test case 
simp(Iterator.iterate(ONE:Rexp)(r => SEQ(r, ONE | ONE)).drop(50).next()) == ONE

// the Iterator produces the rexp
//
//      SEQ(SEQ(SEQ(..., ONE | ONE) , ONE | ONE), ONE | ONE)
//
//    where SEQ is nested 50 times.

*/

}
