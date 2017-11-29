//Chinese and English lists
val chinese: List[String] = List("ling", "yi", "er", "san", "si", "wu", "liu", "qi", "ba", "jiu", "shi")
val english: List[String] = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")

/**
  * Main method that adds chinese and english words, by translating them to integers and doing the operations
  * @param list
  */
def go(list : List[String]): Unit = {
  val translation : List[Int] = translate(list)
  println("Translation: " + printHelper(translation, " "))
  println("Addition: " + printHelper(translation, " + ") + " = " + addition(translation) + " ")
  println()
  println("Multiplication: " + printHelper(translation, " * ") + " = " + multiplication(translation) + " ")
}

/**
  * Translates english and chinese word lists into integer lists
  * @param EClist
  * @return
  */
def translate(EClist : List[String]): List[Int] = {
  EClist match {
    case Nil => Nil
    case head :: tail =>
      chinese.contains(head) match {
        case true => chinese.indexOf(head) :: translate(tail)
        case false =>
          english.contains(head) match {
            case true => english.indexOf(head) :: translate(tail)
            case false => translate(tail)
          }
      }
  }
}

/**
  * Helps print the translation in the same way described in the project instructions
  * +, *, =, etc
  * @param list
  * @param etc
  * @return
  */
def printHelper(list : List[Int], etc : String): String = {
  list match {
    case Nil => ""
    case head :: tail =>
      tail match {
        case Nil => head + ""
        case _ => head + etc + printHelper(tail, etc)
      }
  }
}

/**
  * Adds the list
  * @param Numlist
  * @return
  */
def addition(Numlist : List[Int]): Int = {
  Numlist.foldLeft(0)(_+_)
}

/**
  * Multiplies the list
  * @param Numlist
  * @return
  */
def multiplication(Numlist : List[Int]): Int = {
  Numlist.foldLeft(1)(_*_)
}

//Test cases
go(List("yi", "nine", "six", "ba", "josh"))
go(List("one", "nine", "six", "eight"))
go(List("yi","josh","three","si"))