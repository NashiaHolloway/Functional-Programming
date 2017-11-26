// COSC 455 - Programming Languages: Implementation and Design
// Project 2

// NAME: <Nashia Holloway>


// Test Cases
val pTest1: List[Int] = List (1, 1, 1, 1, 0)
val qTest1: List[Int] = List(1, 0, 1, 1)
val test1ExectedSolution: List[Int] = List(1, 0, 1, 0, 0, 1)

val pTest2: List[Int] = List (1, 0, 0, 1, 1, 0, 1)
val qTest2: List[Int] = List(1, 0, 0, 1, 0)
val test2ExectedSolution: List[Int] = List(1, 0, 1, 1, 1, 1, 1)

val pTest3: List[Int] = List (1, 0, 0, 1, 0, 0, 1)
val qTest3: List[Int] = List(1, 1, 0, 0, 1)
val test3ExectedSolution: List[Int] = List(1, 1, 0, 0, 0, 1, 0)

val pTest4: List[Int] = List (1, 0, 0, 0, 1, 1, 1)
val qTest4: List[Int] = List(1, 0, 1, 1, 0)
val test4ExectedSolution: List[Int] = List(1, 0, 1, 1, 1, 0, 1)

val test5ExectedSolution: List[Int] = List(1, 1, 1, 0, 1, 1)
val test6ExectedSolution: List[Int] = List(1, 1, 0, 0, 0, 1)

/**
  * This function does the binary addition when there are uneven lists and still must
  * finish the add with the carry bits.
  * @param remainingBits
  * @param carryBit
  * @return
  */
def finishBinaryAdd(remainingBits: List[Boolean], carryBit: Boolean): List[Boolean] = {
  (remainingBits.isEmpty, carryBit) match {
    case (false, true) => (!remainingBits.head) :: finishBinaryAdd(remainingBits.tail, remainingBits.head)
    case (false, false) => remainingBits
    case (true, true) => List(true)
  }
}

/**
  * This function determines what the next carry bit should be based on current bits.
  * @param pBit
  * @param qBit
  * @param carryBit
  * @return
  */
def getNextCarryBit(pBit: Boolean, qBit: Boolean, carryBit: Boolean): Boolean = {
  (pBit && qBit) || (pBit && carryBit) || (qBit && carryBit)
}

/**
  * This function does the binary addition of two Booleans and a carry bit.
  * @param pBit
  * @param qBit
  * @param carryBit
  * @return
  */
def addBits(pBit: Boolean, qBit: Boolean, carryBit: Boolean): Boolean = {
  (pBit == qBit) == carryBit
}

/**
  * This function does the binary addition of two boolean lists.
  * Note that the lists may not be equal in length.
  * @param pBits
  * @param qBits
  * @param carryBit
  * @return
  */
def doBinaryAddition(pBits: List[Boolean], qBits: List[Boolean], carryBit: Boolean): List[Boolean] = {
  (pBits.isEmpty, qBits.isEmpty, carryBit) match {
    case (false, true, _) => finishBinaryAdd(pBits, carryBit)
    case (true, false, _) => finishBinaryAdd(qBits, carryBit)
    case (false, false, _) => addBits(pBits.head, qBits.head, carryBit) :: doBinaryAddition(pBits.tail, qBits.tail, getNextCarryBit(pBits.head, qBits.head, carryBit))
  }
}

// This function converts a binary integer list into its corresponding boolean list.
def convertIntListToBooleanList(intList: List[Int]) = {
  intList.map {
    case 0 => false
    case 1 => true
  }
}

// This function converts a boolean list into its corresponding binary integer list.
def convertBooleanListToIntList(booleanList: List[Boolean]) = {
  booleanList.map {
    case false => 0
    case true => 1
  }
}

def compliment(subList : List[Int]) : List[Int] = {
  subList.head match {
    case 0 => subList.updated(subList.head, 1)
    case 1 => compliment(subList.tail)
  }
}

/** This is the "main" function to do binary addition. This function should:
    1. Convert the input parameter lists from integers to boolean. Use Scala reverse
    2. Reverse the lists (since binary addition is performed right to left). Use Scala reverse.
    3. Perform the binary addition with the doBinaryAddition function.
    4. Reverse the lists (to get back in proper order). Use Scala reverse.
    5. Convert the answer back to binary integer form for output.
  Note that the initial carry bit is assumed to be 0 (i.e., false).
*/
def binaryAddition(pList: List[Int], qList: List[Int]) = {
  convertBooleanListToIntList(doBinaryAddition(convertIntListToBooleanList(pList).reverse, convertIntListToBooleanList(qList).reverse, false).reverse)
}

def binarySubtraction(pList: List[Int], qList: List[Int]) = {
  convertBooleanListToIntList(doBinaryAddition(convertIntListToBooleanList(compliment(pList)).reverse, convertIntListToBooleanList(compliment(qList)).reverse, false).reverse)
}

// Testing binary addition.
if (binaryAddition(pTest1, qTest1).equals(test1ExectedSolution)) println("Test 1 passes!") else println("Test 1 fails.")
if (binaryAddition(pTest2, qTest2).equals(test2ExectedSolution)) println("Test 2 passes!") else println("Test 2 fails.")
if (binaryAddition(pTest3, qTest3).equals(test3ExectedSolution)) println("Test 3 passes!") else println("Test 3 fails.")
if (binaryAddition(pTest4, qTest4).equals(test4ExectedSolution)) println("Test 4 passes!") else println("Test 4 fails.")

// Testing binary subtraction.
if (binarySubtraction(pTest2, qTest2).equals(test5ExectedSolution)) println("Test 5 passes!") else println("Test 5 fails.")
if (binarySubtraction(pTest4, qTest4).equals(test6ExectedSolution)) println("Test 6 passes!") else println("Test 6 fails.")
