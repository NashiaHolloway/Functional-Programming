// COSC 455 - Programming Languages: Implementation and Design
// Project 2

// NAME: Nashia Holloway


// Test Cases
val pTest1: List[Int] = List (1, 1, 1, 1, 0)
val qTest1: List[Int] = List(1, 0, 1, 1)
val test1ExpectedSolution: List[Int] = List(1, 0, 1, 0, 0, 1)

val pTest2: List[Int] = List (1, 0, 0, 1, 1, 0, 1)
val qTest2: List[Int] = List(1, 0, 0, 1, 0)
val test2ExpectedSolution: List[Int] = List(1, 0, 1, 1, 1, 1, 1)

val pTest3: List[Int] = List (1, 0, 0, 1, 0, 0, 1)
val qTest3: List[Int] = List(1, 1, 0, 0, 1)
val test3ExpectedSolution: List[Int] = List(1, 1, 0, 0, 0, 1, 0)

val pTest4: List[Int] = List (1, 0, 0, 0, 1, 1, 1)
val qTest4: List[Int] = List(1, 0, 1, 1, 0)
val test4ExpectedSolution: List[Int] = List(1, 0, 1, 1, 1, 0, 1)

val test5ExpectedSolution: List[Int] = List(1, 1, 1, 0, 1, 1)
val test6ExpectedSolution: List[Int] = List(1, 1, 0, 0, 0, 1)

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

/**
  * This function converts a binary integer list into its corresponding boolean list.
  * @param intList
  * @return
  */
def convertIntListToBooleanList(intList: List[Int]) = {
  intList.map {
    case 0 => false
    case 1 => true
  }
}

/**
  * This function converts a boolean list into its corresponding binary integer list.
  * @param booleanList
  * @return
  */
def convertBooleanListToIntList(booleanList: List[Boolean]) = {
  booleanList.map {
    case false => 0
    case true => 1
  }
}

/**
  * This function does the two's compliment for binary subtraction.
  * Map the list to switch bits.
  * @param subList
  * @return
  */
def compliment(subList : List[Int]) : List[Int] = {
  binaryAddition(convertBooleanListToIntList(convertIntListToBooleanList(subList).map
  (bit => !bit)), List(1))
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

/**
  * Main function to do binary subtraction. This function should:
  * 1. Do the binary addition of pList and the two's compliment of qList
  * 2. Drop the absolute value of the difference of the length of the lists.
  * 3. Append the sign bit 1 to the beginning of the list for two's compliment.
  * @param pList
  * @param qList
  * @return
  */
def binarySubtraction(pList: List[Int], qList: List[Int]) = {
  1 :: binaryAddition(pList, compliment(qList)).drop(Math.abs(pList.length - qList.length))
}

// Testing binary addition.
if (binaryAddition(pTest1, qTest1).equals(test1ExpectedSolution)) println("Test 1 passes!") else println("Test 1 fails.")
if (binaryAddition(pTest2, qTest2).equals(test2ExpectedSolution)) println("Test 2 passes!") else println("Test 2 fails.")
if (binaryAddition(pTest3, qTest3).equals(test3ExpectedSolution)) println("Test 3 passes!") else println("Test 3 fails.")
if (binaryAddition(pTest4, qTest4).equals(test4ExpectedSolution)) println("Test 4 passes!") else println("Test 4 fails.")

// Testing binary subtraction.
if (binarySubtraction(pTest2, qTest2).equals(test5ExpectedSolution)) println("Test 5 passes!") else println("Test 5 fails.")
if (binarySubtraction(pTest4, qTest4).equals(test6ExpectedSolution)) println("Test 6 passes!") else println("Test 6 fails.")
