/**
  * 1. PRIME NUMBERS
  *  Create a function, named prime, that takes an integer and returns a
  *  Boolean indicating whether the integer parameter is a prime number.
  *  A prime number is an integer greater than 1 that has no positive divisors
  *  other than 1 and itself.
  * @param num
  * @return
  */
def prime(num : Int): Boolean = {
  (2 until num) forall (x => num % x != 0) //forall to check if it holds for all elements in the range
}
prime(7) //true
prime(45) //false

/**
  * 2. Twin Primes
  * . Create a function, named twinprimes, that takes 2 integer parameters and
  * returns a Boolean  indicating whether the parameters are twin primes.
  * A twin prime is a prime number that differs from another prime number by two,
  * for example the twin prime pair (41, 43).
  * For example, twinprimes (41, 43) should return true and twinprimes (43, 47)
  * would return false.
  */
def twinPrimes(i : Int, j : Int): Boolean = {
  /*
  prime(i) && prime(j) match {
    case _ if i - j == 2 || j - i == 2 => true
    case _ => false
  }
  */
   //MADE MORE FUNCTIONAL ^, but doubled the twinPrimesList for some reason...
  if(prime(i) && prime(j)){
    if((i - j == 2) || (j - i == 2)){
      true
    }
    else
      false
  }
  else false
}
twinPrimes(5, 7) //true
twinPrimes(7, 5) //true
twinPrimes(41, 43) //true
twinPrimes(43, 47) //false

/**
  * 3. Twin Primes List
  *  Create a function, named twinprimeslist, that takes an integer, n,
  *  parameter and returns an integer list of all the twin primes starting up to n.
  *  For example, twinprimeslist (50) should return
  *  [3, 5, 7, 11, 13, 17, 19, 29, 31, 41, 43] (no duplicates).
  */
def twinPrimesList(num : Int): List[Int] = {
  num match {
    case a if a < 3 => Nil
    case _ =>
      if(twinPrimes(num - 2, num) || twinPrimes(num + 2, num)){
        num :: twinPrimesList(num - 1)//added .reverse to order from smallest to largest
      }
      else {
        twinPrimesList(num - 1) //added .reverse to order from smallest to largest
      }
  }
}
twinPrimesList(50) //List(3, 5, 7, 11, 13, 17, 19, 29, 31, 41, 43)
twinPrimesList(100) //List(3, 5, 7, 11, 13, 17, 19, 29, 31, 41, 43, 59, 61, 71, 73)

/**
  * 4. Goldbach’s Conjecture
  * Create a function, named goldbach, that takes an integer and prints
  * the solution satisfying the Goldbach Conjecture. The Goldbach Conjecture states
  * that every positive even number greater than 2 is the sum of two prime numbers.
  * For example, 28 = 5 + 23. Your function is to find the two prime numbers that
  * sum up to a given even integer and print the composition. For example goldbach(28)
  * would print 5 + 23 = 28. You should provide error checking to make sure the
  * integer parameter is even and greater than 2.
  */
def goldback(num : Int): Unit = {
  (evenCheck(num), num > 2) match {
    case (false, false) => println("Number must be even and greater than 2")
    case (false, true) => println("Number must be even")
    case (true, false) => println("Number must be greater than 2")
    case (true, true) => goldbackHelper(num, 1 to num toList)
  }
}

def goldbackHelper(num : Int, primes : List[Int]): Unit = {
  primes.reverse.filter(c => prime(c)) match {
    case head :: _ =>
      if (primes.contains(num - head)){
        if(true){
          println(num - head + " + " + head + " = " + num)
        }
        else {
          Nil
        }
      }
    case _ => println("Your number needs some milk :/")
  }
}

def evenCheck(num : Int): Boolean = {
  num % 2 match {
    case 0 => true
    case _ => false
  }
}
goldback(28) //5 + 23 = 28
goldback(50) //3 + 47 = 50
goldback(3) //Number must be even
goldback(0) //Number must be greater than 2
goldback(-3) //Number must be even and greater than 2

