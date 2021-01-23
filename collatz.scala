// Part 1 about the 3n+1 conjecture
//=================================

object CW6a {

//(1) collatz function below. It should
//    recursively calculate the number of steps needed 
//    until the collatz series reaches the number 1.
//    If needed, can use an auxiliary function that
//    performs the recursion. The function should expect
//    arguments in the range of 1 to 1 Million.

def collatz(n: Long) : Long = //...
  innerFunc(n, 1)
    
def innerFunc(n: Long, count: Int): Int = {

    if (n == 1) {
	  count
	}
    
    else if (n % 2 == 0) { 
      val r = n / 2
	  val countUp = count + 1
	  if (r == 1) countUp
	  else
      innerFunc(r, countUp)
	  
    }
    
    else {
      val r = (3 * n + 1)
      val countUp = count + 1	  
	  if (r == 1) countUp
	  else
      innerFunc(r, countUp)
	  
    }
	
}

//(2)  collatz-bound function below. It should
//     calculate how many steps are needed for each number 
//     from 1 up to a bound and then calculate the maximum number of
//     steps and the corresponding number that needs that many 
//     steps. Again, expect bounds in the range of 1
//     up to 1 Million. The first component of the pair is
//     the maximum number of steps and the second is the 
//     corresponding number.

def collatz_max(bnd: Long) : (Long, Long) = { 

	val listOfResults = 					
	for (n <- (1 to bnd.toInt).toList)
	yield collatz(n.toLong)
	
	val listOfEntries = (1 to bnd.toInt).toList
	val pair = (listOfResults.max.toLong, listOfEntries.apply(listOfResults.indexOf(listOfResults.max)).toLong)
	
	pair
	
}
	
}