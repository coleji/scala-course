package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int =
      if (r < 0 || c < 0 || c > r) return 0     // invalid input
      else if (c == 0 || c == r) 1              // edges are always 1
      else pascal(c, r-1) + pascal (c-1, r-1)   // with the edges covered explicitly, return the sum of
                                                // the above two values (which will always exist)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanceRecurse(opens: Int, chars: List[Char]): Boolean =
        if (opens < 0) false
        else if (chars.isEmpty) opens == 0
        else if (chars.head == '(') balanceRecurse(opens+1, chars.tail)
        else if (chars.head == ')') balanceRecurse(opens-1, chars.tail)
        else balanceRecurse(opens, chars.tail)

      balanceRecurse(0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      var i = 0;
      // take all distinct denominations in desc order, e.g. (100, 50, 25, 10, 5, 1)
      // plug in zero 100's, and recursively get the number of combinations with (50, 25, 10, 5, 1) that add to money
      // then plug in 1x100 and recursively get the number of combinations with (50, 25, 10, 5, 1) that add up to (money - 100)
      // ... etc
      // keep incrementing the 100's coefficient until we go past money and then we're done
      def tryCoefficient(partialSum: Int, coeff: Int, denoms: List[Int]): Int = {
        i = i + 1
        println(i + " steps")
        if (denoms.isEmpty) 0
        else if (partialSum + coeff * denoms.head == money) 1
        else if (partialSum + coeff * denoms.head > money) 0
        else if (denoms.tail.nonEmpty) {
          // compute the partial sum based on the coefficient we're trying now,
          // and get all combinations from smaller denominations
          tryCoefficient(
            partialSum +
            (coeff * denoms.head), 0, denoms.tail) +
            tryCoefficient(partialSum, coeff+1, denoms
          )
        }
        else {
          // there are no more denominations below this one; just increment this coefficient
          tryCoefficient(partialSum, coeff+1, denoms)
        }
      }

      if (coins.isEmpty) 0
      else tryCoefficient(0, 0, coins.distinct.sortWith((x: Int, y: Int) => x > y)) // full list of all distinct denominations in desc order
    }
  }
