package recursion

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
   * @param c  column index
   * @param r  row index
   * @return The number at a position identified by 'c' and 'r'
   */
	  def pascal(c: Int, r: Int): Int =
		  if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  
  /**
   * Exercise 2
   * @param chars The validated string
   * @return true, if parentheses is balanced, false otherwise
   */
	  def balance(chars: List[Char]): Boolean = {
  		def score(chars: List[Char], acc: Int = 0): Int =
  		  if (chars.isEmpty || acc < 0)
  			acc
  		  else {
  			val c = chars.head
  			score(chars.tail, if (c == '(') acc + 1 else if (c == ')') acc - 1 else acc)
  		  }
  		score(chars) == 0
	  }
  
  /**
   * Exercise 3
   * @param money An amount to change
   * @param coins A list of unique denominations for the coins
   * @return The total number of change combinations
   */
    def countChange(money: Int, coins: List[Int]): Int = {
  		def countCombinations(money: Int, coins: List[Int]): Int =
  		  if (money == 0) 1
  		  else if (money < 0 || coins.isEmpty) 0
  		  else countCombinations(money - coins.head, coins) + countCombinations(money, coins.tail)
  	countCombinations(money, coins)
    }
  }
