package recfun
import common._

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
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r ) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def recurse(chars: List[Char], opened: Int, isOpen: Boolean, isMatched: Boolean): Boolean = {
      if (chars.isEmpty) {
        isMatched
      } else if (chars.head == '(') {
        recurse(chars.tail, opened+1, true, false)
      } else if (chars.head == ')') {
        if (isOpen) {
          if (opened == 0) recurse(chars.tail, opened, false, false)
          else if (opened == 1) recurse(chars.tail, opened-1, false, true)
          else recurse(chars.tail, opened-1, true, false)
        } else {
          recurse(chars.tail, opened, isOpen, false)
        }
      } else {
        recurse(chars.tail, opened, isOpen, false)
      }
    }
    recurse(chars, 0, false, true)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money < 0) 0
      else if (coins.isEmpty) 0
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
