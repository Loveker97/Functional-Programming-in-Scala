package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   * recursive funnction to take two Int as input, output the value
   *    in the corresponding  value in pascal triangle
   * @c: column in pascal triangle
   * @r: row in pascal triangle
   * @return: value in the corresponding position
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == 0 || r == c) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   * recursive function to verify if the input string has balanced parentheses
   * @chars: Char array represents the string
   * @return: true or false
   */
  def balance(chars: List[Char]): Boolean = {
    def validParentheses(chars: List[Char], acc: Int) : Int = {
      if (chars.isEmpty || acc < 0 ) acc
      else if (chars.head == '(') validParentheses(chars.tail, acc + 1 )
      else if (chars.head == ')') validParentheses(chars.tail, acc - 1)
      else validParentheses(chars.tail, acc)
    }
    validParentheses(chars, 0) == 0
  }

  /**
   * Exercise 3
   * recursive function to count how many different ways to make change for an amount
   * @money: total amount of money
   * @coins: change list.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
