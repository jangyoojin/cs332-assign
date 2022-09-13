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
    if (c == 0 || r == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def check(chars: List[Char], cnt: Int): Boolean = {
      if(chars.isEmpty) cnt == 0
      else if(chars.head=='(') check(chars.tail, cnt+1)
      else if(chars.head==')') cnt > 0 && check(chars.tail, cnt-1)
      else check(chars.tail, cnt)
    }
    check(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    if(coins.isEmpty || money < 0) 0
    else if(money == 0) 1
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)

    /** 첫번째 항은 money를 만드는 동전의 개수를 제한한다. 따라서 한 가지, 두 가지 등의 동전으로
     * 이루어진 way를 구할 수 있다. 두번째 항은 현재 가진 coins의 모든 동전을 사용하는 방법을 탐색한다.
    **/
  }
}
