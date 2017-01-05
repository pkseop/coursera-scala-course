package recfun

object Main {
  def main(args: Array[String]) {
//    println("Pascal's Triangle")
//    for (row <- 0 to 10) {
//      for (col <- 0 to row)
//        print(pascal(col, row) + " ")
//      println()
//    }
//
//    println(countChange(4, List(2,1)))
//    println(countChange(8, List(2,3,1)))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if(c == 0 || (c == r))
        1
      else
        pascal(c, r -1) + pascal(c-1, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def loop(acc: Int, chars: List[Char]): Boolean = {
        if (!chars.isEmpty) {
          val c = chars.head
          if (c == '(')
            loop(acc + 1, chars.tail)
          else if (c == ')') {
            if(acc > 0)
              loop(acc - 1, chars.tail)
            else
              false
          } else
            loop(acc, chars.tail)
        } else {
          if (acc == 0)
            true
          else
            false
        }
      }
      loop(0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def loop(acc: Int, coins:List[Int], remnant: Int): Int = {
        if(coins.isEmpty)
          acc
        else {
          var v = 0
          val tail = coins.tail
          var res = acc
          while(v <= remnant) {
            if(v == remnant)
              return res + 1
            res = loop(res, tail, remnant - v)
            v += coins.head
          }
          res
        }
      }

      loop(0, coins.sorted, money)
    }
  }
