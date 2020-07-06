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
   */
  def pascal(c: Int, r: Int): Int  = (c,r) match {
    case (0,_) | (_,0) => 1
    case (c,r) if c == r => 1
    case (c,r) => pascal(c-1,r-1) + pascal(c,r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def calc(opened:Int, chars: List[Char]) : Boolean = (opened,chars) match{
      case  (opened, chars) if chars.isEmpty => opened == 0
      case  (opened, chars) => calcNotEmpty(opened,chars)
    }

    def calcNotEmpty(opened:Int, chars: List[Char]) : Boolean = (opened,chars.head) match{
      case  (opened, ')') if opened <= 0 => false
      case  (opened, ')') => calc(opened-1, chars.tail)
      case  (opened, '(') => calc(opened+1, chars.tail)
      case  (_,_ ) => calc(opened, chars.tail)
    }

    calc(0,chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    {

        if(money< 0 || coins.isEmpty)  0
        else if (money == 0) 1
        else countChange(money - coins.head, coins) + countChange(money,coins.tail)

      //      def rec(money: Int, coins: List[Int]): Int = {
        //        case (_,coins) if coins.i sEmpty => 0
//        case (money,_) if money == 0 => 1
//        case (m,c) => {
//          var res = 0
//          if(m - c.head >= 0){
//             rec(money - c.head, c.tail) + rec(money - c.head, c) + rec(money, c.tail) -1
//          }else{
//            rec(money, c.tail)
//          }
//        }
//      }

      //rec(money,coins.sorted.reverse)
    }
}