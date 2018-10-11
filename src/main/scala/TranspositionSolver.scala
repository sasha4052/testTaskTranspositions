
object TranspositionSolver {

  private[TranspositionSolver] def generateConditions(squareList: Array[Array[Int]]): Seq[Int] = {
    val conditions = Seq(
      squareList(0)(1)+squareList(1)(0),
      squareList(0)(2)+squareList(3)(0)+squareList(2)(1),
      squareList(1)(3)+squareList(4)(1)+squareList(5)(0),
      squareList(2)(2)+squareList(6)(0),
      squareList(5)(3)+squareList(9)(1),
      squareList(6)(3)+squareList(7)(2)+squareList(10)(0),
      squareList(11)(1)+squareList(8)(3)+squareList(10)(2),
      squareList(0)(3)+squareList(1)(2)+squareList(3)(1)+squareList(4)(0),
      squareList(2)(3)+squareList(3)(2)+squareList(6)(1)+squareList(7)(0),
      squareList(3)(3)+squareList(4)(3)+squareList(7)(1)+squareList(8)(0),
      squareList(4)(3)+squareList(5)(2)+squareList(8)(1)+squareList(9)(0),
      squareList(7)(3)+squareList(8)(2)+squareList(10)(1)+squareList(11)(0)
    )
    conditions
  }

  private[TranspositionSolver] def checkConditions(cond: Seq[Int]): Boolean = {
    var result: Boolean = true
    for (i <- 0 to cond.length-1)  {
      if(if(i < 7)  cond(i) > 10 else cond(i) != 10) result = false
    }
    result
  }
  private[TranspositionSolver] def changePosition(squareList: Array[Array[Int]], first: Int, second: Int) = {
    val temp: Array[Int] = squareList(first)
    squareList(first) = squareList(second)
    squareList(second) = temp
  }

  def solve(squareList: Array[Array[Int]], currentPosition: Int = 0): Boolean  = {
    var existAns: Boolean = false
    if (currentPosition == 1) {
      if (checkConditions(generateConditions(squareList))) {
        existAns = true
        printList(squareList)
      }
    }
    else {
      for(i <- 0 to currentPosition-1) {
        existAns = existAns||solve(squareList, currentPosition - 1)
        if (currentPosition % 2 == 1) changePosition(squareList, 0, currentPosition - 1)
        else changePosition(squareList, i, currentPosition - 1)
      }
    }
    existAns
  }


  def printList(squareList: Array[Array[Int]])={
    for(i <- 0 to squareList.length-1){
      for(j <- 0 to 3){
        print(squareList(i)(j))
      }
      println()
    }
    println(generateConditions(squareList))
    println("-----------------")
  }

}
