object ExercisesDay03 {
  def main(args: Array[String]): Unit = {
    //Exercise 1
    println(Teams())
    println()
    //Exercise 2
  }

  //Exercise 1
  class Score(name: String) {
    private val team: String = name
    private var Score: Int = 0

    def getName: String = {
      team
    }

    def getScore: Int = {
      Score
    }

    def scoreGoal: Int = {
      Score = Score + 1
      Score
    }

    def isWinner(other: Score): Boolean = {
      var bool = false
      if (Score > other.getScore) {
        bool = true
      }
      else if (Score < other.getScore) {
        bool = false
      }
      else {
        println("Tie game")
      }
      bool
    }

  }

  def Teams(): String = {
    println("Name of Team 1: ")
    val name1 = scala.io.StdIn.readLine()
    println("Name of Team 2: ")
    val name2 = scala.io.StdIn.readLine()
    val Team1 = new Score(name1)
    val Team2 = new Score(name2)
    val r = new scala.util.Random
    val r1 = r.nextInt(100)
    val r2 = r.nextInt(100)
    for (i <- 1 to r1) {
      Team1.scoreGoal
    }
    println(s"${Team1.getName} score: ${Team1.getScore}")
    for (j <- 1 to r2) {
      Team2.scoreGoal
    }
    println(s"${Team2.getName} score: ${Team2.getScore}")
    if (Team1.isWinner(Team2)) {
      s"${Team1.getName} is the winner!"
    }
    else if (Team2.isWinner(Team1)) {
      s"${Team2.getName} is the winner!"
    }
    else {
      s"${Team1.getName} and ${Team2.getName} tie!"
    }
  }

  //Exercise 2
  class sparseVector(vector: Map[Int, Int]) {
    private val v1:Map[Int,Int] = vector
    print("How long is the vector: ")
    private val length:Int = scala.io.StdIn.readInt()
    if (!isSparse) {
      println("The vector is not spars.")
      sys.exit(0)
    }

    def get_vector: Map[Int, Int] = {
      v1
    }

    def isSparse:Boolean = {
      var num = 0
      for (i<-v1){
        num= num + 1
      }
      if ((num/length)*100 > 75){
        true
      }
      else{
        false
      }
    }

    def get_total: Int = {
      val values = v1.values
      var total = 0
      for (i <- values) {
        total = total + i
      }
      total
    }

    def toSparseVector(P1: List[Int]): sparseVector = {
        var retMap = Map[Int, Int]()
        for (i <- 0 to P1.length if P1(i) != 0) {
          retMap += (i -> P1(i))
        }
        sparseVector.this
    }

    def vectorSum(other: sparseVector): sparseVector = {
      var m3 = Map[Int, Int]()
      for (i <- v1.keys) {
        if (other.get_vector.contains(i)) {
          m3 = m3 + (i -> (v1(i) + other.get_vector(i)))
        }
        else {
          m3 = m3 + (i -> v1(i))
        }
      }
      for (j <- other.get_vector.keys) {
        if (!m3.contains(j)) {
          m3 = m3 + (j -> other.get_vector(j))
        }
      }
      sparseVector.this
    }

    def vectorDifference(other:sparseVector): sparseVector = {
      var m= Map[Int,Int]()
      for (i<-v1.keys){
        if (other.get_vector.contains(i)){
          m = m + (i -> Math.abs(v1(i) - other.get_vector(i)))
        }
        else {
          m = m + (i -> (0-v1(i)))
        }
      }
      for (j <- other.get_vector.keys) {
        if (!m.contains(j)) {
          m = m + (j -> (0-other.get_vector(j)))
        }
      }
      sparseVector.this
    }

    def vectorIntScalar(scalar: Int):sparseVector={
      var m = Map[Int, Int]()
      for (i <- v1.keys) {
        m = m + (i -> (v1(i) * scalar))
      }
      sparseVector.this
    }

    def vectorDoubleScalar(scalar: Double):sparseVector={
      var m = Map[Int, Double]()
      for (i <- v1.keys) {
        m = m + (i -> (v1(i).toDouble * scalar))
      }
      sparseVector.this
    }

    def vectorDot(other: sparseVector):Int={
      var m3=Map[Int,Int]()
      for (i<-v1.keys) {
        if (other.get_vector.contains(i)) {
          m3 = m3 + (i -> (v1(i) * other.get_vector(i)))
        }
      }
      m3.values.sum
    }

    def vectorCross(other:sparseVector):sparseVector={
      
    }
  }

}
