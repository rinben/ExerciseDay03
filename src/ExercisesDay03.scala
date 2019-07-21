object ExercisesDay03 {
  def main(args: Array[String]): Unit = {
    //Exercise 1
    //println(Teams())
    //println()
    //Exercise 2
    println(sparseTest())
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
  class sparseVector(vector: (Map[Int, Int], Int)) {
    private val v1:Map[Int,Int] = vector._1
    private val length:Int = vector._2
    def get_vector: Map[Int, Int] = {
      v1
    }

    def isSparse:Boolean = {
      val num = v1.size
      if ((num/length)*100 <= 25){
        true
      }
      else{
        false
      }
    }

    def get_total: Int = {
      if (!isSparse) {
        println("The vector is not sparse.")
        sys.exit(0)
      }
      val values = v1.values
      var total = 0
      for (i <- values) {
        total = total + i
      }
      total
    }

    def vectorSum(other: sparseVector): sparseVector = {
      if (!isSparse) {
        println(s"The vector $v1 is not sparse.")
        sys.exit(0)
      }
      if (!other.isSparse) {
        println(s"The vector $other is not sparse.")
        sys.exit(0)
      }
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
      val sv= new sparseVector((m3,m3.size))
      sv
    }

    def vectorDifference(other:sparseVector): sparseVector = {
      if (!isSparse) {
        println("The vector is not sparse.")
        sys.exit(0)
      }
      if (!other.isSparse) {
        println("The vector is not sparse.")
        sys.exit(0)
      }
      var m= Map[Int,Int]()
      for (i<-v1.keys){
        if (other.get_vector.contains(i)){
          m = m + (i -> (v1(i) - other.get_vector(i)))
        }
        else {
          m = m + (i -> (v1(i)-0))
        }
      }
      for (j <- other.get_vector.keys) {
        if (!m.contains(j)) {
          m = m + (j -> (0-other.get_vector(j)))
        }
      }
      val sv= new sparseVector((m,m.size))
      sv
    }

    def vectorScalar(scalar: Int):sparseVector={
      if (!isSparse) {
        println("The vector is not sparse.")
        sys.exit(0)
      }
      var m = Map[Int, Int]()
      for (i <- v1.keys) {
        m = m + (i -> (v1(i) * scalar))
      }
      val sv= new sparseVector((m,m.size))
      sv
    }

    def vectorDot(other: sparseVector):Int={
      if (!isSparse) {
        println("The vector is not sparse.")
        sys.exit(0)
      }
      if (!other.isSparse) {
        println("The vector is not sparse.")
        sys.exit(0)
      }
      var m3=Map[Int,Int]()
      for (i<-v1.keys) {
        if (other.get_vector.contains(i)) {
          m3 = m3 + (i -> (v1(i) * other.get_vector(i)))
        }
      }
      m3.values.sum
    }
  }

  def toSparseVector(P1: List[Int]): sparseVector = {
    var retMap = Map[Int, Int]()
    for (i <- 0 until P1.length if P1(i) != 0) {
      retMap += (i -> P1(i))
    }
    val ret= new sparseVector((retMap,P1.length))
    if (!ret.isSparse) {
      println("The vector is not sparse.")
      sys.exit(0)
    }
    ret
  }

  def vectors():sparseVector={
    println("Enter a sparse vector in the form x,x,x,x,x,x,x: ")
    val in= scala.io.StdIn.readLine()
    val vector=in.split(',').map(x => x.toInt).toList
    val map=toSparseVector(vector)
    map
  }
  def sparseTest():String={
    val sv1= vectors()
    val sv2= vectors()
    println(s"Testing get_vector method. Sparse vector 1: ${sv1.get_vector}")
    println(s"Testing get_vector method. Sparse vector 2: ${sv2.get_vector}")
    println(s"Testing get_total method. Sparse vector 1 total: ${sv1.get_total}")
    println(s"Testing get_total method. Sparse vector 2 total: ${sv2.get_total}")
    println(s"Testing vectorSum method. Sparse vector 1 + 2 sum: ${sv1.vectorSum(sv2).get_vector}")
    println(s"Testing vectorSum method. Sparse vector 2 + 1 sum: ${sv2.vectorSum(sv1).get_vector}")
    println(s"Testing vectorDifference method. Sparse vector 1 - 2 difference: ${sv1.vectorDifference(sv2).get_vector}")
    println(s"Testing vectorDifference method. Sparse vector 2 - 1 difference: ${sv2.vectorDifference(sv1).get_vector}")
    println(s"Testing vectorIntScalar method. Sparse vector 1 scaled by 2: ${sv1.vectorScalar(2).get_vector}")
    println(s"Testing vectorDot method. Sparse vector 1 dot sparse vector 2: ${sv1.vectorDot(sv2)}")
    println(s"Testing vectorDot method. Sparse vector 2 dot sparse vector 1: ${sv2.vectorDot(sv1)}")
    "\n"
  }

}
