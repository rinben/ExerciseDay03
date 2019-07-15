object ExercisesDay03 {
  def main(args:Array[String]): Unit ={
    println(Teams())
  }
  class Score(name:String){
    private var team:String=name
    private var Score:Int=0

    def getName:String={
      team
    }
    def getScore:Int={
      Score
    }

    def scoreGoal:Int={
      Score=Score+1
      Score
    }

    def isWinner(other:Score):Boolean={
      var bool=false
      if (Score>other.getScore){
        bool=true
      }
      else if (Score<other.getScore){
        bool=false
      }
      else{
        println("Tie game")
      }
      bool
    }

  }
  def Teams():String={
    println("Name of Team 1: ")
    val name1=scala.io.StdIn.readLine()
    println("Name of Team 2: ")
    val name2=scala.io.StdIn.readLine()
    val Team1=new Score(name1)
    val Team2=new Score(name2)
    val r= new scala.util.Random
    val r1=r.nextInt(100)
    val r2=r.nextInt(100)
    for (i<-1 to r1){
      Team1.scoreGoal
    }
    println(s"${Team1.getName} score: ${Team1.getScore}")
    for (j<-1 to r2){
      Team2.scoreGoal
    }
    println(s"${Team2.getName} score: ${Team2.getScore}")
    if (Team1.isWinner(Team2)){
      s"${Team1.getName} is the winner!"
    }
    else if (Team2.isWinner(Team1)){
      s"${Team2.getName} is the winner!"
    }
    else{
      s"${Team1.getName} and ${Team2.getName} tie!"
    }
  }
}
