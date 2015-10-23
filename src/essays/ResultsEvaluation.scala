package essays

object ResultsEvaluation {
  
  def main(args:Array[String]):Unit = {
    
    val labels = ((1 to 5) map {_.toString}) :+ "weightedAvg"
    
    val results = io.Source.fromFile("output/results.txt").mkString.split("\n\n").map{ results =>
      val resultSet = results.split("\n")
      val approach = resultSet.head
      approach -> (labels zip (resultSet.tail map {_.trim.toDouble})  toMap) 
    } toMap
    
    val bestApproachPerLabel = labels map { label => label -> results.map {
      case(approach,labels) =>
      approach -> labels(label)}.toSeq.sortBy(_._2).reverse.head
    } 
    
    bestApproachPerLabel foreach { case(label,(approach,fMeasure)) =>
      println(s"Label:\t$label")
      println(s"Best Approach:\t$approach")
      println(s"Best FMeasure:\t$fMeasure")
      println
    }
    
  }
  
}