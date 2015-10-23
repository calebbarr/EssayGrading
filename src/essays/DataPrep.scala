package essays

object DataPrep {
  
  val FEATURE_THRESHOLD = 0.05
  val BINARIZE = false
  
  lazy val features = io.Source.fromFile("resources/features.txt").getLines
    .toList.filter{_.trim.size > 0}.filter(FEATURE_THRESHOLD)
  
  lazy val (unigrams,bigrams,trigrams) = {
    val allGrams = (1 to 3) map {n => 
      getEssays flatMap {e=>ngrams(e.doc,n)} map {_._1} filter features.contains distinct
    }
    (allGrams(0),allGrams(1),allGrams(2))
  }

  def getEssays = {
    def input = io.Source.fromFile("resources/data.txt")
    val docs = input.mkString.split("scores//")(0).split("[0-9]{2}//").filter(_.size > 0)
    val scores = input.mkString.split("scores//")(1).split("\\s+")
      .filter(_.size > 0).grouped(2).map(_(1)).toArray
    docs zip scores map {case(doc,score)=> new Essay(doc,score)}
  }
  
  def normalize(s:String) = s.replaceAll("[\n\r.,'(\\-)]", "").trim.toLowerCase
  def counts[T](s: Seq[T]) = s.groupBy(identity).mapValues(_.length)
  def ngrams(text:String,n:Int=1) = 
    text.split("\\s+").map(normalize).sliding(n).map{_.reduce{_+"_"+_}}.
    toList.filter{_.trim.size > 0}.groupBy{ identity }.map{x => (x._1->x._2.size.toDouble)}

  implicit class Features(features:List[String]) {    
    def filter(threshold:Double) = {
      assert(threshold >= 0.0 && threshold <= 1.0)
       features.slice(0,math.round((features.size * threshold)).toInt ) 
    }
    def indexOfOrElse(f:String) = 
      features.indexOf(f) match {
        case -1 => features.size
        case i @ _ => i
    }
  }
  
  def main(args:Array[String]) = {
    printArff(getEssays map {_.asInstance})
  }
  
  implicit class Averageable(averageable:Array[Int]) {
    def average = averageable.reduce(_+_)/averageable.size.toDouble
  }
  
  implicit class Binarizable(map:Map[String,Int]) {
    def getOrElse(s:String,i:Int) = 
      if(BINARIZE) if(map.contains(s)) 1.0 else 0.0 else map.getOrElse(s, i)
  }
  
  class Essay(val doc:String, val score:String){
    
    val tokens = doc.split("\\s+") map normalize filter{_.trim.size > 0}
    lazy val (unigrams,bigrams,trigrams) = (ngrams(doc,1),ngrams(doc,2),ngrams(doc,3))
    
    def asBagOfWords = tokens distinct
    def asWeightedBagOfWords = asWordCounts map {
        case (term,count)=>term->count/doc.size.toDouble}
    def asWordCounts = counts(tokens)
    
    def getEssayLength = ("essayLength",doc.size.toDouble)
    def getAverageWordLength = ("avgWordLength",tokens.map(_.size).average)
    def getAvgWordRank = ("avgWordRank",tokens.map(features.indexOfOrElse).average)
    def getNumSentences = ("numSentences", doc.filter(_ == '.').size.toDouble)
    def numQuestions = ("numQuestions", doc.filter(_=='?').size.toDouble)
    
    def getMetaFeatures = List(
      getEssayLength,
      getAverageWordLength,
      getAvgWordRank,
      getNumSentences,
      numQuestions
      )
      
    def createNgramFeatures(n:Int) = 
      n match {
        case 1 => DataPrep.unigrams map {g => (g,unigrams.getOrElse(g,0.0))}
        case 2 => DataPrep.bigrams map {g => (g,bigrams.getOrElse(g,0.0))}
        case 3 => DataPrep.trigrams map {g => (g,trigrams.getOrElse(g,0.0))}
      }
    
    def getNgramFeatures =
      List(
        1,
        2,
        3
      ) flatMap createNgramFeatures
      
    def getAllFeatures =
      getMetaFeatures ::: 
      getNgramFeatures
    
    def asInstance = new Instance(getAllFeatures,score)
    
  }
  
  class Instance(val features:List[(String,Double)], val label:String){
    override def toString = features.map{_._2.toString}.reduce{_+","+_}+","+label
  }
  
  def printArff(instances:Array[Instance]) = {
    val output = new java.io.PrintWriter(new java.io.File("output/essays.arff"))
    output.println("""
     % Essay Scores
     % 
     % Caleb Barr
     % caleb@xbarr.me
     % 
     @RELATION score
     
     """)
    
    instances(0).features foreach { case(label,value) =>
      output.println(s"@ATTRIBUTE $label NUMERIC")
    }
    output.println(s"@ATTRIBUTE score {1,2,3,4,5}")
    output.println
    output.println
    output.println("@DATA")
    instances foreach output.println
    output.println
    output.close
  }
  
}