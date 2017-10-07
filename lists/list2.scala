object list2 extends App {
    
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //PROBLEM 1
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    def avg(scores: List[Double]): Double = (scores.reduceLeft(_ + _))/scores.length
    
    def avgAvg(scores: List[List[Double]]): List[Double] = scores.map{
        case x: List[Double] => avg(x)
    }
    
    def passing(scores: List[List[Double]]): List[Int] = {
        val positions = avgAvg(scores).zipWithIndex.map{
            case (elem, index) if(elem >= 70) => index
            case _ => -1
        }
        
        positions.filter{x: Int => x != -1}
    }
    
    
    def sumSums(scores: List[List[Double]]):Double = {
        val avgs = scores.map{
            case x: List[Double] => avg(x)
        }
        
        avgs.reduceLeft(_ + _)
    }
    
    //TEST CASES
    println(avg(List(93, 89, 90)))
    println(avgAvg(List(List(93, 89, 90), List(75, 76, 68), List(88, 82, 78))))
    
    println(passing(List(List(80, 0, 25), List(45, 55, 18), List(88, 82, 78), List(70), List(70, 70, 69))))
    //passing returns a list of indexes where the average is >= 70
    
    println(sumSums(List(List(93, 89, 90), List(75, 76, 68), List(88, 82, 78))))
    println()

    
    //OUTPUT
    // 90.66666666666667
    // List(90.66666666666667, 73.0, 82.66666666666667)
    // List(2, 3)
    // 246.33333333333337
    
    
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //PROBLEM 2
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    
    def spellCheck(doc: List[String], dictionary: List[String]): List[String] = {
        val setDoc = doc.toSet
        val setDict = dictionary.toSet
        setDoc.diff(setDict).toList
    }
    
    //TEST CASE
    println(spellCheck(List("hi", "my", "name", "is", "phillip"), List("hi", "name", "is")))
    
    //OUTPUT
    // List(phillip, my)
    
    
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //PROBLEM 3
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    def spellCheckFilter(doc: List[String], dictionary: List[String]): List[String] = 
        doc.filter(x => !dictionary.contains(x))

    //TEST CASE
    println(spellCheckFilter(List("hi", "my", "name", "is", "phillip"), List("hi", "name", "is")))
    
    //OUTPUT
    // List(my, phillip)
    
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //PROBLEM 4
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
}