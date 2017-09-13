object sequence extends App {
    
    //++++++++++++++++++++++++
    //problem 1
    def tax(value: Int) = 
        isEven(value) match {
            case true if value < 20000 => value * 0.0
            case true if value < 30000 => value * 0.05
            case true if value < 40000 => value * 0.11
            case true if value < 60000 => value * 0.23
            case true if value < 100000 => value * 0.32
            case true if value >= 100000 => value * 0.50
            case false => 0
            case e => e
            
        }
    
    def isEven(n: Int) =
        try{
            n match {
                case n if n < 0 => throw new Exception("Cannot have non-negative value")
                case n if n%2 == 0 => true
                case _ => false
            }
        } catch {
            case e: Exception => e
        }
        
    
    //tests
    println(tax(35000))
    println(tax(15000))
    println(tax(95000))
    println(tax(-100))
    
    //RESULTS
    // 3850.0
    // 0.0
    // 30400.0
    //java.lang.Exception: Cannot have non-negative value
}