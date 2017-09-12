object sequence extends App {
    
    //++++++++++++++++++++++++
    //problem 1
    def tax(value: Double) = {
        var key = ""
        var tax = 0.0
        
        if(value < 20000) key = "20,000"
        else if(value < 30000) key = "30,000"
        else if(value < 40000) key = "40,000"
        else if(value < 60000) key = "60,000"
        else if(value < 100000) key = "100,000"
        else if(value >= 100000) key = ">100,000"
        
        key match {
            case "20,000" => tax = value * 0.0
            case "30,000" => tax = value * 0.05
            case "40,000" => tax = value * 0.11
            case "60,000" => tax = value * 0.23
            case "100,000" => tax = value * 0.32
            case ">100,000" => tax = value * 0.50
        }
        
        tax
    }
    
    //tests
    println(tax(35000))
    println(tax(15000))
    println(tax(95000))
    println()
    
    //RESULTS
    // 3850.0
    // 0.0
    // 30400.0
}