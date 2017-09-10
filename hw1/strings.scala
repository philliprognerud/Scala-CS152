object strings extends App {
    //++++++++++++++++++++++++
    //problem 1
    def isPal(input: String) = {
        var start = 0
        var end = input.length() - 1
        var isPal = true
        while(end > start){
            if(input(start) != input(end)){ 
                isPal = false
                start = end
            } else {
                start += 1
                end -= 1
            }
        }
        isPal
    }
    
    //tests
    println(isPal("racecar"))
    println(isPal("test"))
    println(isPal("mom"))
    println()
    
    //++++++++++++++++++++++++
    //problem 2
}