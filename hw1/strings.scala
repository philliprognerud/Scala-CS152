object strings extends App {
    //++++++++++++++++++++++++
    //problem 1
    def isPal(str: String) = {
        var input = str.trim
        input = input.toLowerCase
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
    println(isPal("   raCEcar"))
    println(isPal("   25%#%52  "))
    println(isPal("mOM    "))
    println()
    
    //++++++++++++++++++++++++
    //problem 2
    def filterString(str: String) = {
        var input = str.replaceAll("""[\p{Punct}]""", "")
        input = input.replaceAll("\\s", "")
        input = input.toLowerCase
        isPal(input)
    }
    
    
    //tests
    println(filterString("   rac ,e car    "))
    println(filterString("   25% # %52 !!! "))
    println(filterString(".....mOM    "))
    println()
    
    //++++++++++++++++++++++++
    //problem 4
    import scala.util.Random
    def mkWord(value: Int = 8) = {
        var buf = new StringBuilder
        var counter = 0
        while(counter < value){
            buf += Random.alphanumeric.filter(_.isLetter).head
            counter += 1
        }
        buf.toString.toLowerCase
    }
    
    //tests
    println(mkWord())
    println(mkWord(20))
    println(mkWord(4))
    println()
    
    //++++++++++++++++++++++++
    //problem 5
    import scala.util.Random
    def mkSentence(value: Int = 8) = {
        var buf = new StringBuilder
        
        for(_ <- 1 to value){
            var counter = 0
            val rand = Random.nextInt( (10 - 1) + 1 )
            while(counter < rand){
                buf += Random.alphanumeric.filter(_.isLetter).head
                counter += 1
            }
            buf.insert(buf.length(), " ")
        }
        
        var sent = buf.toString.toLowerCase.trim
        sent = sent.substring(0, 1).toUpperCase() + sent.substring(1)
        sent
    }
    
    //tests
    println(mkSentence())
    println(mkSentence(3))
    println(mkSentence(5))
    println()
    
}