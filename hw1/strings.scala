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
    
    //RESULTS
    // true
    // true
    // true
    
    //++++++++++++++++++++++++
    //problem 2
    def isPal2(str: String) = {
        var input = str.replaceAll("""[\p{Punct}]""", "")
        input = input.replaceAll("\\s", "")
        input = input.toLowerCase
        isPal(input)
    }
    
    
    //tests
    println(isPal2("   rac ,e car    "))
    println(isPal2("   25% # %52 !!! "))
    println(isPal2(".....mOM    "))
    println()
    
    //RESULTS
    // true
    // true
    // true
    
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
    
    //RESULTS
    // duqktoxc
    // ulepcoktbfbzbxkhkexc
    // ejsr
    
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
    
    //RESULTS
    // Duk j vr uxyuuf vmeefxq beimzqh vfxeytfrz pensivfu
    // W cgxnymfr mrnrjhdd
    // Szvqernk kbb  visiuo llpllor
    
    
    //++++++++++++++++++++++++
    //problem 8 
    def eval(str: String) = {
        try{
            if(!str.contains("+")) throw new Exception("missing operator")
            else if(!checkLetter(str)) throw new Exception("NumberFormatException")
            else {
                var newStr = str.replaceAll("\\s", "")
                val num1 = newStr.substring(0, newStr.indexOf("+"))
                val num2 = newStr.substring(newStr.indexOf("+")+1, newStr.length())
                num1.toDouble + num2.toDouble
            }
        } catch {
            case e: Exception => e
        }
    }
    
    def checkLetter(str: String) = {
        var noLetter = true
        var i = 0
        while(i != str.length()-1){
            if(str(i).isLetter){
                noLetter = false
            }
            i += 1
        }
        noLetter
    }
    
    //tests
    println(eval("2 * 4"))
    println(eval("21ds + 43"))
    println(eval("3 + 9"))
    println(eval("  -6   +  8   "))
    println()
    
    //RESULTS
    // java.lang.Exception: missing operator
    // java.lang.Exception: NumberFormatException
    // 12.0
    // 2.0
    
    //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //problem 9
    def evalModified(str: String) = {
        try{
            if(!str.contains("*")) throw new Exception("missing operator")
            else if(!checkLetter(str)) throw new Exception("NumberFormatException")
            else {
                if(str.contains("*")){
                    var newStr = str.replaceAll("\\s", "")
                    newStr = newStr.replaceAll("\\(", "")
                    newStr = newStr.replaceAll("\\)", "")
                    
                    val v1 = newStr.substring(0, newStr.indexOf("*"))
                    val v2 = newStr.substring(newStr.indexOf("*")+1, newStr.length())

                    var arr1 = v1.split(",")
                    var arr2 = v2.split(",")
                    var result = 0
                    
                    for(i <- 0 to 2){
                    	result += arr1(i).toInt * arr2(i).toInt
                    }
                    
                    result
                    
                } else {
                    var newStr = str.replaceAll("\\s", "")
                    val num1 = newStr.substring(0, newStr.indexOf("+"))
                    val num2 = newStr.substring(newStr.indexOf("+")+1, newStr.length())
                    num1.toDouble + num2.toDouble
                }
            }
        } catch {
            case e: Exception => e
        }
    }    
    
    //tests
    println(evalModified("(4, 12, 3) * (4,1,21)"))
    println(evalModified("(2, 0, 2) * (2,2,0)"))  
    println(evalModified("(1, 2, 3) * (4,5,6)"))  
    println()
    
    //RESULTS
    // 91.0
    // 4.0
    // 32.0
}