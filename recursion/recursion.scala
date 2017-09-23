object recursion extends App {
    def inc(n: Int) = n + 1
    def dec(n: Int) = n - 1
    def isZero(n: Int) = n == 0
    
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //problem 1
    def add(n: Int, m: Int):Int =
        if(!isZero(m)) add(inc(n), dec(m))
        else n
    
    
    //TEST
    println(add(5, 5))
    println(add(2, 7))
    println(add(2, 1))
    println()
    
    //OUTPUT
    //10
    //9
    //3
    
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //problem 2
    def mul(n: Int, m: Int):Int =
        if(m > 1) n + mul(n, dec(m))
        else n
    
    
    //TEST
    println(mul(5, 5))
    println(mul(2, 7))
    println(mul(2, 1))
    println()
    
    //OUTPUT
    //25
    //14
    //2
    
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //problem 3
    def exp2(m: Int):Int =
        if(isZero(m)) 1
        else if(m > 1) 2 * exp2(dec(m))
        else m*2
        //why does returning 'm' give the value of call stack?
    
    
    //TEST
    println(exp2(5))
    println(exp2(6))
    println(exp2(0))
    println()
    
    //OUTPUT
    //32
    //64
    //1
    
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //problem 4
    def hyperExp(n: Int):Int = 
        if(n <= 1) 1
        else if(n > 1) exp2(exp2(dec(n)))
        else n
        
    //TEST
    println(hyperExp(2))
    println(hyperExp(4))
    println(hyperExp(5))
    println()
    
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //problem 5
    import scala.annotation.tailrec
    def addWithTail(n: Int, m: Int):Int = {
        @tailrec
        def iter(result: Int, x: Int):Int =
            if(x == 0) result
            else iter(inc(result), dec(x))
        iter(n, m)
    }
        
    
    println(addWithTail(5, 5))
}