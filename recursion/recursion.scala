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
    
    // OUPUT
    // 4
    // 256
    // 65536
    
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
        
    def mulWithTail(n: Int, m: Int, acc: Int): Int =
      if (m == 0) acc
      else mulWithTail(n, dec(m), n + acc)
    
    def exp2WithTail(m: Int, acc: Int):Int = {
        if(isZero(m)) 1
        else if(m == 1) acc*2
        else exp2WithTail(dec(m), acc*2)

    }
    
    //TEST
    println(addWithTail(2, 7))
    println(mulWithTail(11, 2, 0))
    println(exp2WithTail(6, 1))
    println()
    
    //OUTPUT
    // 9
    // 22
    // 64

    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //problem 9
    
    //None tail recursive solution
    def fib1(n: Int): Int = n match {
       case 0 | 1 => n
       case _ => fib1(n-1) + fib1(n-2)
    }
    
    //Tail recursive solution
    def fib2(n: Int): Int = { 
      def fib_tail(n: Int, a:Int, b:Int): Int = n match {
        case 0 => a 
        case _ => fib_tail(n-1, b, a+b)
      }
      fib_tail(n, 0, 1)
    }
    
     //TEST
    println(fib1(7))
    println(fib2(7))
    println(fib1(4))
    println(fib2(4))
    println()
    
    //OUTPUT
    // 13
    // 13
    // 3
    // 3
    
    
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //problem 10
    def choose(n: Int, k: Int):Int = {
        if(k == 0) 1
        if(n == k) 1
        else choose(n-1, k-1) + choose(n-1, k)
    }
    
}