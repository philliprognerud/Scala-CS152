object combinators extends App {
    
    def inc(x: Double) = x + 1
    def double(x: Double) = 2 * x
    
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //problem 1
    def compose[A,B,C](f: B => C, g: A => B, x: A): C = f(g(x))
    
    //TEST
    println(compose(double, inc, 2.0))
    println(compose(double, inc, 5.5))
    println(compose(double, inc, 7.4))
    println()
    
    //OUTPUT
    // 6.0
    // 13.0
    // 16.8
    
    
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //problem 2
    def selfIter[T](f: T=>T, n: Int): T => T = Function.chain(List.fill(n)(f))
    
    //TEST
    println(selfIter[Double](compose(double, inc, _), 2)(2.0))
    println(selfIter[Double](compose(double, inc, _), 3)(2.0))
    println(selfIter[Double](compose(double, inc, _), 4)(2.0))
    println()
    
    //OUTPUT
    // 14.0
    // 30.0
    // 62.0


    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //problem 3
    def countPass[T](elem: Array[T]) = {
        var count = 0
        for(x <- elem) 
            x match { 
                case _: Boolean => count += 1
                case _ => None
            }
        count
    }
    
    //TEST
    println(countPass[Any](Array(true, 5, "test", false, false)))
    println(countPass[Any](Array(true, 5, "test", true, "string")))
    println(countPass[Any](Array(true, 5, "test", 1, 1)))
    
    //OUTPUT
    // 3
    // 2
    // 1
    
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //problem 4
    
    def recur(baseVal: Int, combiner: (Int, Int)=>Int): Int=>Int = {
        
    }
    
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //problem 5
    
    def parseDigits(digits: String): Option[Int] = 
        if (digits.matches("[0-9]*")) Some(digits.toInt) else None
}