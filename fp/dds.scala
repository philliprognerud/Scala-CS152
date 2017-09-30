object dds extends App {
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //problem 1
    def controlLoop[S](state: S, cycle: Int, halt: (S, Int)=> Boolean, update: (S, Int)=>S): S =
         if (halt(state, cycle)) state
         else controlLoop(update(state, cycle), cycle + 1, halt, update)
         
    val finalPop = controlLoop(1, 0, (p: Int, t: Int) => p >= 1000, (p: Int, t: Int) => 2 * p)
    
    //TEST
    println(finalPop)
    println()
    
    //OUTPUT 
    //1024
    
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //problem 2
    val finalPop2 = controlLoop(1, 0, (p: Int, t: Int) => p >= 100000, (p: Int, t: Int) => 2 * p)
    
    
    //TEST
    println(finalPop2)
    println()
    
    //OUTPUT 
    //131072
    
    
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //problem 3
    val delta = 1e-5
    
    def deriv(f: Double=>Double): Double=>Double = {
         def df(x: Double) = (f(x + delta) - f(x))/delta
         df _
      }
      
      def solve(f: Double=>Double): Double = {
         def df = deriv(f)
         def goodEnuf(guess: Double, c: Int) = math.abs(f(guess)) <= delta
         def improve(guess: Double, c:Int) = guess - f(guess)/df(guess)
         controlLoop(1.0, 0, goodEnuf,improve)
      }
      
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //problem 4
    def squareRoot(n: Double) = solve((x: Double) => x * x - n)
    
    
    //TEST
    println(squareRoot(144))                                  
    println(squareRoot(121))
    println(squareRoot(4))
    println()
    
    //OUTPUT
    // 12.000000012637258
    // 11.000000001697442
    // 2.0000000944796694
    
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //problem 5
    
    def cubeRoot(n: Double) = solve((x: Double) => x * x * x - n)
    
    //TEST
    println(cubeRoot(144))
    println(cubeRoot(27)) 
    println(cubeRoot(64)) 
    println() 
    
    //OUTPUT
    // 5.241482798286697
    // 3.0000000000019176
    // 4.000000000119973
    
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //problem 6
    def exp(a: Double, n: Int):Double = {
        if(n == 1) a
        else exp(a * a, n-1)
    }
    
    def nthRoot(q: Double, n: Int) = solve((x: Double) => exp(x, n) - q)
    
    //TEST
    println(nthRoot(81, 3))
    println(nthRoot(16, 2)) 
    println(nthRoot(256, 4)) 
    println() 
    
    //OUTPUT
    // 3.000000000001341
    // 4.000000639575587
    // 2.000000000030684
}