object math extends App {
    
    //++++++++++++++++++++++++
    //problem 1
    def solve(a: Double, b: Double, c: Double) = {
        val disc = b * b - 4 *a * c
        if(disc < 0) None
        else {
            val root1 = (-b + Math.sqrt(disc))/(2 * a)
            val root2 = (-b - Math.sqrt(disc))/(2 * a)
            Some(root1, root2)
        }
    }
    
    //tests
    println(solve(2, -2, -4))
    println(solve(1, 0, 1))
    println(solve(1, 0, -1))
    println()
    
    //RESULTS
    // Some((2.0,-1.0))
    // None
    // Some((1.0,-1.0))
    
    //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //problem 2
    def dist(a: (Int, Int), b: (Int, Int)) = {
        Math.sqrt(Math.pow((b._1 - a._1), 2) + scala.math.pow((b._2 - a._2), 2))
        
        //For a 3 dimensional space we would require another variable C.
        //For N-demnsions we would require N variables
        //We would need to calculate each N variable using (N_2 - N_1)^2
        //Distance formula works for any size dimension
    }
    
    //tests
    println(dist((1, 1), (0, 0)))
    println(dist((3, 0), (0, 0)))
    println(dist((3, 3), (2, 5)))
    println()
    
    //RESULTS
    // 1.4142135623730951
    // 3.0
    // 2.23606797749979
    
    //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //problem 3
    def dot(a: (Double, Double, Double), b: (Double, Double, Double)) = {
        val x = a._1 * b._1
        val y = a._2 * b._2
        val z = a._3 * b._3
        x + y + z
    }
    
    //tests
    println(dot((2.0, 3, 4), (2, 2.0, 2)))
    println(dot((4.0, 3.5, 4), (2, 1.0, 0)))
    println(dot((2.0, 0, 4.5), (0, 2.0, 2)))
    println()
    
    //RESULTS
    // 18.0
    // 11.5
    // 9.0
    
    //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //problem 6
    def isPrime(value: Int) = {
        try{
            if(value < 0) throw new Exception
            else if(value == 1 || value == 2) true
            
            //if there does NOT exist a number(i) that divides evenly into
            //our value then we can return true that it is prime
            else if (!(2 to (value-1)).exists(i => value % i == 0)) true
            else false
        } catch {
            case e: Exception => ("exception caught: " + e)
        }
        
        //to write a twin prime detector is quite simple, we just need to
        //dissect each value of the tuple and check whether or not they are prime
        //both val1 && val2 need to be true for condition to return true
    }
    
    //tests
    println(isPrime(17))
    println(isPrime(2))
    println(isPrime(8))
    println(isPrime(-2))
    println()
    
    //RESULTS
    // true
    // true
    // false
    // exception caught: java.lang.Exception
    
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //problem 7
    def phi(value: Int) = {
        var results = 1
        for (i <- 2 to value){
            if (gcd(i, value) == 1) results += 1
        }
        results
    }
    
    //helper function
    def gcd(a: Int, b: Int):Int = {
        if (a == 0) b
        else gcd(b%a, a)
    }
    
    //tests
    println(phi(9))
    println(phi(10))
    println()
    
    //RESULTS
    //6
    //4
    
    
    //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //problem 8
    def rollDice() = {
        val start = 1
        val end   = 6
        val rnd = new scala.util.Random(System.nanoTime)
        val val1 = start + rnd.nextInt( (end - start) + 1 )
        val val2 = start + rnd.nextInt( (end - start) + 1 )
        (val1, val2)
        
        //alternatively we could use math.random
        //(math.random * (6-1) + 1).toInt
        
        //to ensure numbers are always random
        //we can pass in a a random 'seed' by passing in 
        //System.nanoTime, as shown above
    }
    
    
    //tests
    println(rollDice())
    println(rollDice())
    println(rollDice())
    
    //RESULTS
    // (6,2)
    // (2,1)
    // (4,4)
    
}