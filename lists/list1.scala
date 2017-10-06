object list1 extends App {
    import scala.collection.mutable.ListBuffer
    
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //PROBLEM 1
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    //ITERATIVE
    def cubesIter(a1: List[Int]): List[Int] = {
        var cubes = new ListBuffer[Int]()
        for (item <- a1 if item % 2 != 0) yield {
            cubes += Math.pow(item, 3).toInt
        }
        val cubesList = cubes.toList
        cubesList
    }
    
    //RECURSIVE
    def cubesRecur(a: List[Int]): List[Int] = a match {
      case Nil => List[Int]()
      case h::t if (h%2 == 0) => cubesRecur(t)
      case h::t => Math.pow(h,3).toInt :: cubesRecur(t)
    }
    
    //TAIL RECURSIVE
    def cubesTailRecur(a: List[Int], acc: List[Int] = List.empty): List[Int] = a match {
        case Nil => acc
        case h :: t if (h%2 == 0) => cubesTailRecur(t, acc)
        case h :: t  => cubesTailRecur(t, acc :+ Math.pow(h, 3).toInt)
    }
    
    
    // FILTER-MAP-REDUCE
    def cubesMap(a1: List[Int]): List[Int] = {
        
        val isOdd = (x: Int) => x % 2 != 0
        val a1Filtered = a1.filter(isOdd)
        
        val cubeInt = (x:Int) => Math.pow(x, 3).toInt
        val cubes = a1Filtered.map(cubeInt)

        cubes
    }
    
    // TEST CASES
    println(cubesIter(List(1, 2, 3, 4, 5, 6, 7)))
    println(cubesRecur(List(1, 2, 3, 4, 5, 6, 7)))
    println(cubesTailRecur(List(1, 2, 3, 4, 5, 6, 7)))
    println(cubesMap(List(1, 2, 3, 4, 5, 6, 7)))
    println()
    
    // OUTPUT
    // List(1, 27, 125, 343)
    // List(1, 27, 125, 343)
    // List(1, 27, 125, 343)
    // List(1, 27, 125, 343)


    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //PROBLEM 2
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    //ITERATIVE
    def sumOfSumsIter(a: List[Int], b: List[Int]): Int = {
        var sum = 0
        for(elem <- a) sum += elem
        for(elem <- b) sum += elem
        sum
    }
    
    //RECURSION
    
    //STIL NEED THIS ONE **********~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    //TAIL RECURSIVE
    import scala.annotation.tailrec
    def sumOfSumsRecTail(a: List[Int], b: List[Int]): Int = {
      @tailrec
      def recSumInternal(a: List[Int], b: List[Int], acc: Int): Int = {
        (a, b) match {
          case (x :: xs, y :: ys) => recSumInternal(xs, ys, x + y + acc)
          case (x :: xs, Nil) => recSumInternal(xs, Nil, x + acc)
          case (Nil, y :: ys) => recSumInternal(Nil, ys, y + acc)
          case _ => acc
        }
      }
      recSumInternal(a, b, 0)
    }
    
    //FILTER-MAP-REDUCE
    def sumOfSumsReduce(a: List[Int], b: List[Int]): Int = {
        val sum = a.reduceLeft(_ + _) + b.reduceLeft(_ + _)
        sum
    }
    
    
    //TEST CASES
    println(sumOfSumsIter(List(1, 2, 3), List(3, 2, 1)))
    println(sumOfSumsRecTail(List(1, 2, 3), List(3, 2, 1)))
    println(sumOfSumsReduce(List(1, 2, 3), List(3, 2, 1)))
    println()
    
    
    //OUTPUT
    // 12
    // 12
    // 12
    // 12
    
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //PROBLEM 3
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    
    def depth(value: Any): Int = value match {
        case first :: rest => Math.max(depth(first) + 1, depth(rest))
        case _ => 0
    }
    
        
    //TEST CASE
    println(depth(List(List(List(1, 2, List(3))))))
    println()
    
    //OUTPUT
    //4
    
    
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //PROBLEM 6
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    import scala.reflect.ClassTag
    
    //Predicate Function
    def isBool(i: Any) = i match {
        case _: Boolean => true
        case _    => false
    }

    
    //ITERATIVE
    def countBoolIter[A](test: A=>Boolean, a: List[A]) = {
        var count = 0
        for(elem <- a){
            if(test(elem)) count += 1
        }
        count
    }
    
    //RECURSIVE
    
    //TAIL RECURSIVE
    
    //FILTER-MAP-REDUCE
    def countBoolFilter[A](test: A=>Boolean, a: List[A]) = a.filter(test(_)).size
        
    //TEST CASE
    println(countBoolIter(isBool, List(1, true, 3, true, false, "hi")))
    // println(countBoolIter(List(1, true, 3, true, false, "hi")))
    // println(countBoolIter(List(1, true, 3, true, false, "hi")))
    println(countBoolFilter(isBool, List(1, true, 3, true, false, "hi")))
    
    //OUTPUT
    //3
    //3
    //3
    //3
    
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //PROBLEM 7
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    //RECURSIVE
    def all[T](test: T=> Boolean, vals: List[T]): Boolean =
        if (vals == Nil) true
        else test(vals.head) && all(test, vals.tail)
        
        
        
        
        
        
        
    // def isPal(s: String) = s == s.reverse
    
    // all(isPal _, List("mom", "rotator", "dad"))
    
    
    
    
    // def allIter[T](test: T=>Boolean, vals: List[T]) = {
    //     var reuslt = true
    //     for( v <- vals if result ) 
    //         result = result && test(v)
    //         //short circuit execution
            
    //     result
    // }
    
    // isPrime(4)
    
    // allIter(isPrime _, List(2, 4, 5, 7, 11, 13, 17)))
    
    
    
    
    // def makeNat(from: Int): Stream[Int] = from #:: makeNats(from + 1)
    
    // val nats = makeNats(0)
    
    // nats(5)
    
    // nats
    
    // val primes = nats.filter(isPrime _)
    
    // primes(5)
    
    // val cube = primes.map((x: Int) => x * x * x)
    
    
}