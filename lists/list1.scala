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
    def sumOfSumsRec(a: List[Int], b: List[Int]): Int = (a, b) match {
        case (x :: xs, y :: ys) => (x + y) + sumOfSumsRec(xs, ys)
        case (x :: xs, Nil) => x + sumOfSumsRec(xs, Nil)
        case (Nil, y :: ys) => y + sumOfSumsRec(Nil, ys)
        case _ => 0
    }
    
    
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
    println(sumOfSumsRec(List(1, 2, 3), List(3, 2, 1)))
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
    def countBoolRec[A](test: A=>Boolean, a: List[A]): Int = a match {
        case Nil => 0
        case h :: t if(test(h)) => 1 + countBoolRec(test, t)
        case h :: t => countBoolRec(test, t)
    }
    
    //TAIL RECURSIVE
    def countBoolTailRec[A](test: A=>Boolean, a: List[A], acc: Int = 0): Int = a match {
        case Nil => acc
        case h :: t if(test(h)) => countBoolTailRec(test, t, acc+1)
        case h :: t => countBoolTailRec(test, t, acc)
    }
    
    
    //FILTER-MAP-REDUCE
    def countBoolFilter[A](test: A=>Boolean, a: List[A]) = a.filter(test(_)).size
        
    //TEST CASE
    println(countBoolIter(isBool, List(1, true, 3, true, false, "hi")))
    println(countBoolRec(isBool, List(1, true, 3, true, false, "hi")))
    println(countBoolTailRec(isBool, List(1, true, 3, true, false, "hi")))
    println(countBoolFilter(isBool, List(1, true, 3, true, false, "hi")))
    println()
    
    //OUTPUT
    //3
    //3
    //3
    //3
    
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //PROBLEM 7
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    
    //ITERATIVE
    def countAllBoolIter[A](test: A=>Boolean, a: List[A]) = {
        var count = 0
        for(elem <- a) if(test(elem)) count += 1
        if(count != a.size) false
        else true
    }
    
    //RECURSIVE
    def countAllBoolRec[A](test: A=> Boolean, a: List[A]): Boolean = a match {
        case Nil => true
        case h :: t if(test(h)) => val foo = countAllBoolTailRec(test, t); foo
        case _ => false
    }
        
    //TAIL RACURSIVE
    def countAllBoolTailRec[A](test: A=>Boolean, a: List[A]): Boolean = a match {
        case Nil => true
        case h :: t if(test(h)) => countAllBoolTailRec(test, t)
        case _ => false
    }
    
    //FILTER-MAP-REDUCE
    def countAllBoolFilter[A](test: A=>Boolean, a: List[A]) = {
        if(a.filter(test(_)).size == a.size) true
        else false
    }
        
        
    //TEST CASES    
    println(countAllBoolIter(isBool, List(true, true, false, false)))
    println(countAllBoolRec(isBool, List(true, true, false, false)))
    println(countAllBoolTailRec(isBool, List(true, true, false, false)))
    println(countAllBoolFilter(isBool, List(true, true, false, false)))
    println()
    
    
    // OUTPUT
    // true
    // true
    // true
    // true
        
           
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //PROBLEM 8
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        
    //ITERATIVE
    def anyBoolIter[A](test: A=>Boolean, a: List[A]) = {
        var any = false
        for(x <- a if !any) any = test(x) //short circuit execution
        any
    }
    
    //RECURSIVE
    def anyBoolRec[A](test: A=> Boolean, a: List[A]): Boolean = a match {
        case Nil => false
        case h :: t if(!test(h)) => val foo = anyBoolRec(test, t); foo
        case _ => true
    }
    
    //TAIL RECURSIVE
    def anyBoolTailRec[A](test: A=> Boolean, a: List[A]): Boolean = a match {
        case Nil => false
        case h :: t if(!test(h)) => anyBoolTailRec(test, t)
        case _ => true
    }
    
    //FILTER-MAP-REDUCE
    def anyBoolFilter[A](test: A=>Boolean, a: List[A]) = {
        if(a.filter(test(_)).size > 0) true
        else false
    }
    
    //TEST CASES
    println(anyBoolIter(isBool, List(1, 2, "hi", "test", false)))
    println(anyBoolRec(isBool, List(1, 2, "hi", "test", false)))
    println(anyBoolTailRec(isBool, List(1, 2, "hi", "test", false)))
    println(anyBoolFilter(isBool, List(1, 2, "hi", "test", false)))
    println()
    
    
    
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //PROBLEM 10
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
     
    
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //PROBLEM 13
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    
    //An infinitely long stream of 1's
    def infiniteOnes(n: Int = 1): Stream[Int] = n #:: infiniteOnes(n)
    
    //The stream of all non-negative integers
    def makeInts(n: Int = 0): Stream[Int] = n #:: makeInts(n + 1)

    
    //The stream of all non-negative even integers
    def makeEvenInts(n: Int = 0): Stream[Int] = n #:: makeEvenInts(n + 2)
    
    //The stream of all squares of integers
    def intSquares(ints: Stream[Int]): Stream[Int] = ints.map{
        case n: Int => n * n
    }
    
    
    //TEST CASES
    val ones = infiniteOnes().map(x => x)
    println((ones.take(10)).toList)
    
    val ints = makeInts().map(x => x)
    println((ints.take(10)).toList)
    
    val evenInts = makeEvenInts().map(x => x)
    println((evenInts.take(10)).toList)
    
    val squares = intSquares(ints).map(x => x)
    println((squares.take(10)).toList)
    
    //OUTPUT
    // List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    // List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    // List(0, 2, 4, 6, 8, 10, 12, 14, 16, 18)
    // List(0, 1, 4, 9, 16, 25, 36, 49, 64, 81)

    
    
}