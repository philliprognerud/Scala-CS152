object generic extends App {
    val q = new Queue()
    
    q.enqueue("matt")
    q.enqueue("alissa")
    q.enqueue("phillip")
    q.enqueue("peter")
    q.enqueue("christy")

    for(i <- 0 to q.getSize()-1){
        println(q.toString())
        q.dequeue()
    } 
    
    println("No one else is in the list")
}

class Queue() {
    import scala.collection.mutable.ListBuffer
    private var q = new ListBuffer[Any]()
    
    def enqueue(item: Any) = q = q :+ item
    def dequeue() = q.remove(0)
    
    def isEmpty():Boolean = {
        if(q.size == 0) true
        else false
    }
    
    def getSize(): Int = q.toList.size
    
    override def toString():String = q.toList.mkString(" ")
}