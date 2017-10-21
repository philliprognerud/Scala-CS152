import scala.util.Random
object indus extends App {
    val dvd0 = Item("The Matrix DVD", 15.50, "DVD World")
    val dvd1 = Item("The Matrix DVD", 15.50, "DVD World")
    val dvd2 = Item("The Terminator DVD", 13.25, "DVD World")
    val dvd3 = Item("Ironman DVD", 18.25, "DVD Planet")
    
    println(dvd0.description == dvd1.description) 
    
    var inventory = scala.collection.mutable.Map[Description, Int]()
    inventory += (dvd1.description -> 5, dvd2.description -> 3, dvd3.description -> 2)
    
    println(inventory.mkString("\n"))
    
    // OUTPUT -----------------------------------------------------------------
    // TRUE
    // Description(Ironman DVD,18.25,DVD Planet) -> 2
    // Description(The Matrix DVD,15.5,DVD World) -> 5
    // Description(The Terminator DVD,13.25,DVD World) -> 3
}

case class Description(description: String, price: Double, supplier: String)
case class Item(id: Int, description: Description)

object Item {
  def apply(description: String, price: Double, supplier: String): Item = {
    val itemId = Random.nextInt(100)
    new Item(itemId, Description(description, price, supplier))
  }
}