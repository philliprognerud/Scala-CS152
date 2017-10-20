object Dungeon extends App {
   
    val puff = new Dragon("Puff")
    val thor = new Knight("Thor")
    while(puff.getHealth() > 0 && thor.getHealth() > 0) {
        puff.attack(thor.getName())
        thor.attack(puff.getName())
          
        print(puff.getName() + ": " + puff.getHealth() + "hp | ")
        println(thor.getName() + ": " + thor.getHealth() + "hp")
    }
    
    if(puff.getHealth() == 0 && thor.getHealth() == 0) println("It's a draw!")
    else if(puff.getHealth() == 0) println(thor.getName() + " is the Winner!")
    else if(thor.getHealth() == 0) println(puff.getName() + " is the Winner!")
}

class Dragon(name: String) extends Player(name) {
    val r = new scala.util.Random
    
   def attack(victim: String) = {
       val damage = r.nextInt(100)
       val newHealth = this.getHealth() - damage
       
       if(newHealth <= 0) this.setHealth(0)
       else this.setHealth(newHealth)

       println(name + " is flaming " + victim + " for " + damage.toString())
   }
}

class Knight(name: String) extends Player(name) {
    val r = new scala.util.Random
    
   def attack(victim: String) = {
       val damage = r.nextInt(100)
       val newHealth = this.getHealth() - damage
       
       if(newHealth <= 0) this.setHealth(0)
       else this.setHealth(newHealth)
       
       println(name + " is stabbingg " + victim + " for " + damage.toString())
   }
}

class Player(name: String) {
    private var health = 100
    def getHealth():Int = health
    def setHealth(newHealth: Int) = health = newHealth
    def getName(): String = name
}

