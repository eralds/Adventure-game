package o1.adventure


/** A 'Enemy' represents a monster in the castle the user has to defeat in order to win
  * @param name the name of the enemy
  * @param inputAttack the attack of the average mosnter of that class
  * @param inputHealth the health of the average mosnter of that class
  * @param gold the gold that is going to be dropped of the average mosnter of that class*
*/

class Enemy(val name: String, inputAttack: Int, inputHealth: Int, gold: Int) {

  /**get a number from 1 to 5 inclusive
    * A 3 means the monster has the same stats as the average mosnter of his class
    * a 1 or 2 means a -40% or -20% in all stats like health, attack or gold
    * a 4 or 5 means a +20% or +40 incrase in all stats
    * */
  private var strength: Int =  Math.floor(math.random()*5 + 1).toInt


  private var MAX_HEALTH = inputHealth + ((strength - 3) * (inputHealth/5) )
  private var attack = inputAttack + ((strength - 3) * (inputAttack/5) )
  private var health = MAX_HEALTH

  val dropGold = gold + ((strength - 3) * (gold/5) )

  def enemy_attack = attack
  def enemy_health = health
  def enemy_max_health = MAX_HEALTH
  def enemy_name = realName

  //change the name of the monster depending on his streagth
  var realName = strength match {
    case 1 => "weakened "
    case 2 => "unlucky "
    case 3 => ""
    case 4 => "lucky "
    case 5 => "superior "
  }

  realName += name


  /** Enemy attacks the player durign his turn */
  def enemyTurn(player: Player): Unit = {
    println("\n" + realName + " attacked player " + player.name + " and dealt " +  player.dealDamage(this.enemy_attack) + " damage")
    ui.AdventureTextUI.game.playEffect("damage")
    Thread.sleep(1000)
  }

  /** Deals some amount of damage to this enemy object */
  def dealDamage(damage: Int): Int = {
    this.health -= damage
    damage
  }

   /** This method will be executed when this monster object has died */
  def wasKilledBy(player: Player) = {
    println(s"Enemy $realName was killed by the player")
    player.getCoins(this.dropGold)
    Thread.sleep(1000)
    var keyS = ""
    //if he was a vapire he will drop a key
    if (this.name == "Vampire") {
      var keyS = "and a key"
      player.getItem(key)
    }
    println(s"He droped $dropGold gold coins " + keyS)
  }
}


class Zombie(name: String) extends Enemy(name, 5, 50, 15)
class Goblin(name: String) extends Enemy(name, 15, 150, 25)
class Vapire(name: String) extends Enemy(name, 25, 350, 60)
class Demon(name: String) extends Enemy(name, 50, 500, 0)



