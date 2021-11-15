package o1.adventure


import o1.Buffer
import scala.io.StdIn._
import scala.util.control.Breaks.{breakable, break}

/** A `Player` object represents a player character controlled by the real-life user of the program.
  *
  * A player object's state is mutable: the player's location and possessions can change, for instance.
  *
  * @param startingArea  the initial location of the player */
class Player(val name: String, startingArea: Area,val game: Adventure) {

  private var currentLocation = startingArea        // gatherer: changes in relation to the previous location
  private var lastLocation = startingArea           // the location the user was before going to this one
  private var quitCommandGiven = false              // one-way flag
  private val possessions = Buffer[Item]()     // container of all the items that the player has

  //the amount of times the user has talked to the old man in the entrance, used to change his dialogue options
  private var timeTalked = 0

  //player stats

  private var weapon = new Weapon(-999, "First Weapon", 10, 0)
  private var armor = new Armor(-999, "First Armor", 100, 0)

  private val MAX_HEALTH = 100
  private var health = player_max_health
  private var attack = weapon.attack
  private var coins = 5

  def player_attack = weapon.attack
  def player_health = health
  def player_max_health = MAX_HEALTH + armor.defense
  def player_coins = coins
  def player_name = name




  /** Determines if the player has indicated a desire to quit the game. */
  def hasQuit = this.quitCommandGiven


  /** Returns the current location of the player. */
  def location = this.currentLocation

  def previousLocation = this.lastLocation


  /** Attempts to move the player in the given direction. This is successful if there
    * is an exit from the player's current location towards the direction name. Returns
   */
  def go(direction: String): String = {
    val destination = this.location.neighbor(direction)
    if (destination.isDefined)
      {
        /** The user cannor go to other rooms if there are monsters in his current
          *  room unless he wants to go back to his last location */
        if (!this.currentLocation.enemies.isEmpty)
         {
            if (destination.get == this.lastLocation) {
               var current = this.currentLocation
               this.currentLocation = destination.getOrElse(this.currentLocation)
               this.lastLocation = current
               "You go " + direction + "."
            }
            else {
               "You can't go there are monsters in the room"
            }
         }
        //the user can't go to a locked room
        else if (!destination.get.locked) {
          var current = this.currentLocation
          this.currentLocation = destination.getOrElse(this.currentLocation)
          this.lastLocation = current
          "You go " + direction + "."
        }
        else
        {
            "The door is locked. You have to find a key."
        }
      }
    else {"There are no doors for you to go " + direction}
  }


  /** Causes the player to rest for a short while (this has no substantial effect in game terms).
    * Returns a description of what happened. */
  def rest() = {
    "You rest for a while. Better get a move on, though."
  }
  //Shows how many more turns does the defending effect last
  private var defendLastingTurns = 0

  //using this command during battle makes the used take only 40% of damage
  def defend() = {
    defendLastingTurns = 2
    "You will recieve only 40% damage in the next 2 turns#e"
  }

  //goes back to the room he was before
  def back() = {
     var current = this.currentLocation
     this.currentLocation = this.lastLocation
     this.lastLocation = current
     "You go back to " + currentLocation.name
  }

  /** Signals that the player wants to quit the game. Returns a description of what happened
    * within the game as a result (which is the empty string, in this case). */
  def quit() = {
    this.quitCommandGiven = true
    game.isOver = true
    "\nThe user has quit the game"
  }

  //method that is used to talk to old man in the entrance
  def talk = {
    if (currentLocation.name == "Entrance") {
      var potion = allItems.Potions(1)
      println("You decide to talk to the old man. ")
      Thread.sleep(500)
      var response = Vector("1. You should focus your strength at only one enemy at a time if there are more in the room", "2. You need a key in order to enter the demon's room, the vapire may have it.",
      "3. The first thing I would do if I were you is head for a shop nearby", "4. Don't forget to drink your health potion if you are low on health. Take this:")
      println("His response is: \n" + response(timeTalked))
      Thread.sleep(1000)
      //he gives you a potion if it is the 4th time you speak to him
      if (timeTalked == 3 && potion != null) {
        println("\nYou recieve a 100Hp potion.")
        this.getItem(potion)
        potion = null
        Thread.sleep(1000)
      }
      timeTalked = (timeTalked + 1) % 4
      "You feel more ready now to fight the demon. You may continue to \"talk\" to the old man if you wish"
    }else {
      "There is no one you can talk to."
    }
  }

  /** Returns a brief description of the player's state, for debugging purposes. */
  override def toString = "Now at: " + this.location.name


  /** Causes the player to examine the item of the given name. This is successful if such
    * an item is currently in the player's possession. Returns a description of the result,
    * which, if the attempt is successful, includes a description of the item. The description
    * has the form: "You examine the ITEM.\nDESCRIPTION"  */
  def examine(itemIdOption: Option[Int]) = {
    val itemId = itemIdOption.getOrElse(-1)
    if (itemId < 0 || itemId + 1 > possessions.size) {
      "There is no item with that id in your inventory. Use command \"inventory\" first to check"
    }
    else {
      "You examine the " + this.possessions(itemId).name + ":\n" + this.possessions(itemId).description
    }

  }


  /** Causes the player to list what they are carrying. Returns a listing of the player's
    * possessions or a statement indicating that the player is carrying nothing. The return
    * value has the form "You are carrying:\nITEMS ON SEPARATE LINES" or "You are empty-handed."
   */
  def inventory = {
    if (this.possessions.isEmpty)
      "You are empty-handed."
    else
      "You are carrying:\n" +  this.possessions.map(e => s"(${possessions.indexOf(e)}) ${e.name}").mkString("\n")
  }

  /**
    * Method used to use a specific item
    * @param itemIdOption the id that the user has typed, if the used has not typed an intiger it is equal to -1
    *
    */
  def use(itemIdOption: Option[Int]) = {
    val itemId = itemIdOption.getOrElse(-1)
    if (itemId < 0 || itemId + 1 > possessions.size) {
      "There is no item with that id in your inventory. Use command \"inventory\" first to check"
    }
    else {
      if (possessions(itemId).name == "key") {
        if (ui.AdventureTextUI.game.player.location.name != "Room4") {
          "Can only be used in Room 4 to open the room of the Demon"
        } else {
          ui.AdventureTextUI.game.demon_lair.locked = false
          this.possessions.remove(itemId)
          "You unlocked the door to the demon room"
        }
      } else {
        if (player_health == player_max_health) {
          "You already have full Hp"
        } else {
          val recovered = math.min(this.player_max_health, this.player_health + possessions(itemId).asInstanceOf[Potion].restored) - player_health
          health = health + recovered
          this.possessions.remove(itemId)
          s"You recovered $recovered hp"
        }
      }
    }
  }

  //method executed after the user types the command attack in a room with monsters
  def attackEnemies() = {
    if (this.currentLocation.enemies.isEmpty) {
      "There are no enemies to fight here"
    } else {
      startBattle(this.currentLocation.enemies)
    }
  }

  //starts the battle
  def startBattle(enemies: Buffer[Enemy]):String = {
    var battleEnded = false //checks if the battle has ended
    var playerWon = false //checks if the player has won
    var playerRun = false //checks if the player has run away
    game.playEffect("eyeflash")
    println("You attack: " + enemies.map(_.enemy_name).mkString(", ") ) //displays a list of every enemy
    while(!battleEnded) {
      //shows current stats of player and enemies at the start of every turn
      println("="*150 + "\nCurrent battle stats\n\n" + s"PLAYER $name\nhealth: ${player_health}/${player_max_health}\nattack: ${player_attack}atk" +  "\n\n(id) ENEMIES ")
      enemies.foreach(e => println(s"\n(${enemies.indexOf(e)}) ${e.realName}\nhealth: ${e.enemy_health}/${e.enemy_max_health}\nattack: ${e.enemy_attack}atk"))

      /** executes the playerTurn method
         Only if the result is possitive the player has run away else the player turn ends*/
      if(this.playerTurn())
      {
         battleEnded = true
         playerRun = true
      }

      //checks if any enemy was killed
      breakable {
        for (e <- enemies) {
          if (e.enemy_health <= 0) {
            e.wasKilledBy(this)
            enemies -= e
            break()
          }
        }
      }
      //check if all enemies are deafeted
      if (enemies.isEmpty) {
         battleEnded = true
         playerWon = true
      }
      //if not then continue with the enemy turn
      if (!battleEnded) {
        println("\n Enemy turn")
        Thread.sleep(1000)
        enemies.foreach(_.enemyTurn(this))
        //check if the player has died
        if (this.health <= 0) {
          battleEnded = true
          game.isOver = true
        }
      }
    }
    if (playerWon) {
      //if this room was the demon lair room the game has ended
      if (this.location.name == "Demon Lair") {
        game.isOver = true
        game.isWon = true
      }
      //change the description of a room after killing all enemies
      this.location.changeDescription("No mosters here anymore!")
      "\n The player defeated all enemies"
    } else if(playerRun) {
       this.back()
      "\n The player run from battle "
    }else{
      "\n The player was defeated"
    }
  }

  //A command that when executed deals some amount of damage to the used
  def dealDamage(damage: Int): Int = {
    //if defend effect is still lasting recieve only 40% of the attack
    if (defendLastingTurns > 0) {
      this.health -= damage * 4 / 10
      damage * 4 / 10
    } else {
      this.health -= damage
      damage
    }
  }


  def playerTurn(): Boolean= {
    //this is turned false when the user turn ends
    var playerTurn = true

    //removes one round from the effect of defend
    if (defendLastingTurns > 0 ) {
      defendLastingTurns -= 1
    }

    var returnIfRun = false //this is the value the function return, it changes to true only if the player runs from battle
    while(playerTurn) {
      //get the command
      var command = readLine("\nPlayer turn\n\nCommand (use \"list\" to see a list of all comands you can use during battle): ")
      while (command == "") {
        command = readLine("Command: ")
      }
      Thread.sleep(500)
      val action = new Action(command)
      val outcomeReport = action.battleExecute(this)
      var result = outcomeReport.getOrElse("Unknown command: \"" + command + "\". (Use list to see a list of all commands)")
      //if there is a "#e" at the end of the string that is a signal that shows that the turn has ended
      if (result.contains("#e")) {
         playerTurn = false
         result = result.dropRight(2)
         println(result)
      //if there is a "#run" at the end of the string that is a signal that shows that the player has run away
      } else if (result.contains("#run")) {
        Thread.sleep(500)
        game.playEffect("eyeflash")
        returnIfRun = true //return ture in the end of the program
        playerTurn = false //ends the player turn loop
      } else {
         println(result)
      }
    }
    returnIfRun
  }

  //player attacks a specific enemy
  def attackEnemy(enemyIdOption: Option[Int]) = {
    val enemyId = enemyIdOption.getOrElse(-99)

    if (enemyId == -99) {
      "Format your comand in the form attack (id) where id is an intiger"
    }
    else if(this.currentLocation.enemies.size < enemyId + 1) {
      "There is no enemy with that id"
    } else {
      ui.AdventureTextUI.game.playEffect("damage")
      "\nPlayer " + name + " attacked " + this.currentLocation.enemies(enemyId).realName + " and dealt " +  this.currentLocation.enemies(enemyId).dealDamage(this.player_attack) + " damage#e"
    }
  }
  //ends the turn, the #e at the end is a signal for ending the turn
  def endTurn(): String = {
    "You end your turn#e"
  }

  /**
    * @param itemNoOption the id of the item the user is going to buy from the shop
    * @return
    */
  def buy(itemNoOption: Option[Int]): String = {
    val itemNo = itemNoOption.getOrElse(-2)
    if (itemNo < -1 || itemNo > 12 || itemNo == 0) {
        "There are not any items with that id"
    }
    else if (this.currentLocation.name != "The Shop") {
        "You can only buy things in the shop"
    }
     // 1 - 4 are the ids for weapons
    else if ((itemNo >= 1 && itemNo <= 4)) {
       if(player_attack >= allItems.Weapons(itemNo - 1).attack) {
          "You can't buy this weapon you already have a stronger one."
       } else if(allItems.Weapons(itemNo - 1).price > coins) {
          "You can't afford this weapon"
       } else {
         this.weapon = allItems.Weapons(itemNo - 1)
         coins = coins - allItems.Weapons(itemNo - 1).price
         "You succesfully bought " + this.weapon.name
       }
    }
    /** -1 is the id of Excalibur, it is not showcased as a option to buy as it is an easter egg
     that the user can use to make sure he/she wins the game*/
    else if(itemNo == -1) {
      if(player_attack >= allItems.Weapons(4).attack) {
          "You can't buy this weapon you already have a stronger one."
       } else {
         coins = coins - allItems.Weapons(4).price
         this.weapon = allItems.Weapons(4)
         "You succesfully bought " + this.weapon.name
       }
    }
     // 5 - 8 are the ids for armors
    else if (itemNo >= 5 && itemNo <= 8) {
       if(armor.defense >= allItems.Armors(itemNo - 5).defense) {
          "You can't buy this armor you already have a stronger one."
       } else if(allItems.Armors(itemNo - 5).price > coins) {
          "You can't afford this weapon"
       } else {
         this.health += allItems.Armors(itemNo - 5).defense - this.armor.defense
         this.armor = allItems.Armors(itemNo - 5)
         coins = coins - allItems.Armors(itemNo - 5).price
         "You succesfully bought " + this.armor.name
       }
    }
    // 9 - 11 are the ids for potions
    else if ((itemNo >= 9 && itemNo <= 11) ) {
        if(allItems.Potions(itemNo - 9).price > coins) {
          "You can't afford this potion"
       } else {
         this.possessions += allItems.Potions(itemNo - 9)
         coins = coins - allItems.Potions(itemNo - 9).price
         "You succesfully bought " + allItems.Potions(itemNo - 9).name
       }
    }
    else {
      ""
    }
  }

  //user gets this amount of coins
  def getCoins(coins: Int) = {
    this.coins += coins
  }

  //adds this item to the possessions list
  def getItem(item: Item) = {
    this.possessions += item
  }

  //prints all the user stats health, attack, coins, weapon and armor
  def stats() = {
    "\n  Stats  \n " + s" Health ${player_health}hp/${player_max_health}hp  \n  Coins ${coins}c \n  Weapon: ${weapon.name} (attack ${player_attack}) \n  Armor: ${armor.name} (health added ${armor.defense}hp) \n  Character health (without armor) 100hp"
  }
}


