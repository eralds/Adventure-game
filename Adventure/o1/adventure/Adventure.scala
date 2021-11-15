package o1.adventure

import o1.{Buffer, KeepRepeating, Mute, Sound}

import scala.io.StdIn.readLine


/** The class `Adventure` represents text adventure games. An adventure consists of a player and
  * a number of areas that make up the game world. It provides methods for playing the game one
  * turn at a time and for checking the state of the game.
  *
  * N.B. This version of the class has a lot of "hard-coded" information which pertain to a very
  * specific adventure game that involves a small trip through a twisted forest. All newly created
  * instances of class `Adventure` are identical to each other. To create other kinds of adventure
  * games, you will need to modify or replace the source code of this class. */
class Adventure {


  val GAME_VOLUME = -8.toFloat
  var volume = GAME_VOLUME

  /** title sceen  */
   val bigTitle = """


  | |  | |                    /\      | |               | |
  | |__| | ___ _ __ ___      /  \   __| |_   _____ _ __ | |_ _   _ _ __ ___
  |  __  |/ _ \ '__/ _ \    / /\ \ / _` \ \ / / _ \ '_ \| __| | | | '__/ _ \
  | |  | |  __/ | | (_) |  / ____ \ (_| |\ V /  __/ | | | |_| |_| | | |  __/
  |_|  |_|\___|_|  \___/  /_/    \_\__,_| \_/ \___|_| |_|\__|\__,_|_|  \___|
  Game by Erald Shahinas / Music by Toby Fox

  """

  val pressToStart = "Type S and press Enter to Start"

  //game story
  def introText1 = """Welcome to this world #name (Press Enter to continue the dialogue)
There was once upon a time a prince and a princess who lived in a castle far far away
The prince and the princess loved each other deeply and all the Kingdom lived in peace """.replaceAll("#name", name).split("\n")

  def introText2 ="""However one day the kingdom was invaded by the Evil Demon and his monsters
They destoyed the village and kidnaped the princess
You #name are the only one who can save the princess and restore peace into the kingdom
You have to go to the lair of the Evil Demon and defeat all monsters before you can save the princess.
Good luck, you will need it!(Press enter to continue)""".replaceAll("#name", name).split("\n")

  //all the differend room areas
  private val entrance      = new Area("Entrance", "You find yourself before the enrance of the demons castle.\nThere is on old man you may talk to him by using the comand \"talk\"", Buffer())
  private val room1         = new Area("Room1", "There are currently 2 zombies in this room\nYou may engage in combat with them if you use the command \"attack\" or go back by using \"back\"", Buffer(new Zombie("Zombie 1"), new Zombie("Zombie 2")))
  private val room2         = new Area("Room2", "There are currently 2 zombies in this room\nYou may engage in combat with them if you use the command \"attack\" or go back by using \"back\"",  Buffer(new Zombie("Zombie 1"), new Zombie("Zombie 2")))
  private val shop          = new Area("The Shop", "Hello there hero\nI am the shop manager\nIf you wish to buy something use the comand buy followed by the item number like \"buy 1\" or you can go \"back\" if you wish", Buffer())
  private val hallway       = new Area("Hallway", "There are no monsters in the hallway, Phew.", Buffer())
  private val vampire_lair  = new Area("Vampire's Lair", "You feel an evil presence in the room\nYou can \"atack\" if you want but be careful \nyou may go back if you are not ready for this", Buffer(new Vapire("Vampire")))
  private val room3         = new Area("Room3", "There are currently 3 zombies in this room\nYou may engage in combat with them if you use the command \"attack\" or go back by using \"back\"",  Buffer(new Zombie("Zombie 1"), new Zombie("Zombie 2"), new Zombie("Zombie 3")))
  private val room4         = new Area("Room4", "There are currently 2 goblins in this room\nThe seem scarier than the zombies you have faced\nYou may engage in combat with them if you use the command \"attack\" or go back by using \"back\"", Buffer(new Goblin("Goblin 1"), new Goblin("Goblin 2")))
          val demon_lair    = new Area("Demon Lair", "You can't go \"back\" anymore hero\nYour only option is to attack\nThe hope of the entire kingdom lies in your hands.\nGood Luck Hero!", Buffer(new Demon("Evil Demon")))

  //the demon lair is locked and the player needs a key in order to unlock it
  demon_lair.locked = true

  //setting neighbours for each room
      entrance.setNeighbors(Vector("north" -> room2, "east" -> room1))
         room1.setNeighbors(Vector("north" -> hallway, "east" -> shop, "west" -> entrance))
         room2.setNeighbors(Vector("north" -> vampire_lair,"east" -> hallway, "south" -> entrance))
          shop.setNeighbors(Vector("west" -> room1))
  vampire_lair.setNeighbors(Vector("south" -> room2))
         room3.setNeighbors(Vector("east" -> room4,"south" -> hallway))
         room4.setNeighbors(Vector("west" -> room3, "south" -> demon_lair))
    demon_lair.setNeighbors(Vector())
       hallway.setNeighbors(Vector("north" -> room3,"west" -> room2, "south" -> room1))

    //create a sound object with the theme of the game
   var theme = Sound.apply("music/game_theme.wav", volume)

    //create a player object and initialize the name variable
   private var name = ""
   var player = new Player( this.start(), entrance, this)

    /**
    * The first function in the game. Returns the welcome screen, plays the welcome music and waits for the player to start
    */
  def start() = {
    //plays the intro song
    var introS = Sound.apply("music/intro.wav", volume)
    introS.play(KeepRepeating)

    //prints the welcome screen
    println(this.bigTitle + this.pressToStart)

    var hasStarted = false;
    while (!hasStarted) {
      hasStarted = (readLine("").toLowerCase == "s")
    }

    //get the player name
    while (name == "") {
      name = readLine("\nInput your name (then press Enter): ")
    }

    //prints the game story
    for (t <- introText1){
      println(t)
      readLine()
    }

    //stop the intro music and play the game theme
    introS.stop()
    theme.play(KeepRepeating)

    //prints the second part
    for (t <- introText2){
      println(t)
      readLine()
    }
    name
  }



  /** Determines if the game has ended */
  var isOver = false

  /**Determines if the player is a winner */
  var isWon = false

  //plays the boss music
  val bossMusic = Sound.apply("music/threatening.wav", volume)
  def playBossMusic = {
     theme.stop()
      bossMusic.play(KeepRepeating)
  }

  /**This method is executed after the player wins the game
    * It continues the story and also palys different music*/
  def win() =  {
    bossMusic.stop()
    val gamefinished = Sound.apply("music/ending.wav", volume)
    Sound.apply("music/snd_heavydamage.wav", volume).play()
    Thread.sleep(2000)
    gamefinished.play()
    Thread.sleep(3000)
    println("\nYou saved the princess hero!\n")
    Thread.sleep(4000)
    println("The whole kingdom is celebrating your victory\n")
    Thread.sleep(4000)
    println(s"Thank you ${player.name} in the name of all the villagers I say thank you\n")
    Thread.sleep(6000)
    println("The end")
    //waits for the player to hit enter to stop the game
    readLine()
  }

  /**initialie the shop music object and also make it so the music starts
   when you go to the shop and ends when you leave it*/
  private val shopMusic = Sound.apply("music/shop.wav", volume)
  private var shopMusicIsPlaying = false
  def startShopMusic() = {
    if (!shopMusicIsPlaying) {
      theme.stop()
      shopMusic.play(KeepRepeating)
      shopMusicIsPlaying = true
    }
  }

  def stopShopMusic() = {
    bossMusic.stop()
   if (shopMusicIsPlaying) {
      theme.play(KeepRepeating)
      shopMusic.stop()
      shopMusicIsPlaying = false
   }
  }

  /**
    * Plays a sound file that is reapeated only once
    */
  def playEffect(file: String) = {
    val effect = Sound.apply(s"music/$file.wav", volume)
    effect.play()
  }


  /** This method is executed when the player has lost the game */
  def lost() = {
    bossMusic.stop()
    theme.stop()
    val gameover = Sound.apply("music/gameover.wav", volume)
    gameover.play()
    println("\nYou Lost!\n")
     //wits for the player to hit enter to stop the game
    readLine()
  }

   /** Mutes or unmutes the game if muted */
  def mute() = {
    if (volume == GAME_VOLUME) {
      theme.stop()
      volume = Mute
      "You muted the game"
    }
    else {
       theme.play(KeepRepeating)
      volume = GAME_VOLUME
      "You unmuted the game"
    }
  }



  /** Plays a turn by executing the given in-game command, such as "go west". Returns a textual
    * report of what happened, or an error message if the command was unknown. In the latter
    * case, no turns elapse. */
  def playTurn(command: String) = {
    val action = new Action(command)
    val outcomeReport = action.execute(this.player)
    outcomeReport.getOrElse("Unknown command: \"" + command + "\". (Use list to see a list of all commands)")
  }


}

