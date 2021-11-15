package o1.adventure.ui

import o1.adventure._
import scala.io.StdIn._

import o1._

/** The singleton object `AdventureTextUI` represents a fully text-based version of the
  * Adventure game application. The object serves as a possible entry point for the game,
  * and can be run to start up a user interface that operates in the text console.
  * @see [[AdventureGUI]] */
object AdventureTextUI extends App {


  val game = new Adventure
  private var player = game.player

  this.run()

  /** Runs the game. First, a welcome message is printed, then the player gets the chance to
    * play any number of turns until the game is over, and finally a goodbye message is printed. */
  private def run() = {

    while (!this.game.isOver) {
      this.printAreaInfo()
      this.playTurn()
    }
    if (game.isWon)
    {
      game.win()
    }
    else if (player.hasQuit){
      //do nothing
    } else {
      game.lost()
    }
  }


  /** Prints out a description of the player character's current location, as seen by the character. */
  private def printAreaInfo() = {
    val area = this.player.location
    Thread.sleep(500)
    println("\n" + area.name)
    Thread.sleep(500)
    println("-" * area.name.length)
    println(area.fullDescription + "\n")
  }



  /** Requests a command from the player, plays a game turn accordingly, and prints out a report of what happened.  */
  private def playTurn() = {
    println()
    var command = readLine("Command (use \"list\" to see a list of all comands): ")
    while (command == "") {
      command = readLine("Command: ")
    }
    Thread.sleep(500)
    println(s"\n${"="*150}\n")
     Thread.sleep(500)
    val turnReport: String = this.game.playTurn(command)
    if (!turnReport.isEmpty) {
      println(turnReport)
    }

  }

}


