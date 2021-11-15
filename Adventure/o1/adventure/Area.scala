package o1.adventure

import scala.collection.mutable.Buffer
import scala.collection.mutable.Map

/** The class `Area` represents locations in a text adventure game world. A game world
  * consists of areas. In general, an "area" can be pretty much anything: a room, a building,
  * an acre of forest, or something completely different. What different areas have in
  * common is that players can be located in them and that they can have exits leading to
  * other, neighboring areas. An area also has a name and a description.
  * @param name         the name of the area
  * @param description  a basic description of the area (typically not including information about items)
  * @param enemies       a buffer of all the enemies that are present in a room*/
class Area(var name: String, var description: String,val enemies: Buffer[Enemy]) {

  private val neighbors = Map[String, Area]()
  private val contents = Map[String, Item]()


  /** Returns the area that can be reached from this area by moving in the given direction. The result
    * is returned in an `Option`; `None` is returned if there is no exit in the given direction. */
  def neighbor(direction: String) = this.neighbors.get(direction)

  /** Changes the desctription of the room */
  def changeDescription(newDesc: String)= {
    description = newDesc
  }

  /** Adds an exit from this area to the given area. The neighboring area is reached by moving in
    * the specified direction from this area. */
  def setNeighbor(direction: String, neighbor: Area): Unit = {
    this.neighbors += direction -> neighbor
  }


  /** Adds exits from this area to the given areas. Calling this method is equivalent to calling
    * the `setNeighbor` method on each of the given direction--area pairs.
    * @param exits  contains pairs consisting of a direction and the neighboring area in that direction
    * @see [[setNeighbor]] */
  def setNeighbors(exits: Vector[(String, Area)]): Unit = {
    this.neighbors ++= exits
  }


  /** Returns a multi-line description of the area as a player sees it. This includes a basic
    * description of the area as well as information about exits and items. If there are no
    * items present, the return value has the form "DESCRIPTION\n\nExits available:
    * DIRECTIONS SEPARATED BY SPACES". If there are one or more items present, the return
    * value has the form "DESCRIPTION\nYou see here: ITEMS SEPARATED BY SPACES\n\nExits available:
    * DIRECTIONS SEPARATED BY SPACES".The items and directions are listed in an arbitrary order. */
  def fullDescription = {
    if (this.enemies.isEmpty) {
      val exitList = "\n\nExits available: " + this.neighbors.keys.mkString(" ")
      //if the player is inside the shoop change the text displayed
      if (this.name == "The Shop") {
          //also start the shop music
         ui.AdventureTextUI.game.startShopMusic()
        var shopDesc = """
        You can buy:

        (id) Weapons [attack/price]    | Armors [health inceased/price]       | Potions (price)
        ----------------------------------------------------------------------------------------------
        (1) Longspear [20atk/20c]      | (5) Wooden shield [200hp/20c]        | (9) 40 Hp Potion 5c
        (2) Longsword [30atk/30c]      | (6) Chainmail [300hp/30c]            | (10) 100 Hp Potion 10c
        (3) Angelblade [40atk/40c]     | (7) Iron armor [400hp/40c]           | (11) 200 Hp Potion 20c
        (4) Silent Death [50atk/50c]   | (8) Armour of Achilles" [500hp/50c]  |
        """
        this.description + shopDesc + exitList
      } else {
        ui.AdventureTextUI.game.stopShopMusic()
        this.description + exitList
      }
    }
    else {
      //stops the shop music when the user leaves the shop
      ui.AdventureTextUI.game.stopShopMusic()
      if (this.name == "Demon Lair") {
          //plays the boss music if the player is inside the Demon Lair room
         ui.AdventureTextUI.game.playBossMusic
      }
      val exitList = "\n\nExits available: you have to defeat the monsters first"
      this.description + exitList
    }


  }

  //the room can be locked or unclocked
  var locked = false

  /** Returns a single-line description of the area for debugging purposes. */
  override def toString = this.name + ": " + this.description.replaceAll("\n", " ").take(150)

}
