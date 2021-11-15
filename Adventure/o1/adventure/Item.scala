package o1.adventure

/** The class `Item` represents items in a text adventure game. Each item has a name
  * and a  *  longer description. (In later versions of the adventure game, items may
  * have other features as well.)
  *
  * N.B. It is assumed, but not enforced by this class, that items have unique names.
  * That is, no two items in a game world have the same name.
  *
  * @param name         the item's name
  * @param description  the item's description */
class Item(val name: String, val description: String) {

  /** Returns a short textual representation of the item (its name, that is). */
  override def toString = this.name

}


class Potion(val id: Int, name: String, description: String, val restored: Int,  val price: Int) extends Item(name, description)

class Weapon(val id: Int, name: String, val attack: Int, val price: Int) extends Item(name, " ")

class Armor(val id: Int, name: String, val defense: Int, val price: Int) extends Item(name, " ")

object key extends Item("key", "Can be used to open the room of the Demon")

object allItems {
  val Weapons = Vector(new Weapon(1, "Longspear", 20, 20), new Weapon(2, "Longsword", 30, 30),  new Weapon(3, "Angelblade", 40, 40), new Weapon(4, "Silent Death", 50, 50), new Weapon(0, "Excalibur", 100, 0))
  val Armors = Vector(new Armor(5, "Wooden shield", 200, 20), new Armor(6, "Chainmail", 300, 30),  new Armor(7, "Iron armor", 400, 40), new Armor(8,"Armour of Achilles", 500, 50))
  val Potions = Vector(new Potion(9, "40 Hp Potion", "Restores 40 health points", 40, 5), new Potion(10, "100 Hp Potion", "Restores 100 health points", 100, 10), new Potion(11, "200 Hp Potion", "Restores 200 health points", 200, 20))
}