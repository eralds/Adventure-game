package o1.adventure

/** The class `Action` represents actions that a player may take in a text adventure game.
  * `Action` objects are constructed on the basis of textual commands and are, in effect,
  * parsers for such commands. An action object is immutable after creation.
  * @param input  a textual in-game command such as "go east" or "rest" */
class Action(input: String) {

  private val commandText = input.trim.toLowerCase
  private val verb        = commandText.takeWhile( _ != ' ' )
  private val modifiers   = commandText.drop(verb.length).trim

  private val game = ui.AdventureTextUI.game

  /** Causes the given player to take the action represented by this object, assuming
    * that the command was understood. Returns a description of what happened as a result
    * of the action (such as "You go west."). The description is returned in an `Option`
    * wrapper; if the command was not recognized, `None` is returned. */
  def execute(actor: Player) = this.verb match {
    case "attack"         => Some(actor.attackEnemies())
    case "back"           => Some(actor.back())
    case "buy"            => Some(actor.buy(this.modifiers.toIntOption))
    case "examine"        => Some(actor.examine(this.modifiers.toIntOption))
    case "help"           => Some(HELP)
    case "inventory"      => Some(actor.inventory)
    case "list"           => Some(LIST)
    case "mute"           => Some(game.mute())
    case "quit"           => Some(actor.quit())
    case "rest"           => Some(actor.rest())
    case "talk"           => Some(actor.talk)
    case "use"            => Some(actor.use(this.modifiers.toIntOption))
    case "go"             => Some(actor.go(this.modifiers))
    case "stats"          => Some(actor.stats())
    case other            => None
  }

    /** this method can only be used while the player is in commbat */
   def battleExecute(actor: Player) = this.verb match {

    case "attack"         => Some(actor.attackEnemy(this.modifiers.toIntOption))
    case "defend"         => Some(actor.defend())
    case "examine"        => Some(actor.examine(this.modifiers.toIntOption))
    case "help"           => Some(HELP)
    case "inventory"      => Some(actor.inventory)
    case "list"           => Some(COMBATLIST)
    case "mute"           => Some(game.mute())
    case "quit"           => Some(actor.quit())
    case "run"            => Some("#run")
    case "use"            => Some(actor.use(this.modifiers.toIntOption))
    case "end"            => Some(actor.endTurn())
    case "go"             => Some(actor.go(this.modifiers))
    case "stats"          => Some(actor.stats())
    case other            => None
   }



  val COMBATLIST = """

  ALL COMMANDS DURING BATTLE

  attack [enemy id]     => Attacks the enemy with that id (ex attack 1) and ends the turn
  run                   => Runs from conflict and go back to the room the player was before
  end                   => Does nothing and then ends the turn
  defend                => You recieve only 40% damage during the next 2 turns and then ends the turn
  help                  => Gives tips to the player
  inventory             => Gives a list of all the player items
  list                  => Shows a list of all possible comands
  use [item number]     => Uses an item {example 'use 2'} (This comand should be used after using 'inventory') and then ends the turn
  examine [item number] => Gives the description of the item with that id {example 'examine 1'} (This comand should be used after using 'inventory')
  mute                  => Stops or starts the music
  quit                  => Quits the game
  stats                 => Shows all the character stats (e.g health, attack)
  """

  val LIST = """

  ALL COMMANDS

  attack                => Engages in combat with enemies if there are any
  back                  => Goes back to the room before
  buy [item number]     => Buys the item with that number (can only be used in the shop)
  examine [item number] => Gives the description of the item with that id {example 'examine 1'} (This comand should be used after using 'inventory')
  help                  => Gives tips to the player
  inventory             => Gives a list of all the player items
  list                  => Shows a list of all possible comands
  mute                  => Stops or starts the music
  quit                  => Quits the game
  rest                  => The player rests (no changes to player stats)
  talk                  => Talks to an NPC if there are any in the room
  use [item number]     => Uses an item {example 'use 2'} (This comand should be used after using 'inventory')
  go [direction]        => The player goes to the room in the coresponding direction if the room exists and the player can enter
  stats                 => Shows all the character stats (e.g health, attack)
  """


  val HELP = """
  HELP

  You should focus your attacks on one monster if there are more than 1 enemies in the room.
  Use "defend" during battle you will need it.
  You need a key in order to enter the demon's room, the vapire has it.
  The shop is very useful and you should check it out before facing the vampire.
  Don't forget to drink health potions if you are low on health.

  """

  /** Returns a textual description of the action object, for debugging purposes. */
  override def toString = this.verb + " (modifiers: " + this.modifiers + ")"


}
