package de.scala_bs


import akka.actor.{ActorSystem, Props}


// This App will start a Mia GameActor.
object Mia {
  
  val system = ActorSystem("Mia")
  
  val gameLogic = system.actorOf(Props(classOf[GameActor], 2), "game-logic")
  
//for(i <- (1 to 2)) {
//  val player = system.actorOf(Props(classOf[PlayerActor], "player-"+i), "player-"+i)
//  player ! Join(gameLogic)
//}
  
  //val pingActor = system.actorOf(PingActor.props, "pingActor")
  //pingActor ! PingActor.Initialize
  system.awaitTermination()
}


trait GameLogic {
  import scala.util.Random
  import scala.math.{max, min}
  import GamePhase.Dice
  
  def randomDice: Dice = {
    def die: Int = Random.nextInt(5) + 1
    
    val one = die
    val two = die
    
    Dice(max(one, two), min(one, two))
  }
  
}


import scala.collection.mutable.ListBuffer
import akka.actor.{Actor, ActorLogging, ActorRef}


// This actor is a central instance that runs the game.
class GameActor(noPlayers: Int) extends Actor with ActorLogging with GameLogic {
  import SetupPhase._, GamePhase._
  
  
  def receive = setupPhase
  
  
  // players who are joining but are not joined yet
  val joiningPlayers = collection.mutable.HashSet[ActorRef]()
  var joinedPlayersCount = 0
  
  def setupPhase: Receive = {
    case JoinRequest(name) => // save the player as joining and ask him to join
      val ref = sender()
      joiningPlayers += ref
      ref ! Join
    case Joined => // remove the player from joining and track them as joined. if the player capacity is reached start the game.
      val ref = sender()
      joiningPlayers -= ref
      players(joinedPlayersCount) = ref
      joinedPlayersCount += 1
      if(joiningPlayers == 0 && joinedPlayersCount == noPlayers)
        self ! Start
    case Start => // switch to game phase, send the first message to the start player
      context.become(gamePhase)
      rollDice()
  }
  
  
  // list of participants
  val players = new Array[ActorRef](noPlayers)
  
  // index of the active player
  var activePlayer = 0
  
  // the dice that have been rolled this round
  val rolledDice = ListBuffer[Dice]()
  
  def rollDice(): Unit = {
    val dice = randomDice
    players(activePlayer) ! dice
    rolledDice += dice
  }
  
  def gamePhase: Receive = {
    case Number(no) => // TODO: If 21 broadcast to all players
      // broadcast to all players except the sender
      // set active player to next
      // roll new dice
    //case More =>
      // broadcast to all players except the sender
      // set active player to next
      // roll new dice
    case Liar =>
      // broadcast to all players except the sender
      // determine the outcome of the game
      // restart the game with new first player
    case NewTurn(firstPlayer) =>
      rolledDice.clear()
      activePlayer = firstPlayer
      rollDice()
  }
  
}


object SetupPhase {
  case class JoinRequest(name: String)
  case object Join
  case object Joined
  case object Start
}


object GamePhase {
  case class Dice(die1: Int, die2: Int)
  case class Number(no: Int)
  //case object More
  case object Liar
  case class NewTurn(firstPlayer: Int)
}


class PlayerActor(name: String, server: ActorRef) extends Actor with ActorLogging {
  import SetupPhase._, GamePhase._
  
  server ! JoinRequest(name)
  
  
  def receive = setupPhase
  
  
  def setupPhase: Receive = {
    case Join => // JoinRequest accepted, change to gamePhase
      context.become(gamePhase)
      server ! Joined
  }
  
  
  // cache all dice
  val dice = ListBuffer[Option[Dice]]()
  
  def gamePhase: Receive = {
    case d @ Dice(die1, die2) => // it is your turn, decide!
      dice += Some(d)
      // send Number(no)
      // send Liar
    case Number(21) => // TODO: bei einer 21 muss aufgedeckt werden... das sollte also der Master tun und broadcast!
      // A mia!
    case Number(no) => // another player has declared a number
      if(no.toString.matches("[1-6]{2}")) { // valid dice number
        val first = no / 10
        val second = no % 10
        if(first >= second) { // valid mia number
          val d = Dice(first, second)
          dice += Some(d)
        } else { // invalid mia number
          
        }
      } else { // invalid dice number
        
      }
    //case More => // another player has said more
      // guess the number
    case Liar => // another player has declared a liar
      // a new turn starts, reset all values
  }
  
}