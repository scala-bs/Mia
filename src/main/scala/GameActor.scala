package de.scala_bs.mia


import scala.collection.mutable.HashSet
import akka.actor.{ActorRef, Actor}


class GameActor(noOfPlayers: Int) extends Actor {
  
  // Shared state
  
  val players = new Array[ActorRef](noOfPlayers)
  
  val playerNames = new Array[String](noOfPlayers)
  
  def receive = connectionPhase
  
  
  // Connection state
  
  val joiningPlayers = HashSet[ActorRef]()
  var joinedPlayersCount = 0
  
  def connectionPhase: Receive = {
    case JoinRequest => // save the player as joining and ask him to join
      val ref = sender()
      joiningPlayers += ref
      ref ! Join
    case Joined(playerName) =>
      // remove the player from joining and add them to players. if the player capacity is reached start the game.
      val ref = sender()
      joiningPlayers -= ref
      players(joinedPlayersCount) = ref
      playerNames(joinedPlayersCount) = playerName
      joinedPlayersCount += 1
      self ! Start
    case Start if joiningPlayers.size == 0 && joinedPlayersCount == noOfPlayers =>
      context.become(gamePhase)
      self ! Start
  }
  
  
  // Game state
  
  lazy val permille = Array.fill(players.size)(10)
  
  var currentPlayer = 0
  
  def randomDice: Dice = {
    def die: Int = scala.util.Random.nextInt(5) + 1
    
    val one = die
    val two = die
    
    Dice(scala.math.max(one, two), scala.math.min(one, two))
  }
  
  var dice = List[Dice]()
  var numbers = List[Dice]()
  var hasThrownDice = false
  
  def gamePhase: Receive = {
    case Start =>
      players(currentPlayer) ! Turn
    case ThrowDice if sender() == players(currentPlayer) && !hasThrownDice =>
      hasThrownDice = true
      val d = randomDice
      dice = d :: dice
      sender() ! d
    case n @ Number(number) if sender() == players(currentPlayer) => print(" "+currentPlayer+"("+number+") ")
      hasThrownDice = false
      
      val one = number / 10
      val two = number % 10
      numbers = Dice(one, two) :: numbers
      
      players foreach (_ forward n)
      currentPlayer = (currentPlayer + 1) % noOfPlayers
      players(currentPlayer) ! Turn
    case Lie if sender() == players(currentPlayer) => print(" "+currentPlayer+"(Lie) ")
      players foreach (_ ! Lie)
      
      val looser = {
        if(numbers.head == dice.head) {
          currentPlayer
        } else {
          val previousPlayer = (currentPlayer - 1 + noOfPlayers) % noOfPlayers
          previousPlayer
        }
      }
      
      reset(looser)
      
      nextTurn(looser)
    case YouLoose if sender() == players(currentPlayer) => print(" "+currentPlayer+"(YouLoose) ")
      players foreach (_ ! YouLoose)
      
      val looser = {
        if(dice.isEmpty || dice.length < 2) {
          currentPlayer
        } else {
          val after = dice.head
          val before = dice.tail.head
          if(before < after) {
            currentPlayer
          } else {
            val previousPlayer = (currentPlayer - 1 + noOfPlayers) % noOfPlayers
            previousPlayer
          }
        }
      }
      
      reset(looser)
      
      nextTurn(looser)
  }
  
  def reset(looser: Int): Unit = {
    currentPlayer = looser
    dice = List[Dice]()
    numbers = List[Dice]()
    hasThrownDice = false
    permille(looser) -= 1
  }
  
  def nextTurn(looser: Int): Unit = {
    if(permille(looser) <= 0) {
      players foreach (_ ! Looser(playerNames(looser)))
      context.system.shutdown()
    } else {
      players(looser) ! Turn
    }
    
    println(permille.mkString("[", ",", "]"))
  }
  
}