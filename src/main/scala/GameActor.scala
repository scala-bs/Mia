package de.scala_bs.mia


import scala.collection.mutable.HashSet
import akka.actor.{ActorRef, Actor}


class GameActor(noOfPlayers: Int) extends Actor {
  
  // Shared state
  
  val players = new Array[ActorRef](noOfPlayers)
  
  def receive = connectionPhase
  
  
  // Connection state
  
  val joiningPlayers = HashSet[ActorRef]()
  var joinedPlayersCount = 0
  
  def connectionPhase: Receive = {
    case JoinRequest => // save the player as joining and ask him to join
      val ref = sender()
      joiningPlayers += ref
      ref ! Join
    case Joined => // remove the player from joining and add them to players. if the player capacity is reached start the game.
      val ref = sender()
      joiningPlayers -= ref
      players(joinedPlayersCount) = ref
      joinedPlayersCount += 1
      self ! Start
    case Start if joiningPlayers.size == 0 && joinedPlayersCount == noOfPlayers =>
      context.become(gamePhase)
      self ! Start
    case m @ _ => println("c "+m)
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
  
  def gamePhase: Receive = {
    case Start =>
      players(currentPlayer) ! Turn
    case ThrowDice if sender() == players(currentPlayer) =>
      val d = randomDice
      dice = d :: dice
      sender() ! d
    case n: Number if sender() == players(currentPlayer) => print(" "+currentPlayer+"("+n.number+") ")
      players foreach (_ forward n)
      currentPlayer = (currentPlayer + 1) % noOfPlayers
      players(currentPlayer) ! Turn
    case Lie if sender() == players(currentPlayer) => print(" "+currentPlayer+"(Lie) ")
      players foreach (_ ! Lie)
      
      val looser =
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
      
      currentPlayer = looser
      dice = List[Dice]()
      permille(looser) -= 1
      
      if(permille(looser) <= 0) context.system.shutdown()
      else players(looser) ! Turn
      
      println(permille.mkString("[", ",", "]"))
  }
  
}