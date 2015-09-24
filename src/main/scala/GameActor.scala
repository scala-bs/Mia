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
    case m @ _ => println("c "+m)
  }
  
  // Game state
  
  lazy val permille = Array.fill(players.size)(0)
  var currentPlayer = 0
  var diceRolls = List.empty[Dice]
  var pronouncements = List.empty[Number]
  
  def gamePhase: Receive = {
    case Start =>
      players(currentPlayer) ! Turn
      
    case ThrowDice if sender() == players(currentPlayer) =>
      println(s"Player $currentPlayer rolls a die")
      
      val d = Dice.random()
      diceRolls = d :: diceRolls
      sender() ! d
      
    case n: Number if sender() == players(currentPlayer) =>
      pronouncements = n :: pronouncements
      println(s"Player $currentPlayer says ${n.number}")
      players foreach (_ forward n)
      
      currentPlayer = (currentPlayer + 1) % noOfPlayers
      players(currentPlayer) ! Turn
      
    case InvalidPronouncement if sender() == players(currentPlayer) =>
      // check if the last pronouncement was really invalid to continue with the game
      
      val wasInvalid = pronouncements match {
        case p1 :: tail if !p1.toDice.isDefined => true   // if the last announcement cannot be translated into dice, the announcement was invalid
        case p1 :: p2 :: tail => p1 <= p2                 // if the last announcement is smaller or equal than the announcement before, it is invalid
        case _ => true
      }
      
      val looser = if(wasInvalid) {
        (currentPlayer - 1 + noOfPlayers) % noOfPlayers
      } else {
        currentPlayer
      }
      
      roundEnds(looser)
      currentPlayer = looser
      checkGameEnd match {
        case Some(gameLooserInd) => endGame(gameLooserInd)
        case None => players(currentPlayer) ! Turn
      }
      
    case Lie if sender() == players(currentPlayer) => 
      println(s"Player $currentPlayer says Lie!")
      players foreach (_ ! Lie)
      
      // check, if last pronouncement was a lie
      val looser = diceRolls match {
        case Nil => currentPlayer // if no dice were rolled, current player loses
        case lastRoll :: tail if lastRoll.toNumber == pronouncements.head => currentPlayer // if the last pronouncement matches with the last die, current player looses
        case _ => (currentPlayer - 1 + noOfPlayers) % noOfPlayers // else the previous player loses
      }
      
      currentPlayer = looser
      
      roundEnds(looser)
      checkGameEnd match {
        case Some(gameLooserInd) => endGame(gameLooserInd)
        case None => players(looser) ! Turn 
      }
      
      println(permille.mkString("[", ",", "]"))
  }
  
  def roundEnds(looserIndex: Int) {
    players foreach (_ ! LooseTurn(playerNames(looserIndex)))
    
    diceRolls = List.empty[Dice]
    pronouncements = List.empty[Number]
    permille(looserIndex) += 1
  }
  
  def checkGameEnd = {
    val looserIndex = permille.indexWhere(_ >= 10)
    if(looserIndex == -1) None else Some(looserIndex)
  }
  
  def endGame(gameLooserInd: Int) = {
    players foreach (_ ! Looser(playerNames(gameLooserInd)))
    context.system.shutdown()
  }
}
