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
      val d = Dice.random()
      diceRolls = d :: diceRolls
      sender() ! d
      
    case n: Number if sender() == players(currentPlayer) =>
      pronouncements = n :: pronouncements
      println(s"Player $currentPlayer says ${n.number}")
      players foreach (_ forward n)
      
      // check if the number is valid to continue with the game
      val validPronouncement = if(!diceRolls.isEmpty && !diceRolls.tail.isEmpty) {
        val lastPlayersRoll = diceRolls.tail.head
        
        n.toDice match {
          case Some(d: Dice) if d > lastPlayersRoll => true 
          case None => lastPlayersRoll.toNumber < n // if the player said a number that cannot be translated to a die, just compare, if it is bigger than the last number
        }
      } else true
      
      if(!validPronouncement) {
        roundEnds(currentPlayer)
        checkGameEnd match {
          case Some(gameLooserInd) => endGame(gameLooserInd)
          case None => players(currentPlayer) ! Turn
        }
      } else {
        currentPlayer = (currentPlayer + 1) % noOfPlayers
        players(currentPlayer) ! Turn
      }
      
    case Lie if sender() == players(currentPlayer) => 
      println(s"Player $currentPlayer says Lie!")
      players foreach (_ ! Lie)
      
      // check, if last pronouncement was a lie
      val looser = diceRolls match {
        case Nil => currentPlayer // if no dices were rolled, current player loses
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