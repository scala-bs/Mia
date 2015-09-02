package de.scala_bs.mia


import akka.actor.{ActorSystem, Props}


object Run extends App {
  
  val system = ActorSystem("Mia")
  val gameLogic = system.actorOf(Props[GameActor], "game-logic")
  
  gameLogic ! Start
  
  //Thread.sleep(1000)
  //system.shutdown()
  
}


import akka.actor._


class GameActor extends Actor {
  
  val players = Array[ActorRef](
    context.actorOf(Props[PlayerActor], "player-1"),
    context.actorOf(Props[PlayerActor], "player-2"),
    context.actorOf(Props[PlayerActor], "player-3")
  )
  
  val permille = Array[Int](
    10,
    10,
    10
  )
  
  val noOfPlayers = 3
  var currentPlayer = 0
  
  def randomDice: Dice = {
    def die: Int = scala.util.Random.nextInt(5) + 1
    
    val one = die
    val two = die
    
    Dice(scala.math.max(one, two), scala.math.min(one, two))
  }
  
  var dice = List[Dice]()
  
  def receive = { // TODO: Track if player decides not to throw dice!
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


class PlayerActor extends Actor {
  
  def receive = gamePhase
  
  
  var dice = List[Dice]()
  
  def gamePhase: Receive = {
    case Turn if dice.isEmpty || dice.length < 2 => {
      sender() ! ThrowDice
    }
    case Turn => {
      val after = dice.head
      val before = dice.tail.head
      if(before < after) sender() ! ThrowDice
      else sender() ! Lie
    }
    case Dice(die1, die2) => {
      sender() ! Number(die1*10+die2)
    }
    case Number(number) => {
      val one = number / 10
      val two = number % 10
      dice = Dice(one, two) :: dice
    }
    case Lie => {
      dice = List[Dice]()
    }
  }
  
}


case object Start
case object Turn
case object ThrowDice
case class Dice(die1: Int, die2: Int) extends Ordered[Dice] {
  
  def compare(that: Dice): Int = this match {
    case Dice(2, 1) => 1                                  // if I have a mia... I always win
    case Dice(die1, die2) if die1 == die2 => that match { // if I have doubles...
      case Dice(2, 1) => -1                               // ...and he has a mia I loose
      case Dice(d1, d2) if d1 != d2 => 1                  // ...and he has no doubles I win
      case Dice(d1, d2) if die1 < d1 => -1                // ...and he has bigger doubles I loose
      case _ => 1                                         // ...and he has smaller or equal doubles I win
    }
    case Dice(die1, die2) => that match {                 // if I have no doubles...
      case Dice(2, 1) => -1                               // ...and he has a mia I loose
      case Dice(d1, d2) if d1 == d2 => -1                 // ...and he has doubles I loose
      case Dice(d1, d2) if die1*10+die2 < d1*10+d2 => -1  // ...and he has a bigger number I loose
      case _ => 1                                         // ...and he has a smaller or equal number I win
    }
  }
    
    
}
case class Number(number: Int)
case object Lie