package de.scala_bs

package object mia {
  
  // messages for connection phase

  case object JoinRequest
  case object Join
  case class Joined(playerName: String)


  // messages for game phase

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
  case class Looser(playerName: String)
  
}