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
  
  object Dice {
    def random(): Dice = {
      def die: Int = scala.util.Random.nextInt(6) + 1
      
      val one = die
      val two = die
      
      Dice(scala.math.max(one, two), scala.math.min(one, two))
    }
  }
  
  case class Dice(die1: Int, die2: Int) extends Ordered[Dice] {
    def compare(that: Dice): Int = this match {
      case Dice(2, 1) => 1                                  // if I have a mia... I always win
      case Dice(die1, die2) if die1 == die2 => that match { // if I have doubles...
        case Dice(2, 1) => -1                               // ...and he has a mia I loose
        case Dice(d1, d2) if d1 != d2 => 1                  // ...and he has no doubles I win
        case Dice(d1, d2) if d1 == d2 && die1 < d1 => -1    // ...and he has bigger doubles I loose
        case _ => 1                                         // ...and he has smaller or equal doubles I win
      }
      case Dice(die1, die2) => that match {                 // if I have no doubles...
        case Dice(2, 1) => -1                               // ...and he has a mia I loose
        case Dice(d1, d2) if d1 == d2 => -1                 // ...and he has doubles I loose
        case Dice(d1, d2) if die1*10+die2 <= d1*10+d2 => -1 // ...and he has a bigger or equal number I loose
        case _ => 1                                         // ...and he has a smaller or equal number I win
      }
    }
    
    def toNumber = Number(die1*10+die2) 
  }
  
  case class Number(number: Int) extends Ordered[Number] {
    def toDice = if(number >= 11 && number <= 66) {
      Some(Dice(number/10, number%10))
    } else None
    
    def compare(that: Number): Int = (this.toDice, that.toDice) match {
      case (Some(thisDice), Some(thatDice)) => thisDice compare thatDice
      case _ => this.number compare that.number
    }
  }
  
  case object Lie
  case object InvalidPronouncement
  case class LooseTurn(playerName: String)
  case class Looser(playerName: String)
}