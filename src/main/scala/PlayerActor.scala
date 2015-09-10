package de.scala_bs.mia


import akka.actor.{ActorSelection, Actor}


class PlayerActor(server: ActorSelection, name: String) extends Actor {
  
  server ! JoinRequest
  
  
  // Shared state
  
  def receive = connectionPhase
  
  
  // Connection state
  
  def connectionPhase: Receive = {
    case Join =>
      context.become(gamePhase)
      server ! Joined(name)
    case m @ _ => println("p "+m)
  }
  
  
  // Game state
  
  var dice = List[Dice]()
  
  def gamePhase: Receive = {
    case Looser(_) =>
      context.system.shutdown()
    case Turn if dice.isEmpty || dice.length < 2 =>
      sender() ! ThrowDice
    case Turn =>
      val after = dice.head
      val before = dice.tail.head
      if(before < after) sender() ! ThrowDice
      else sender() ! Lie
    case Dice(die1, die2) =>
      sender() ! Number(die1*10+die2)
    case Number(number) =>
      val one = number / 10
      val two = number % 10
      dice = Dice(one, two) :: dice
    case Lie =>
      dice = List[Dice]()
  }
  
}