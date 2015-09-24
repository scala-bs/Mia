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
  }
  
  
  // Game state
  
  var dice = List[Dice]()
  
  def gamePhase: Receive = {
    case Looser(_) =>
      context.system.shutdown()
  //case Turn =>
  //  sender() ! ThrowDice
  //  sender() ! YouLoose
  //  sender() ! Lie
  //case Dice(die1, die2) =>
  //  sender() ! Number(die1*10+die2)
  //case Number(number) =>
  //  do not send messages here! just keep track of the number!
  //case YouLoose | Lie =>
  //  this is when a new round starts
    case message @ _ =>
      println(message)
  }
  
}