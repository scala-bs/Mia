package de.scala_bs.mia


import akka.actor.{ActorSystem, Props}


object Run extends App {
  
  val system = ActorSystem("Mia")
  val gameLogic = system.actorOf(Props(classOf[GameActor], 3), "game-logic")
    
  for(i <- 1 to 3) {
    system.actorOf(Props(classOf[PlayerActor], gameLogic), "player-"+i)
  }
  
}