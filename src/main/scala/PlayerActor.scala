package de.scala_bs.mia

import akka.actor.{ActorSelection, Actor}

class PlayerActor(server: ActorSelection, name: String) extends Actor {
  
  def receive = {
    case _ => None
  }
  
}