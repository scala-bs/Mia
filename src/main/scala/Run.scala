package de.scala_bs.mia


import akka.actor.{ActorSystem, Props}
import com.typesafe.config.ConfigFactory


object Run extends App {
  
  if(args.size != 1 || !Set("player", "player1", "player2", "server").contains(args(0))) {
    println("Please provide a run mode: either 'player' or 'server'.")
  } else if(args(0) == "server") {
    
    val config = ConfigFactory.load("server")
    val noOfPlayers = config.getInt("no-of-players")
    
    val system = ActorSystem("Mia-Server", config)
    system.actorOf(Props(classOf[GameActor], noOfPlayers), "server")
    
  } else {
        
    val player = args(0)
    
    val config = ConfigFactory.load(player)
    val playerName = config.getString("player-name")
    val serverAddress = config.getString("server-address")
    
    val system = ActorSystem("Mia-Player", config)
    val server = system.actorSelection(serverAddress)
    
    system.actorOf(Props(classOf[PlayerActor], server, playerName), player)
    
  }
  
}