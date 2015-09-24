package de.scala_bs.mia


import akka.actor.{ActorSystem, Props}
import com.typesafe.config.ConfigFactory


object Run extends App {
  
  if(args.size != 1 || !Set("player", "player1", "player2", "server", "local").contains(args(0))) {
    println("Please provide a run mode: either 'player', 'player1', 'player2', 'server', or 'local'.")
  } else if(args(0) == "local") {
    
    val system = ActorSystem("Mia")
    system.actorOf(Props(classOf[GameActor], 3), "server")
    
    val serverSelection = system.actorSelection("akka://Mia/user/server")
    system.actorOf(Props(classOf[PlayerActor], serverSelection, "player1"), "player1")
    
    Thread.sleep(500)
    
    system.actorOf(Props(classOf[PlayerActor], serverSelection, "player2"), "player2")
    
    Thread.sleep(500)
    
    system.actorOf(Props(classOf[PlayerActor], serverSelection, "player3"), "player3")
    
    Thread.sleep(2000)
    
    system.shutdown()
    
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