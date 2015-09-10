# Mia

This is an actor implementation of the popular German drinking game *Meiern*.
It is also known by different names such as Mäxchen or Mäxle.
For more information on the game have a look at [Wikipedia](https://de.wikipedia.org/wiki/Meier).

The current implementation in mia.scala is just a first draft and has to be refined.


## How to run

Open three shells and execute each of the following commands in its own shell:

$ ./activator "run server"
$ ./activator "run player"
$ ./activator "run player1"
$ ./activator "run player2"


## How to configure

The number of players needed to start a game can be set by no-of-players in server.conf.

The player name can be set by player-name in player.conf, player1.conf, and player2.conf.

The server address can be set by server-address in player.conf, player1.conf, and player2.conf.