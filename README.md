# Sicefish
A chess engine written in Haskell.

(Feel free to put this on the course website!)

## Running Sicefish

In order to run Sicefish, simply run `stack run` in your terminal. This will execute the chess engine, which is a command line application which communicates using the UCI protocol, which is detailed [here](http://wbec-ridderkerk.nl/html/UCIProtocol.html).

Note that our engine only supports the main UCI commands, which include:

- `uci`: Query the engine to obtain author information and ensure the application supports UCI.
- `ucinewgame`: Initiate a new game (that is, reset the position).
- `isready`: Query the engine to check whether it is ready or not.
- `position`: Input a specific chess position. This command accepts both FEN (see [here](https://www.chessprogramming.org/Forsyth-Edwards_Notation)) as well as `startpos`, which is the starting position. Additional moves can be specified afterwards.
- `go`: Run the engine on the given position. That is, ask the engine to find the best move.
- `quit`: Exit the engine.

Of course, running the engine as a command line application is quite unwieldy. For a better user experience, we recommend using a GUI that communicates using the universal chess interface. One such GUI is [Arena Chess](http://www.playwitharena.de/). To load the engine, go to Engines -> Install new engine, then select the executable produced by `stack build`. From then, the engine is loaded and you can play against it, or, to have it play against itself, press Ctrl+D.

