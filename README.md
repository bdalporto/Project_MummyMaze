# How to Run
Download the project:

     git clone https://github.com/bdalporto/Project_MummyMaze.git
     
Then, navigate to directory of project and execute the following:

     stack build
     stack exec mum-game-exe

To run the unit tests (from within project directory):

     stack test

# CSE 230 FA21 Project Proposal
#### Group Members:  Brandon DalPorto, Sarah Ekaireb

Mummy Maze Deluxe Game Remake

 We plan on implementing a maze based game very similar to the classic mummy maze game found here: https://www.microsoft.com/en-us/p/mummy-maze-classic/9wzdncrd35fd.

The game involves a player: the Indiana Jones-like adventurer and an evil mummy that is chasing him. The goal of the player is to reach a doorway leading towards the exit on each level. The player may move one tile at a time, whereas the mummy can move two tiles at a time. The player is at an advantage, as there are various walls scattered across each level, and the mummy is not very smart, running into walls at times if it finds that to be the shortest path. 

The game gets more difficult as the levels progress, including possible additions if the development of the game progresses well over the rest of the quarter. 
Inspired from the original game creators, we could possibly implement the following features to make each level more difficult:

* Different kinds of mummies
* Scorpions
* Keys
* Darker room
* Traps 

Another way to make the game more interesting would be using a seed-method of level generation, creating a random level based on RNG parameters to an input function.

We plan on developing this game in stages:
1. Get the game functionality working, no major graphical components
2. Implement basic graphics
3. Implement features to each level to increase difficulty
4. Implement graphics to make it resemble the original design (or something greater than simple models or shapes).

![Alt text](https://bigheadghost.github.io/img/2014/mummy_maze_with_longest_solution.jpg "Optional title")


# UPDATES (Checkpoint 2)
####
As of 11/29, we've implemented all of the features for a solid baseline of our game. We have a level selection screen, instructions, and the core logic and graphics of the game. We've implemented some additional features that we originally considered unnecessary, but that would make the game more difficult / fun (see below). It has not all been an easy process, as there have been some frustrations with the UI and features available for implementing graphical features. Given the available tools of th ebrick library, we believe we have a great looking and functional game. We do not expect to use any additional libraries outside of those already implemented -- mainly Brick, Vty, Control, and Data.

***Implemented add-ons in addition to base game***
* Traps                             
* Keys -----> Walls / Exit             
* Controls UI                           
    - Display Level name                 
* Instructions Page                
* Make graphics better             
* Open fixed size terminal         ***IMPLEMENTED***
* Border                           ***Different solution implemented***

***TO-DO***
* Dark Rooms ***Need to complete***
* Different Mummies ***need to complete***
* Make all levels   ***Partially done***
* Character Legend (In instruction page?)

Here's a little sneak peak!

![image](https://user-images.githubusercontent.com/91349464/143976949-938f3c28-27b9-4120-acf7-1d733c66c898.png)


​​

