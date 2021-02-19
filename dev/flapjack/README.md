# flapjack

## Rules

Attempts at devising a strategy for the game "flapjack". There is more to the
rules, but for our purposes the game goes like this;

1. The game is played with half a deck of cards, which contains a red suite and
   a black suit. (No joker.) This half-deck is shuffled.
2. The player draws cards one at a time. After each draw, the game stops if:
   a. The player decides to stop.
   b. There are no more cards in the deck.
   c. The current score for the player's hand is 25 or more.

The score of a hand is calculated as:

- A red card has negative value; a black one has positive value.
- An ace counts as either 1 or 11, at the player's choice.
- 2 to 10 are worth their numeric value.
- Faces are worth 10 each.

After the drawing stops, the player gains a number of points calculated as
follows. If the hand score is between 16 and 25 inclusive, the player gains (25
- score) points (i.e. 25 gives 0, 22 gives 3, 16 gives 9). If the score is
anything else, the player gets 10 points. The goal is to have as few points as
possible.

## Result

I have tried a few different approaches, but I have not found a strategy that
beats a simple "stop if your current point gain is 6 or lower".
