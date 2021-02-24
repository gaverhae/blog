# titans

An early exploration of Haskell; the problem this is solving is that of
optimizing a store in the "Shop Titans" game. In that game, the player owns a
store frequented by adventurers. Every adventurer has a class, and the player
has no control over the rate of arrival of adventurer or their class.
Adventurers will walk around the entire store and then buy something they can
use (or leave if there wasn't anything for them).

Therefore, the optimum arrangement would be one that:
1. Is as small as possible, so adventurers don't spend a lot of time walking
   around.
2. Has at least one item for every single class, such that any adventurer
   coming in will find at least one thing to buy.

The player can also get better at crafting recipes they craft often, further
reinforcing the usefulness of restricting the range of items produced.

This code served its purpose, but I don't play the game anymore so it's
unlikely to see any further development.
