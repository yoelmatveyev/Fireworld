# Twolives

Due to the most unfortunate sudden demise of John Horton Conway, may he rest in peace, I added a new rule to this project called Twolives. It's a variety of the old good Conway's Game of Life, in which there exist two types of live cells of opposite "sign"; Two identically behaving "life forms" may coexist and interact with each other.

The rules of survival and birth are usual, but strictly symmetrical. 
A "negative" cell survives, if and only if it has 2 or 3 "negative" live neighbours.
A "negative" cell is born, if and only if it has 3 "negative" live neighbours.  

**Importantly**, the number of neighbours is counted as **the total sum of their numerical values**, which may be 1 or -1. 
Thus, a "positive" cell may be born, if surrounded by 5 "positive" and 2 "negative" neighbours.
A "negative" cell survives, if surrounded by 5 "negative" and 3 "positive" cells etc.

Of course, since these "two lives" are completely symmetrical, there is nothing inherently "negative" or "positive" in any of them.

# Fireworld

I invented this rule in 2002 while playing with Mcell (Mirek's Cellebration), a great cellular automata playground made by Mirosław (Mirek) Wójtowicz. To my knowledge, for a long while it was the only program of its kind, where you could set your own rules in various flexible ways. Now, since old Mcell files and rules are convertible to Golly (sometimes by some modification or by a custom script though), I decided to start converting and publicizing my old patterns, as well as engineering new ones. As a side note, Mcell works perfectly on Linux/Unix under Wine.

Originally I called this rule "Minimum", as it was deliberately meant to be "minimalistic" in its survival/birth mechanism. Soon I had discovered that this rule must be Turing-complete, as it exhibits in its own way the same complexity as Brian Silverman's Wireworld (hence the new name of my rule). Natural evolution of random patterns often resembles the operation of some complicated machinery or circuitry, occasionally producing fractal-like structures.

Although I did not have time to build a full computer model in Fireworld, patterns provided in this repository (logic gates, various types of ROM and RAM, binary counters, data buses etc.) may serve as a sufficient reason to believe that it must be possible.

Unlike Wireworld, almost nothing in Fireworld is stable, besides stand-alone 1x1 blocks and Life-like 2x2 blocks surrounded by supporting 1x1 blocks. Tiny orthogonal ships, guns, puffers, rakes, breeders are the common natural inhabitants of this universe. As these structures interact with each other in complex ways, the behavior of this rule combines Life-like and Wireworld-like features.

# Rule

Fireworld operates in the Moore neighbourhood. Cells may have 3 states: empty, alive and dead.

A empty cell becomes alive only if it is surrounded by one live cell horizontally or vertically adjacent to it, while one other cell is adjacent to it in diagonal. To get born, so to speak, the cell needs two parents of opposite sex. If some other cells in its neighbourhood are dead, they count as empty and don't interfere with the birth.

A live cell survives either if there are no other live cells in its neighbourhood or there are exactly three live cells adjacent to it in a particular way: two adjacent horizontally or vertically, while the third one is adjecent diagonally. In terms of survival, dead cells also count as empty and don't matter.

A dead cell becomes empty in the next generation.

This rule may be abbreviated in Golly simply as **03ajkr/2ak/3**.

# Positronic

As a follow-up to the above mentioned symmetrical variety of Conway's Game of Life, I added a symmetrical version of Fireworld. It's a 5-state rule that allows to
"positively" and "negatively" charged worlds of "matter" and "antimatter" to behave identically and interact with each other.

The rules are exactly the same as in Fireworld, except that the number of orthogonal neighbours and the number of diagonal neighbours are counted separately as **the total sum of values** of the surrounding live cells (either 1 or -1). "Negative" cells are born and survive according to the same rules as in Fireworld, except that the respective number of live neighbours must also be negative.

As long as there only positive or negative cells around, the system behaves 100% identically. The list of possible posititions of possible "positive" and "negative" orthogonal and diagonal neighbours was generated and verified by a computer algorithm. "Negative" cells are encoded as State 3; Dead "negative" cells are encoded as State 4.

The name of the rule is, of course, inspired by Lieutenant Data's positronic brain.


# Fireworld: Copyright by Yoel Matveyev 2002
# Twolives and Positronic: Copyright by Yoel Matveyev 2020
