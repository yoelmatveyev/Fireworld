As this project grew, I decided to add my experiments with other previously unknown rules and an entirely new rule family.

# Twolives

Due to the most unfortunate sudden demise of John Horton Conway, may he rest in peace, I added a new rule to this project called Twolives. It's a variety of the old good Conway's Game of Life, in which there exist two types of live cells of opposite "sign"; Two identically behaving "life forms" may coexist and interact with each other.

The rules of survival and birth are usual, but strictly symmetrical. 
A "negative" cell survives, if and only if it has 2 or 3 "negative" live neighbours.
A "negative" cell is born, if and only if it has 3 "negative" live neighbours.  

Importantly, the number of neighbours is counted as the total sum of their numerical values, which may be 1 or -1. 
Thus, a "positive" cell may be born, if surrounded by 5 "positive" and 2 "negative" neighbours.
A "negative" cell survives, if surrounded by 5 "negative" and 3 "positive" cells etc.

Of course, since these "two lives" are completely symmetrical, there is nothing inherently "negative" or "positive" in any of them.

Twolives is just of many possible cyclical multistate extensions of Conway's Game of Life, denoted as **D3a41a52a-2a31a42a53a3a41a52a** in my notation of cyclical multistate rules (see below).

# Positronic Rule Family

As a follow-up to the above mentioned experiments with multistate rules and extensions of Conway's Game of Life, I started experimeting with symmetrical versions of Fireworld. There is a large family of 5-state rules, which I call Positronic (named after Lieutenant Data's positronic brain, of course). Every rule in this family has 2 parallel systems 1 live state and 1 corresponding dead state. Each system acts on its own exactly the same as Fireworld, but when they interact, additional rules are added in a way that must preserve the symmetry, meaning that if all states in any pattern are replaces by their opposites, the behavoir of the pattern doesn't change.

Such "interstate" rules allow for creating still lifes, stable reflectors as simple a single dot of the opposite state, arbitrary period guns, guns controlled by an on/off switch and other interesting patterns.

Read more in **Positronic.md**.

# Cyclical multistate rules

## Invented by Yoel Matveyev in July 2020

Classical cellular automata such as Conway's Game of Life can be generalized by creating multistate rule with 2 or more "parallel universes", which act by themselves as the original rule. When these "parallel universes" collide, additional rules create new, hybrid forms of behavior, which may look entirely differently that the original rule. 

Additionally, there is an enterely new, unexplored field of multistate rules, which don't resemble any classical binary rules. I started exploring this field in June 2020. 

For the sake of symmetry and consistency, state transformations must be **cyclical**. 

For example, if 2 cells of state A and 1 cell of state B give birth to a cell of state C in a 3-state automation (not counting the "ground" zero state), 
then 2 cells of state B and 1 cell of state C must give birth to a cell of state A, and 2 cells of state C and 1 cell of state A must give birth to a cell of state B.

Not that this does not define the behavior of a cell surrounded by 2 cells of state B and 1 cell of state A. In a multistate universe it's a different rule, which may cause nothing or give birth to either A, B or C. If a cell is surrounded by an equal numbers of all states, it may not become alive, because it is impossible to define a cyclicallt consistent birth rule.

Thing become even more complicated, when it comes to survival rules. A surviving cell may be transformed in some cases into a different state; if a cell of state C survives or gets transformed by 2 cells of state B and 1 cell of state A, it does not imply that the same must happen with a cell of state B or A. These may be two different additional rules. The aformentioned rule does imply that if a cell of state C gets transformed into state B by 2 cells of state B and 1 cell of state A, then a cell of state A gets transformed into state C by 2 cells of state C and 1 cell of state B, and a cell of state B gets transformed into state A by 2 cells of state A and 1 cell of state C.

Writing such rule tables by hand is a laborous task prone to errors. Therefore I developed a notation for such rules and wrote a Lisp program that produced the corresponding rule files. For the time being, only "totalistic", i.e. permutable rules, in which the position of the neighbors doesn't matter, are taken into consideration.

Multistate cyclical rules may be defined as generalizations of the "Generations" family of rules or rules with other symmetries, although the number of various possible state combinations may become exhaustively large. For more details, read the file **Multistate_cyclical_CA.md**. For the Positrinic Rule Family, which extends the Fireworld rule in a sumilar way, read **Positronic.md**.

Some 3-state symmetrical rules (4-state, if the empty state is counted) exhibit the complexity of the Fireworld and inherit some patterns of the regular Game of Life and Seeds without exploding into chaos. Two rules seems the most promissing, discovered in July 2020, which I named Gluons and Gluonic,**T2b-0a011a3n** and **T2b102b-0a011a3n** in my notation. Another rule, Morse, **T11c12a21b-2a3a**, is also quite interesting.

# Gluons

## Invented by Yoel Matveyev in July 2020

***1. A cell is born only if surrounded by 2 cells of the same color. The newborn cell takes the next color in the cycle (i.e. two red cells give birth to a green cell, green to blue, blue to red).***

***2. A cell survives, if it stands alone, surrounded by 2 cell of 2 different colors or by 3 cells of any color.***

This rule has proven to have oscillators of all periods and guns for all periods above p>=14, as well as 6, 8 and 12.

# Gluonic

## Invented by Yoel Matveyev in July 2020

***1. A cell is born, if surrounded by 2 cells of the same color. The newborn cell takes the next color in the cycle: two red cells give birth to a green cell, 2 green to blue, 2 blue to red.***

***2. A cell of a certain color is born, when surrounded by one cell of the previous color in the cycle and two cells of the next color: one red cell and two blue cells give birth to a green cell, 1 green cell and 2 red cells give birth to a blue cell, 1 blue cells and 2 green cells give birth to a red cell.***

***3. A cell survives, if it stands alone, surrounded by 2 cells of 2 different colors or surrounded by 3 cells of any color.***

Note that since this rule is cyclical, it does not imply that two red cells and 1 blue cell give birth to a green cell etc. That would be another cyclical rule (too exploding, actually). Very similar to Gluons, but this extra rule adds some unique dynamics, sparky spaceships and other interesting things.

# Morse

## Invented by Yoel Matveyev in September 2020

***1. 2 cells of 2 different colors give births to a cell of the third color.***

***2. 1 cell of one color and 2 cells of another color give birth to a cell of the first (minority) color.***

***3. A cell survives, if surrounded by 2 or 3 cells of the same color.***

The survival rule ensures that all Conway's GOL still lifes work in this rule too (in 3 possible color phases). Many more still lifes are available; some appear naturally. This rule is abundant with a rich variety of natural orthogonal ships moving at the speed of light, rakes, puffers, 2 different c/1 replicators and breeders. Patterns tend to explode, but not chaotically; evolving networks of interacting infinitely growing replicators resemble Morse code.

THere are oscillators of many small even periods, many spaceships, puffers, rakes, breeders, some guns. Quite a few simple replicator-based oscillators have exorbitant periods well over 10e8.

# Conway's Game of Life cyclical extensions

Multistate cyclical rules with a given number of states can emulate any rule with a smaller number of states, including all totallistic Life-like rules. Thus, Conway's Game of Life can be augmented by additional rules, only applied when 2 or more different states interact. As long as he entire pattern is made of the same color, the rule behaves exactly like the classic Life, but changes its behavior radically, when different states or color collide, producing map-like fractals, stable structures, chaos etc. 

Examples of such rules in this repository include **SeaLife**, **Triple_Swamps** and **Triple_Behavior_Life**.

# Lace

## Invented by Yoel Matveyev in September 2020

Another simple way to have c/1 spaceships without complete chaos, besides Generations rules like Fireworld or above-explained the cyclical rules is to "cut" the rule's totalitistic behavior. One such rule is **B2ae/S1e3y5**, which I call **Lace**, because it tends to explode into infinite, but not chaotic fractal structures that resemble sophisticated lace-like ornaments. The same happens to many other rules that contain B2ea, including plain B2ea/S, but this rule has extremely simple and easily controllable guns (the most basic one made of just 2 cells), which appears naturally as parts of rakes and puffers.
