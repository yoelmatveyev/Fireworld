As this project grew, I decided to add my experiments with other previously unknown rules.

# Twolives

Due to the most unfortunate sudden demise of John Horton Conway, may he rest in peace, I added a new rule to this project called Twolives. It's a variety of the old good Conway's Game of Life, in which there exist two types of live cells of opposite "sign"; Two identically behaving "life forms" may coexist and interact with each other.

The rules of survival and birth are usual, but strictly symmetrical. 
A "negative" cell survives, if and only if it has 2 or 3 "negative" live neighbours.
A "negative" cell is born, if and only if it has 3 "negative" live neighbours.  

**Importantly**, the number of neighbours is counted as **the total sum of their numerical values**, which may be 1 or -1. 
Thus, a "positive" cell may be born, if surrounded by 5 "positive" and 2 "negative" neighbours.
A "negative" cell survives, if surrounded by 5 "negative" and 3 "positive" cells etc.

Of course, since these "two lives" are completely symmetrical, there is nothing inherently "negative" or "positive" in any of them.

# Positronic

As a follow-up to the above mentioned symmetrical variety of Conway's Game of Life, I added a symmetrical version of Fireworld. It's a 5-state rule that allows to
"positively" and "negatively" charged worlds of "matter" and "antimatter" to behave identically and interact with each other.

The rules are exactly the same as in Fireworld, except that the number of orthogonal neighbours and the number of diagonal neighbours are counted separately as **the total sum of values** of the surrounding live cells (either 1 or -1). "Negative" cells are born and survive according to the same rules as in Fireworld, except that the respective number of live neighbours must also be negative.

As long as there only positive or negative cells around, the system behaves 100% identically. The list of possible posititions of possible "positive" and "negative" orthogonal and diagonal neighbours was generated and verified by a computer algorithm. "Negative" cells are encoded as State 3; Dead "negative" cells are encoded as State 4.

The name of the rule is, of course, inspired by Lieutenant Data's positronic brain.

# Cyclical multistate rules

Classical cellular automata such as Conway's Game of Life can be generalized by creating multistate rule with 2 or more "parallel universes", which act by themselves as the original rule. When these "parallel universes" collide, additional rules create new, hybrid forms of behavior, which may look entirely differently that the original rule. 

Additionally, there is an enterely new, unexplored field of multistate rules, which don't resemble any classical binary rules. I started exploring this field in June 2020. 

For the sake of symmetry and consistency, state transformations must be **cyclical**. 

For example, if 2 cells of state A and 1 cell of state B give birth to a cell of state C in a 3-state automation (not counting the "ground" zero state), 
then 2 cells of state B and 1 cell of state C must give birth to a cell of state A, and 2 cells of state C and 1 cell of state A must give birth to a cell of state B.

Not that this does not define the behavior of a cell surrounded by 2 cells of state B and 1 cell of state A. In a multistate universe it's a different rule, which may cause nothing or give birth to either A, B or C. If a cell is surrounded by an equal numbers of all states, it may not become alive, because it is impossible to define a cyclicallt consistent birth rule.

Thing become even more complicated, when it comes to survival rules. A surviving cell may be transformed in some cases into a different state; if a cell of state C survives or gets transformed by 2 cells of state B and 1 cell of state A, it does not imply that the same must happen with a cell of state B or A. These may be two different additional rules. The aformentioned rule does imply that if a cell of state C gets transformed into state B by 2 cells of state B and 1 cell of state A, then a cell of state A gets transformed into state C by 2 cells of state C and 1 cell of state B, and a cell of state B gets transformed into state A by 2 cells of state A and 1 cell of state C.

Writing such rule tables by hand is a laborous task prone to errors. Therefore I developed a notation for such rules and wrote a Lisp program that produced the corresponding rule files. For the time being, only "totalistic", i.e. permutable rules, in which the position of the neighbors doesn't matter, are taken into consideration.

In principle, multistate cyclical rules may be defined as generalizations of the "Generations" family of rules or rules with other symmetries, although the number of various possible state combinations may become exhaustively large. For more details, read the file **Multistate_cyclical_CA.md**.
