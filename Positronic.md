The Positronic rules are a large family of 5-state rules extending Fireworld. 

Every rule in this family has 2 parallel systems 1 live state and 1 corresponding dead state, represented, respectively, as 1 and 2 vs. 3 and 4. Each system acts on its own exactly the same as Fireworld, but when they interact, additional rules are added in a way that must preserve the symmetry, meaning that if all states in any pattern are replaces by their opposites, the behavoir of the pattern doesn't change.

Such "interstate" rules allow for creating still lifes, stable reflectors as simple a single dot of the opposite state, arbitrary period guns, guns controlled by an on/off switch and other interesting patterns.

By far, the following rule tables are added to the /Positronic subsirectory of the repository:

# Positronic0

A generic rule with 2 parallel Fireworlds, without any "interstate" rules. Included by definition in all other Positronic rules.

# Positronic1

The number of orthogonal neighbours and the number of diagonal neighbours are counted separately as **the total sum of values** of the surrounding live cells (either 1 or -1). "Negative" cells are born and survive according to the same rules as in Fireworld, except that the respective number of live neighbours must also be negative.

The long list of possible posititions of "positive" and "negative" orthogonal and diagonal neighbours was generated and verified by a computer algorithm.

This rules behaves quite similar to the generic Postronic0, but has unique still lifes and some interesting reactions.

# Positronic2

A cell is born, if surrounded by 1 orthogonal and 1 diagonal live neighbour of the same state, and 1 live neighbour of the opposite state.

A cell survives, if it has one diagonal neighbour of the opposite state.

A cell survives, if it has 1 orthogonal and 1 diagonal neighbour, one of which must be of the opposite state.

A cell survives, if it has one orthogonal neighbour of the opposite state and at least one diagonal dead neighbour.

In this rule a single dot of the opposite state acts either as a slided 180-degree stable reflector or a controllable 12p gun, which makes possible to create various stable reflectors, ship multipliers, arbitrary period guns etc.

# Positronic3

A cell is born, if surrounded by 1 orthogonal and 1 diagonal live neighbour of the same state, and 1 live neighbour of the opposite state.

A cell is born if surrounded by 1 orthogonal and 1 diagonal neighbour of the opposites state and at least 2 dead neighbours.

A cell survives, if it has 1 orthogonal and 1 diagonal neighbour of the opposite state.

In this rule a single dot of the opposite state acts as a stable 90-degree reflector.


