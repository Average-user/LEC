# Logic Expressions Converter

A program to transform logic expressions into their Conjunctive normal form (CNF) and
Disjunctive Normal Form (DNF) representations.

So far the program works fine with any expression composed by:

| Operators             | ASCII representation   | Also known as |
|:----------------------|:----------------------:|:--------------|
| Negation              | !                      | Not           |
| Conjunction           | &                      | And           |
| Disjunction           | \|                     | Or            |
| Implication           | >                      | Implies       |
| Equality              | =                      | Iff           |
| Exclusive Disjunction | +                      | Xor           |


### To do list:

- [x] Parser
- [ ] Converter
    - [x] De Morgan's Law
    - [x] Distributivity
    - [ ] Redundant clauses elimination
- [x] Tests
    - [x] Equality between a formula and his (CNF, DNF) conversion
    - [x] Correctness of conversions (CNF, DNF)
- [ ] Human readable error messages
- [ ] Benchs


Some examples of how does this work:
```Text
$ stack exec LEC-exe

Insert Expression:
!(A | B) & C

Parsed Expression:       !(A | B) & C
Conjunctive normal form: !A & !B & C
Disjunctive normal form: !A & !B & C


Insert Expression:
!(A | B) | C

Parsed Expression:       !(A | B) | C
Conjunctive normal form: (!A | C) & (!B | C)
Disjunctive normal form: (!A & !B) | C


Insert Expression:
(A = B)

Parsed Expression:       A = B
Conjunctive normal form: (A | !A) & (A | !B) & (B | !A) & (B | !B)
Disjunctive normal form: (A & B) | (!A & !B)


Insert Expression:
(A = B) > C

Parsed Expression:       (A = B) > C
Conjunctive normal form: (!A | !B | C) & (A | B | C)
Disjunctive normal form: (!A & A) | (!A & B) | (!B & A) | (!B & B) | C
```
