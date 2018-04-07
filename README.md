# Logic Expressions Converter

A program to convert logic expression into their Conjunctive normal form (CNF) and
Disjunctive Normal Form (DNF) representations.

So far the program works fine with any expression composed by:

| Operators             | ASCII representation   | Also known as |
|:----------------------|:----------------------:|:--------------|
| Negation              | !                      | Not           |
| Conjunction           | &                      | And           |
| Implication           | >                      | Implies       |
| Equality              | =                      | Iff           |
| Exclusive Disjunction | +                      | Xor           |


### To do list:

- [x] Parser
- [ ] Converter
    - [x] De Morgan's Law
    - [x] Distributivity
    - [ ] Redundant clauses elimination
- [ ] Tests
    - [x] Equality between a formula and his (CNF, DNF) conversion
    - [ ] Correctness of conversions (CNF, DNF)
- [ ] Human readable error messages


Some examples of how does this work:
```Text
$ stack exec LEC-exe
Insert expression:
!(a | b) & c

Parsed Expression:       ¬(a ∨ b) ∧ c
Conjunctive Normal Form: ¬a ∧ ¬b ∧ c
Disjunctive Normal Form: ¬a ∧ ¬b ∧ c

Insert expression:
!(a | b) | c

Parsed Expression:       ¬(a ∨ b) ∨ c
Conjunctive Normal Form: (¬a ∨ c) ∧ (¬b ∨ c)
Disjunctive Normal Form: (¬a ∧ ¬b) ∨ c

Insert expression:
a = b

Parsed Expression:       a ≡ b
Conjunctive Normal Form: (a ∨ ¬a) ∧ (a ∨ ¬b) ∧ (b ∨ ¬a) ∧ (b ∨ ¬b)
Disjunctive Normal Form: (a ∧ b) ∨ (¬a ∧ ¬b)
```
