# MTGSims
MTG Simulations

The goal of this repository is to provide tools for and examples of simulation-based analysis of MTG deck construction. I will be building several 'simulators'of MTG matches with differing circumstances. I will perform statistical analysis on the results of these simulations. For simplicity, I am building each simulation with the following axioms:

1. Deck size is 60
2. Creatures' power and toughness are equal to their mana cost
3. All creatures have haste
4. All lands/creatures are the same color, and have only one color
5. All lands are basic
6. You always go first

Simulators:

deckbuilder.rb
Opponent cannot cast spells. Selects a number of lands randomly between 1 and 59 (The game cannot be won with 0 or 60 lands). Fills the remainder of the deck with creatures of random cost (from 1 to 54, since it's impossible to play 55 lands). Loss comes from drawing out deck without having killed your opponent (would require >48 lands).

deckbuilder-4.rb
Opponent cannot cast spells. Game limited to 4 turns. Selects a number of lands randomly between 1 and 56 (The game cannot be won with 0 or >56 lands). Fills the remainder of the deck with creatures of random cost (from 1 to 4).
