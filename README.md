# Poker-Project
## Introduction
This repository contains a total of nine R functions which are needed to run a poker calculator that approximates the probability of ending up with the best 5-card poker hand in any betting round of Texas Hold'em. 
## R Functions
### 1. Cards
It models a deck of cards via a vector containing the integers from 1 to 52. Each of these integers represents a specific card in a standard deck. The R function returns a predefined number of cards by first shuffling the values of the vector and returning a 2-row matrix. The first row is an integer from 1 to 13, with 1 representing an Ace, 2 a 2, all the way to 13 representing a king. The second row is an integer from 1 to 4, which provides information on the type of suit. For the purposes of this project, 1 represents a spade, 2 represents a club, 3 represents a diamond, and 4 hearts.
### 2 Matrixtonumber
This R function converts the output of the *cards* R function into an integer from 1 to 52.
### 3.Numbertomatrix
This R function converts an integer from 1 to 52 to a matrix with the same format as the *cards* R function's output.
### 4. Hand
It receives a 2 by 5 matrix from the *cards* R function and returns an integer from 1 to 10. Each integer represents a specific, 5-card poker hand. One represents a 'card high' hand, Two a 'pair', all the way to ten representing a 'Royal Flush.'
### 5. Handrank
It requires a 2 by 5 matrix in the format of the *cards* R function, and the output of the *Hand* function from the said matrix. It then provides a numerical score which is used by the poker calculator to deferentiate between different variations within a specific hand. For example, it assigns a higher value to a pair of 3s than to a pair of 2s. Additionally, it would assign a higher value to the hand 3-3-J-10-4 than 3-3-J-10-2, due to the former's kicker. 
### 6. Handmaker
It requires input on the user's hole cards and any available community cards in the format of the *cards* function, and returns the score through the *handrank* R function of the best possible 5-card poker hand. For example, by the river when there are 5 community cards, it will go through all 21 combinations ways in which the seven cards (2 hole cards plus the 5 community cards) can make a 5-card poker hand and return the highest score given by the *handrank* R function.
### 7. Handmakertwo
Similiar to *Handmaker* but it only returns the highest poker hand, not the specific score. For example, it will indicate if the highest 5-card poker hand was a pair, or a set, but not a specific score outlining what type of pair or set it it.
### 8. oddsNL_simple
This is the simple poker calculator, which uses all the above functions to approximate the probability of ending up with the best 5-card poker hand by a specific betting round. It requires the user's hole cards in the format given by the *cards* function, as well the number opponents, the total number of simulations that need to be carried out in order to arrive at the approximated figure, and the round for which the proability is required (i.e., probability of having the best hand by the flop, turn, or river).
### 9. oddsNL
Similiar to *oddsNL_simple* but it also allows the user to specify any known hole cards from the opponents, as well as any known community cards.
