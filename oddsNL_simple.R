oddsNL_simple<-function(usercards,betround=1,players=1,sims=100){
 #this function looks at the hole cards (i.e. cards), and the number of players (excluding the user)
#provides the probability of winning 
#it calculates this by doing sims number of simulations

    oddsNL_simple=matrix(0,1,2); #to create the 1 by 2 vector that saves the results
         
           #first is to convert the cards in the 'card matrix' into a number between 1 and 52
           card1=usercards[1,1]+(usercards[2,1]-1)*13; #in our simulation, each suit has 13 cards
           card2=usercards[1,2]+(usercards[2,2]-1)*13;
           holecards=c(card1,card2);
           
             wins=0;
             ties=0;
             userrank=0; #the rank of the user
             opponentrank=rep(0,players); #the vector saving the rank of the opponents' hands
             
               #need to take out cards for other players and communal cards (flop, turn, and river)
               totalcards=players*2+(2+betround); #two hole cards for each player and # community cards depending on betting round
               scc=totalcards-(1+betround); #start index for the columns, should be 3 for the flop (i.e., round=1)
               for (j in 1:sims){
                     #it will determine what number out of sims does the users hole cards win by the specified round
                       othercards=cards(totalcards,holecards);#new set of cards per simulation, excluding the hole cards of user
                       communitycards=othercards[,scc:totalcards]; #this will be the shared cards  
                       userrank=handmaker(usercards,communitycards,betround); #rank of user's hand
                       for (i in 1:players){
                             ohs=(i-1)*2+1; #opponent hand start index for column
                             ohe=(i-1)*2+2; #opponent hand end index for column
                             opponentrank[i]=handmaker(othercards[,ohs:ohe],communitycards,betround); #saves the rank of each opponent's hand
                         }
                       topopponent=max(opponentrank); #gets the top ranking opponent
                       if (userrank>topopponent){
                             wins=wins+1; #records the number of times that the user wins
                         }else if (userrank==topopponent){
                               ties=ties+1; #keep track of ties too
                           }
                   }
               
                 oddsNL_simple[1]=wins/sims;
                 oddsNL_simple[2]=ties/sims;
                 return(oddsNL_simple);
             } 