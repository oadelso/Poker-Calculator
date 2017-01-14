oddsNL<-function(usercards,oppcards=0,tablecards=0,players=1,sims=10,betround=1){
  #this function looks at the whole cards (i.e. usercards), and the number of players (excluding the user)
  #provides the probability of winning 
  #it calculates this by doing sims number of simulations
  
  #determine if opponent cards and/or table cards are known
  oppdim=dim(oppcards); 
  tabledim=dim(tablecards);
  
  nooppcards=is.null(oppdim); #no opponent cards are known
  notablecards=is.null(tabledim); #no table cards are known
  
  #in order to avoid the NULL output of the two statements
  if (nooppcards){
    oppdim=rep(0,2);
  }
  if (notablecards){
    tabledim=rep(0,2);
  }
  
  knowncards=cbind(usercards,oppcards,tablecards); #matrix containing all known cards
  
  oddsNL=matrix(0,1,2); #to create the 1 by 2 vector that saves the results
  
  
  #first is to convert the knowncard matrix into numbers between 1 and 52
  knowncardsvals=matrixtonumber(knowncards);
  wholecards=knowncards[,1:2];
  
  wins=0;
  ties=0;
  userrank=0; #the rank of the user
  opponentrank=rep(0,players); #the vector saving the rank of the opponents' hands
  
  #need to take out cards for other players and communal cards (flop, turn, and river)
  totalcards=players*2+(betround+2)-tabledim[2]-oppdim[2]; #two whole cards for each player in addition
                                                           #to any community card needed for the betting round
                                                           #minus known cards for both players and cards on the table
                                                           #example, 1 additional player (no known cards for him/her), simulating the river (roundbet=3)
                                                           #knowing the flop, requires 2+(5)-3-0=4; 2 for the player
                                                           #two cards for the two subsequent betting rounds
  
  scc=(totalcards-((betround+2)-tabledim[2]))+1; #start index for the community card columns
                                                #for simulation of the river, we need a total of 5 community cards
                                                #if the flop cards are kown (tabledim[2]==3), then we only need 4 cards
                                                #these two cards will be stored at the end of the simulated card matrix
                                                #so in the case where there are 4 cards to be simulated (as the example above)
                                                #the simulated community cards will be in columns 3 to 4. 
  
  for (j in 1:sims){
    #it will determine what number out of sims does the users whole cards win by the specified round
    
    if (totalcards>0){
      #simulations are only needed when we do not know some of the cards needed for the round
      simulatedcards=cards(totalcards,knowncards);#new set of cards per simulation, excluding known cards
    }
    
    
    if (tabledim[2]==(betround+2)){
      #no need to simulate community cards
      communitycards=tablecards;
    } else if (tabledim[2]==0){
      #no known community cards, so all needed coummunity cars for the round need to be simulated
      communitycards=simulatedcards[,scc:totalcards];
    } 
    else{
      #known table cards plus the simulated ones
      communitycards=cbind(tablecards,simulatedcards[,scc:totalcards]); 
    }
    
    
    userrank=handmaker(usercards,communitycards,betround); #rank of user's hand
      j=0; #to help track the number of players for which the whole cards are not known
      l=0; #keep track of players whose cards are known
      for (i in 1:players){
        if (oppdim[2]>=i*2){
          #we know cards for the first lth opponents
          l=l+1;
          ohs=(l-1)*2+1;
          ohe=(l-1)*2+2;
          opponentrank[i]=handmaker(oppcards[,ohs:ohe],communitycards,betround); #saves the rank of each opponent's hand
          
        }else{
          j=j+1; #additional opponents need to have their cards simulated
          ohs=(j-1)*2+1; #opponent hand start index for column
          ohe=(j-1)*2+2; #opponent hand end index for column
          opponentrank[i]=handmaker(simulatedcards[,ohs:ohe],communitycards,betround); #saves the rank of each opponent's hand
        }
      }
    topopponent=max(opponentrank); #gets the top ranking opponent
    if (userrank>topopponent){
      wins=wins+1; #records the number of times that the user wins
    }else if (userrank==topopponent){
      ties=ties+1; #keep track of ties too
    }
  }
  
  oddsNL[1]=wins/sims;
  oddsNL[2]=ties/sims;
  return(oddsNL);
} 
  