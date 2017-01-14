handrank<-function(usercards,hand){
  #ranks different hands withint a specific hand type (i.e. to determine if
  #a card high is Ace high or ten high, and rank the former better than
  #later given the specific cards that make up the hand)
  
  handrank=0;
  #Ace is represented in as a 1 via the cards function, but it needs to be ranked higher than a king (13)
  #hence, we will change any ace in the hand from 1 to a 14, so that the hands get ranked accordingly
  for (j in 1:5){
    if (usercards[1,j]==1){
      usercards[1,j]=14; #so that an Ace ranks higher than a King
    }
  }
  
  #we arrange the cards in increasing order, with Ace=14 and King=13
  usercards=usercards[,order(usercards[1,1:5])];
  cardtype=usercards[1,];

  #Card High Hand
  if (hand==1){
    #just add the values of the hand, so a A,K,Q,J,9=14+13+12+11+9=59 
      handrank=sum(cardtype);
    }

  
  #Pair
  if (hand==2){
    for (i in 1:4){
      #to determine which pair it is
      if (usercards[1,i+1]==usercards[1,i]) break
      
    }
      handrank=usercards[1,i]*100+sum(cardtype)-2*usercards[1,i];
      #an example would be 3,3,4,5,6, which would be scored as
      #300+(3+3+4+5+6)-6=315
    }     

#two pair
  if(hand==3){
    n=0;
    for (i in 1:4){
      #to determine the first pair
      if (usercards[1,i+1]==usercards[1,i]){
        n=n+1
        if (n==1){
          fp=i; #firstpair
        }else if (n==2){
          sp=i; #secondpair
        }
      }
    }
    handrank=max(cardtype[fp],cardtype[sp])*1000+sum(cardtype)-2*max(cardtype[fp],cardtype[sp]);
    #hightest score of a two pair would be A,A,K,K,Q = 14,000+(13+13+12)=14,038
  }

#set
  if(hand==4){
    for (i in 1:4){
      #to determine which pair it is
      if (usercards[1,i+1]==usercards[1,i]) break
      
    }
    handrank=usercards[1,i]*10000+sum(usercards[1,])-3*usercards[1,i];
    #an example would be 2,2,2,5,6, which would be scored as
    #2000+(2+2+2+5+6)-6=20,011, higher than the highest ranking two pair (above)
  }     
 
  #straight
  if(hand==5 | hand==9){
    #need to determine if the Ace in the hand is for A,2,3,4,5 or for 10,J,Q,K,A
    if(cardtype[1]==2){
      handrank=5*10^hand;
    }else{
      handrank=cardtype[5]*10^hand;
      #In a straight there is no quicker, so no need to account the other cards in the hand
    }
  }
  
  #flush
  if(hand==6){
    handrank=cardtype[5]*10^6;
    #the strenght of the flush is fullly determined by the highest ranking card in the hand
    #which happens to be in the 5th position
  }
  
  #fullhouse
  if (hand==7){
    if(cardtype[1]==cardtype[3]){
      handrank=cardtype[1]*10^7+sum(cardtype)-3*cardtype[1];
    }else{
      handrank=cardtype[3]*10^7+sum(cardtype)-3*cardtype[3];
    }
  }
  
 #four of a kind
  if (hand==8){
    if(cardtype[1]==cardtype[4]){
      #such that Q,Q,Q,Q,K
      handrank=cardtype[1]*10^8+cardtype[5];
    }else{
      #such that 10,10,10,10,Q
      handrank=cardtype[4]*10^8+cardtype[1];
    }
  }
  
  #royal flush
  if (hand==10){
    handrank=10^hand;
  }
    
  return(handrank);
} 