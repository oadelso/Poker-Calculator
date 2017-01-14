hand<-function(cards){
  #receives a set of five cards and gives the type of hand that it is
  cards=cards[,order(cards[1,])];
  #arranges the cards in increasing order by hand type, not by suite
  #recall that  is the second row of the matrix
  cardtype=cards[1,];
  cardsuit=cards[2,];
  delta=c(5,5,5,5);
  deltasuit=c(5,5,5,5);
  hand=0;

  for (i in 1:4){
    delta[i]=cardtype[i+1]-cardtype[i];
    deltasuit[i]=cardsuit[i+1]-cardsuit[i];
  }
    delta=sort(delta);
    deltasuit=sort(deltasuit);
  
    if (isTRUE(all.equal(deltasuit,rep(0,4)))){
      hand=6; #flush
    }
  
  #straight and royal flush alternative 1    
    if (isTRUE(all.equal(delta,rep(1,4)))){
    hand=5; #straight
    if (isTRUE(all.equal(deltasuit,rep(0,4)))){
      hand=9; #straight flush
      if (cardtype[5]==13 & cardtype[1]==1){
        hand=10; #royal flush
      }
    }
  }
  
    #straight and royal flush alternative 2    
    if (cardtype[2]==10 & isTRUE(all.equal(delta,c(1,1,1,9)))){
      #hand is a straight, with the second condition being when it is
      #10,J (11),Q (12), K (13) and an ace (1)
      hand=5; #straight
      if (isTRUE(all.equal(deltasuit,rep(0,4)))){
        hand=9; #straight flush
        if (cardtype[5]==13 & cardtype[1]==1){
          hand=10; #royal flush
        }
      }
    }  
    
      
  if (delta[1]==0){
    hand=2; #pair
  }  
  
  if (isTRUE(all.equal(delta[1:2],c(0,0)))){
    hand=3; #two pair
    if(cardtype[1]==cardtype[3] | cardtype[2]==cardtype[4]|cardtype[3]==cardtype[5]){
      hand=4; #set
    }
  }
  
  if (isTRUE(all.equal(delta[1:3],c(0,0,0)))){
    hand=7; #full house
    if(cardtype[1]==cardtype[4] | cardtype[2]==cardtype[5]){
      hand=8; #four of a kind
    }
  }
  
  if (hand==0){
    hand=1;
  }
    hand
}