cards<-function(number=5,removecards=0){
  #gives a two by number matrix which represents a set of random cards
  #the first column gives the type of the card, and the second row the 
  #suite
  
  cards=matrix(0,2,number);
  #the shuffle should remove cards that have already been shown
  deck=1:52;
  deck=setdiff(deck,removecards);
  shuffle=sample(deck,length(deck));
  for (i in 1:number)
  {
    if (shuffle[i]<=13) 
    {
      cards[1,i]=shuffle[i];
      cards[2,i]=1;}
    else if (shuffle[i]<=26)
    {
      #The ace will be represented by a 14
      cards[1,i]=shuffle[i]-13;
      cards[2,i]=2;}
    else if (shuffle[i]<=39)
    {
      cards[1,i]=shuffle[i]-26;
      cards[2,i]=3;}
    else{
      cards[1,i]=shuffle[i]-39;
      cards[2,i]=4;}
  }
  return(cards);
}