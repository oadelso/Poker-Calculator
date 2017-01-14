numbertomatrix<-function(cardnumbers){
  #turns a number from 1 to 52 to a 2 by 1 vector, with the first row giving the type of card
  #1 for Ace, 13 for king, and the second row giving the type of suite, 1=spades, 2=trebol,
  #3=diamonds,4=hearts. These have been chosen arbitrarly.
  numberofcards=length(cardnumbers); 
  numbertomatrix=matrix(0,2,numberofcards);
  for (i in 1:numberofcards){
    if (cardnumbers[i]<=13) 
    {
      numbertomatrix[1,i]=cardnumbers[i];
      numbertomatrix[2,i]=1;}
    else if (cardnumbers[i]<=26)
    {
      #The ace will be represented by a 14
      numbertomatrix[1,i]=cardnumbers[i]-13;
      numbertomatrix[2,i]=2;}
    else if (cardnumbers[i]<=39)
    {
      numbertomatrix[1,i]=cardnumbers[i]-26;
      numbertomatrix[2,i]=3;}
    else{
      numbertomatrix[1,i]=cardnumbers[i]-39;
      numbertomatrix[2,i]=4;}
  }
  numbertomatrix
}