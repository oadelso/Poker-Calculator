matrixtonumber<-function(cardmatrix){
  #converts a card matrix, which contains suite and card type in a 2 by n (n being the number of cards) matrix, into a 
  #number between 1 and 52
  sizecm=dim(cardmatrix); #gets dimensions of matrix
  ncards=sizecm[2]; #number of cards to convert
  matrixtonumber=rep(0,ncards);
  
  for (i in 1:ncards){
    matrixtonumber[i]=cardmatrix[1,i]+(cardmatrix[2,i]-1)*13;
  }
  matrixtonumber
}