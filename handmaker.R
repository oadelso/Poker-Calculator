handmaker<-function(wholecards,communitycards,betround=1){
  #provides the rank of the best hand of all hands made possible through the
  #combination of whole cards and community cards
  
  thandmaker=0; #testhandmaker, to help in turn and river where multiple hands can be made from the community cards
  
  if (betround==1){
    usercards=cbind(wholecards,communitycards); #only one possible 5 card combination
    handmaker=handrank(usercards,hand(usercards)); #the ouput is the rank of the hand
  }
  
  if (betround==2){
    #a total of six hands need to be evaluted, only the highest rank gets saved
    
    #1
    usercards=cbind(wholecards,communitycards[,1:3]); #the flop cards
    handmaker=handrank(usercards,hand(usercards)); #the ouput is the rank of the hand
    if (handmaker>thandmaker){
      thandmaker=handmaker; #saves the highest ranking hand
    }
    #2
    usercards=cbind(wholecards,communitycards[,2:4]); #last two flop card and the turn card
    handmaker=handrank(usercards,hand(usercards)); #the ouput is the rank of the hand
    if (handmaker>thandmaker){
      thandmaker=handmaker; #saves the highest ranking hand
    }
    #3
    usercards=cbind(wholecards,communitycards[,c(1,3,4)]); #first and third flop card and the turn card
    handmaker=handrank(usercards,hand(usercards)); #the ouput is the rank of the hand
    if (handmaker>thandmaker){
      thandmaker=handmaker; #saves the highest ranking hand
    }
    #4
    usercards=cbind(wholecards,communitycards[,c(1,2,4)]); #first two flop cards and the turn card
    handmaker=handrank(usercards,hand(usercards)); #the ouput is the rank of the hand
    if (handmaker>thandmaker){
      thandmaker=handmaker; #saves the highest ranking hand
    }
    #5
    usercards=cbind(wholecards[,1],communitycards); #first whole card all community cards
    handmaker=handrank(usercards,hand(usercards)); #the ouput is the rank of the hand
    if (handmaker>thandmaker){
      thandmaker=handmaker; #saves the highest ranking hand
    }
    #6
    usercards=cbind(wholecards[,2],communitycards); #second whole card all community cards
    handmaker=handrank(usercards,hand(usercards)); #the ouput is the rank of the hand
    if (handmaker>thandmaker){
      thandmaker=handmaker; #saves the highest ranking hand
    }
    handmaker=thandmaker; #only the highest score is returned
  }
 
  
  if (betround==3){
    #a total of six hands need to be evaluted, only the highest rank gets saved
    
    #1
    usercards=cbind(wholecards,communitycards[,1:3]); #the flop cards
    handmaker=handrank(usercards,hand(usercards)); #the ouput is the rank of the hand
    if (handmaker>thandmaker){
      thandmaker=handmaker; #saves the highest ranking hand
    }
    #2
    usercards=cbind(wholecards,communitycards[,2:4]); #last two flop card and the turn card
    handmaker=handrank(usercards,hand(usercards)); #the ouput is the rank of the hand
    if (handmaker>thandmaker){
      thandmaker=handmaker; #saves the highest ranking hand
    }
    #3
    usercards=cbind(wholecards,communitycards[,c(1,3,4)]); #first and third flop card and the turn card
    handmaker=handrank(usercards,hand(usercards)); #the ouput is the rank of the hand
    if (handmaker>thandmaker){
      thandmaker=handmaker; #saves the highest ranking hand
    }
    #4
    usercards=cbind(wholecards,communitycards[,c(1,2,4)]); #first two flop cards and the turn card
    handmaker=handrank(usercards,hand(usercards)); #the ouput is the rank of the hand
    if (handmaker>thandmaker){
      thandmaker=handmaker; #saves the highest ranking hand
    }
    #5
    usercards=cbind(wholecards[,1],communitycards[,c(1:4)]); #first whole card all first four community cards
    handmaker=handrank(usercards,hand(usercards)); #the ouput is the rank of the hand
    if (handmaker>thandmaker){
      thandmaker=handmaker; #saves the highest ranking hand
    }
    #6
    usercards=cbind(wholecards[,2],communitycards[,c(1:4)]); #second whole card all community cards
    handmaker=handrank(usercards,hand(usercards)); #the ouput is the rank of the hand
    if (handmaker>thandmaker){
      thandmaker=handmaker; #saves the highest ranking hand
    }
    
    #continue with additional river cards
    #7
    usercards=cbind(communitycards); #no whole card all community cards (cc)
    handmaker=handrank(usercards,hand(usercards)); #the ouput is the rank of the hand
    if (handmaker>thandmaker){
      thandmaker=handmaker; #saves the highest ranking hand
    }
    #8
    usercards=cbind(wholecards[,1],communitycards[,c(2:5)]); # first whole card and 2nd to 5th cc
    handmaker=handrank(usercards,hand(usercards)); #the ouput is the rank of the hand
    if (handmaker>thandmaker){
      thandmaker=handmaker; #saves the highest ranking hand
    }
    #9
    usercards=cbind(wholecards[,2],communitycards[,c(2:5)]); # second whole card and 2nd to 5th cc
    handmaker=handrank(usercards,hand(usercards)); #the ouput is the rank of the hand
    if (handmaker>thandmaker){
      thandmaker=handmaker; #saves the highest ranking hand
    }
    #10
    usercards=cbind(wholecards[,1],communitycards[,c(1,3,4,5)]); # first whole card and 1st,3rd,4th,5th cc
    handmaker=handrank(usercards,hand(usercards)); #the ouput is the rank of the hand
    if (handmaker>thandmaker){
      thandmaker=handmaker; #saves the highest ranking hand
    }
    #11
    usercards=cbind(wholecards[,2],communitycards[,c(1,3,4,5)]); # second whole card and 1st,3rd,4th,5th cc
    handmaker=handrank(usercards,hand(usercards)); #the ouput is the rank of the hand
    if (handmaker>thandmaker){
      thandmaker=handmaker; #saves the highest ranking hand
    }
    #12
    usercards=cbind(wholecards[,1],communitycards[,c(1,2,3,5)]); # first whole card and 1st,2nd,3rd,5th cc
    handmaker=handrank(usercards,hand(usercards)); #the ouput is the rank of the hand
    if (handmaker>thandmaker){
      thandmaker=handmaker; #saves the highest ranking hand
    }
    #13
    usercards=cbind(wholecards[,2],communitycards[,c(1,2,3,5)]); # second whole card and 1st,2nd,3rd,5th cc
    handmaker=handrank(usercards,hand(usercards)); #the ouput is the rank of the hand
    if (handmaker>thandmaker){
      thandmaker=handmaker; #saves the highest ranking hand
    }
    #14
    usercards=cbind(wholecards[,1],communitycards[,c(1,2,4,5)]); # first whole card and 1st,2nd,4th,5th cc
    handmaker=handrank(usercards,hand(usercards)); #the ouput is the rank of the hand
    if (handmaker>thandmaker){
      thandmaker=handmaker; #saves the highest ranking hand
    }
    #15
    usercards=cbind(wholecards[,2],communitycards[,c(1,2,4,5)]); # second whole card and 1st,2nd,4th,5th cc
    handmaker=handrank(usercards,hand(usercards)); #the ouput is the rank of the hand
    if (handmaker>thandmaker){
      thandmaker=handmaker; #saves the highest ranking hand
    }
    #16
    usercards=cbind(wholecards,communitycards[,c(1,2,5)]); # both whole cards and 1st,2nd, and 5th cc
    handmaker=handrank(usercards,hand(usercards)); #the ouput is the rank of the hand
    if (handmaker>thandmaker){
      thandmaker=handmaker; #saves the highest ranking hand
    }
    #17
    usercards=cbind(wholecards,communitycards[,c(2,3,5)]); # both whole cards and 3rd,4th, and 5th cc
    handmaker=handrank(usercards,hand(usercards)); #the ouput is the rank of the hand
    if (handmaker>thandmaker){
      thandmaker=handmaker; #saves the highest ranking hand
    }
    #18
    usercards=cbind(wholecards,communitycards[,c(3,4,5)]); # both whole cards and 3rd,4th, and 5th cc
    handmaker=handrank(usercards,hand(usercards)); #the ouput is the rank of the hand
    if (handmaker>thandmaker){
      thandmaker=handmaker; #saves the highest ranking hand
    }
    #19
    usercards=cbind(wholecards,communitycards[,c(1,3,5)]); # both whole cards and 3rd,4th, and 5th cc
    handmaker=handrank(usercards,hand(usercards)); #the ouput is the rank of the hand
    if (handmaker>thandmaker){
      thandmaker=handmaker; #saves the highest ranking hand
    }
    #20
    usercards=cbind(wholecards,communitycards[,c(1,4,5)]); # both whole cards and 3rd,4th, and 5th cc
    handmaker=handrank(usercards,hand(usercards)); #the ouput is the rank of the hand
    if (handmaker>thandmaker){
      thandmaker=handmaker; #saves the highest ranking hand
    }
    #21
    usercards=cbind(wholecards,communitycards[,c(2,4,5)]); # both whole cards and 3rd,4th, and 5th cc
    handmaker=handrank(usercards,hand(usercards)); #the ouput is the rank of the hand
    if (handmaker>thandmaker){
      thandmaker=handmaker; #saves the highest ranking hand
    }
    handmaker=thandmaker; #only the highest score is returned
  }
    return(handmaker);
}