handmakertwo<-function(holecards,communitycards,betround=1){
  #Input
  #A 2 by 2 matrix, 'holecards', representing the user's 2 hole cards in matrix format as
  #defined in the 'cards' R function.
  #A 2 by 3/4/5 matrix, 'communitycards', representing the community cards known to the user.
  #'Betround', an integer value from 1 to 3, inclusive, which provides informaiton on the round (i.e, 
  #1=flop, 2=turn, and 3=river).
  #Output
  #An integer value between 1 and 10, inclusive, giving the hand type of the best hand from all hands made possible through the
  #combination of hole cards (two) and community cards (between 3 and 5), as defined in the 'hand' R function.
  
  
  thandmakertwo=0; #testhandmakertwo, to help in turn and river where multiple hands can be made from the community cards
  
  if (betround==1){
    usercards=cbind(holecards,communitycards); #only one possible 5 card combination
    handmakertwo=hand(usercards); #the ouput is the rank of the hand
  }
  
  if (betround==2){
    #a total of six hands need to be evaluted, only the highest rank gets saved
    
    #1
    usercards=cbind(holecards,communitycards[,1:3]); #the flop cards
    handmakertwo=hand(usercards); #the ouput is the rank of the hand
    if (handmakertwo>thandmakertwo){
      thandmakertwo=handmakertwo; #saves the highest ranking hand
    }
    #2
    usercards=cbind(holecards,communitycards[,2:4]); #last two flop card and the turn card
    handmakertwo=hand(usercards); #the ouput is the rank of the hand
    if (handmakertwo>thandmakertwo){
      thandmakertwo=handmakertwo; #saves the highest ranking hand
    }
    #3
    usercards=cbind(holecards,communitycards[,c(1,3,4)]); #first and third flop card and the turn card
    handmakertwo=hand(usercards); #the ouput is the rank of the hand
    if (handmakertwo>thandmakertwo){
      thandmakertwo=handmakertwo; #saves the highest ranking hand
    }
    #4
    usercards=cbind(holecards,communitycards[,c(1,2,4)]); #first two flop cards and the turn card
    handmakertwo=hand(usercards); #the ouput is the rank of the hand
    if (handmakertwo>thandmakertwo){
      thandmakertwo=handmakertwo; #saves the highest ranking hand
    }
    #5
    usercards=cbind(holecards[,1],communitycards); #first whole card all community cards
    handmakertwo=hand(usercards); #the ouput is the rank of the hand
    if (handmakertwo>thandmakertwo){
      thandmakertwo=handmakertwo; #saves the highest ranking hand
    }
    #6
    usercards=cbind(holecards[,2],communitycards); #second whole card all community cards
    handmakertwo=hand(usercards); #the ouput is the rank of the hand
    if (handmakertwo>thandmakertwo){
      thandmakertwo=handmakertwo; #saves the highest ranking hand
    }
    handmakertwo=thandmakertwo; #only the highest score is returned
  }
  
  
  if (betround==3){
    #a total of six hands need to be evaluted, only the highest rank gets saved
    
    #1
    usercards=cbind(holecards,communitycards[,1:3]); #the flop cards
    handmakertwo=hand(usercards); #the ouput is the rank of the hand
    if (handmakertwo>thandmakertwo){
      thandmakertwo=handmakertwo; #saves the highest ranking hand
    }
    #2
    usercards=cbind(holecards,communitycards[,2:4]); #last two flop card and the turn card
    handmakertwo=hand(usercards); #the ouput is the rank of the hand
    if (handmakertwo>thandmakertwo){
      thandmakertwo=handmakertwo; #saves the highest ranking hand
    }
    #3
    usercards=cbind(holecards,communitycards[,c(1,3,4)]); #first and third flop card and the turn card
    handmakertwo=hand(usercards); #the ouput is the rank of the hand
    if (handmakertwo>thandmakertwo){
      thandmakertwo=handmakertwo; #saves the highest ranking hand
    }
    #4
    usercards=cbind(holecards,communitycards[,c(1,2,4)]); #first two flop cards and the turn card
    handmakertwo=hand(usercards); #the ouput is the rank of the hand
    if (handmakertwo>thandmakertwo){
      thandmakertwo=handmakertwo; #saves the highest ranking hand
    }
    #5
    usercards=cbind(holecards[,1],communitycards[,c(1:4)]); #first whole card all first four community cards
    handmakertwo=hand(usercards); #the ouput is the rank of the hand
    if (handmakertwo>thandmakertwo){
      thandmakertwo=handmakertwo; #saves the highest ranking hand
    }
    #6
    usercards=cbind(holecards[,2],communitycards[,c(1:4)]); #second whole card all community cards
    handmakertwo=hand(usercards); #the ouput is the rank of the hand
    if (handmakertwo>thandmakertwo){
      thandmakertwo=handmakertwo; #saves the highest ranking hand
    }
    
    #continue with additional river cards
    #7
    usercards=cbind(communitycards); #no whole card all community cards (cc)
    handmakertwo=hand(usercards); #the ouput is the rank of the hand
    if (handmakertwo>thandmakertwo){
      thandmakertwo=handmakertwo; #saves the highest ranking hand
    }
    #8
    usercards=cbind(holecards[,1],communitycards[,c(2:5)]); # first whole card and 2nd to 5th cc
    handmakertwo=hand(usercards); #the ouput is the rank of the hand
    if (handmakertwo>thandmakertwo){
      thandmakertwo=handmakertwo; #saves the highest ranking hand
    }
    #9
    usercards=cbind(holecards[,2],communitycards[,c(2:5)]); # second whole card and 2nd to 5th cc
    handmakertwo=hand(usercards); #the ouput is the rank of the hand
    if (handmakertwo>thandmakertwo){
      thandmakertwo=handmakertwo; #saves the highest ranking hand
    }
    #10
    usercards=cbind(holecards[,1],communitycards[,c(1,3,4,5)]); # first whole card and 1st,3rd,4th,5th cc
    handmakertwo=hand(usercards); #the ouput is the rank of the hand
    if (handmakertwo>thandmakertwo){
      thandmakertwo=handmakertwo; #saves the highest ranking hand
    }
    #11
    usercards=cbind(holecards[,2],communitycards[,c(1,3,4,5)]); # second whole card and 1st,3rd,4th,5th cc
    handmakertwo=hand(usercards); #the ouput is the rank of the hand
    if (handmakertwo>thandmakertwo){
      thandmakertwo=handmakertwo; #saves the highest ranking hand
    }
    #12
    usercards=cbind(holecards[,1],communitycards[,c(1,2,3,5)]); # first whole card and 1st,2nd,3rd,5th cc
    handmakertwo=hand(usercards); #the ouput is the rank of the hand
    if (handmakertwo>thandmakertwo){
      thandmakertwo=handmakertwo; #saves the highest ranking hand
    }
    #13
    usercards=cbind(holecards[,2],communitycards[,c(1,2,3,5)]); # second whole card and 1st,2nd,3rd,5th cc
    handmakertwo=hand(usercards); #the ouput is the rank of the hand
    if (handmakertwo>thandmakertwo){
      thandmakertwo=handmakertwo; #saves the highest ranking hand
    }
    #14
    usercards=cbind(holecards[,1],communitycards[,c(1,2,4,5)]); # first whole card and 1st,2nd,4th,5th cc
    handmakertwo=hand(usercards); #the ouput is the rank of the hand
    if (handmakertwo>thandmakertwo){
      thandmakertwo=handmakertwo; #saves the highest ranking hand
    }
    #15
    usercards=cbind(holecards[,2],communitycards[,c(1,2,4,5)]); # second whole card and 1st,2nd,4th,5th cc
    handmakertwo=hand(usercards); #the ouput is the rank of the hand
    if (handmakertwo>thandmakertwo){
      thandmakertwo=handmakertwo; #saves the highest ranking hand
    }
    #16
    usercards=cbind(holecards,communitycards[,c(1,2,5)]); # both whole cards and 1st,2nd, and 5th cc
    handmakertwo=hand(usercards); #the ouput is the rank of the hand
    if (handmakertwo>thandmakertwo){
      thandmakertwo=handmakertwo; #saves the highest ranking hand
    }
    #17
    usercards=cbind(holecards,communitycards[,c(2,3,5)]); # both whole cards and 3rd,4th, and 5th cc
    handmakertwo=hand(usercards); #the ouput is the rank of the hand
    if (handmakertwo>thandmakertwo){
      thandmakertwo=handmakertwo; #saves the highest ranking hand
    }
    #18
    usercards=cbind(holecards,communitycards[,c(3,4,5)]); # both whole cards and 3rd,4th, and 5th cc
    handmakertwo=hand(usercards); #the ouput is the rank of the hand
    if (handmakertwo>thandmakertwo){
      thandmakertwo=handmakertwo; #saves the highest ranking hand
    }
    #19
    usercards=cbind(holecards,communitycards[,c(1,3,5)]); # both whole cards and 3rd,4th, and 5th cc
    handmakertwo=hand(usercards); #the ouput is the rank of the hand
    if (handmakertwo>thandmakertwo){
      thandmakertwo=handmakertwo; #saves the highest ranking hand
    }
    #20
    usercards=cbind(holecards,communitycards[,c(1,4,5)]); # both whole cards and 3rd,4th, and 5th cc
    handmakertwo=hand(usercards); #the ouput is the rank of the hand
    if (handmakertwo>thandmakertwo){
      thandmakertwo=handmakertwo; #saves the highest ranking hand
    }
    #21
    usercards=cbind(holecards,communitycards[,c(2,4,5)]); # both whole cards and 3rd,4th, and 5th cc
    handmakertwo=hand(usercards); #the ouput is the rank of the hand
    if (handmakertwo>thandmakertwo){
      thandmakertwo=handmakertwo; #saves the highest ranking hand
    }
    handmakertwo=thandmakertwo; #only the highest score is returned
  }
  return(handmakertwo);
}