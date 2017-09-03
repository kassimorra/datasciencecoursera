rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  options(warn=-1)
  
  
  
  desease <- c("heart attack", "heart failure", "pneumonia")
  ## Read outcome data
  base <- read.csv("outcome-of-care-measures.csv", colClasses = "character")  
  
  ##Get Heart Atack Data
  mha <-  data.frame(base[,2], base[,7], base[,11])
  colnames(mha) <- c( "hospital" , "state", "deaths")
  mha$deaths <- as.numeric(as.character(mha$deaths))
  mha <- mha[complete.cases(mha),]
  mha <- mha[order(mha$deaths, mha$hospital),]#sort de data frame for lowest deaths
  mha <- mha[mha$state == state,]
  
  
  ##Get Heart Failure Data
  mhf <-  data.frame(base[,2], base[,7], base[,17])
  colnames(mhf) <- c( "hospital", "state", "deaths")
  mhf$deaths <- as.numeric(as.character(mhf$deaths))
  mhf <- mhf[complete.cases(mhf),]
  mhf <- mhf[order(mhf$deaths, mhf$hospital),]#sort de data frame for lowest deaths
  mhf <- mhf[mhf$state == state,]
  
  ##Get Pneumonia Data
  mpn <-  data.frame(base[,2], base[,7], base[,23])
  colnames(mpn) <- c("hospital", "state", "deaths")
  mpn$deaths <- as.numeric(as.character(mpn$deaths))
  mpn <- mpn[complete.cases(mpn),]
  mpn <- mpn[order(mpn$deaths, mpn$hospital),]#sort de data frame for lowest deaths
  mpn <- mpn[mpn$state == state,]
  
  
  ##Modify num if best is 1 or worst is the last
  if ( num == "Best" ) num <-  1
  else if ( num =="worst") {
    if ( outcome == "heart attack" ) num = nrow(mha) 
    else if ( outcome == "heart failure" ) num = nrow(mhf) 
    else if ( outcome == "pneumonia" ) num = nrow(mpn) 
  }
  
  
  ## Check that state and outcome are valid
  if (! (state %in% base$State) ) {
    stop (paste("Error in best(", state, ", ", outcome, ") : invalid state"), call. = F)
  }
  if (! max(outcome %in% desease) ) {
    stop (paste("Error in best(", state, ", ", outcome, ") : invalid outcome"), call. = F)
  }
  
  
  
  ##identify what type desease is and return the best hospital
  if  (outcome == "heart attack"){
    print(mha[num,1], max.levels = 0)
  }
  else if (outcome == "heart failure"){
    print(mhf[num,1], max.levels = 0)
    
  }
  else if (outcome == "pneumonia"){
    print(mpn[num,1], max.levels = 0)
    
  }
  
  options(warn=0, show.error.messages = T)  
}