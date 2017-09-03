best <- function(state, outcome) {
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
  
  
  ##Get Heart Failure Data
  mhf <-  data.frame(base[,2], base[,7], base[,17])
  colnames(mhf) <- c( "hospital", "state", "deaths")
  mhf$deaths <- as.numeric(as.character(mhf$deaths))
  mhf <- mhf[complete.cases(mhf),]
  mhf <- mhf[order(mhf$deaths, mhf$hospital),]#sort de data frame for lowest deaths
  
  ##Get Pneumonia Data
  mpn <-  data.frame(base[,2], base[,7], base[,23])
  colnames(mpn) <- c("hospital", "state", "deaths")
  mpn$deaths <- as.numeric(as.character(mpn$deaths))
  mpn <- mpn[complete.cases(mpn),]
  mpn <- mpn[order(mpn$deaths, mpn$hospital),]#sort de data frame for lowest deaths
  
  
  
  
  ## Check that state and outcome are valid
  if (! (state %in% base$State) ) {
    stop (paste("Error in best(", state, ", ", outcome, ") : invalid state"), call. = F)
  }
  if (! max(outcome %in% desease) ) {
    stop (paste("Error in best(", state, ", ", outcome, ") : invalid outcome"), call. = F)
  }
  
  ##identify what type desease is and return the best hospital
  if  (outcome == "heart attack"){
    print(head(mha[mha$state == state,]$hospital, 1), max.levels = 0)
  }
  else if (outcome == "heart failure"){
    print(head(mhf[mhf$state == state,]$hospital, 1), max.levels = 0)
  }
  else if (outcome == "pneumonia"){
    print(head(mpn[mpn$state == state,]$hospital, 1), max.levels = 0)
  }
  options(warn=0, show.error.messages = T)
}