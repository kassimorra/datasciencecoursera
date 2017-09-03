remove(result)
rankall <- function(outcome, num = "best") {
  ## Read outcome data
  #options(warn=-1)
  base <- read.csv("outcome-of-care-measures.csv")
  desease <- c("heart attack", "heart failure", "pneumonia")
  states <- unique(base$State)
  states <- states[order(states)]
  ## Check that state and outcome are valid
  if (! max(outcome %in% desease) ) {
    stop (paste("invalid outcome"), call. = F)
  }  
  
  if ( outcome == 'heart attack')       {base <- data.frame(base[,2], base[,7], base[,11])}
  else if ( outcome == 'heart failure') {base <- data.frame(base[,2], base[,7], base[,17])}
  else if ( outcome == 'pneumonia')     {base <- data.frame(base[,2], base[,7], base[,23])}
  
  colnames(base) <- c("hospital", "state", "deaths")
  base$deaths <- as.numeric(as.character(base$deaths))
  base <- base[complete.cases(base),]  
  
  ## For each state, find the hospital of the given rank
  #options(warn=0)  
  for (i in 1:length(states)){
    retorno <- findrank(num = num, base = base[base$state == states[i],])

    ajustado <- data.frame(retorno$hospital, states[i])
    rownames(ajustado) <- states[i]
    
    if ( !exists ("result") ){
      result <- ajustado
    }
     
    else {
      result <- rbind(result, ajustado)
    }
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  return(result)
  
}

findrank <- function(num, base){
  ##Modify num if best is 1 or worst is the last
  
  if ( num == "best" ) num <- 1
  else if ( num  == "worst") num <- nrow(base)
  tmp <- base[order(base$deaths, base$hospital),]#sort de data frame for lowest deaths
  
  return(tmp[num,])
}