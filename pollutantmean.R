pollutantmean <- function(directory, pollutant, id = 1:332){
  ##'directory' is a character vector of length 1 indicating 
  ##the location of csv files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the polluant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID number
  ## to be used
  
  ## Return the mean of the pollutant across all the monitors list
  ## in the 'id' vector (ignoring NA values)
  ## Note: Do not round the result !
  base <- data.frame()
  for (item in id){
    csvName <- paste(directory, "/", formatC(item, width=3, flag="0"), '.csv', sep="")
    base <- rbind(base, read_csv(csvName, col_names=T))
  }
  mean(as.double(base[[pollutant]]), na.rm = T)
}
