
complete <- function (directory , id = 1:332) {
  base <- data.frame()
  for (item in id){
    csvName <- paste(directory, "/", formatC(item, width=3, flag="0"), '.csv', sep="")
    arquivo <- read_csv(csvName, col_names=T)
    arquivo_limp <- arquivo[complete.cases(arquivo),]
    base <- rbind(base, c(item, nrow(arquivo_limp)))
  }
  base
}
