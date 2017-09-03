
corr <- function (directory, threshold = 0){
  arquivos <- dir("specdata")
  saida <- c()
  
  for ( item in arquivos ){
    
    csvName <- paste(directory, "/", formatC(item, width=3, flag="0"), sep="")
    arquivo <- read_csv(csvName, col_names=T, comment = "")
    arquivo_limp <- arquivo[complete.cases(arquivo),]
    
    if (nrow(arquivo_limp) > threshold ) {
      saida <- c(saida, cor(as.numeric(arquivo_limp$sulfate), as.double(arquivo_limp$nitrate)))
    }
  }
  saida
}