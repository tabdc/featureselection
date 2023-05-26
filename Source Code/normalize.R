# Input: df->dataframe
# Output: normalized dataframe
normalize <- function(df){
  nor <-function(x) {(x -min(x))/(max(x)-min(x))}
  data_norm<-as.data.frame(lapply(df[,1:(NCOL(df)-1)],nor))
  return(data_norm)
}

