# sums the diagonal of a table which are the correct assigned observations and
# divides by all observations

accuracy <- function(x){
  sum(diag(x)/(sum(rowSums(x)))) * 100
}

