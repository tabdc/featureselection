# df->dataframe, x-> % to assign training set
train_test_split <- function(df,x){
  set.seed(42)
  sample <- sample.int(n = nrow(df), size = floor(x*nrow(df)), replace = F)
  train <- df[sample,1:NCOL(df)]
  test  <- df[-sample,1:NCOL(df)]
  return(list(train,test))
}

