source('accuracy.r')
source('macro_recall.r')
source('macro_precision.r')
source('macro_f1.r')

print_results <- function(cl_test_labels,predictions){
  
  tab<-table(predictions,cl_test_labels)
  
  # Accuracy
  cat("Accuracy:",accuracy(tab),"\n")
  
  # Macro Recall
  a <- macro_recall(cl_test_labels,predictions)
  #cat("Barbunya macro recall:",a[[1]])
  #cat("Bombay macro recall:",a[[2]])
  #cat("Cali macro recall:",a[[3]])
  #cat("Dermason macro recall:",a[[4]])
  #cat("Horoz macro recall:",a[[5]])
  #cat("Seker macro recall:",a[[6]])
  #cat("Sira macro recall:",a[[7]])
  a_vector<-unlist(a, recursive = TRUE, use.names = TRUE)
  cat("Macro Recall:",100*mean(a_vector),"\n")
  
  # Macro Precision
  b <- macro_precision(cl_test_labels,predictions)
  #cat("Barbunya macro precision:",b[[1]])
  #cat("Bombay macro precision:",b[[2]])
  #cat("Cali macro precision:",b[[3]])
  #cat("Dermason macro precision:",b[[4]])
  #cat("Horoz macro precision:",b[[5]])
  #cat("Seker macro precision:",b[[6]])
  #cat("Sira macro precision:",b[[7]])
  b_vector<-unlist(b, recursive = TRUE, use.names = TRUE)
  cat("Macro Precision:",100*mean(b_vector),"\n")
  
  # Macro F1
  c <- macro_f1(a_vector,b_vector)
  #cat("Barbunya macro F1:",c[[1]])
  #cat("Bombay macro F1:",c[[2]])
  #cat("Cali macro F1:",c[[3]])
  #cat("Dermason macro F1:",c[[4]])
  #cat("Horoz macro F1:",c[[5]])
  #cat("Seker macro F1:",c[[6]])
  #cat("Sira macro F1:",c[[7]])
  c_vector<-unlist(c, recursive = TRUE, use.names = TRUE)
  cat("Macro F1:",100*mean(c_vector),"\n")
}

