# outputs list of class macro f1
macro_f1 <- function(mean_recall_vector,mean_precision_vector){
  
  f1_barbunya <- (2*(mean_recall_vector[1]*mean_precision_vector[1]))/(mean_recall_vector[1]+mean_precision_vector[1])
  f1_bombay <- (2*(mean_recall_vector[2]*mean_precision_vector[2]))/(mean_recall_vector[2]+mean_precision_vector[2])
  f1_cali <- (2*(mean_recall_vector[3]*mean_precision_vector[3]))/(mean_recall_vector[3]+mean_precision_vector[3])
  f1_dermason <- (2*(mean_recall_vector[4]*mean_precision_vector[4]))/(mean_recall_vector[4]+mean_precision_vector[4])
  f1_horoz <-(2*(mean_recall_vector[5]*mean_precision_vector[5]))/(mean_recall_vector[5]+mean_precision_vector[5])
  f1_seker <-(2*(mean_recall_vector[6]*mean_precision_vector[6]))/(mean_recall_vector[6]+mean_precision_vector[6])
  f1_sira <- (2*(mean_recall_vector[7]*mean_precision_vector[7]))/(mean_recall_vector[7]+mean_precision_vector[7])

  return(list(f1_barbunya,f1_bombay,f1_cali,f1_dermason,f1_horoz,f1_seker,f1_sira))
}
