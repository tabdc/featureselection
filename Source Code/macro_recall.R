# outputs list of class macro recall
macro_recall <- function(test_labels,pred_labels){
  
  barbunya<-0
  bombay<-0
  cali<-0
  dermason<-0
  horoz<-0
  seker<-0
  sira<-0
  barbunya_corr<-0
  bombay_corr<-0
  cali_corr<-0
  dermason_corr<-0
  horoz_corr<-0
  seker_corr<-0
  sira_corr<-0
  
  for (i in 1:length(test_labels)) {
    if (test_labels[i]=='BARBUNYA' | test_labels[i]==1) {
      barbunya <- barbunya+1
      if(pred_labels[i]=='BARBUNYA' | pred_labels[i]==1){
        barbunya_corr <- barbunya_corr+1    
      }
    }
    else if (test_labels[i]=='BOMBAY' | test_labels[i]==2) {
      bombay <- bombay+1
      if(pred_labels[i]=='BOMBAY' | pred_labels[i]==2){
        bombay_corr <- bombay_corr+1    
      }
    }
    else if (test_labels[i]=='CALI' | test_labels[i]==3) {
      cali <- cali+1
      if(pred_labels[i]=='CALI' | pred_labels[i]==3){
        cali_corr <- cali_corr+1    
      }
    }
    else if (test_labels[i]=='DERMASON' | test_labels[i]==4) {
      dermason <- dermason+1
      if(pred_labels[i]=='DERMASON' | pred_labels[i]==4){
        dermason_corr <- dermason_corr+1    
      }
    }
    else if (test_labels[i]=='HOROZ' | test_labels[i]==5) {
      horoz <- horoz+1
      if(pred_labels[i]=='HOROZ' | pred_labels[i]==5){
        horoz_corr <- horoz_corr+1    
      }
    }
    else if (test_labels[i]=='SEKER' | test_labels[i]==6) {
      seker <- seker+1
      if(pred_labels[i]=='SEKER' | pred_labels[i]==6){
        seker_corr <- seker_corr+1    
      }
    }
    else if (test_labels[i]=='SIRA' | test_labels[i]==7) {
      sira <- sira+1
      if(pred_labels[i]=='SIRA' | pred_labels[i]==7){
        sira_corr <- sira_corr+1    
      }
    }
  }
  
  mac_recall_barbunya<- barbunya_corr/barbunya
  mac_recall_bombay<- bombay_corr/bombay
  mac_recall_cali<- cali_corr/cali
  mac_recall_dermason<- dermason_corr/dermason
  mac_recall_horoz<- horoz_corr/horoz
  mac_recall_seker<- seker_corr/seker
  mac_recall_sira<- sira_corr/sira
  
  return(list(mac_recall_barbunya,mac_recall_bombay,mac_recall_cali,mac_recall_dermason,mac_recall_horoz,mac_recall_seker,mac_recall_sira))
}

