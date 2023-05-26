#Input: dfframe->df; mutual info. matrix->mim; #of parameters to extract->K
#Ouput: Selected K features

require(infotheo)
source("MI.r")

CCMI <- function(df,K){
  
  # S->Contains features to keep
  S <- rep(NA,16)
  # F->Vector of features for comparison (from which we remove features as we proceed)
  F <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
  
  # Compute the mutual information with the class
  
  relevance <- c(1:16)
  
  for (i in 1:16) {
    relevance[i]<-MI(df[,i],df[,17])
  }
  
  # Pick the first most relevant feature argmax_i I(Xi,C)
  fm <- which.max(relevance)
  S[1] <- fm
  F<-F[-fm]
  
  count<-1
  
  C<-df[,17]
  cov_matrix <- cov(df[,1:16],df[,1:16])

  # Discretize df to use infotheo function condinformation() and build the conditional mutual information matrix
  df_discretize<-discretize(df,disc="equalwidth",nbins=sqrt(NROW(df$Class)))
  MI_cond_matrix <- matrix(NA,nrow=16,ncol=16,byrow=TRUE)
  for (i in c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) {
    for (j in c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) {
      MI_cond_matrix[i,j]<-condinformation(df_discretize[,i],df_discretize[,j],C) 
    }
  }
  
  # Build the mutual information matrix for the features
  MI_matrix <- matrix(NA,nrow=16,ncol=16,byrow=TRUE)
  for (i in c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) {
    for (j in c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) {
      MI_matrix[i,j]<-MI(df_discretize[,i],df_discretize[,j]) 
    }
  }
  
  #algorithm from "feature selection based on mutual information with correlation coefficient" Zhou et al.
  while (count<K) {
    JCCMI <- rep(NA, ncol(df)-count)
    for (m in F) {
      s_vec<-S[!is.na(S)]
      for (i in s_vec){
        Xm<-df[,m]
        Xi<-df[,i]
        rho<-cov_matrix[m,i]/(sqrt(sd(Xm))*sqrt(sd(Xi)))
        a<-min(MI_cond_matrix[m,])
        b<-min(abs(rho)*MI_matrix[m,i])
      }
      JCCMI[m] <- (a-b)
    }
    fnew<-which.max(JCCMI)
    count<-count+1
    S[count]<-fnew
    F<-F[! F %in% fnew]
  }
  
  s_vec<-S[!is.na(S)]
  
  return(s_vec)
}

