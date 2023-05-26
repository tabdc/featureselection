#Input: dfframe->df; mutual info. matrix->mim; #of parameters to extract->K
#Ouput: Selected K features

require(infotheo)
source("MI.r")

MICC <- function(df,K){
  
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
  
  k <- sqrt(NROW(df[,17]))
  C<-df[,17]
  
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
    MICC <- rep(NA, ncol(df)-count)
    for (i in F) {
      s_vec<-S[!is.na(S)]
      sum<-0
      Xi<-df[,i]
      Xi <- discretize(Xi, disc="equalwidth", nbins=k)
      for (s in s_vec){
        Xs<-df[,s]
        Xs <- discretize(Xs, disc="equalwidth", nbins=k)
        aux_vec<-c(entropy(Xi,method="shrink"),entropy(Xs,method="shrink"))
        sum<-sum+MI(Xi,Xs)/(min(aux_vec))
      }
      MICC[i] <- relevance[i]/((1/(length(s_vec)))*sum)-relevance[i]
    }
    fnew<-which.max(MICC)
    count<-count+1
    S[count]<-fnew
    F<-F[! F %in% fnew]
  }
  
  s_vec<-S[!is.na(S)]
  
  return(s_vec)
}
