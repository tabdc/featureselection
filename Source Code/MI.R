# Computes the corrected mutual information

require(infotheo) 

MI <- function(X,Y){
    
  k <- sqrt(NROW(X)) # =length(Y)
  
  delta_X <- (max(X)-min(X))/k
  delta_X_XY <- (max(X)-min(X))/(k^(0.25))
  X_d <- discretize(X, disc="equalwidth", nbins=k)
  entropy_X <- entropy(X_d, method = "shrink")
  delta_Y <- (max(Y)-min(Y))/k
  delta_Y_XY <- (max(Y)-min(Y))/(k^(0.25))
  Y_d <- discretize(Y, disc="equalwidth", nbins=k)
  entropy_Y <- entropy(Y_d, method = "shrink")
  
  XY_d <- cbind(X_d,Y_d)
  entropy_XY <- entropy(XY_d, method = "shrink")
  
  #return(entropy_X+log(delta_X)+entropy_Y+log(delta_Y)-entropy_XY-log(delta_X_XY)-log(delta_Y_XY))
  return(entropy_X+entropy_Y-entropy_XY)
}
