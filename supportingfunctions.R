

############## Find variables with zero variance #########

variables_zerovariance = function(X){
  Var0Variable <- which(apply(X,2,var) == 0)
  if (length(Var0Variable) == 0) {
    print("No variables with zero variance")
  } else {
    print(sprintf("%d variable(s) with zero variance", length(Var0Variable)))
    print("Variable number:")
    print(Var0Variable)
   #print("The variable(s) is(are) deleted.")
  }
  return(Var0Variable)
}

############## Optimize gamma to maximize variance of Gaussian gram matrix ########

optimize_gamma_grammatrix = function(X, CandidatesOfGamma){
  
##### Calculate gram matrix of Gaussian kernel and its variance for each gamma candidate ########
  
  VarianceOfKernelMatrix <- NULL
  DistanceMatrics <- dist(X, diag = TRUE, upper = TRUE)^2
  for (CandidateOfGamma in CandidatesOfGamma) {
    KernelMatrix <- exp(-CandidateOfGamma*DistanceMatrics)
    VarianceOfKernelMatrix <- c(VarianceOfKernelMatrix,var(c(as.vector(KernelMatrix),as.vector(KernelMatrix),rep(1,nrow(X)))))
  } 

  ############ Decide the optimal gamma with the maximum variance value ########
  
  OptimalGamma <-  CandidatesOfGamma[which(VarianceOfKernelMatrix == max(VarianceOfKernelMatrix))]
  return( OptimalGamma[1] )
}
