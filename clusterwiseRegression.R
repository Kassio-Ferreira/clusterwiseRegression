# Clusterwise Regression implementation:
# author: Kássio Ferreira da Silva
# Mon Dec 12 09:50:18 2016 ------------------------------

# INPUT: dataset and number of clusters (k);
# OUTPUT: List with classification indexes and clusters of individuals;

Clusterwise <- function(dados,k){
  
  Pi = sample(1:k,size=nrow(dados),replace=TRUE)  # generating index for initial partition 
  Pi1 = NULL                                      # vector to store new index
  test = 0                                        # flag
  print(Pi)                                       # print initial partition
            
  # computing centroids:
  betas = matrix(NA, nrow = k, ncol=ncol(dados))
  
  while(test == 0){
    
    for(i in 1:k){
      if(is.element(i,Pi))
      betas[i, ] = lm(dados[Pi==i,1]~dados[Pi==i,2:ncol(dados)])$coef # regression coefficients of each cluster;
    }
    
    for(j in 1:nrow(dados)){
      residual = NULL;
      for(z in 1:k){
        residual[z] = (dados[j,1] - (betas[z,1] + betas[z,-1]%*%dados[j,-1]))^2 # computing residuals: objet vs. partition;
      }
      Pi1[j] <- which.min(residual)  # storing index that minimizes the residual;
    }
    
    if(identical(Pi,Pi1)){  # if the new partition is identical to the current, stop;
      test = 1
    }else{                  # in other case, substitute the current partition by the new one and restart;
      Pi <- Pi1  
    }
    
    print(Pi)
    
  }
  
  # Final groups: list with objects in each cluster 
   finalGroups = list()
   
   for(i in 1:k)
     finalGroups[[i]] <- dados[Pi==i, ]
   
   # plot data and colour it by cluster;
   plot(dados[,1],dados[,2],col=Pi, xlim = c(min(dados[,1]),max(dados[,1])), 
        ylim = c(min(dados[,2]),max(dados[,2])))
   
  return(list(clusters = finalGroups, index = Pi))
}

# generating a dataset to test;
library(mvtnorm)

dados1 = rmvnorm(200, mean=c(1,1,1,1)) 
dados2 = rmvnorm(200, mean=c(4,4,4,4)) 
dados = rbind(dados1,dados2)
original = c(rep(1,200),rep(2,200)) # original groups
plot(dados[,1],dados[,2],col=original, xlim = c(min(dados[,1]),max(dados[,1])), 
     ylim = c(min(dados[,2]),max(dados[,2])))


fitt = Clusterwise(dados,k=2)     

# "matching" rate:
mean(original == fitt$index)

