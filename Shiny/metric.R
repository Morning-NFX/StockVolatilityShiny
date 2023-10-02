comp_metric <- function(predict, actual) {
  RMSE <- vector()
  MAD <- vector()
  DA <- vector()
  
  for (i in 1 : length(predict)) {
    RMSE <- c(RMSE, mean((sqrt(mean((actual[[i]]$volatility - predict[[i]]) ^ 2))/actual[[i]]$volatility)*100))
    
    MAD <- c(MAD, mean(abs(actual[[i]]$volatility - predict[[i]])))
    
    DA <- c(DA, mean(sign(predict[[i]] - actual[[i]]$volatility) == sign(actual[[i]]$volatility - mean(actual[[i]]$volatility))))                            
  }
  
  newList = list("RMSE" = RMSE, "MAD" = MAD, "DA" = DA)
  return(newList)
}

comp_arma_metric <- function(vol, vol.val, pred.arma) {
  RMSE.lm <- vector()
  MAD.lm <- vector()
  DA.lm <- vector()
  
  for (i in 1 : length(vol)) {
    
    RMSE.lm <- c(RMSE.lm, sqrt(mean((vol.val[[i]]$volatility - pred.arma[[i]]) ^ 2))/vol.val[[i]]$volatility*100)
    
    DA.lm <- c(DA.lm, mean(sign(pred.arma[[i]] - vol.val[[i]]$volatility) == sign(vol.val[[i]]$volatility - mean(vol.val[[i]]$volatility))))
    
    MAD.lm <- c(MAD.lm, mean(abs(vol.val[[i]]$volatility - pred.arma[[i]])))
    
  }
  newList = list("RMSE" = RMSE.lm, "DA" = DA.lm, "MAD" = MAD.lm)
  return(newList)
}