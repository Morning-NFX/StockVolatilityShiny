library(dplyr)
library(tidyverse)

comp_vol <- function(x) {
  return(sqrt(sum(x ^ 2)))
}

comp_quar <- function(x) {
  return(length(x) / 3 * sum(x ^ 4))
}

get_vol <- function(stock) {
  
  stock <- stock %>% mutate(
    WAP = (bid_price1 * ask_size1 + ask_price1 * bid_size1) / (bid_size1 + ask_size1),
    
    BidAskSpread =  ask_price1 / bid_price1 - 1
  )
  
  time_IDs <- unique(stock$time_id)[1:5]
  
  log_r1 <- list()
  
  for (i in 1 : length(time_IDs)) {
    
    sec <- stock %>% filter(time_id == time_IDs[i]) %>% pull(seconds_in_bucket)
    price <- stock %>% filter(time_id == time_IDs[i]) %>% pull(WAP)
    log_r <- log(price[-1] / price[1:(length(price) - 1)])
    log_r1[[i]] <- data.frame(time = sec[-1], log_return = log_r)
    time.no.change <- (1:600)[!(1:600 %in% log_r1[[i]]$time)]
    
    if (length(time.no.change) > 0) {
      new.df <- data.frame(time = time.no.change, log_return = 0)
      log_r1[[i]] <- rbind(log_r1[[i]], new.df)
      log_r1[[i]] <- log_r1[[i]][order(log_r1[[i]]$time),]
    }
  }
  
  vol <- list()
  for (i in 1 : length(log_r1)) {
    
    log_r1[[i]] <- log_r1[[i]] %>% mutate(time_bucket = ceiling(time / 30))
    vol[[i]] <- aggregate(log_return ~ time_bucket, data = log_r1[[i]], FUN = comp_vol)
    colnames(vol[[i]]) <- c('time_bucket', 'volatility')
  }
  
  return(vol)
}

get_train_val <- function(vol) {
  vol.train <- list()
  vol.val <- list()
  for (i in 1 : length(vol)) {
    vol.train[[i]] <- vol[[i]][1:16, ]
    vol.val[[i]] <- vol[[i]][-(1:16), ]
  }
  newList = list("train" = vol.train, "val" = vol.val)
  return(newList)
}

comp_hav <- function(vol, vol.train) {
  
  list.HAV <- list()
  len.train <- length(vol.train[[1]]$volatility)
  
  for (i in 1 : length(vol)) {
    mean.vol <- rep(0, len.train - 5)
    for (j in 1 : 5) {
      mean.vol <- mean.vol + vol.train[[i]]$volatility[j : (j + len.train - 6)] / 5
    }
    list.HAV[[i]] <- data.frame(vol = vol.train[[i]]$volatility[-(1:5)], 
                                vol_1 = vol.train[[i]]$volatility[5:(len.train - 1)],
                                mean_vol_5 = mean.vol)
  }
  
  HAV.ols.models <- list()
  HAV.wls.models <- list()
  
  for (i in 1 : length(vol)) {
    HAV.ols.models[[i]] <- lm(vol ~ vol_1 + mean_vol_5, list.HAV[[i]])
  }
  
  return(HAV.ols.models)
}

comp_pred <- function(vol, vol.val, vol.train, HAV.ols.models) {
  list.HAV <- list()
  pred.ols.hav <- list()
  
  for (i in 1 : length(vol)) {
    
    mean.vol <- mean(vol.train[[i]]$volatility[5:16])
    
    list.HAV[[i]] <- data.frame(vol = vol.val[[i]]$volatility, 
                                vol_1 = vol.val[[i]]$volatility,
                                mean_vol_5 = mean.vol)
    pred.ols.hav[[i]] <- predict(HAV.ols.models[[i]], newdata = list.HAV[[i]])
    
  }
  return(pred.ols.hav)
}

comp_mse <- function(vol, vol.val, pred.ols.hav) {
  RMSE.lm <- vector()
  MSE.lm <- vector()
  MAD.lm <- vector()
  DA.lm <- vector()
  
  for (i in 1 : length(vol)) {
    
    RMSE.lm <- c(RMSE.lm, sqrt(mean((vol.val[[i]]$volatility - pred.ols.hav[[i]]) ^ 2))/vol.val[[i]]$volatility*100)
    
    DA.lm <- c(DA.lm, mean(sign(pred.ols.hav[[i]] - vol.val[[i]]$volatility) == sign(vol.val[[i]]$volatility - mean(vol.val[[i]]$volatility))))
    
    MSE.lm <- c(MSE.lm, mean((vol.val[[i]]$volatility - pred.ols.hav[[i]]) ^ 2))
    
    MAD.lm <- c(MAD.lm, mean(abs(vol.val[[i]]$volatility - pred.ols.hav[[i]])))
    
  }
  newList = list("rmse" = RMSE.lm, "da" = DA.lm, "mse" = MSE.lm, "mad" = MAD.lm)
  return(newList)
}

