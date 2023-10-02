arma_get_vol <- function(stock) {
  
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
  
  return(list(volatility = vol, log_returns = log_r1))
}



fit_arma <- function(vol, log) {
  spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                     mean.model = list(armaOrder = c(1, 1)), 
                     distribution.model = "norm")
  ARMA_GARCH.models <- list()
  
  for (i in 1:length(vol)) {
    ARMA_GARCH.models[[i]] <- ugarchfit(spec = spec, data = log[[i]] %>% 
                                          filter(time <= 480) %>% pull(log_return),
                                        solver = 'hybrid')
  }
  
  return(ARMA_GARCH.models)
}


arma_get_train_val <- function(vol) {
  vol.train <- list()
  vol.val <- list()
  for (i in 1 : length(vol)) {
    vol.train[[i]] <- vol[[i]][1:16, ]
    vol.val[[i]] <- vol[[i]][-(1:16), ]
  }
  newList = list("train" = vol.train, "val" = vol.val)
  return(newList)
}



arma_calculate_RV_pred <- function(arma_garch_models, vol) {
  RV.pred <- vector("list", length = length(arma_garch_models))
  
  for (i in 1:length(arma_garch_models)) {
    arma_garch_model <- arma_garch_models[[i]]
    RV.pred[[i]] <- rep(0, length(vol))
    
    for (j in 1:length(vol)) {
      fspec <- getspec(arma_garch_model)
      setfixed(fspec) <- as.list(coef(arma_garch_model))
      future.path <- fitted(ugarchpath(fspec, n.sim = 30, m.sim = 1000))
      
      future.path[is.na(future.path)] <- 0 
      
      RV.pred[[i]][j] <- mean(sqrt(colSums(future.path^2)))
    }
  }
  
  return(RV.pred)
}




