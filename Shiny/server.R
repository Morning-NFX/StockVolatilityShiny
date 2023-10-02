library(shiny)
library(shinythemes)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(randomForest)
library(rugarch)
library(shinyalert)
library(caret)

source("metric.R")
source("HAV_RV.R")
source("ARMA_GARCH.R")

shinyServer(function(input, output, session) {
  set.seed(3888)
  load("stock_summary.RData")
  
  ## HOME ##
  # perform stock clustering
  clustering <- reactive({
    # scaling
    stock_summary_scale <- scale(stock_summary[, c("mean_WAP", "mean_BidAskSpread")])
    # kmeans
    km <- kmeans(stock_summary_scale, centers = 4, nstart = 10)
    stock_summary$cluster <- factor(km$cluster)
    stock_summary
  })
  # cluster plot
  output$clusterPlot <- renderPlotly({
    stock_summary <- clustering()
    p <- ggplot(stock_summary, aes(x = mean_WAP, y = mean_BidAskSpread, color = cluster, text = paste("Stock ID:", stock_id))) +
      geom_point() +
      labs(x = "WAP", y = "BidAskSpread", color = "Cluster") +
      theme_bw()
    ggplotly(p, tooltip = c("text")) 
  })
  
  ## MODEL ##
  observeEvent(input$predict, {
    if (is.null(input$stockFile)) {
      # Display a notification if no stock file is uploaded
      shinyalert::shinyalert(
        title = "Error",
        text = "Please upload a stock file before predicting.",
        type = "error",
        showCancelButton = FALSE
      )
    }
  })
  
  stock <- eventReactive(input$predict,
                        {req(input$stockFile)
                          data <- read.csv(input$stockFile$datapath)
                          metric <- input$metric
                          list("data" = data, "metric" = metric)})
  
  rf <- reactive({
    df <- stock()$data
    df <- df |> mutate(WAP = (bid_price1 * ask_size1 + ask_price1 * bid_size1) / (bid_size1 + ask_size1), BidAskSpread = ask_price1 / bid_price1 - 1,
                       time_bucket = ceiling(seconds_in_bucket / 30), 
                       num_order = bid_size1 + ask_size2 + ask_size1 + bid_size2,
                       BidAskSpread2 = ask_price2 / bid_price2 - 1,
                       Imbalance = (bid_size1 - ask_size1)/(bid_size1 + ask_size1))
    
    log_r1 <- list()
    time_IDs <- unique(df$time_id)
    for (i in 1 : length(time_IDs)) {
      sec <- df %>% filter(time_id == time_IDs[i]) %>% pull(seconds_in_bucket)
      price <- df %>% filter(time_id == time_IDs[i]) %>% pull(WAP)
      log_r <- log(price[-1] / price[1:(length(price) - 1)])
      log_r1[[i]] <- data.frame(time = sec[-1], log_return = log_r)
      time.no.change <- (1:600)[!(1:600 %in% log_r1[[i]]$time)]
      if (length(time.no.change) > 0) {
        new.df <- data.frame(time = time.no.change, log_return = 0)
        log_r1[[i]] <- rbind(log_r1[[i]], new.df)
        log_r1[[i]] <- log_r1[[i]][order(log_r1[[i]]$time), ]
      }
    }
    
    vol <- list()
    for (i in 1 : length(log_r1)) {
      log_r1[[i]] <- log_r1[[i]] %>% mutate(time_bucket = ceiling(time / 30))
      vol[[i]] <- aggregate(log_return ~ time_bucket, data = log_r1[[i]], FUN = comp_vol)
      colnames(vol[[i]]) <- c('time_bucket', 'volatility')
    }
    
    
    vol.train <- list()
    vol.val <- list()
    
    for (i in 1 : length(log_r1)) {
      vol.train[[i]] <- vol[[i]][1:16, ]
      vol.val[[i]] <- vol[[i]][-(1:16), ]
    }
    
    len.train <- length(vol.train[[1]]$volatility)
    len.val <- length(vol.val[[1]]$volatility)
    training <- list()
    pred <- list()
    rf_list <- list()
    testing <- list()
    
    for (i in 1 : length(vol)) {
      stats.bucket <- df %>% 
        filter(time_id == time_IDs[i] & time_bucket != 0) #%>% select(c(BidAskSpread, WAP, BidAskSpread2, num_order, time_bucket, Imbalance)) 
      # for each 30-sec time bucket
      mean.price <- aggregate(WAP ~ time_bucket, data = stats.bucket, FUN = mean)
      mean.order <- aggregate(num_order ~ time_bucket, data = stats.bucket, FUN = mean)
      mean.BAS <- aggregate(BidAskSpread ~ time_bucket, data = stats.bucket, FUN = mean)
      mean.BAS2 <- aggregate(BidAskSpread2 ~ time_bucket, data = stats.bucket, FUN = mean)
      mean.imbalance <- aggregate(Imbalance ~ time_bucket, data = stats.bucket, FUN = mean)
      
      training[[i]] <- data.frame(volatility = vol.train[[i]]$volatility[-1], 
                                  price = mean.price$WAP[1:(len.train - 1)],
                                  order = mean.order$num_order[1:(len.train - 1)],
                                  BidAskSpread = mean.BAS$BidAskSpread[1:(len.train - 1)],
                                  BidAskSpread2 = mean.BAS2$BidAskSpread2[1:(len.train - 1)],
                                  Imbalance = mean.imbalance$Imbalance[1:(len.train - 1)])
      
      
      testing[[i]] <- 
        data.frame(volatility = vol.val[[i]]$volatility, 
                   price = mean.price$WAP[len.train:(len.train + len.val - 1)],
                   order = mean.order$num_order[len.train:(len.train + len.val - 1)],
                   BidAskSpread = mean.BAS$BidAskSpread[len.train:(len.train + len.val - 1)],
                   Imbalance = mean.imbalance$Imbalance[len.train:(len.train + len.val - 1)],
                   BidAskSpread2 = mean.BAS2$BidAskSpread2[len.train:(len.train + len.val - 1)]
        )
      
      if (!any(is.na(training[[i]])) & !any(is.na(training[[i]]))) {
        rf_list[[i]] <- randomForest(volatility~., data = training[[i]],
                                     importance=TRUE, proximity=TRUE, weights = 0.8 ^
                                       (((len.train - 2):0) / 2), ntree = 10)
        pred[[i]] <- predict(rf_list[[i]], newdata = testing[[i]])
      }
      
    }
    
    list(pred = pred, actual = vol.val)
  })
  
  rfMetric <- reactive({
    metric <- comp_metric(rf()$pred, rf()$actual)
    metric
  })
  
  output$rfPlot <- renderPlotly({
    predict <- rf()$pred
    actual <- rf()$actual
    
    pred.ols.df <- data.frame(actual[[1]]$time_bucket, predict[[1]])
    colnames(pred.ols.df) <- c('time_bucket', 'prediction_ols')
    
    p <- ggplot() + 
      geom_line(data = pred.ols.df, aes(x = time_bucket, y = prediction_ols), color = "red") +
      geom_point(data = pred.ols.df, aes(x = time_bucket, y = prediction_ols)) +
      geom_line(data = actual[[1]], aes(x = time_bucket, y = volatility), color = "black") +
      geom_point(data = actual[[1]], aes(x = time_bucket, y = volatility)) +
      ylim(range()$min, range()$max) +
      xlab('Time Buckets') +
      ylab('Volatility')
    
    ggplotly(p) %>%
      layout(
        annotations = list(
          list(
            x = 1,
            y = mean(pred.ols.df$prediction_ols),
            text = "Prediction",
            font = list(color = "red", size = 11),
            showarrow = FALSE,
            xref = "paper",
            yref = "y"
          ),
          list(
            x = 1,
            y = mean(actual[[1]]$volatility),
            text = "Actual",
            font = list(color = "black", size = 11),
            showarrow = FALSE,
            xref = "paper",
            yref = "y"
          )
        )
      )
  })
  
  havrv <- reactive({
    stock_vol = get_vol(stock()$data)
    result = get_train_val(stock_vol)
    train = result$train
    val = result$val
    hav_ols = comp_hav(stock_vol, train)
    pred_old = comp_pred(stock_vol, val, train, hav_ols)
    
    list(pred = pred_old, actual = val)
  })
  
  havMetric <- reactive({
    metric <- comp_metric(havrv()$pred, havrv()$actual)
    metric
  })
  
  output$havPlot <- renderPlotly({
    predict <- havrv()$pred
    actual <- havrv()$actual
    
    pred.ols.df <- data.frame(actual[[1]]$time_bucket, predict[[1]])
    colnames(pred.ols.df) <- c('time_bucket', 'prediction_ols')
    
    p <- ggplot() + 
      geom_line(data = pred.ols.df, aes(x = time_bucket, y = prediction_ols), color = "red") +
      geom_point(data = pred.ols.df, aes(x = time_bucket, y = prediction_ols)) +
      geom_line(data = actual[[1]], aes(x = time_bucket, y = volatility), color = "black") +
      geom_point(data = actual[[1]], aes(x = time_bucket, y = volatility)) +
      ylim(range()$min, range()$max) +
      xlab('Time Buckets') +
      ylab('Volatility')
    
    ggplotly(p) %>%
      layout(
        annotations = list(
          list(
            x = 1,
            y = mean(pred.ols.df$prediction_ols),
            text = "Prediction",
            font = list(color = "red", size = 11),
            showarrow = FALSE,
            xref = "paper",
            yref = "y"
          ),
          list(
            x = 1,
            y = mean(actual[[1]]$volatility),
            text = "Actual",
            font = list(color = "black", size = 11),
            showarrow = FALSE,
            xref = "paper",
            yref = "y"
          )
        )
      )
  })
  
  linear <- reactive({
    stock_data <- stock()$data
    stock_data <- stock_data %>% mutate(WAP = (bid_price1 * ask_size1 + ask_price1 * bid_size1) / (bid_size1 + ask_size1))
    stock_data <- stock_data %>% mutate(BidAskSpread1 = ask_price1 / bid_price1 - 1)
    stock_data <- stock_data %>% mutate(BidAskSpread2 = ask_price2 / bid_price2 - 1)
    stock_data <- stock_data %>% mutate(time_bucket = ceiling(seconds_in_bucket / 30),
                                        num_order = bid_size1 + ask_size1 + bid_size2 + ask_size2)
    stock_data <- stock_data %>% mutate(OrderImbalance = (bid_size1 - ask_size1)/(bid_size1 + ask_size1))
    
    #log_return
    log_r1 <- list()
    time_IDs <- unique(stock_data$time_id)
    for (i in 1 : length(time_IDs)) {
      sec <- stock_data %>% filter(time_id == time_IDs[i]) %>% pull(seconds_in_bucket)
      price <- stock_data %>% filter(time_id == time_IDs[i]) %>% pull(WAP)
      log_r <- log(price[-1] / price[1:(length(price) - 1)])
      log_r1[[i]] <- data.frame(time = sec[-1], log_return = log_r)
      time.no.change <- (1:600)[!(1:600 %in% log_r1[[i]]$time)]
      if (length(time.no.change) > 0) {
        new.df <- data.frame(time = time.no.change, log_return = 0)
        log_r1[[i]] <- rbind(log_r1[[i]], new.df)
        log_r1[[i]] <- log_r1[[i]][order(log_r1[[i]]$time), ]
      }
    }
    
    #regression model
    vol <- list()
    comp_vol <- function(x) {
      return(sqrt(sum(x ^ 2)))
    }
    for (i in 1 : length(log_r1)) {
      log_r1[[i]] <- log_r1[[i]] %>% mutate(time_bucket = ceiling(time / 30))
      vol[[i]] <- aggregate(log_return ~ time_bucket, data = log_r1[[i]], FUN = comp_vol)
      colnames(vol[[i]]) <- c('time_bucket', 'volatility')
    }
    
    vol.train <- list()
    vol.val <- list()
    
    for (i in 1 : length(log_r1)) {
      vol.train[[i]] <- vol[[i]][1:16, ]
      vol.val[[i]] <- vol[[i]][-(1:16), ]
    }
    
    list.reg <- list() # list for regression
    stock_data <- stock_data %>% mutate(time_bucket = ceiling(seconds_in_bucket / 30),
                                        num_order = bid_size1 + ask_size1 + bid_size2 + ask_size2)
    len.train <- length(vol.train[[1]]$volatility)
    
    for (i in 1 : length(vol)) {
      stats.bucket <- stock_data %>% 
        filter(time_id == time_IDs[i] & time_bucket != 0) %>% 
        select(c(BidAskSpread1, BidAskSpread2, WAP, num_order, time_bucket,OrderImbalance)) 
      # for each 30-sec time bucket, we compute the following statistics
      mean.price <- aggregate(WAP ~ time_bucket, data = stats.bucket, FUN = mean)
      mean.order <- aggregate(num_order ~ time_bucket, data = stats.bucket, FUN = mean)
      mean.BAS1 <- aggregate(BidAskSpread1 ~ time_bucket, data = stats.bucket, FUN = mean)
      mean.OI <- aggregate(OrderImbalance ~ time_bucket, data = stats.bucket, FUN = mean)
      mean.BAS2 <- aggregate(BidAskSpread2 ~ time_bucket, data = stats.bucket, FUN = mean)
      list.reg[[i]] <- data.frame(volatility = vol.train[[i]]$volatility[-1], 
                                  price = mean.price$WAP[1:(len.train - 1)],
                                  order = mean.order$num_order[1:(len.train - 1)],
                                  BidAskSpread1 = mean.BAS1$BidAskSpread1[1:(len.train - 1)],
                                  OrderImbalance = mean.OI$OrderImbalance[1:(len.train - 1)],
                                  BidAskSpread2 = mean.BAS2$BidAskSpread2[1:(len.train - 1)])
    }
    
    
    lm.models <- list()
    
    for (i in 1 : length(vol)) {
      lm.models[[i]] <- lm(volatility ~ price + order + BidAskSpread1 + OrderImbalance + BidAskSpread2,  list.reg[[i]],
                           weights = 0.8 ^ (((len.train - 2):0) / 2))
    }
    
    list.reg.val <- list()
    len.val <- length(vol.val[[1]]$volatility)
    pred.lm <- list()
    
    for (i in 1 : length(vol)) {
      stats.bucket <- stock_data %>% 
        filter(time_id == time_IDs[i] & time_bucket != 0) %>% 
        select(c(BidAskSpread1, BidAskSpread2, WAP, num_order, time_bucket, OrderImbalance))
      mean.price <- aggregate(WAP ~ time_bucket, data = stats.bucket, FUN = mean)
      mean.order <- aggregate(num_order ~ time_bucket, data = stats.bucket, FUN = mean)
      mean.BAS1 <- aggregate(BidAskSpread1 ~ time_bucket, data = stats.bucket, FUN = mean)
      mean.OI <- aggregate(OrderImbalance ~ time_bucket, data = stats.bucket, FUN = mean)
      mean.BAS2 <- aggregate(BidAskSpread2 ~ time_bucket, data = stats.bucket, FUN = mean)
      list.reg.val[[i]] <- 
        data.frame(volatility = vol.val[[i]]$volatility, 
                   price = mean.price$WAP[len.train:(len.train + len.val - 1)],
                   order = mean.order$num_order[len.train:(len.train + len.val - 1)],
                   BidAskSpread1 = mean.BAS1$BidAskSpread1[len.train:(len.train + len.val - 1)],
                   OrderImbalance = mean.OI$OrderImbalance[len.train:(len.train + len.val - 1)],
                   BidAskSpread2 = mean.BAS2$BidAskSpread2[len.train:(len.train + len.val - 1)])
      pred.lm[[i]] <- predict(lm.models[[i]], newdata = list.reg.val[[i]])
    }

    list(pred = pred.lm, actual = vol.val)
  })
  
  linearMetric <- reactive({
    metric <- comp_metric(linear()$pred, linear()$actual)
    metric
  })
  
  output$linearPlot <- renderPlotly({
    predict <- linear()$pred
    actual <- linear()$actual
    
    pred.ols.df <- data.frame(actual[[1]]$time_bucket, predict[[1]])
    colnames(pred.ols.df) <- c('time_bucket', 'prediction_ols')
    
    p <- ggplot() + 
      geom_line(data = pred.ols.df, aes(x = time_bucket, y = prediction_ols), color = "red") +
      geom_point(data = pred.ols.df, aes(x = time_bucket, y = prediction_ols)) +
      geom_line(data = actual[[1]], aes(x = time_bucket, y = volatility), color = "black") +
      geom_point(data = actual[[1]], aes(x = time_bucket, y = volatility)) +
      ylim(range()$min, range()$max) +
      xlab('Time Buckets') +
      ylab('Volatility')
    
    ggplotly(p) %>%
      layout(
        annotations = list(
          list(
            x = 1,
            y = mean(pred.ols.df$prediction_ols),
            text = "Prediction",
            font = list(color = "red", size = 11),
            showarrow = FALSE,
            xref = "paper",
            yref = "y"
          ),
          list(
            x = 1,
            y = mean(actual[[1]]$volatility),
            text = "Actual",
            font = list(color = "black", size = 11),
            showarrow = FALSE,
            xref = "paper",
            yref = "y"
          )
        )
      )
  })
  
  armagarch <- reactive({
    stock <- arma_get_vol(stock()$data)
    volatility_ls <- stock$volatility
    log_returns_ls <- stock$log_returns
    
    arma_list <- fit_arma(volatility_ls, log_returns_ls)
    pred_list <- arma_calculate_RV_pred(arma_list, volatility_ls)

    train_val <- arma_get_train_val(volatility_ls)
    val <- train_val$val
    
    volatility_ls <- lapply(volatility_ls, function(df) df[-(1:16), ])
    pred_list <- lapply(pred_list, function(vec) vec[1:4])
    
    list(pred = pred_list, actual = volatility_ls)
  })
  
  armaMetric <- reactive({
    metric <- comp_metric(armagarch()$pred, armagarch()$actual)
    metric
  })

  output$armaPlot <- renderPlotly({
    predict <- armagarch()$pred
    actual <- armagarch()$actual
    
    pred.ols.df <- data.frame(actual[[1]]$time_bucket, predict[[1]])
    colnames(pred.ols.df) <- c('time_bucket', 'prediction_ols')
    
    p <- ggplot() + 
      geom_line(data = pred.ols.df, aes(x = time_bucket, y = prediction_ols), color = "red") +
      geom_point(data = pred.ols.df, aes(x = time_bucket, y = prediction_ols)) +
      geom_line(data = actual[[1]], aes(x = time_bucket, y = volatility), color = "black") +
      geom_point(data = actual[[1]], aes(x = time_bucket, y = volatility)) +
      ylim(range()$min, range()$max) +
      xlab('Time Buckets') +
      ylab('Volatility')
    
    ggplotly(p) %>%
      layout(
        annotations = list(
          list(
            x = 1,
            y = mean(pred.ols.df$prediction_ols),
            text = "Prediction",
            font = list(color = "red", size = 11),
            showarrow = FALSE,
            xref = "paper",
            yref = "y"
          ),
          list(
            x = 1,
            y = mean(actual[[1]]$volatility),
            text = "Actual",
            font = list(color = "black", size = 11),
            showarrow = FALSE,
            xref = "paper",
            yref = "y"
          )
        )
      )
  })
  
  range <- reactive({
    act <- havrv()$actual[[1]][,2]
    hav <- havrv()$pred[[1]]
    linear <- linear()$pred[[1]]
    rf <- rf()$pred[[1]]
    arma <- armagarch()$pred[[1]]

    max_val <- max(max(act), max(hav), max(linear), max(rf), max(arma))
    min_val <- min(min(act), min(hav), min(linear), min(rf), min(arma))
    list(max = max_val, min = min_val)
  })
  
  metricCompare <- reactive({
    selection <- input$metric
    if(selection == "RMSE") { 
      # RMSE - lower better
      rf <- min(rfMetric()$RMSE)
      hav <- min(havMetric()$RMSE)
      linear <- min(linearMetric()$RMSE)
      arma <- min(armaMetric()$RMSE)
      
      variables <- c("rf", "hav", "linear","arma")
      best_index <- which.min(c(rf, hav, linear, arma))
      best_model <- variables[best_index] 
      return(best_model)
      
    } else if(selection == "MAD") {
      # MAD - lower better
      rf <- min(rfMetric()$MAD)
      hav <- min(havMetric()$MAD)
      linear <- min(linearMetric()$MAD)
      arma <- min(armaMetric()$MAD)
      
      variables <- c("rf", "hav", "linear","arma")
      best_index <- which.min(c(rf, hav, linear, arma))
      best_model <- variables[best_index] 
      return(best_model)
      
    } else if(selection == "DA") {
      # DA - higher better
      rf <- max(rfMetric()$MAD)
      hav <- max(havMetric()$MAD)
      linear <- max(linearMetric()$MAD)
      arma <- max(armaMetric()$MAD)
      
      variables <- c("rf", "hav", "linear","arma")
      best_index <- which.max(c(rf, hav, linear, arma))
      best_model <- variables[best_index]
      return(best_model)
    }
  })
  
  output$linearBest <- renderUI({
    if (metricCompare() == "linear") {
      HTML('<a class=best-model>Best Model</a>')
    } else {
      HTML('<br>')
    }
  })
  
  output$havBest <- renderUI({
    if (metricCompare() == "hav") {
      HTML('<a class=best-model>Best Model</a>')
    } else {
      HTML('<br>')
    }
  })
  
  output$armaBest <- renderUI({
    if (metricCompare() == "arma") {
      HTML('<a class=best-model>Best Model</a>')
    } else {
      HTML('<br>')
    }
  })
  
  output$rfBest <- renderUI({
    if (metricCompare() == "rf") {
      HTML('<a class=best-model>Best Model</a>')
    } else {
      HTML('<br>')
    }
  })
  
  metricSelection <- eventReactive(input$predict, {
    req(input$stockFile)
    input$metric
  })
  
  stockSelection <- eventReactive(input$predict, {
    req(input$stockFile)
    input$stockFile$name
  })
  
  output$linearMetricTable <- renderTable({
    selection = input$metric
    summary_data = NULL
    if (selection == "RMSE") {
      summary_data <- data.frame(
        Metric = "RMSE",
        Min = min(linearMetric()$RMSE),
        Median = median(linearMetric()$RMSE),
        Max = max(linearMetric()$RMSE)
      )
      summary_data$Min[summary_data$Metric == "RMSE"] <- round(summary_data$Min[summary_data$Metric == "RMSE"], 2)
      summary_data$Max[summary_data$Metric == "RMSE"] <- round(summary_data$Max[summary_data$Metric == "RMSE"], 2)
      summary_data$Median[summary_data$Metric == "RMSE"] <- round(summary_data$Median[summary_data$Metric == "RMSE"], 2)
    } else if (selection == "MAD") {
      summary_data <- data.frame(
        Metric = "MAD",
        Min = min(linearMetric()$MAD),
        Median = median(linearMetric()$MAD), 
        Max = max(linearMetric()$MAD)
      )
      summary_data$Min[summary_data$Metric == "MAD"] <- sprintf("%.2e", summary_data$Min[summary_data$Metric == "MAD"])
      summary_data$Max[summary_data$Metric == "MAD"] <- sprintf("%.2e", summary_data$Max[summary_data$Metric == "MAD"])
      summary_data$Median[summary_data$Metric == "MAD"] <- sprintf("%.2e", summary_data$Median[summary_data$Metric == "MAD"])
    } else if (selection == "DA") {
      summary_data <- data.frame(
        Metric = "DA",
        Min = min(linearMetric()$DA),
        Median = median(linearMetric()$DA),
        Max = max(linearMetric()$DA)
      )
    }
    summary_data
  })
  
  output$havMetricTable <- renderTable({
    selection = input$metric
    summary_data = NULL
    if (selection == "RMSE") {
      summary_data <- data.frame(
        Metric = "RMSE",
        Min = min(havMetric()$RMSE),
        Median = median(havMetric()$RMSE),
        Max = max(havMetric()$RMSE)
      )
      summary_data$Min[summary_data$Metric == "RMSE"] <- round(summary_data$Min[summary_data$Metric == "RMSE"], 2)
      summary_data$Max[summary_data$Metric == "RMSE"] <- round(summary_data$Max[summary_data$Metric == "RMSE"], 2)
      summary_data$Median[summary_data$Metric == "RMSE"] <- round(summary_data$Median[summary_data$Metric == "RMSE"], 2)
    } else if (selection == "MAD") {
      summary_data <- data.frame(
        Metric = "MAD",
        Min = min(havMetric()$MAD),
        Median = median(havMetric()$MAD),
        Max = max(havMetric()$MAD)
      )
      summary_data$Min[summary_data$Metric == "MAD"] <- sprintf("%.2e", summary_data$Min[summary_data$Metric == "MAD"])
      summary_data$Max[summary_data$Metric == "MAD"] <- sprintf("%.2e", summary_data$Max[summary_data$Metric == "MAD"])
      summary_data$Median[summary_data$Metric == "MAD"] <- sprintf("%.2e", summary_data$Median[summary_data$Metric == "MAD"])
    } else if (selection == "DA") {
      summary_data <- data.frame(
        Metric = "DA",
        Min = min(havMetric()$DA),
        Median = median(havMetric()$DA),
        Max = max(havMetric()$DA)
      )
    }
    summary_data
  })
  
  output$armaMetricTable <- renderTable({
    selection = input$metric
    summary_data = NULL
    if (selection == "RMSE") {
      summary_data <- data.frame(
        Metric = "RMSE",
        Min = min(armaMetric()$RMSE),
        Median = median(armaMetric()$RMSE),
        Max = max(armaMetric()$RMSE)
      )
      summary_data$Min[summary_data$Metric == "RMSE"] <- round(summary_data$Min[summary_data$Metric == "RMSE"], 2)
      summary_data$Max[summary_data$Metric == "RMSE"] <- round(summary_data$Max[summary_data$Metric == "RMSE"], 2)
      summary_data$Median[summary_data$Metric == "RMSE"] <- round(summary_data$Median[summary_data$Metric == "RMSE"], 2)
    } else if (selection == "MAD") {
      summary_data <- data.frame(
        Metric = "MAD",
        Min = min(armaMetric()$MAD),
        Median = median(armaMetric()$MAD),
        Max = max(armaMetric()$MAD)
      )
      summary_data$Min[summary_data$Metric == "MAD"] <- sprintf("%.2e", summary_data$Min[summary_data$Metric == "MAD"])
      summary_data$Max[summary_data$Metric == "MAD"] <- sprintf("%.2e", summary_data$Max[summary_data$Metric == "MAD"])
      summary_data$Median[summary_data$Metric == "MAD"] <- sprintf("%.2e", summary_data$Median[summary_data$Metric == "MAD"])
    } else if (selection == "DA") {
      summary_data <- data.frame(
        Metric = "DA",
        Min = min(armaMetric()$DA),
        Median = median(armaMetric()$DA),
        Max = max(armaMetric()$DA)
      )
    }
    summary_data
  })
  
  output$rfMetricTable <- renderTable({
    selection = input$metric
    summary_data = NULL
    if (selection == "RMSE") {
      summary_data <- data.frame(
        Metric = "RMSE",
        Min = min(rfMetric()$RMSE),
        Median = median(rfMetric()$RMSE),
        Max = max(rfMetric()$RMSE)
      )
      summary_data$Min[summary_data$Metric == "RMSE"] <- round(summary_data$Min[summary_data$Metric == "RMSE"], 2)
      summary_data$Max[summary_data$Metric == "RMSE"] <- round(summary_data$Max[summary_data$Metric == "RMSE"], 2)
      summary_data$Median[summary_data$Metric == "RMSE"] <- round(summary_data$Median[summary_data$Metric == "RMSE"], 2)
    } else if (selection == "MAD") {
      summary_data <- data.frame(
        Metric = "MAD",
        Min = min(rfMetric()$MAD),
        Median = median(rfMetric()$MAD),
        Max = max(rfMetric()$MAD)
      )
      summary_data$Min[summary_data$Metric == "MAD"] <- sprintf("%.2e", summary_data$Min[summary_data$Metric == "MAD"])
      summary_data$Max[summary_data$Metric == "MAD"] <- sprintf("%.2e", summary_data$Max[summary_data$Metric == "MAD"])
      summary_data$Median[summary_data$Metric == "MAD"] <- sprintf("%.2e", summary_data$Median[summary_data$Metric == "MAD"])
    } else if (selection == "DA") {
      summary_data <- data.frame(
        Metric = "DA",
        Min = min(rfMetric()$DA),
        Median = median(rfMetric()$DA),
        Max = max(rfMetric()$DA)
      )
    }
    summary_data
  })
  
  observeEvent(input$predict, {
    output$currentInfo <- renderUI({
      tags$div(
        tags$a(class="metric-text", tags$b("Current Showing")),
        tags$p(paste("Stock:", stockSelection())),
        tags$p(paste("Metric:", input$metric))
      )
    })
  })
  
  output$metricDescription <- renderUI({
    selection = input$metric
    content <- NULL
    if (selection == "RMSE") {
      content <- HTML('
                      <a class="metric-text"><b>RMSE(Scaled)</b></a>
                      <p style="text-align:justify;">A preliminary exploration of our stock data reveals that the actual volatility is of a small magnitude and hence, the raw root mean squared error is not informative. Thus, we have expressed root mean squared error as a percentage of the actual volatility in our testing dataset. </p>
                      ')
    } else if (selection == "MAD") {
      content <- HTML('
                      <a class="metric-text"><b>Mean Absolute Deviation</b></a>
                      <p style="text-align:justify;">To observe the true scale of our predictions, we have incorporated mean absolute deviation, which is a measure similar to standard deviation but preferred as it is more robust to outliers. It is also more intuitive to interpret as the “actual” average deviation, instead of accounting for its position in the normal distribution.</p>
                      <img src="/img/MAD.png" height="50px">
                      ')
    } else if (selection == "DA") {
      content <- HTML('
                      <a class="metric-text"><b>Directional Accuracy</b></a>
                      <p style="text-align:justify;">As the previous two metrics only measure magnitude between a predicted and actual value, directional accuracy allows us to evaluate our models’ performance in predicting the direction of realised volatility. Hence, using all 3 metrics in conjunction allows us to evaluate our models holistically.</p>
                      ')
    }
    content
  })
})