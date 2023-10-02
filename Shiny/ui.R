library(shiny)
library(plotly)

stock_list <- list.files("data")
stock_metric <- c("RMSE", "MAD", "DA")

shinyUI(navbarPage(
    # Configuration
    title = "",
    theme = "style/style.css",
    footer = includeHTML("template/footer.html"),
    fluid = TRUE,
    collapsible = TRUE,

    # HOME
    tabPanel("Optiver Stock",
             includeHTML("template/hometop.html"),
             fluidRow(
               column(1),
               # cluster diagram
               column(class="text-center", 10,
                      tags$h3("Our Current Research"),
                      column(6, class = "col-md-offset-1",
                          div(style = "max-height: calc(100vh - 200px); margin: 0 auto;",
                              plotlyOutput("clusterPlot", height="100%", width="100%", inline=TRUE)
                          )),
                      column(3,
                             tags$p(style="text-align: justify;", "Clustering is a common method for data aggregation and can be ideal when presented with exorbitant amounts of data. Using K-means clustering, an unsupervised learning algorithm, we have partitioned over 100 stock CSV files into 4 clusters based on Bid-Ask Spread and weighted average price (WAP). K-means operates as a centroid-based algorithm, determining a centroid and minimizing the sum of distances between data points and their clusters. It calculates a new centroid in each iteration and stops when no more centroid reassignments can be made, creating well-defined clusters. "),
                             tags$p(style="text-align: justify;", "Our main motivation for clustering is to address the issue of stocks having varying characteristics and determine whether specific models will perform better on stocks with particular attributes. As Bid-Ask spread and WAP are common orderbook statistics, the choice was made to cluster on those two attributes."))
                             )
               ),
             column(1)
             ),

    # PREDICTION
    tabPanel("Predictions",
             HTML(r"(
                  <h1 class="page-title">
                    <span class="decorative-line"></span>
                    Model Predictions
                    <span class="decorative-line"></span>
                  </h1>)"),
             fluidRow(class="text-center",
                      column(1),
                      column(2,
                             uiOutput("currentInfo")),
                      column(width = 3, align = "center",
                             fileInput("stockFile", "Stock", accept = ".csv")
                             ),
                      column(width = 3, align = "center",
                             selectInput("metric", "Metrics", choices = stock_metric)
                             ),
                      column(2,
                             uiOutput("metricDescription")),
                      column(1)
             ),
             fluidRow(
               column(width = 12, align = "center",
                      actionButton("predict", "Predict!", style="margin: 5px 0;", class="btn-primary btn-lg"),
                      uiOutput("notifications")
               )
             ),
             fluidRow(
               # linear regression
               column(class="text-center split-line-right", 3,
                      tags$h3("Linear Regression", onclick = "$('li:eq(2) a').tab('show');", class="hover-effect"),
                      uiOutput("linearBest"),
                      plotlyOutput("linearPlot"),
                      div(tableOutput("linearMetricTable"), style = "display: flex; justify-content: center; align-items: center; height: 100%;")),
               # HAV-RV
               column(class="text-center split-line-right", 3,
                      tags$h3("HAV-RV", onclick = "$('li:eq(2) a').tab('show');", class="hover-effect"),
                      uiOutput("havBest"),
                      plotlyOutput("havPlot"),
                      div(tableOutput("havMetricTable"), style = "display: flex; justify-content: center; align-items: center; height: 100%;")),
               # ARMA-GARCH
               column(class="text-center split-line-right", 3,
                      tags$h3("ARMA-GARCH", onclick = "$('li:eq(2) a').tab('show');", class="hover-effect"),
                      uiOutput("armaBest"),
                      plotlyOutput("armaPlot"),
                      div(tableOutput("armaMetricTable"), style = "display: flex; justify-content: center; align-items: center; height: 100%;")),
               # random forest
               column(class="text-center", 3,
                      tags$h3("Random Forest", onclick = "$('li:eq(2) a').tab('show');", class="hover-effect"),
                      uiOutput("rfBest"),
                      plotlyOutput("rfPlot"),
                      div(tableOutput("rfMetricTable"), style = "display: flex; justify-content: center; align-items: center; height: 100%;"))
             )
    ),

    # MODEL INFO
    tabPanel("Model Info",
             HTML(r"(
                  <h1 class="page-title">
                    <span class="decorative-line"></span>
                    Model Information
                    <span class="decorative-line"></span>
                  </h1>)"),
             fluidRow(
               column(2),
               # linear regression
               column(class="text-center split-line-right", 4,
                      tags$h3("Linear Regression"),
                      tags$p(style = "text-align: justify; font-size:15px;", "A linear regression model is a statistical model used to establish a linear relationship between the independent and dependent variables. It assumes that the dependent variable can be described by a straight line with a parameter called the regression coefficient, which is used to measure the relationship between the independent and dependent variables. The goal of a linear regression model is to estimate the regression coefficient by minimizing the difference between the actual observations and the model predictions in order to obtain inferences about the predictive power of the dependent variable and the relationship between the variables. In this project, the independent variables we use are WAP, order, BidAskSpread1, BidAskSpread2 and Order Imbalance to estimate the volatility.")),
               # HAV-RV
               column(class="text-center", 4,
                      tags$h3("HAV-RV"),
                      tags$p(style = "text-align: justify; font-size:15px;", "HAV model is one of the most popular models to predict return volatility. It directly fits the data on the time series to realized volatility."),
                      tags$p(style = "text-align: justify; font-size:15px;", "This model performs better in predicting long-term volatility, and it usually uses realized volatility over the past week and month to fit the model. However, since our time series is not long, here we use the volatility at t-1 and the average volatility from t-5 to t-1 to fit the volatility at time t."),
                      tags$p(style = "text-align: justify; font-size:15px;", "In addition, in the project, we use ordinary least squares to estimate the values of β0, β1 and β2.")),
               column(2)),
             tags$hr(class="split"),
             fluidRow(
               column(2),
               # ARMA-GARCH
               column(class="text-center split-line-right", 4,
                      tags$h3("ARMA-GARCH"),
                      tags$p(style = "text-align: justify; font-size:15px;", "ARMA-GARCH is a popular econometric model used for analyzing and forecasting financial time series data, particularly in the field of volatility modeling. The model combines the Autoregressive Moving Average (ARMA) model, which captures the serial dependence in the mean of the series, with the Generalized Autoregressive Conditional Heteroscedasticity (GARCH) model, which captures the time-varying volatility or variance. ARMA-GARCH models are widely used due to their ability to capture the conditional heteroscedasticity and volatility clustering often observed in financial data.")
                      ),
               # random forest
               column(class="text-center", 4,
                      tags$h3("Random Forest"),
                      tags$p(style = "text-align: justify; font-size:15px;", "Random forest is an ensemble machine learning algorithm which can be used for either classification or regression. It operates by constructing numerous decision trees which are trained on bootstrapped samples of the training set (draws from the training set with replacement). By constructing each decision tree using varying subsets of features, the model introduces a layer of randomness to avoid overfitting and introduce more diversity. The final output is determined by averaging all values for regression and taking the majority consensus for classification.")),
               column(2)
               )
    ),

    # ABOUT
    tabPanel("About",
             includeHTML("template/about.html"))
))
