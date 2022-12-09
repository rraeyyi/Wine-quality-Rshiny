library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(caret)
library(DT)

# Obtain original data
red <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", sep = ";")
white <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", sep = ";")
red["color"] <- "Red"
white["color"] <- "White"
data <- rbind(red, white) %>% select("FixedAcidity" = "fixed.acidity",
                                       "VolatileAcidity" = "volatile.acidity",
                                       "CitricAcid" =  "citric.acid",
                                       "ResidualSugar" = "residual.sugar",
                                       "Chlorides" = "chlorides", 
                                       "FreeSulfurDioxide" = "free.sulfur.dioxide",
                                       "TotalSulfurDioxide" = "total.sulfur.dioxide",
                                       "Density" = "density", 
                                       "pH" = "pH",
                                       "Sulphates" = "sulphates",
                                       "Alcohol" = "alcohol",
                                       "Quality" = "quality",
                                       "Color" = "color")

ui <- dashboardPage(
  dashboardHeader(title = "Wine Quality"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about"),
      menuItem("Data Exploration", tabName = "exploration"),
      menuItem("Modeling", tabName = "modeling"),
      menuItem("Data", tabName = "data")
    )
  ),
  
  dashboardBody(
    tabItems(
      # About tab content
      tabItem(tabName = "about",
              strong("INTRODUCTION"), br(), 
              "The purpose of this app is to visualize the data, fit models and eventually predict wine quality by selecting various of factors.", br(), "The data is from UCI machine learning repository, which includes two datasets related to red and white from vinho verde wine samples, from the north of Portugal.", br(), 
              "Download or learn more about the original data through:", a(href = "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/", "Wine Quality Datasets"), br(), br(),
              strong("PAGES & TABS"),br(),
              "There are four major pages: About, Data Exploration, Modeling, Data.", br(),
              "- About page includes introduction information.", br(),
              "- Data Exploration page includes summary reports", br(),
              "- Modeling page includes modeling information, fitting models and predictions", br(),
              "- Data page includes the datasets, subsets and final dataset for download ", br(), br(),
              uiOutput("img")
      ),
      
      # Data Exploration tab content
      tabItem(tabName = "exploration",
              h4("Select Color:"),
              selectizeInput("color", "Color", 
                             choice = c("Red", "White")),
              h4("Select Variable:"),
              selectizeInput("variables", "Variables", 
                             choice = c("Fixed Acidity", "Volatile Acidity", "Citric Acid", "Residual Sugar", "Chlorides", "Free Sulfur Dioxide", "Total Sulfur Dioxide", "Density", "pH", "Sulphates", "Alcohol")), 
              h4("Rate Classification:"),
              h5("Low represents quality is less than 5."),
              h5("High represents quality is greater than 6."), 
              checkboxInput("rate", "Rate"),
              h4("Select Summary Type:"),
              radioButtons("summary", label = "",
                           choices=c("Numeric Summaries", "Graphical Summaries")),
              conditionalPanel(condition = "input.summary == 'Graphical Summaries'",
                               radioButtons("plot", label = "Select plot type",
                                            choices = c("Bar plot", "Box plot")),
                               mainPanel(plotOutput("plot"))),
              conditionalPanel(condition = "input.summary == 'Numeric Summaries'",
                               mainPanel(tableOutput("tab")))
      ),
              
      # Modeling tab content
      tabItem(tabName = "modeling",
              tabsetPanel(type = "tabs",
                          tabPanel("Modeling Info",
                                   h4("LASSO:"),
                                   h5("LASSO stands for Least Absolute Shrinkage and Selection Operator. It is a statistical formula for the regularisation of data models and feature selection."),
                                   h5("The main advantage of a LASSO regression model is that it has the ability to set the coefficients for features it does not consider interesting to zero. This means that the model does some automatic feature selection to decide which features should be included."),
                                   h5("When the number of predictors are more than the number of observations, Lasso Regression method gets into trouble. Also, if there are two or more highly collinear variables then Lasso Regression will select one of them randomly which is not a good technique in data interpretation."), br(),
                                   h4("Random Forest:"), 
                                   h5("Random Forest approach is an ensemble learning method for classification, regression and other tasks that operates by constructing a multitude of decision trees at training time. For classification tasks, the output of the random forest is the class selected by most trees. For regression tasks, the mean or average prediction of the individual trees is returned"),
                                   h5("Random forest model can perform both regression and classification tasks. Also, it prevents overfitting by using multiple trees."),
                                   h5("The main limitation of random forest is that a large number of trees can make the algorithm too slow and ineffective for real-time predictions."), br(),
                                   h4("Boosted Tree:")),
                          tabPanel("Modeling Fitting",
                                   h4("Select the Predictors:"),
                                   selectInput("predictors","Predictors",
                                               choice = c("FixedAcidity", "VolatileAcidity", "CitricAcid", "ResidualSugar", "Chlorides", "FreeSulfurDioxide", "TotalSulfurDioxide", "Density", "pH", "Sulphates", "Alcohol"), multiple = TRUE),
                                   mainPanel(tableOutput("model"))
                                   ),
                          tabPanel("Prediction", h4("Select the Parameters for Variables:"),
                                   sliderInput("FixedAcidity", "Fixed Acidity", min = 0, max = 20, value = 0.7, step = 0.1),
                                   sliderInput("VolatileAcidity", "Volatile Acidity", min = 0, max = 2, value = 0.7, step = 0.1),
                                   sliderInput("CitricAcid", "Citric Acid", min = 0, max = 2, value = 0.7, step = 0.1),   
                                   sliderInput("ResidualSugar",  "Residual Sugar", min = 0, max = 100, value = 40, step = 0.1),
                                   sliderInput("Chlorides", "Chlorides", min = 0, max = 1, value = 0.7 , step = 0.1),
                                   sliderInput("FreeSulfurDioxide", "Free Sulfur Dioxide", min = 0, max = 300, value = 0.7 , step = 0.1),
                                   sliderInput("TotalSulfurDioxide", "Total Sulfur Dioxide", min = 0, max = 500, value = 150),
                                   sliderInput("Density", "Density", min = 0.5, max = 1.5, value = 1, step = 0.1),
                                   sliderInput("pH", "pH",min = 1, max = 5, value = 2, step = 0.1),
                                   sliderInput("Sulphates", "Sulphates" ,min = 0, max = 2, value = 1, step = 0.1),
                                   sliderInput("Alcohol", "Alcohol", min = 0, max = 15, value = 12, step = 0.1), br(),
                                   h4("Boosted Tree Model Prediction:"),
                                   tableOutput("prediction")
                                   )
                          )
              ),
    
    # Data tab content
    tabItem(tabName = "data",
            checkboxInput("sub", "Subset data by wine type"),
            conditionalPanel(condition = "input.sub == 0",
            dataTableOutput("ogtab")),
            conditionalPanel(condition = "input.sub == 1",
                             selectizeInput("type", "Type", 
                                            choice = c("Red", "White")),
            dataTableOutput("table")
            ),
            downloadButton("downloadData", "Download")
    )
    )
  )
)

server <- function(input, output, session) {
  
  # Subset data by wine type
  getData <- reactive({
    if (input$color == "Red"){
      filter(data, data$Color == "Red")
    } else {
      filter(data, data$Color == "White")
    }
  })

  # Output image
  output$img <- renderUI({
    tags$img(src = "https://cdn.analyticsvidhya.com/wp-content/uploads/2021/04/45245download.jpg")
  })

  # Create numeric summary
  output$tab <- renderTable({
    wine <- getData()
    if (str_length(input$variables) <= 10){
      var <- input$variables
    } else {
      var <- gsub(" ", "", input$variables)
    }
    
    if (input$rate){
      wine %>% mutate(Rate = case_when(Quality <= 4 ~ "Low", 
                                       Quality <= 6 ~ "Average",
                                       Quality >= 7 ~ "High")) %>%
        select(Rate, var) %>% group_by(Rate) %>% summarize(mean = mean(get(var)), sd = sd(get(var)))
    } else {
      sum <- wine %>% select(var) %>% summary() %>% as.data.frame()
      sum %>% separate(Freq, c("Stat", "Value"), sep=":") %>% pivot_wider(names_from = Stat, values_from = Value) %>% select(-Var1, Variable = Var2)
    }
        }) 
  
  # Create plot
  output$plot <- renderPlot({
    wine <- getData()
    # Select variable
    if (str_length(input$variables) <= 10){
      var <- input$variables
    } else {
      var <- gsub(" ", "", input$variables)
    }
    observe({updateSelectizeInput(session, "plot")})
    
    # Plot based on selection
    if (input$plot == "Bar plot"){
      if (input$rate){
        sub <- wine %>% mutate(Rate = case_when(Quality <= 4 ~ "Low", 
                                                Quality <= 6 ~ "Average",
                                                Quality >= 7 ~ "High")) %>% select(var, Rate)
        h <- ggplot(sub, aes(x = get(var)))
        h + geom_bar(aes(fill = Rate)) + scale_fill_discrete(name = "Rate") + 
        labs(x = input$variables)
      } else {
        g <- ggplot(wine, aes(x = get(var)))
        g + geom_bar(aes(fill = as.factor(Quality))) + scale_fill_discrete(name = "Quality") + 
          labs(x = input$variables)
      }
    } else {
      if (input$rate){
        sub <- wine %>% mutate(Rate = case_when(Quality <= 4 ~ "Low", 
                                                Quality <= 6 ~ "Average",
                                                Quality >= 7 ~ "High")) %>% select(var, Rate)
        h <- ggplot(sub, aes(x = get(var)))
        h + geom_boxplot(aes(fill = Rate)) + scale_fill_discrete(name = "Rate") + 
          labs(x = input$variables)
    } else {
      g <- ggplot(wine, aes(x = get(var)))
      g + geom_boxplot(aes(fill = as.factor(Quality))) + scale_fill_discrete(name = "Quality") + 
        labs(x = input$variables)
    }
    }
  })


  # Create fit formula
  fit <- reactive({
    as.formula(paste0("Quality ~ ", paste(input$predictors, collapse = "+")))
  })
  
  # Split data
  splitData <- reactive({
    wine <- getData()
    set.seed(216)
    intrain <- createDataPartition(wine$Quality, p = 0.7, list = FALSE)
    training <- wine[intrain,]
    testing <- wine[-intrain,]
    split <- list(training, testing)
    return(split)
    })
  
  # Fit model
  model <- reactive({
    split <- splitData()
    training <- split[[1]]
    testing <- split[[2]]
    control <- trainControl(method = "cv", number = 5)
 
    # Fit LASSO model
    lasso_model <- train(fit(),
                         data = training,
                         method = "lasso",
                         preProcess = c("center", "scale"),
                         trControl = control)
    lasso_predict <- predict(lasso_model, newdata = testing)
    lasso_perform <- postResample(lasso_predict, obs = testing$Quality)
    
    # Fit random forest model
    rf_model <- train(fit(), 
                      data = training, 
                      method = "rf", 
                      preProcess = c("center", "scale"), 
                      trControl = control, 
                      tuneGrid = expand.grid(mtry = 1:((ncol(training) - 1)/3)))
    rf_predict <- predict(rf_model, newdata = testing)
    rf_perform <- postResample(rf_predict, obs = testing$Quality)
    
    # Fit boosted tree model  
    gbm_model <- train(fit(),
                       data = training,
                       method = "gbm",
                       preProcess = c("center", "scale"),
                       trControl = control,
                       verbose = FALSE)
    gbm_predict <- predict(gbm_model, newdata = testing)
    gbm_perform <- postResample(gbm_predict, obs = testing$Quality)
    
    table <- as_tibble(rbind(lasso_perform, rf_perform, gbm_perform))
  })
  
  output$model <- renderTable({
    if(!is.null(input$predictors)){
      table <- model()
      Model <- c("Lasso", "Random Forest", "Boosted Tree")
      performance <- cbind(Model, table)
      performance
    } else {
      print("Please select predictors!")
    }
  })
  
  # Output prediction
  output$prediction <- renderTable({
    choices <- data.frame(FixedAcidity = input$FixedAcidity,
                          VolatileAcidity = input$VolatileAcidity,
                          CitricAcid = input$CitricAcid, 
                          ResidualSugar = input$ResidualSugar,
                          Chlorides = input$Chlorides,
                          FreeSulfurDioxide = input$FreeSulfurDioxide,
                          TotalSulfurDioxide = input$TotalSulfurDioxide,
                          Density = input$Density, 
                          pH = input$pH,
                          Sulphates = input$Sulphates,
                          Alcohol = input$Alcohol)
    split <- splitData()
    training <- split[[1]]
    testing <- split[[2]]
    control <- trainControl(method = "cv", number = 5)
    training <- training %>% select(-Color)
    gbm_model <- train(Quality~.,
                       data = training,
                       method = "gbm",
                       preProcess = c("center", "scale"),
                       trControl = control,
                       verbose = FALSE)
    prediction <- predict(gbm_model, choices)
    predict <- data.frame("Prediction"= prediction)
    if (predict <=4) {
      rate <- data.frame("Rate"= "Low")
    } else if (predict >=7){
      rate <- data.frame("Rate"= "High")
    } else {
      rate <- data.frame("Rate"= "Average")
    }
    cbind(choices, predict, rate)
  })
  
  # Output original data table
  output$ogtab <- renderDataTable({
    datatable(data)
  })
  
  # Output data table  
  output$table <- renderDataTable({
    if (input$sub){
      if (input$type == "Red"){
        wine <- filter(data, data$Color == "Red")
      } else {
        wine <- filter(data, data$Color == "White")
      }
      datatable(wine)
    }
  })
  
  # Download as file
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$type, "Wine.csv", sep = "")
    },
    content = function(file) {
      write.csv(getData(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)