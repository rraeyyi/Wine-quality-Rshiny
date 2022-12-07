library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(stringr)
library(caret)
library(DT)

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
              h4("Select the Variables:"),
              selectizeInput("variables", "Variables", 
                             choice = c("Fixed Acidity", "Volatile Acidity", "Citric Acid", "Residual Sugar", "Chlorides", "Free Sulfur Dioxide", "Total Sulfur Dioxide", "Density", "pH", "Sulphates", "Alcohol")), br(), 
              h4("Select the Plot Type:"),
              radioButtons("plot", label = "Plot Type", 
                           choices = c("Bar plot", "Box plot")),
              plotOutput("plot")
      ),
              
      # Modeling tab content
      tabItem(tabName = "modeling",
              tabsetPanel(type = "tabs",
                          tabPanel("Modeling Info"),
                          tabPanel("Modeling Fitting", 
                                   h4("Select the Predictors:"),
                                   selectInput("predictors","Predictors",
                                               choice = c("FixedAcidity", "VolatileAcidity", "CitricAcid", "ResidualSugar", "Chlorides", "FreeSulfurDioxide", "TotalSulfurDioxide", "Density", "pH", "Sulphates", "Alcohol"), multiple = TRUE),
                                   h4("Select Number of Cross Validation:"),
                                   sliderInput("number", "Cross Validation", min = 0, max = 10, value = 5, step =1),
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
                                   h4('Your Choices:'),
                                   tableOutput("choices")
                                   )
                          )
              ),
    
    # Data tab content
    tabItem(tabName = "data",
            h4("Select the Wine Type:"),
            selectizeInput("type", "Type", selected = "Red", choices = c("Red", "White")),
            mainPanel(dataTableOutput("table"))
            )
    )
    )
  )


server <- function(input, output, session) {
  # Read in data
  getData <- reactive({
    if (input$type == "Red"){
      wine<-read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", sep = ";")
    } else {
      wine<-read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", sep = ";")
    }
    wine <- wine %>% select("FixedAcidity" = "fixed.acidity",
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
                            "Quality" = "quality")
    return(wine)
  })

  # Create fit formula
  fit <- reactive({
    as.formula(paste0("Quality ~ ", paste(input$predictors, collapse = "+")))
  })

  # Split data
  splitData <- reactive({
    set.seed(216)
    wine <- getData()
    intrain <- createDataPartition(wine$Quality, p = 0.7, list = FALSE)
    intrain <- splitData()
    training <- wine[intrain,]
    testing <- wine[-intrain,]
    
    # Set up cross validation
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
                      trControl = control, 
                      preProcess = c("center", "scale"), 
                      tuneGrid = expand.grid(mtry = 1:((ncol(training) - 1)/3)))
    rf_predict <- predict(rf_model, newdata = testing)
    rf_perform <- postResample(rf_predict, obs = testing$Quality)
    
    # Fit boosted tree model  
    gbm_model <- train(fit(),
                       data = training,
                       method = "gbm",
                       trControl = control,
                       preProcess = c("center", "scale"),
                       verbose = FALSE)
    gbm_predict <- predict(gbm_model, newdata = testing)
    gbm_perform <- postResample(gbm_predict, obs = testing$Quality)
  
  })

  output$model <- renderTable({
    if(!is.null(input$predictors)){
      table <- as_tibble(rbind(lasso_perform, rf_perform, gbm_perform))
      Model <- c("Lasso", "Random Forest", "Boosted Tree")
      performance <- cbind(Model, table)
      performance
    } else {
      print("Please select predictors!")
    }
  })
  
  # Output image
  output$img <- renderUI({
    tags$img(src = "https://cdn.analyticsvidhya.com/wp-content/uploads/2021/04/45245download.jpg")
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
    g <- ggplot(wine, aes(x = get(var)))
    
    observe({updateSelectizeInput(session, "plot")})
    
    # Plot based on selection
    if (input$plot == "Bar plot"){
      g + geom_bar(aes(fill = as.factor(Quality))) + scale_fill_discrete(name = "Quality") + 
        labs(x = input$variables)
      } else {
        g + geom_boxplot(aes(fill = as.factor(Quality))) + scale_fill_discrete(name = "Quality") + 
          labs(x = input$variables)
      }
  })
  
  
  # Show choices
  output$choices <- renderTable({
    choices <- data.frame(FixedAcidity = input$FixedAcidity,
                          VolatileAcidity = input$VolatileAcidity,
                          CitricAcid = input$CitricAcid, 
                          ResidualSugar = input$ResidualSugar,
                          Chlorides =input$Chlorides,
                          FreeSulfurDioxide = input$FreeSulfurDioxide,
                          TotalSulfurDioxide = input$TotalSulfurDioxide,
                          Density = input$Density, 
                          pH = input$pH,
                          Sulphates =input$Sulphates,
                          Alcohol =input$Alcohol
                          )
    choices
  })
  
  # Output table    
  output$table <- renderDataTable({
    datatable(getData())
  })
}

shinyApp(ui, server)