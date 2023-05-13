# Wine Quality Rshiny Practice

+ Description   

The purpose of this app is to visualize the data, fit models and eventually predict wine quality by selecting various of factors.   
The data is from UCI machine learning repository, which includes two datasets related to red and white from vinho verde wine samples, from the north of Portugal. 

Download or learn more about the original data through: [Wine Quality Datasets](https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/)

+ Packages List 

library(shiny)   
library(shinydashboard)   
library(dplyr)   
library(tidyr)   
library(ggplot2)   
library(stringr)   
library(caret)    
library(DT)   

+ Install Packages

packages <- c("shiny", "shinydashboard", "dplyr", "tidyr", "ggplot2", "stringr", "caret", "DT")

installed_packages <- packages %in% rownames(installed.packages())  
if (any(installed_packages == FALSE)) {  
  install.packages(packages[!installed_packages])   
}

+ Run the App

shiny::runGitHub(repo = "Final-Project", username = "rraeyyi", ref="main")
