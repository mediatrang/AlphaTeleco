library(ggplot2)
library(readr)
library(stringr)
library(ggplot2)
library(magrittr)
library(dplyr)
library(shiny)
library(ggrepel)
library(readxl)
###Import data
alpha <- read_xlsx('AlphaTelco.xlsx')
churn <- alpha %>% filter(Churn=='Yes')
no_churn <- alpha %>% filter(Churn=='No')

###Summary tenure of no_churn data
summary(no_churn$Tenure)

###Loyal customers who belong to top 20% tenure of no_churn dataset
loyalty <- arrange(no_churn, desc(Tenure)) %>% slice(1:(nrow(no_churn)*0.2))

library(shiny)
category <- c('Gender','SeniorCitizen','Partner','Dependents',
              'PhoneService','MultipleLines','InternetService','TechSupport',
              'StreamingTV','StreamingMovies','Contract','PaperlessBilling','PaymentMethod')
continous <- c('Tenure','MonthlyCharges','TotalCharges')

ui <- fluidPage(
  titlePanel('Assignment 3'),
  column(3,
         wellPanel(
           radioButtons('var','Explore categorical attributes', category, selected = 'Gender')
         ),
         wellPanel(
           radioButtons('var1', 'Explore continous attributes', continous, selected = 'MonthlyCharges')
         )
  ),
  column(9,
         tabsetPanel(
           tabPanel('Churned customers',
                    fluidRow(
                      column(6,plotOutput('pie1')),
                      column(6,plotOutput('his1'))),
                    fluidRow(plotOutput('char1'))
           ),
           tabPanel('Non churned customers',
                    fluidRow(
                      column(6,plotOutput('pie2')),
                      column(6,plotOutput('his2'))),
                    fluidRow(plotOutput('char2'))
           ),
           
           tabPanel('Loyal customers',
                    fluidRow(
                      column(6,plotOutput('pie3')),
                      column(6,plotOutput('his3'))),
                    fluidRow(plotOutput('char3'))
           ),
           
           tabPanel('Task 2',
                    mainPanel(verbatimTextOutput('tab1'),
                              plotOutput('bar'))
           )
         )
  )
)




