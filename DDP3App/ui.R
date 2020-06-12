library(shiny)
library(miniUI)

shinyUI(fluidPage(
    titlePanel("Predict Gas Mileage from Engine Dislacement"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("sliderDISP","What is the volume of engine displacement of the given cars?", 0, 500, value = 100),
            checkboxInput("showModel1","Show Model 1", value = TRUE),
            checkboxInput("showModel2","Show Model 2", value = TRUE)
        ),
        mainPanel(
            plotOutput("plot1"),
            h3("Predicted Engine Displacement from Model 1"),
            textOutput("pred1"),
            h3("Predicted Engine Displacement from Model 2"),
            textOutput("pred2")
        )
    )
))