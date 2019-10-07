library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
require('rsconnect')
require(ggplot2)


fluidPage(theme = shinytheme("lumen"),
    titlePanel("Bodyfat Prediction"),
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "gender", label = strong("Gender"),
                    choices = c("Male","Female"),
                    selected = "Male"),
              
        sliderInput(inputId = "Years", label = strong("Age(Years):"),
                    min = 0, max = 100, value = 45, step = 1,
                    animate = animationOptions(interval = 400)),
        
        sliderInput(inputId = "WEIGHT", label=strong("WEIGHT(lbs)*:"),
                    min = 0, max = 800, value = 178, step = 0.05,
                    animate = animationOptions(interval = 500)),
          
        sliderInput(inputId = "ABDOMEN", label=strong("Abdomen circumference (cm)*:"),
                    min = 0, max = 200, value = 92, step = 0.1,
                    animate = animationOptions(interval = 1000)),
        
        checkboxInput(inputId = "smoother", label = strong("Robust Model"), value = TRUE),
        
        # Display only if the smoother is checked
        conditionalPanel(condition = "input.smoother == true",
                         sliderInput(inputId = "WRIST", label = strong("Wrist circumference (cm):"),
                                     min = 0, max = 50, value = 18, step = 0.1,
                                     animate = animationOptions(interval = 500)),
                         HTML("Wrist data gives model more precision.")),
        
        submitButton("Update")
      ),
      mainPanel(
        plotOutput(outputId = "lineplot"),
        textOutput(outputId = "desc"),
        textOutput(outputId = "warnings"),
        tags$a(href = "https://www.healthstatus.com/measuring-body-fat-percentage-home/",
               "Source: Health Data Reference", target = "_blank"),
        textOutput(outputId = "warning"),
        tags$a(href = "https://github.com/jyan75/stat-628-module-2",
               "If you have any questions, you can comment in our Github", target = "_blank")
      )
    )
)