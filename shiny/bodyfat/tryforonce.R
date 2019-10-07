library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
require('rsconnect')
require(ggplot2)

model3<-function(weight,abdomen,wrist){
  a=-24.394-0.084*weight+0.888*abdomen-1.302*wrist
  if(a<0) return(0)
  if(a>=0) return(a)
}

model2<-function(weight,abdomen){
  a=-43.178-0.12*weight+0.904*abdomen
  if(a<0) return(0)
  if(a>=0) return(a)
}


trend_data <- read_csv("../data/cleanfile.csv")
# Define UI for application that draws a histogram
ui<-fluidPage(theme = shinytheme("lumen"),
  titlePanel("Bodyfat Prediction"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "gender", label = strong("Gender"),
                  choices = c("Male","Female"),
                  selected = "Male"),
      
      sliderInput(inputId = "Years", label = strong("Age(Years)):"),
                  min = 1, max = 100, value = 30, step = 1,
                  animate = animationOptions(interval = 100)),
      
      sliderInput(inputId = "WEIGHT", label=strong("WEIGHT(lbs)*):"),
                  min = 1, max = 100, value = 30, step = 0.1,
                  animate = animationOptions(interval = 1000)),
                  
      sliderInput(inputId = "ABDOMEN", label=strong("Abdomen circumference (cm)*:"),
                  min = 1, max = 100, value = 30, step = 0.1,
                  animate = animationOptions(interval = 1000)),
      
      checkboxInput(inputId = "smoother", label = strong("Robust Model"), value = FALSE),
      
      # Display only if the smoother is checked
      conditionalPanel(condition = "input.smoother == true",
                       sliderInput(inputId = "WRIST", label = strong("Wrist circumference (cm):"),
                                   min = 1, max = 100, value = 30, step = 0.1,
                                   animate = animationOptions(interval = 1000)),
                       HTML("Wrist data leads model more precise.")),
      
      submitButton("Update")
      ),
    mainPanel(
      plotOutput(outputId = "lineplot"),
      textOutput(outputId = "desc"),
      tags$a(href = "https://www.healthstatus.com/measuring-body-fat-percentage-home/",
           "Source: Health Data", target = "_blank")
    )
  )
)


server <- function(input, output) {
  
  # Fit model
  if(!input$smoother) select_trend<-reactive({input$ABDOMEN+input$Weight})
  if(input$smoother) select_trend<-reactive({input$ABDOMEN+input$WEIGHT+input$WRIST})
  
  selected_trends <- reactive({
    req(input$ABDOMEN)
    req(input$WEIGHT)
    if(input$smoother) req(input$WRIST)
    validate(need(!is.na(input$ABDOMEN) & !is.na(input$WEIGHT),
                  "Error: Please provide weight and abdomen."))
    if(input$smoother) validate(need(input$input$WRIST,
                                     "Error: Please provide wrist."))
    if(!input$smoother){
      model2(input$WEIGHT,input$ABDOMEN)
    }else{
      model3(input$WEIGHT,input$ABDOMEN,input$WRIST)
    }
  })
  
  Data<- trend_data
  
  output$lineplot <- renderPlot({
    #color = "#434343"
    #par(mar = c(4, 4, 1, 1))
    #plot(x = selected_trends()$date, y = selected_trends()$close, type = "l",
         #xlab = "Date", ylab = "Trend index", col = color, fg = color, col.lab = color, col.axis = color)
    p <- ggplot(Data, aes(x=bodyfat)) + 
      geom_density()
    p+ geom_vline(aes(xintercept=select_trend),
                  color="blue", linetype="dashed", size=1)
  })
  a=character()
  # Pull in description of trend
  if(Gender=="Female") a=paste(a,"This prediction is based on Male's data! This prediction may not be precise.\n")
  if(age<20|age>50) a=paste(a,"This prediction is based on the data age from 20-50, so it may be unprecise for you.\n")
  a=paste(a,"Your estimated bodyfat is",as.character(select_trend)) 
  output$desc <- renderText({
    a()
  })
}

shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
