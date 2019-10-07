library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
require('rsconnect')
require(ggplot2)
require("XML")
library (RCurl)


model3<-function(weight,abdomen,wrist){
  a=-24.394-0.084*weight+0.888*abdomen-1.302*wrist
  if(a<0) return(0)
  if(a>60) return(60)
  if(a<=60&a>=0) return(a)
}

model2<-function(weight,abdomen){
  a=-43.178-0.12*weight+0.904*abdomen
  if(a<0) return(0)
  if(a>60) return(60)
  if(a<=60&a>=0) return(a)
}

#url = getURL("https://raw.githubusercontent.com/jyan75/stat-628-module-2/master/data/cleanfile.csv?token=AMRRRJOVM7XQ2OKQMCKWR7S5TLFWQ")
trend_data <-read.csv("cleanfile.csv")


server <- function(input, output) {
  selected_trends <- reactive({
    req(input$WEIGHT)
    req(input$ABDOMEN)
    if(!input$smoother){
      model2(input$WEIGHT,input$ABDOMEN)
    }else{
      model3(input$WEIGHT,input$ABDOMEN,input$WRIST)
    }
  })
  
  output$lineplot <- renderPlot({
    p <- ggplot(trend_data, aes(x=BODYFAT)) + geom_density()
    p+ geom_vline(aes(xintercept=selected_trends()),
                     color="blue", linetype="dashed", size=1)
  }, height=400)
  a1<-reactive({
    req(input$gender)
    req(input$Years)
    a=character()
    if(input$gender=="Female") a=paste(a,"This prediction is based on Male's data!
                                 This prediction may not be precise.\n")
    if(input$Years<20|input$Years>50) a=paste(a,"This prediction is based on the data age from
                              20 to 50, so it may be imprecise for you.\n")
    if(selected_trends()==0 | selected_trends()==60) a=paste(a,"This may be inaccurate because
                                                          the result is extreme.\n")
    
    a=paste(a,"Your estimated bodyfat is ",as.character(selected_trends()),".",sep="")
    
    if(selected_trends()<4) a=paste(a,"Your bodyfat is Essential Fat.")
    if(selected_trends()>=4&selected_trends()<13) a=paste(a,"Your bodyfat is Atheletes.")
    if(selected_trends()>=13&selected_trends()<17) a=paste(a,"Your bodyfat is Fit.")
    if(selected_trends()>=17&selected_trends()<25) a=paste(a,"Your bodyfat is Acceptable.")
    if(selected_trends()>=25) a=paste(a,"Your bodyfat is Obese.")
    a
  })
  output$desc <- renderText({
    a1()
  })
}
