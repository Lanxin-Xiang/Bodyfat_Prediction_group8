#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
f <- function(input)
{
    #age
    age = input$Age
    #weight
    weight = 0
    if(input$weight=='g'){ weight = (input$Weight)/453.592}
    else if (input$weight=='kg') { weight = (input$Weight)/0.453592 }
    else  {weight = input$Weight}
    #height
    height = 0
    if(input$height=='cm'){ height = (input$Height)/2.54}
    else if(input$height=='m') {height = (input$Height)/0.0254}
    else if(input$height=='inch') {height = input$Height}
    #abdomen
    abdomen = 0
    if(input$abdomen=='inch'){ abdomen = (input$Abdomen)*2.54}
    else if (input$abdomen=='m') { abdomen = (input$Abdomen)/100 }
    else if (input$abdomen=='cm') {abdomen = input$Abdomen}
    #neck
    neck = 0
    if(input$neck=='inch'){ neck = (input$Neck)*2.54}
    else if (input$neck=='m') { neck = (input$Neck)/100 }
    else  {neck = input$Neck}
    #biceps
    biceps = 0
    if(input$biceps=='inch'){biceps = (input$Biceps)*2.54}
    else if (input$biceps=='m'){ biceps = (input$Biceps)/100 }
    else  {biceps = input$Biceps}
    #wrist
    wrist = 0
    if(input$wrist=='inch'){wrist = (input$Wrist)*2.54}
    else if (input$wrist=='m'){ biceps = (input$Wrist)/100 }
    else  {wrist = input$Wrist}
    #thigh
    thigh = 0
    if(input$thigh=='inch'){wrist = (input$Thigh)*2.54}
    else if (input$thigh=='m'){ biceps = (input$Thigh)/100 }
    else  {thigh = input$Thigh}
    #ankle
    ankle = 0
    if(input$ankle=='inch'){wrist = (input$Ankle)*2.54}
    else if (input$ankle=='m'){ biceps = (input$Ankle)/100 }
    else  {ankle = input$Ankle}
    #forearm
    forearm = 0
    if(input$forearm=='inch'){wrist = (input$Forearm)*2.54}
    else if (input$forearm=='m'){ biceps = (input$Forearm)/100 }
    else  {forearm = input$Forearm}
    
    result = 0
    
    if (age>=22 && age< 45 && (neck*biceps*wrist*thigh*ankle*forearm)==0)
        result=-0.14*weight-0.19*height+0.92*abdomen-28.12
    else if(age>=45 && age<60 && (neck*biceps*wrist*thigh*ankle*forearm)==0)
        result=-0.06*weight-0.14*height+0.82*abdomen-36.21
    else if(age>=60 && (neck*biceps*wrist*thigh*ankle*forearm)==0)
        result=-0.02*weight-0.17*height+0.56*abdomen-16.93
    else if(age>=22 && age< 45&&(neck*biceps*wrist*thigh*ankle*forearm)!=0)
        result= -0.39*height-0.55*neck+0.74*abdomen+0.30*biceps-2.2*wrist+29.07
    else if(age>=45 && age< 60&&(neck*biceps*wrist*thigh*ankle*forearm)!=0)
        result= 0.68*abdomen+0.29*thigh-1.32*ankle+0.59*forearm-1.8*wrist-15.27
    else if(age>=60&&(neck*biceps*wrist*thigh*ankle*forearm)!=0)
        result= -0.18*weight+0.77*neck+0.67*abdomen+1.81*ankle+0.75*biceps-0.8*forearm-82.07
    else if(age<0||weight<0||height<0||abdomen<0||neck<0||biceps<0||wrist<0||thigh<0||ankle<0||forearm<0)
        result= 'you input the error data, please keep all the data greater or equal to 0.'
    
    if (result<0)
        result= 'enter your correct body data'

    return(result)
}
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("BodyFat"),
    uiOutput("Developer1",container = h5),
    uiOutput("Developer2",container = h5),
    uiOutput("Developer3",container = h5),
    textOutput("String",container = h5),
    hr(),
    textOutput("String1",container = h3),
    fluidRow(
        column(2,
               numericInput("Age", label = h4("Age"), value = 0),
               ),
        column(2,
               numericInput("Weight", label = h4("Weight"), value = 0),
               selectInput(inputId = "weight", "Unit of Measurement", 
                           choices = c("lbs","kg","g"))
               ),
        column(2,
               numericInput("Height", label = h4("Height"), value = 0),
               selectInput(inputId = "height", "Unit of Measurement", 
                           choices = c("inch","cm","m"))
               ),
        column(2,
               numericInput("Abdomen", label = h4("Abdomen"), value = 0),
               selectInput(inputId = "abdomen", "Unit of Measurement", 
                           choices = c("inch","cm","m"))
               ),
    ),
    hr(),
    textOutput("String2",container = h3),
    fluidRow(
        column(2,
               numericInput("Neck", label = h3("Neck"), value = 0),
               selectInput(inputId = "neck", "Unit of Measurement", 
                           choices = c("inch","cm","m"))
               ),
        column(2,
               numericInput("Biceps", label = h3("Biceps"), value = 0),
               selectInput(inputId = "biceps", "Unit of Measurement", 
                           choices = c("inch","cm","m"))
               ),
        column(2,
               numericInput("Wrist", label = h3("Wrist"), value = 0),
               selectInput(inputId = "wrist", "Unit of Measurement", 
                           choices = c("inch","cm","m"))
               ),
        column(2,
               numericInput("Thigh", label = h3("Thigh"), value = 0),
               selectInput(inputId = "thigh", "Unit of Measurement", 
                           choices = c("inch","cm","m"))
        ),
        column(2,
               numericInput("Ankle", label = h3("Ankle"), value = 0),
               selectInput(inputId = "ankle", "Unit of Measurement", 
                           choices = c("inch","cm","m"))
        ),
        column(2,
               numericInput("Forearm", label = h3("Forearm"), value = 0),
               selectInput(inputId = "forearm", "Unit of Measurement", 
                           choices = c("inch","cm","m"))
        ),
    ),
    hr(),
    fluidRow(
        column(5,
               textOutput("Bodyfat",container=h3),
               verbatimTextOutput("bodyfat")),
               textOutput("table",container = h6),
               textOutput("table1",container = h6),
               textOutput("table2",container = h6),
               textOutput("table3",container = h6),
               textOutput("table4",container = h6),
    ),
    actionButton(
        inputId = "calculate",
        label = "Calculate!",
        style = "pill", 
        color = "danger"
    )
    
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    url1 <- a("he324@wisc.edu", href="http://he324@wisc.edu")
    output$Developer1 = renderUI({
        tagList("Yuyang He, ", url1)  
    })
    url2 <- a("tluo48@wisc.edu", href="http://tluo48@wisc.edu")
    output$Developer2 = renderUI({
        tagList("Tianyue Luo, ", url2)  
    })  
    url3 <- a("lxiang25@wisc.edu", href="http://lxiang@wisc.edu")
    output$Developer3 = renderUI({
        tagList("Lanxin Xiang, ", url3)  
    })  
    output$String = renderText({
        "input your body data and calculate your BodyFat"
    })
    output$String1 = renderText({
        "Enter your body data into the following four data frames, and you can calculate your BodyFat."
    })
    output$String2 = renderText({
        "If you know all the 6 data, you can additionally input the data into the following 6 data frames, then you can calculate your BodyFat more precisely. 
        Otherwise, keep the six data 0."
    })
    output$Bodyfat = renderText({
        "BodyFat"
    })

  #  output$bodyfat = renderPrint({
  #      f(input)
  #  })
    calculate_click <- 0
    calculate_text <- 0
    output$bodyfat <- renderPrint({
        if(input$calculate > calculate_click){
            calculate_click  <<-  calculate_click + 1
            calculate_text = f(input)
        }
        calculate_text
    })
    output$table <- renderText({
        "Reference Sheet"
    })
    output$table1 <- renderText({
        "6%~14%: Athletes"
    })
    output$table2 <- renderText({
        "14%~18%: Fit"
    })
    output$table3 <- renderText({
        "18%~25%: Average"
    })
    output$table4 <- renderText({
        "25%+: Obese"
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
