library(shiny)

ui <- fluidPage(
  titlePanel("Body Mass Index"),
  sidebarPanel(
    numericInput("wt","Weight in Kg", 70, min=30,max=200),
    numericInput("ht", 'height in cm', 165, min = 50, max =250),
    submitButton('Submint')
  ),
  mainPanel(
    h3("Result"),
    h4('Your weight'),
    verbatimTextOutput('inputvalue1'),
    h4('Your height'),
    verbatimTextOutput('inputvalue2'),
    h4('Your BMI is'),
    verbatimTextOutput("results")
  )
)

server <- function(input, output){
  bmi_calc <- function(weight, height){weight/(height/100)^2}
  output$inputvalue1 <- renderPrint({input$wt})
  output$inputvalue2 <- renderPrint({input$ht})
  output$results <- renderPrint({bmi_calc(input$wt, input$ht)})
}

shinyApp(ui = ui, server= server)
