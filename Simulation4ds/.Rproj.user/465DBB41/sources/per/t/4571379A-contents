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