### UI for our shiny application
# It contains 8 inputs, each being specified below


ui <- fluidPage(

  # Application title
  titlePanel("Stock price prediction through co-evolution of stocks"),

  # Sidebar with a slider input
  sidebarLayout(
    h3("User inputs"),
    sidebarPanel(
      textInput(
        inputId = "response",
        label = "Stock to be predicted (Enter stock ID)",
        value = "AAPL"
      ),
      textInput(
        inputId = "predictor1",
        label = "1st Stock used to make prediction (Enter stock ID)",
        value = "AMZN"
      ),
      textInput(
        inputId = "predictor2",
        label = "2nd Stock used to make prediction (Enter stock ID)",
        value = "QCOM"
      ),
      textInput(
        inputId = "predictor3",
        label = "3rd Stock used to make prediction (Enter stock ID)",
        value = "INTC"
      ),
      textInput(
        inputId = "predictor4",
        label = "4th Stock used to make prediction (Enter stock ID)",
        value = "MSFT"
      ),
      numericInput(
        inputId = "shift",
        label = "The extend of a stock being non-Makovian (>=1)",
        value = 5
      ),
      textInput(
        inputId = "key",
        label = "Please provide your API key for Alpha Vantage API (https://www.alphavantage.co)",
        value = "AQ0HLGYS3N1NKBF5"
      ),
      checkboxGroupInput(
        inputId = "detail",
        label = "Show model details or not",
        choices = list("Yes" = 1, "No" = 2),
        selected = 2
      ),
      actionButton(
        inputId = "action",
        label = "Perform Regression"
      )
    )
  ),

  # Show outputs
  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel("Prediction", plotOutput("plot0")),
      tabPanel("Model Summary", verbatimTextOutput("summary")),
      tabPanel("Correlation map 1", plotOutput("plot1")),
      tabPanel("Correlation map 2", plotOutput("plot2")),
      tabPanel("Correlation map 3", plotOutput("plot3")),
      tabPanel("Correlation map 4", plotOutput("plot4")),
      tabPanel("Stock price time series", plotOutput("plot5"))
    )
  )
)
