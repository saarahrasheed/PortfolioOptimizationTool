#SMTT PORTFOLIO OPTIMIZATION TOOL

#This code is divided into 4 sections:
# SECTION 0: LOADING LIBRARIES AND DATA FORMATION
# SECTION 1: USERINTERFACE
# SECTION 2: SERVER SIDE
#This section is further sub-divided into two parts:
#           SECTION 2A: COMPUTATIONS 
#           SECTION 2B: RENDERING 
# SECTION 3: RUNNING THE APPLICATION

#All these parts are necessary for the formation of ShinyR.


#INSTRUCTIONS
# The application requires data provided in the form of excel .csv file. The data should contain company name in the
#first row, dates in the first column.

#Our file contains complete daily closing price data of 181 companies from 2009 to 2016. The prices are adjusted for dividends.
#For optimal performance, please select all companies. Also, you can choose to change the company names by replacing symbols in the
#checkerboxInput() function with the ones of your choice. You can only run SECTION 0 to generate company names and select from there
#as well.

#If no. of companies in the code are to be altered, for instance we have made a portfolio for 4 companies, if you wish to change that to 
#say 5 companies following changes in the code are required:
# 1. In SECTION 1, Editing the checkerboxInput() to include the company symbols you want so they can appear on the UI of the application
# 2. In SECTION 1, Adding additional numericInput() syntax to include share % invested for the 5th company. Make sure to name the
#     ID of the numericInput() is of the format percx. Where x will be 5 in this case.
# 3. In SECTION 2B, Edit the condition for validation input$perc1 + input$perc2 + input$perc3 + input$perc4 = 100 to
#    input$perc1 + input$perc2 + input$perc3 + input$perc4 + input$perc5 = 100 to ensure error handling. This condition ensures that
#    results are only displayed if the sum of the ratio of investment provided by user equals 100%. This particular condition is used 
#    for validating results of first tab panel, Optimized Results.


#SECTION 0: LOADING LIBRARIES AND DATA FORMATION

#Loading required libraries

library(shiny)

#Our core computation are done through fPortfolio library
require(fPortfolio)

#Reading data from local source, conversion and formation of data is done through the following libraries
require(timeSeries)
require(lubridate)
library(dplyr)
require(readr)

#Used for plots
require(PerformanceAnalytics)
require(corrplot)

#First, loading data from local storage. Edit file variable to store string of address of where the data is located
file <- 'D:/companies181.csv'
PSX0916Prices <- read.csv( file , header = TRUE, stringsAsFactors = FALSE)


#converts to S4 class then S3 class date
PSX0916Prices$Date <- as.timeDate(PSX0916Prices$Date)
PSX0916Prices$Date <- as_datetime(PSX0916Prices$Date)

#S4 class is easily wrapped up as a timeseries using as.timeseries function
PSXPrices <- as.timeSeries(x = PSX0916Prices) 

#separate out companies name which will be used ahead
companies_names <- colnames(PSXPrices)


#SECTION 1: USERINTERFACE

#Here we will design the main interface of the tool using Shiny.
#Shiny has three parts:
#ui.R: for designing user interface
#server.R: for calculations and computations as well as generating output
#   - computation
#   - rendering


ui <- fluidPage(
  titlePanel("SMTT Portfolio Optimization Tool"), #define title of tool
  sidebarLayout(                                  #Designing sidebar layout where input will be taken
    sidebarPanel(
      helpText("Please select companies you want to or have invested in along with
               a ratio of wealth invested. This information is necessary to optimize your
               portfolio."),
      
      #First we will request user to input their wealth (basically it will be how much they are willing to invest)
      numericInput("wealth", "Your Wealth", value = 10000, min = 1000, max = 100000, step = 500), 
      
      #Next we give option of being able to select which companies to invest in. Currently the application supports only 4 but the structure is scalable 
      checkboxGroupInput("comp", "Companies", choices = c("ENGRO",	"ICI",	"HINO",	"HBL")),
      
      #Next we give the option to the user to choose which strategy to use to invest. Here we have three options
      selectInput("strat", "Strategy", choices = c("Maximum Sharpe Ratio", "Maximum Return", "Minimum Risk")),
      
      #For maxreturn, target risk is required, so if the user chooses maxreturn strategy, only then this option to choose
      #target risk will appear. By default the value is set to be 0.1
      conditionalPanel(
      condition = "input.strat == 'Maximum Return'",
      sliderInput(
        "targetrisk", "Set Target Risk", value = 0.1, max = 1, min = 0.01, step = 0.01)
        ),
      
      #Next we provide input to user to select how much to invest in each companies. These are percentages.
      #By default, the values are set at 25% each. Max investment allowed in each company is 40%
      numericInput("perc1", "Percentage Invested in Company 1", value = 25, min = 0, max = 40),
      numericInput("perc2", "Percentage Invested in Company 2", value = 25, min = 0, max = 40),
      numericInput("perc3", "Percentage Invested in Company 3", value = 25, min = 0, max = 40),
      numericInput("perc4", "Percentage Invested in Company 4", value = 25, min = 0, max = 40)
    ),
    
    mainPanel(
      #Here we define what tabs to display and what each tab will contain. The Output() functions (e.g. textOutput), define what kind 
      #of output will be displayed in the respective tab.
      tabsetPanel(
        position = "Right",
        tabPanel(title = "Optimized Results", titlePanel("Returns Comparison"), textOutput("OpResults"), titlePanel("Weights Selected"), textOutput("unoptspecifics"), 
                 titlePanel("Weights Assigned"), textOutput("Specifics"), titlePanel("Wealth Generated"), textOutput("WealthG")),
        tabPanel("Graphs",titlePanel("Returns Plot"), plotOutput("returnsplot"), titlePanel("Drawdown Plot"), plotOutput("ddplot")),
        tabPanel("Efficient Frontier", plotOutput("PFFPlot")),
        tabPanel("Weights Plot", plotOutput("WeightsPlot")),
        tabPanel("Correlation Matrix", plotOutput("corr"))
      )
    )
    #defining the position of the input panel
  ), position = "left"
)

#SECTION 2: SERVER SIDE 
#All computations will be performed in the server function. The function itself contains reactive variables which update their values
#based on the user input provided.

server <- function(input, output){
  
  #SECTION 2A: Computations
  
  #First variable defined is companies_return(). This variable is computing Daily Returns of the companies selected by first subsetting
  #the pricing data from the orginal data file loaded in the application.
  companies_return <- reactive({
    companies.matched <- match(input$comp, companies_names)
    companies.data.subset <- PSXPrices[, companies.matched]
    class(companies.data.subset)
    DailyReturns <- returns(companies.data.subset)

  })
  
  #This variable determines which strategy the user chose and based on that computes a portfolio of data type FPORTFOLIO. 
  #The returned variable is used ahead.
  designed_portfolio <- reactive(
    {
      strat <- input$strat
      designedportfolio <- NULL
      if(strat == "Maximum Sharpe Ratio")
        {designedportfolio <- tangencyPortfolio(companies_return())
        } #specify covariance estimators and try for different and compare output
      if(strat == "Minimum Risk") 
      {designedportfolio <- minvariancePortfolio(companies_return())
        }
      if(strat == "Maximum Return")
      {  Spec <- NULL
      Spec<- portfolioSpec()
      setTargetRisk(Spec) <- input$targetrisk
      designedportfolio <- maxreturnPortfolio(companies_return(), spec = Spec)
      }
             
      portf <- designedportfolio@portfolio
      portf
      #for personal reference:
      #using @ operator to access the appropriate portion
      #@description can be used to keep a log
      #accessing the appropriate slot, finally subsetting the slot with $
    }
  )

  #Now extracting the weights of the portfolio optimized.
  portfolio_weights <- reactive({
    specifications <- designed_portfolio()
    slotting <- slot(specifications, "portfolio")
    weightage <- slotting$weights 
    weightage
  })
  
  #Here we are determining how much is our overall return provided the ratio of investment is the one the user provided. Therefore,
  #these results are unoptimized.
  unopt_return <- reactive({ #decide how to preview wealth. Should I split the function?
    dailyreturncomp <- companies_return()
    perc_invested <- c(input$perc1, input$perc2, input$perc3, input$perc4)
    AvgDailyReturn <- colMeans(companies_return())
    Daysperyear <- 1983/8
    AnnualizedReturn <- AvgDailyReturn*Daysperyear
    returns_selected <- AnnualizedReturn
    returncumm <- crossprod(perc_invested, returns_selected)
    returncumm
  })
  
  
  #Here we are determining how much our overall return is provided the ratio of investment is the weights we computed using optimization 
  #techniques. Therefore, these results are optimized
  opt_return <- reactive({
    dailyreturncomp <- companies_return()
    perc_invested <- portfolio_weights()*100
    AvgDailyReturn <- colMeans(companies_return())
    Daysperyear <- 1983/8
    AnnualizedReturn <- AvgDailyReturn*Daysperyear
    returns_selected <- AnnualizedReturn
    returncummopt <- crossprod(perc_invested, returns_selected)
    returncummopt
  })
  
  #SECTION 2B: RENDERING
  #The following portion renders the output generated in section 2A by bringing them in the format that can be read by UI i.e. section 1.
  
  #The results for first tab, Optimized Results will be rendered here. We have used renderPrint() 
  #because we need to print text as well as user input that can not be simply coerced into a string using cat() function. 
  output$OpResults <- renderPrint({
    #For error handling, we have used validate() function to specify what text to display as an alternative to error 
    #When running the application.
    validate(
      need(input$comp, "Please select companies to begin optimization. Default % invested are set to 25%."),
      need(input$perc1 + input$perc2 + input$perc3 + input$perc4 == 100, "Please choose a share of investment that equals 100%.")
    )
    
    #A comparison of unopt.returns and opt.returns are shown under the heading "Returns Comparison
    print(paste("The unoptimized return on investment based on the information provided is", unopt_return(), "% annually.", "Whereas, the portfolio generated after optimization by strategy", input$strat, "gives return", opt_return() , "% annually"))})
  
  #Here we will generate plots of returns of the companies selected
  output$returnsplot <- renderPlot({
    
    #in case no companies are selected, validate() will display this error message
    validate(
      need(input$comp, "Please select companies to begin optimization. Default % invested are set to 25%.")
    )
    
    #After validation, returns wil be plotted. Type specifies what kind of plot we are using. Here we have used line plot denoted by "l"
    plot(x = companies_return(), y = companies_return(), type = "l",
         xlab = "Time", ylab = "Daily Returns")
  }) 
  
  #Here we will generate max. drawdowns plot of the companies selected. Similar validation will be performed before execution
  output$ddplot <- renderPlot({
    validate(
      need(input$comp, "Please select companies to begin optimization. Default % invested are set to 25%.")
    )
    dd <- drawdowns(companies_return())
    plot(dd)
  })
  
  #A plot showing weights of the companies assigned in relation to the return and risk of the portfolio will be computed here
  output$WeightsPlot <- renderPlot({
    validate(
      need(input$comp, "Please select companies to begin optimization. Default % invested are set to 25%.")
    )
    pf <- portfolioFrontier(companies_return())
    weightsPlot(pf)
  })
  
  #Efficient frontier showing all efficient portfolio combinations will be plotted here. Similar validation is being performed
  output$PFFPlot <- renderPlot({
    validate(
      need(input$comp, "Please select companies to begin optimization. Default % invested are set to 25%.")
    )
    pf <- portfolioFrontier(companies_return())
    frontierPlot(pf, col = c('red', 'blue'), pch = 10)
  })
  
  #To show the correlation between the companies we have chosen to plot a correlogram through package corrplot. 
  #This will visualize the correlation between the companies. Similar validation is being performed
  output$corr <- renderPlot({
    validate(
      need(input$comp, "Please select companies to begin optimization. Default % invested are set to 25%.")
    )
    matt <- cor(companies_return())
    corrplot(matt, method = "number")
  })
  
  #Here further results for the first tab are generated, that is weights generated.
  #We used renderText() instead of renderPrint() because the output involved can be simply coerced into a string.
  output$Specifics <- renderText({
    validate(
      need(input$comp, "Please select companies to begin optimization. Default % invested are set to 25%."),
      need(input$perc1 + input$perc2 + input$perc3 + input$perc4 == 100, "Please choose a share of investment that equals 100%.")
    )
    print(paste(input$comp, portfolio_weights()*100))
  })
  
  #Here further results for the first tab are generated, that is weights assigned by the user. 
  output$unoptspecifics <- renderText({
    validate(
      need(input$comp, "Please select companies to begin optimization. Default % invested are set to 25%."),
      need(input$perc1 + input$perc2 + input$perc3 + input$perc4 == 100, "Please choose a share of investment that equals 100%."),
      need(length(input$comp) == 4, "Please select all companies for which % is specified.")  
    )
    x <- c(input$perc1, input$perc2, input$perc3, input$perc4)
    print(paste(input$comp, x))
  })
  
  #Here further results for the first tab are generated, that is a change in wealth after investing in the companies as per our optimized weights
  output$WealthG <- renderText({
    validate(
      need(input$comp, "Please select companies to begin optimization. Default % invested are set to 25%."),
      need(input$perc1 + input$perc2 + input$perc3 + input$perc4 == 100, "Please choose a share of investment that equals 100%."),
      need(length(input$comp) == 4, "Please select all companies for which % is specified.")  
    )
    profitt <- input$wealth * opt_return()
    profit <- round(profitt, 2)
    print(paste("Wealth invested is PKR", input$wealth, ". After investing as per optimized weights, the expected wealth will be PKR", profit))
  })
  
}


#SECTION 3: RUNNING THE APPLICATION

#Here we simply call the shinyApp() function and pass our ui() and server() as arguments in order to run the application
shinyApp(ui = ui, server = server) 


