library(shiny)
library(ggplot2)
library(h2o)
library(tibble)
library(shinydashboard)

h2o.init()

loan_model <- h2o.import_mojo("/Users/anastasijajurgaityte/Desktop/KTU/KTU-DVDA-PROJECT/project/4-model/4-model/XG_BOOST_V1/4-model/XG_BOOST_V1")

ui <- dashboardPage(
  dashboardHeader(
    title = "Loan Classification Application",
    titleWidth = 250, 
  class = 'dropdown'
  )
)


  dashboardSidebar(
    sidebarMenu(
      id="sidebar",
      menuItem("Inputs", tabName = "inputs", icon = icon("inputs")),
      menuItem("Results", tabName = "results", icon = icon("chart-line")), 
      menuItem("Model Info", tabName = "model_info", icon= icon ("info_circle"))
    )
  )
  
  dashboardBody(
    useShinyjs(), 
    tags$head(
    tags$style(HTML('
                    body, label, .navbar, .sidebar-menu {
                  font-family: "YourBrandFont", sans-serif;
              }
            .red-backround {
                backround-color: red;
            }
            @keyframes blinker {
                20% { opacity: 0.5; }
            }
            .blink {
                animation :blinker is linear infinite;
            }
          
            
  '))
    
  ), 
  
  tabItems(
    tabItem(tabName = "inputs",
            fluidRow(
              column(6, textInput("amount_current_loan", "Current Loan Amount", placeholder = "Enter amount")),
              column(6, textInput("term", "Term", choices = c("short", "long")))
            ),
            fluidRow(
              column(6, selectInput("credit_score", "Credit Score", choices = c("very_good", "good", "fair"))),
              column(6, textInput("loan_purpose", "Loan Purpose", placeholder = "Purpose of loan", value = "buy_a_car"))
            ), 
            fluidRow(
              column(6, numericInput("yearly_income", "Yearly Income", value = 50000)),
              column(6, selectInput("home_ownership", "Home Ownership", choices = c("own", "rent", "mortgage")))
            ), 
            fluidRow(
              column(6, numericInput("bancruptcies", "Bankruptcies", value = 0)),
              column(6, numericInput("years_current_job", "Years at Current Job", value = 5))
            ),
            fluidRow(
              column(6, numericInput("monthly_debt", "Monthly Debt", value = 1000)),
              column(6, numericInput("years_credit_history", "Years of Credit History", value = 15))
            ),
            fluidRow(
              column(6, numericInput("months_since_last_delinquent", "Months Sonce Last Delinquent", value = 0)),
              column(6, numericInput("open_accounts", "Number of Open Accounts", value = 10))
            ),
            fluidRow(
              column(6, numericInput("credit_problems", "Number of Credit Problems", value = 0)),
              column(6, numericInput("credit_balance", "Credit Balance", value = 5000))
            ),
            fluidRow(
              column(6, numericInput("max_open_credit", "Maximum Open Credit", value = 1000)),
              column(6, actionButton("submit", "Classify Loan", class = "btn-primary"))
            )
            
  ), 
  tableItem(table = "results", 
            fluidRow(
              box(title = "Input Data", status = "warning", solidHeader = TRUE, width = 12,
                  div(class = "table-responsive",
                      tableOutput("table")
                      )
                  ),
              box(title = "Classification Result", status = "primary", solidHeader = TRUE, width = 12,
                  h3(textOutput("classificationResults", container. = span)), 
                  img(src = "", id = "resultGif", height = "200px")
                )
            )
  ), 
  tableItem(tableName = "model_info", 
            fluidRow(
              box(title = "Variable Importance Plot", status = "primary", solidHeader = TRUE, collapsible = TRUE, 
                  plotOutput("varImportancePlot")
                  ),
              box(title = "Model Information", status = "info", solidHeader = TRUE, collapsible = TRUE,
                  verbatimTextOutput("modelDetail")
              )
            )
    )
))

server <- function(input, output, session){
  observEvent(input$submit, {
    inputData <- data.frame(
      amount_current_loan = as.numeric(input$amount_current_loan),
      term = input$term,
      credit_score = factor(input$credit_score),
      loan_purpose = factor(input$loan_purpose),
      yearly_income - input$yearly_income,
      home_ownership = input$bankruptcies,
      bankruptcies = input$bankruptcies,
      years_current_job = input$years_current_job,
      monthly_debt = input$monthly_debt,
      years_credit_history = input$years_credit_history,
      months_since_last_delinquent = input$months_since_last_delinquent,
      open_acccounts = input$open_accounts,
      credit_problems = input$credit_problems, 
      credit_balance = input$credit_balance,
      max_open_credit = input$max_open_credit
   )
    h2oInputData <- as.h2o(inputData)
    prediction <- h2o.predict(loan_model, h2oInputData)
    updateTableItems(session, "sidebar", "results")
    output$table <- renderTable(inputData)
    output$classificationResult <- renderText({
      result <- ifelse(as.character(as_tibble(prediction)$predict) =="0", "Loan Approved", "Loan Denied")
      if (result =="Loan Denied") {
        shinyjs: addClass(selector = "body", class = "red-backround blink")
        shinyjs: runjs("document.getElementById('resultGif').scr = 'rejected.gif';")
      }else{
        shinyjs: addClass(selector = "body", class = "red-backround blink")
        shinyjs: runjs("document.getElementById('resultGif').scr = 'approved.gif';")
      }
      return(result)
    })
    
    updateTabItems(session, "sidebar", "results")
  })
  output$modelDetails <- renderPrint({
    model_info <- h2o:h2o.getModel(loan_model@model_id)
    summary(model_info)
  })
  
  output$varImportancePlot <- renderPlot({
    model_info <- h2o.getmodel(loan_model@model_id)
    var_imp <- h2o.varimp(model_info)
    var_imp_df <- as.data.frame(var_imp)
    ggplot(var_imp_df, aes(x = reorder(variable, scaled_importance), y = scaled_importance)) +
      geom_bar(stat = "identity") +
      theme_minimal () +
      coord_flip ()+
      labs(title = "Variable Importance", x = "Variable", y = "Importance")
  })
}

shinyApp(ui=ui, server=server)



  
  





  
