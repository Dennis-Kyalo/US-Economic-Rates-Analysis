# LIBRARIES ----
library(shiny)
library(shinyWidgets)
library(shinythemes)


library(tidyquant)
library(tidyverse)
library(timetk)
library(plotly)
library(readr)


source(file = "00_scripts/economics_analysis.R")

# DATA ----
full_forecast_tbl  <<- read_rds("00_data/full_forecast_tbl.rds")
full_accuracy_tbl  <<- read_rds("00_data/full_accuracy_tbl.rds")


rate_picker <- function(id) {
    selectInput(
        inputId = id,
        label   = "Rates List (Pick one to Analyze)",
        choices = c(
            "INFLATION"             = "Inflation",
            "6-MONTH TREASURY BILL" = "Tbill",
            "BANK PRIME LOAN"       = "Bank",
            "30-YEAR FIXED RATE MORTGAGE AVERAGE" = "Mortgage",
            "LONG-TERM GOVERNMENT BOND YIELDS"    = "Bond"
        )
    )
}



# UI --------------------------------------------------------------------------
ui <- 
    navbarPage(
        
        title       = h3("Economics Analysis and Forecasting"),
        inverse     = FALSE,
        collapsible = TRUE,
        theme       = shinytheme("cyborg"),
        
        
        # 1.0 Tab Panel 1 -----------------------------------------------------------
        
        # * Side Panel -----------------
        tabPanel(
            title = h4("Forecast"),
            
            # CSS
            # shinythemes::themeSelector(),
            tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
            ),
            
            # JS 
            shinyjs::useShinyjs(),
            
            
            # ** Header 1 -------------
            div(class = "container",
                id    = "header_1",
                h1("U.S. Economic Rates Forecasts"),
                p("The graph displays a one-year projection of various economic 
              rates based on several forecasting methods."),
                hr()
            ),
            
            
            
            # ** Accuracy Metrics ------------
            div(
                class = "container",
                id    = "metrics_container",
                
                div(
                    column(
                        width = 12,
                        h5("Accuarcy Merics")
                    )
                ),
                div(
                    id    = "favorite_cards",
                    
                    fluidRow(
                        
                        column(
                            
                            width = 3,
                            info_card(title     = "RMSE", 
                                      value     =  verbatimTextOutput(outputId = "rmse_metric")
                            )
                        ),
                        column(
                            width = 3,
                            info_card(title     = "MAPE", 
                                      value     = verbatimTextOutput(outputId = "mape_metric")
                            )
                        ),
                        column(
                            width = 3,
                            info_card(title     = "MAE", 
                                      value     = verbatimTextOutput(outputId = "mae_metric")
                            )
                        ),
                        column(
                            width = 3,
                            info_card(title     = "RSQ", 
                                      value     = verbatimTextOutput(outputId = "rsq_metric") 
                            )
                        )
                    )
                    
                )
            ), 
            
            div(
                class = "container",
                id    = "left_1",
                
                fluidRow(
                    
                    # ** Rate Selection 1 ---------
                    column(
                        width = 3,
                        
                        wellPanel(
                            rate_picker(id = "rate_selection_1"),
                            
                            
                            # ** Model Selection ---------
                            selectInput(inputId = "model_selection",
                                        label   = "Model List (Pick one to Analyze)", 
                                        choices = c(
                                            "AUTO ARIMA"   = "MODEL:ARIMA",
                                            "ARIMA SEASONAL DECOMPOSITION" = "SEASONAL DECOMP: ARIMA",
                                            "ETS"     = "MODEL:ETS",
                                            "ETS SEASONAL DECOMPOSITION" = "SEASONAL DECOMP: ETS",
                                            "PROPHET" = "PROPHET",
                                            "TBATS"   = "BATS"
                                        ) , 
                                        
                            ),
                            
                            # ** Action Buttons 1 ----------
                            actionButton(inputId = "analyze_1", label = "Analyze", 
                                         icon = icon("play", class = "")),
                            actionButton(inputId = "reset_1", label = "Reset", 
                                         icon = icon("sync"), class = "btn pull-right")
                            
                        ),
                        
                        
                        # ** Data Description -----------
                        wellPanel(
                            
                            h4("About the data"),
                            hr(),
                            p("The data was acquired from the FRED’s Economic Data.",
                              br(),
                              br(),
                              a(href = "https://fred.stlouisfed.org/", class = "btn btn-primary", "Learn More"))
                            
                        )
                    ),
                    
                    
                    # * Main Panel ------------
                    # ** Plot Output 1 -----------
                    column(
                        width = 9, 
                        wellPanel(
                            div(
                                plotlyOutput(outputId = "plotly1", height = "100%")
                            ) 
                        )
                        
                    )
                    
                )
                
            )
            
        ),
        
        
        # 2.0 Tab Panel 2 -----------------------------------------------------------
        
        tabPanel(
            title = h4("Analysis"),
            
            # * Side Panel ----------------------------------------------
            # ** Header 2 ----------------
            div(class = "container",
                id    = "header_2",
                h1("U.S. Economic Rates Analysis"),
                p("The plot is used to compare various economic rates, and in particular, it demonstrates 
                  how the rate of inflation impacts other market interest rates."),
                hr()
            ),
            
            
            div(
                class = "container",
                id    = "left_2", 
                
                fluidRow(
                    
                    # ** Rate Selection 2 -------------------
                    column(
                        width = 3,
                        wellPanel(
                            tags$style(".my-class {font-size: 80%; line-height: 1.6;}"),
                            pickerInput(
                                inputId = "rate_selection_2",
                                label   = "Rates List (Select to Analyze)",
                                choices = c(
                                    "INFLATION"             = "inflation_rate",
                                    "6-MONTH TREASURY BILL" = "tbill_rate",
                                    "BANK PRIME LOAN"       = "bank_rate",
                                    "30-YEAR FIXED RATE MORTGAGE AVERAGE" = "mortgage_rate",
                                    "LONG-TERM GOVERNMENT BOND YIELDS"    = "bond_rate"
                                ), selected = "inflation_rate", 
                                multiple = TRUE, 
                                options = list(
                                    style = "my-class",
                                    `actions-box` = TRUE,
                                    `dropdown-align-right` = TRUE,
                                    `deselect-all-text` = "DESELECT",
                                    `select-all-text` = "SELECT",
                                    `none-selected-text` = "NONE SELECTED",
                                    `multiple-separator` = " | "
                                ),
                            ),
                            
                            
                            # ** Action Buttons 2 -----------------
                            actionButton(inputId = "analyze_2", label = "Analyze", icon = icon("play")),
                            actionButton(inputId = "reset_2", label = "Reset", 
                                         icon = icon("sync"), class = "btn pull-right")
                            
                        ),
                        
                        # ** Data Description -------------------
                        wellPanel(
                            h4("About the data"),
                            hr(),
                            p("The data was acquired from the FRED’s Economic Data.",
                              br(),
                              br(),
                              a(href = "https://fred.stlouisfed.org/", class = "btn btn-primary", "Learn More"))
                            
                        )
                    ),
                    
                    # * Main Panel --------------------------------------------
                    # ** Plot Output 2 ----------------
                    column(width = 9,
                           
                           div( class = "well",
                                style = "height: 550px;",
                                plotlyOutput(outputId = "plotly2", height = "100%")
                           )
                           
                           
                    )
                    
                )
            ) 
            
        ),
        
        
        
        # 3.0 Tab Panel 3 -----------------------------------------------------------
        
        tabPanel( 
            
            # * Side Panel ----------------------------------------
            
            title = h4("Diagnostics"),
            
            # ** Header 3 -------------------
            div(class = "container",
                id    = "header_3",
                h1("U.S. Economic Rates Analysis"),
                p("The plots show the Seasonal-Trend decomposition and Seasonal features of the 
                      economic rates respectively."),
                hr()
            ),
            
            div(
                class = "container",
                id    = "left_3",
                
                fluidRow(
                    column(
                        
                        # ** Rate Selection 3 -----------
                        width = 3,
                        wellPanel(
                            rate_picker(id = "rate_selection_3"),
                            actionButton(inputId = "analyze_3", label = "Analyze", icon = icon("play")),
                            actionButton(inputId = "reset_3", label = "Reset", 
                                         icon = icon("sync"), class = "btn pull-right")
                        ),
                        
                        # ** Data Description ------------
                        wellPanel(
                            h4("About the data"),
                            hr(),
                            p("The data was acquired from the FRED’s Economic Data.",
                              br(),
                              br(),
                              a(href = "https://fred.stlouisfed.org/", class = "btn btn-primary", "Learn More"))
                            
                        )
                        
                    ), 
                    
                    # * Main Panel ------------------------------------------
                    column(
                        width = 9, 
                        div(
                            class = "well",
                            tabsetPanel(
                                
                                # ** Plot Output 3 ---------------
                                tabPanel(
                                    title = "Decomposition",
                                    hr(),
                                    div(
                                        class = "panel_body",
                                        style = "height: 600px;",
                                        plotlyOutput(outputId = "plotly3", height = "100%", inline = TRUE)   
                                    )
                                ), 
                                
                                
                                # ** Plot Output 4 ---------------
                                tabPanel(
                                    
                                    title = "Seasonal",
                                    hr(),
                                    div(
                                        class = "panel_body",
                                        style = "height: 600px;",
                                        plotlyOutput(outputId = "plotly4", height = "100%", inline = TRUE)
                                        
                                    )
                                    
                                )
                                
                                
                            )  
                        )
                        
                        
                    ) 
                    
                )
                
 
            )
            
            
        )                
    )


# SERVER ---------------------------------------------------------------------------------

server <- function(input, output, session) {
    
      # * Tab Panel 1 -----------------------------------------------------------
    
    observeEvent(eventExpr = input$reset_1, handlerExpr = {
        
        updateSelectInput(
            session = session, 
            inputId = "rate_selection_1", 
            selected = "Inflation")
        
        updateSelectInput(
            session  = session, 
            inputId  = "model_selection",
            selected = "MODEL:ARIMA")
        
        
        shinyjs::delay(ms = 300, expr = {
            shinyjs::click(id = "analyze_1")
        })
        
        
    })
    
    
    rate_symbol_1 <- eventReactive(input$analyze_1, {
        input$rate_selection_1
    }, ignoreNULL = FALSE)
    
    model_symbol <- eventReactive(input$analyze_1, {
        input$model_selection
    }, ignoreNULL = FALSE)
    
    
    output$plotly1 <- renderPlotly({
        plot_forecast_function(title = rate_symbol_1(), model = model_symbol())
        
    })
    
    
    # Metrics Reactivity
    output$rmse_metric <- renderText(accuracy_metrics(title = rate_symbol_1(), 
                                                      model = model_symbol(), 
                                                      metric = "rmse"))
    
    output$mape_metric <- renderText(accuracy_metrics(title = rate_symbol_1(), 
                                                      model = model_symbol(), 
                                                      metric = "mape"))
    
    output$mae_metric <- renderText(accuracy_metrics(title = rate_symbol_1(), 
                                                     model = model_symbol(), 
                                                     metric = "mae"))
    
    output$rsq_metric <- renderText(accuracy_metrics(title = rate_symbol_1(), 
                                                     model = model_symbol(), 
                                                     metric = "rsq"))
    
    
    # * Tab Panel 2 -----------------------------------------------------------
    
    
    observeEvent(eventExpr = input$reset_2, handlerExpr = {
        
        updatePickerInput(
            session = session, 
            inputId = "rate_selection_2", 
            selected = "inflation_rate")
        
        
        shinyjs::delay(ms = 300, expr = {
            shinyjs::click(id = "analyze_2")
        })
        
        
    })
    
    
    rate_symbol_2 <- eventReactive(input$analyze_2, {
        input$rate_selection_2
    }, ignoreNULL = FALSE)
    
    output$plotly2 <- renderPlotly({
        plot_analytics_function(rate_symbol_2())
    })
    
    
    # * Tab Panel 3 -----------------------------------------------------------
    
    observeEvent(eventExpr = input$reset_3, handlerExpr = {
        
        updatePickerInput(
            session = session, 
            inputId = "rate_selection_3", 
            selected = "Inflation")
        
        
        shinyjs::delay(ms = 300, expr = {
            shinyjs::click(id = "analyze_3")
        })
        
        
    })
    
    
    
    rate_symbol_3 <- eventReactive(input$analyze_3, {
        input$rate_selection_3
    }, ignoreNULL = FALSE)
    
    output$plotly3 <- renderPlotly({
        plot_stl_function(rate_symbol_3())
    })
    
    output$plotly4 <- renderPlotly({
        plot_seasonal_function(rate_symbol_3())
    })
    
}




# RUN APP -----------------------------------------------------------------------
shinyApp(ui = ui, server = server)
