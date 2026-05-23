library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(MASS)
library(tidyverse)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Retirement Calculator"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Portfolio Setup", tabName = "portfolio", icon = icon("wallet")),
      menuItem("Simulation Results", tabName = "results", icon = icon("chart-line")),
      menuItem("Help & Documentation", tabName = "help", icon = icon("question-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .info-box {
          min-height: 60px;
        }
      "))
    ),
    
    tabItems(
      # Portfolio Setup Tab
      tabItem(tabName = "portfolio",
              fluidRow(
                box(
                  title = "Portfolio Configuration",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  
                  fluidRow(
                    column(4,
                           numericInput("current_age", "Current Age:", value = 30, min = 18, max = 100),
                           numericInput("annual_expenses", "Annual Expenses ($):", value = 60000, min = 1000),
                           numericInput("inflation_rate", "Expected Inflation Rate (%):", value = 3, min = 0, max = 10, step = 0.1),
                           numericInput("num_simulations", "Number of Simulations:", value = 1000, min = 100, max = 10000, step = 100)
                    ),
                    column(8,
                           h4("ETF Holdings"),
                           helpText("Enter expected nominal arithmetic returns, not log returns, before inflation. The calculator will automatically convert them for GBM."),
                           numericInput("num_etfs", "Number of ETFs:", value = 3, min = 1, max = 10),
                           br(),
                           uiOutput("etf_inputs")
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Correlation Matrix",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  helpText("Enter correlation values between -1 and 1. Matrix must be symmetric with diagonal = 1."),
                  uiOutput("correlation_matrix")
                ),
                
                box(
                  title = "Tax Drag Settings",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  helpText("Enter annual tax drag as a percentage for each ETF (reduces returns)."),
                  uiOutput("tax_drag_inputs")
                )
              ),
              
              fluidRow(
                box(
                  width = 12,
                  actionButton("run_simulation", "Run Simulation", class = "btn-success btn-lg", icon = icon("play")),
                  br(),
                  br(),
                  verbatimTextOutput("validation_messages")
                )
              )
      ),
      
      # Results Tab
      tabItem(tabName = "results",
              fluidRow(
                valueBoxOutput("retirement_age_box"),
                valueBoxOutput("retirement_probability_box"),
                valueBoxOutput("portfolio_value_box")
              ),
              
              fluidRow(
                box(
                  title = "Portfolio Value Projection",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("portfolio_projection", height = "400px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Probability Distribution at Key Ages",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("probability_distribution", height = "350px")
                ),
                
                box(
                  title = "Retirement Target Achievement Probability",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("retirement_probability", height = "350px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Detailed Statistics",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  DT::dataTableOutput("statistics_table")
                )
              )
      ),
      
      # Help Tab
      tabItem(tabName = "help",
              fluidRow(
                box(
                  title = "How to Use This Calculator",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  h4("Overview"),
                  p("This Financial Independence/Retire Early (FIRE) calculator uses Monte Carlo simulation with multivariate geometric Brownian motion to project your portfolio's growth and estimate when you can retire."),
                  
                  h4("Key Concepts"),
                  tags$ul(
                    tags$li(strong("4% Rule:"), " The Trinity Study suggests you can safely withdraw 4% of your portfolio annually in retirement, requiring 25× your annual expenses."),
                    tags$li(strong("Age-Adjusted Target:"), " The calculator adjusts the 25× rule based on your age. Before age 49, you need more than 25× (longer retirement). After 49, you need less (shorter retirement)."),
                    tags$li(strong("Monte Carlo Simulation:"), " Runs thousands of random scenarios to show the range of possible outcomes."),
                    tags$li(strong("Tax Drag:"), " Annual percentage reduction in returns due to taxes (capital gains, dividends, etc.).")
                  ),
                  
                  h4("Input Fields"),
                  tags$ul(
                    tags$li(strong("ETF Details:"), " Enter ticker symbols, current balances, and regular contribution amounts."),
                    tags$li(strong("Contribution Frequency:"), " Choose how often you contribute. Annual contributions are made on January 1st for maximum compounding."),
                    tags$li(strong("Expected Return & Volatility:"), " Enter arithmetic, not log, nominal returns before inflation. Vaues are converted to log returns for GBM. S&P 500 historically ~10% arithmetic return, ~15% volatility."),
                    tags$li(strong("Correlation Matrix:"), " How ETFs move together. 1 = perfect correlation, 0 = independent, -1 = inverse correlation."),
                    tags$li(strong("Tax Drag:"), " Estimate based on your tax bracket and ETF distributions. Typically 0.5-2% annually. Applied as a multiplicative reduction to returns.")
                  ),
                  
                  h4("Outputs Explained"),
                  tags$ul(
                    tags$li(strong("Retirement Age:"), " Median age when your portfolio reaches the retirement target."),
                    tags$li(strong("Success Probability:"), " Percentage of simulations achieving the target by age 65."),
                    tags$li(strong("Portfolio Projections:"), " Shows both nominal (future dollars) and real (today's dollars) values."),
                    tags$li(strong("Probability Distributions:"), " Range of possible portfolio values at different ages.")
                  ),
                  
                  h4("Tips for Accuracy"),
                  tags$ul(
                    tags$li("Use realistic nominal return expectations (e.g., 10% for U.S. equities, 3-5% for bonds)."),
                    tags$li("Include all investment accounts in your portfolio."),
                    tags$li("Account for major future expenses, including medical ones in retirement, in your annual spending estimate."),
                    tags$li("Run more simulations (5000+) for more reliable results."),
                    tags$li("Update correlations based on asset classes (stocks typically 0.7-0.9 with each other).")
                  )
                )
              )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive values to store simulation results
  simulation_results <- reactiveValues(
    data = NULL,
    summary = NULL
  )
  
  # Dynamic ETF inputs
  output$etf_inputs <- renderUI({
    n <- input$num_etfs
    if (is.null(n)) return()
    
    lapply(1:n, function(i) {
      fluidRow(
        column(2,
               textInput(paste0("etf_name_", i), paste("ETF", i, "Symbol:"), 
                         value = if(i == 1) "VTI" else if(i == 2) "VXUS" else if(i == 3) "BND" else paste0("ETF", i))
        ),
        column(2,
               numericInput(paste0("initial_balance_", i), "Principal ($):", 
                            value = if(i == 1) 100000 else if(i == 2) 50000 else 25000, min = 0)
        ),
        column(2,
               numericInput(paste0("contribution_", i), "Contribution ($):", 
                            value = if(i == 1) 1000 else 500, min = 0)
        ),
        column(2,
               selectInput(paste0("frequency_", i), "Frequency:", 
                           choices = c("Biweekly" = 26, "Monthly" = 12, "Annual" = 1),
                           selected = 12)
        ),
        column(2,
               numericInput(paste0("expected_return_", i), "Return (%):", 
                            value = if(i == 1) 10 else if(i == 2) 9 else 4, min = -20, max = 30, step = 0.5)
        ),
        column(2,
               numericInput(paste0("volatility_", i), "Volatility (%):", 
                            value = if(i == 1) 15 else if(i == 2) 17 else 5, min = 0, max = 50, step = 0.5)
        )
      )
    })
  })
  
  # Dynamic correlation matrix - More than 6 ETF makes input boxes too small
  output$correlation_matrix <- renderUI({
    n <- input$num_etfs
    if (is.null(n)) return()
    
    # Calculate column width - ensure minimum width for usability
    col_width <- max(1, floor(10/n))
    if (n > 5) {
      # For many ETFs, use a scrollable container
      matrix_content <- div(style = "overflow-x: auto;",
                            # Create header row
                            fluidRow(
                              column(2, ""),  # Empty corner
                              lapply(1:n, function(j) {
                                etf_name <- input[[paste0("etf_name_", j)]]
                                if (is.null(etf_name) || etf_name == "") etf_name <- paste("ETF", j)
                                column(width = col_width, 
                                       strong(etf_name),
                                       style = "text-align: center; min-width: 80px;")
                              })
                            ),
                            
                            tags$hr(style = "margin: 5px 0;"),
                            
                            # Create matrix rows
                            lapply(1:n, function(i) {
                              etf_name <- input[[paste0("etf_name_", i)]]
                              if (is.null(etf_name) || etf_name == "") etf_name <- paste("ETF", i)
                              
                              row_cells <- lapply(1:n, function(j) {
                                if (i == j) {
                                  cell_input <- numericInput(
                                    paste0("corr_", i, "_", j),
                                    label = NULL,
                                    value = 1,
                                    min = 1, max = 1,
                                    width = "90%"
                                  )
                                } else if (i < j) {
                                  default_val <- if(i == 1 && j == 2) 0.7 else if(i == 1 && j == 3) 0.3 else if(i == 2 && j == 3) 0.2 else 0.5
                                  cell_input <- numericInput(
                                    paste0("corr_", i, "_", j),
                                    label = NULL,
                                    value = default_val,
                                    min = -1, max = 1, step = 0.01,
                                    width = "90%"
                                  )
                                } else {
                                  cell_input <- numericInput(
                                    paste0("corr_", i, "_", j),
                                    label = NULL,
                                    value = 0,
                                    min = -1, max = 1, step = 0.01,
                                    width = "90%"
                                  )
                                }
                                column(width = col_width, cell_input, style = "min-width: 80px;")
                              })
                              
                              fluidRow(
                                column(2, strong(etf_name), style = "padding-top: 8px;"),
                                row_cells
                              )
                            })
      )
      return(matrix_content)
    } else {
      # Original layout for 5 or fewer ETFs
      header_row <- fluidRow(
        column(2, ""),  # Empty corner
        lapply(1:n, function(j) {
          etf_name <- input[[paste0("etf_name_", j)]]
          if (is.null(etf_name) || etf_name == "") etf_name <- paste("ETF", j)
          column(width = floor(10/n), 
                 strong(etf_name),
                 style = "text-align: center;")
        })
      )
      
      matrix_rows <- lapply(1:n, function(i) {
        etf_name <- input[[paste0("etf_name_", i)]]
        if (is.null(etf_name) || etf_name == "") etf_name <- paste("ETF", i)
        
        row_cells <- lapply(1:n, function(j) {
          if (i == j) {
            cell_input <- numericInput(
              paste0("corr_", i, "_", j),
              label = NULL,
              value = 1,
              min = 1, max = 1,
              width = "90%"
            )
          } else if (i < j) {
            default_val <- if(i == 1 && j == 2) 0.7 else if(i == 1 && j == 3) 0.3 else if(i == 2 && j == 3) 0.2 else 0.5
            cell_input <- numericInput(
              paste0("corr_", i, "_", j),
              label = NULL,
              value = default_val,
              min = -1, max = 1, step = 0.01,
              width = "90%"
            )
          } else {
            cell_input <- numericInput(
              paste0("corr_", i, "_", j),
              label = NULL,
              value = 0,
              min = -1, max = 1, step = 0.01,
              width = "90%"
            )
          }
          column(width = floor(10/n), cell_input)
        })
        
        fluidRow(
          column(2, strong(etf_name), style = "padding-top: 8px;"),
          row_cells
        )
      })
      
      tagList(
        header_row,
        tags$hr(style = "margin: 5px 0;"),
        matrix_rows
      )
    }
  })
  
  # Dynamic tax drag inputs
  output$tax_drag_inputs <- renderUI({
    n <- input$num_etfs
    if (is.null(n)) return()
    
    lapply(1:n, function(i) {
      etf_name <- input[[paste0("etf_name_", i)]]
      if (is.null(etf_name)) etf_name <- paste("ETF", i)
      
      fluidRow(
        column(6,
               strong(etf_name)
        ),
        column(6,
               numericInput(paste0("tax_drag_", i), "Tax Drag (%):", 
                            value = 0.5, min = 0, max = 5, step = 0.1)
        )
      )
    })
  })
  
  # Synchronize correlation matrix
  observe({
    n <- input$num_etfs
    if (is.null(n)) return()
    
    for (i in 1:n) {
      for (j in 1:n) {
        if (i < j) {
          observeEvent(input[[paste0("corr_", i, "_", j)]], {
            updateNumericInput(session, paste0("corr_", j, "_", i), 
                               value = input[[paste0("corr_", i, "_", j)]])
          })
        }
      }
    }
  })
  
  # Run simulation with progress bar
  observeEvent(input$run_simulation, {
    
    # Validate inputs
    validation_msg <- ""
    n <- input$num_etfs
    
    # Build correlation matrix
    corr_matrix <- matrix(0, n, n)
    for (i in 1:n) {
      for (j in 1:n) {
        if (i <= j) {
          corr_matrix[i, j] <- input[[paste0("corr_", i, "_", j)]]
          corr_matrix[j, i] <- corr_matrix[i, j]
        }
      }
    }
    
    # Check if correlation matrix is valid
    eigen_vals <- eigen(corr_matrix)$values
    if (any(eigen_vals < -1e-8)) {
      validation_msg <- "Error: Correlation matrix is not positive semi-definite. Please check your correlation values."
      output$validation_messages <- renderText(validation_msg)
      return()
    }
    
    # Gather ETF data
    etf_data <- data.frame(
      name = character(n),
      initial_balance = numeric(n),
      contribution = numeric(n),
      frequency = numeric(n),
      expected_return = numeric(n),
      volatility = numeric(n),
      tax_drag = numeric(n),
      stringsAsFactors = FALSE
    )
    
    for (i in 1:n) {
      etf_data$name[i] <- input[[paste0("etf_name_", i)]]
      etf_data$initial_balance[i] <- input[[paste0("initial_balance_", i)]]
      etf_data$contribution[i] <- input[[paste0("contribution_", i)]]
      etf_data$frequency[i] <- as.numeric(input[[paste0("frequency_", i)]])
      etf_data$expected_return[i] <- input[[paste0("expected_return_", i)]] / 100
      etf_data$volatility[i] <- input[[paste0("volatility_", i)]] / 100
      etf_data$tax_drag[i] <- input[[paste0("tax_drag_", i)]] / 100
    }
    
    # Run Monte Carlo simulation with progress indicator
    withProgress(message = 'Running simulation', value = 0, {
      results <- run_monte_carlo(
        etf_data = etf_data,
        corr_matrix = corr_matrix,
        current_age = input$current_age,
        annual_expenses = input$annual_expenses,
        inflation_rate = input$inflation_rate / 100,
        num_simulations = input$num_simulations,
        max_years = max(0, 65 - input$current_age),
        session = session
      )
    })
    
    simulation_results$data <- results$simulations
    simulation_results$summary <- results$summary
    
    output$validation_messages <- renderText("Simulation completed successfully!")
    
    # Switch to results tab
    updateTabItems(session, "tabs", "results")
  })
  
  # Monte Carlo simulation function with corrected math
  run_monte_carlo <- function(etf_data, corr_matrix, current_age, annual_expenses, 
                              inflation_rate, num_simulations, max_years, session = NULL) {
    
    n_etfs <- nrow(etf_data)
    years <- seq(0, max_years, by = 1/12)  # Monthly steps
    n_periods <- length(years)
    dt <- 1/12  # Monthly time step
    
    # Initialize results array
    portfolio_values <- matrix(0, num_simulations, n_periods)
    
    # Calculate GBM parameters with proper tax drag treatment
    log_drifts <- numeric(n_etfs)
    gbm_sigmas <- numeric(n_etfs)
    
    for (i in 1:n_etfs) {
      # Apply tax drag multiplicatively: gross_factor = (1 + r) * (1 - tax_drag)
      gross_factor_after_tax <- (1 + etf_data$expected_return[i]) * (1 - etf_data$tax_drag[i])
      
      # Convert to log drift for GBM
      log_drifts[i] <- log(gross_factor_after_tax)
      
      # For GBM, calculate the volatility of log returns
      # More accurate: σ_log = sqrt(log(1 + (σ_arith/(1+r))^2))
      arith_return <- etf_data$expected_return[i]
      arith_vol <- etf_data$volatility[i]
      
      # Using the more accurate formula
      gbm_sigmas[i] <- sqrt(log(1 + (arith_vol / (1 + arith_return))^2))
    }
    
    # Generate correlated random numbers
    set.seed(123)  # For reproducibility
    
    # Simulation progress tracking
    progress_step <- max(1, floor(num_simulations / 20))
    
    # Contributions are added BEFORE growth calculations to ensure proper compounding.
    # - Annual: Added at beginning of each year (maximum compounding benefit)
    # - Monthly: Added at beginning of each month
    # - Biweekly: Added every 2 weeks (26 times per year)
    
    for (sim in 1:num_simulations) {
      # Update progress bar
      if (!is.null(session) && sim %% progress_step == 0) {
        incProgress(1/20, detail = paste("Simulation", sim, "of", num_simulations))
      }
      
      # Generate correlated Brownian motion
      Z <- matrix(rnorm(n_periods * n_etfs), n_periods, n_etfs)
      L <- t(chol(corr_matrix))
      W <- Z %*% t(L)
      
      # Initialize ETF values
      etf_values <- matrix(0, n_periods, n_etfs)
      etf_values[1, ] <- etf_data$initial_balance
      
      # Simulate each ETF
      for (t in 2:n_periods) {
        for (i in 1:n_etfs) {
          # Add contributions at the beginning of the period (before growth)
          contribution_added <- FALSE
          
          if (etf_data$frequency[i] == 1) {
            # Annual: Contribute at the beginning of each year (month 1, 13, 25, ...)
            if ((t - 1) %% 12 == 0) {
              etf_values[t-1, i] <- etf_values[t-1, i] + etf_data$contribution[i]
              contribution_added <- TRUE
            }
          } else if (etf_data$frequency[i] == 12) {
            # Monthly: Contribute at the beginning of each month
            etf_values[t-1, i] <- etf_values[t-1, i] + etf_data$contribution[i]
            contribution_added <- TRUE
          } else if (etf_data$frequency[i] == 26) {
            # Biweekly: Contribute every 2 weeks (approximately every 0.46 months)
            months_elapsed <- (t - 1)
            biweekly_periods_elapsed <- floor(months_elapsed * 26 / 12)
            prev_months_elapsed <- max(0, t - 2)
            prev_biweekly_periods <- floor(prev_months_elapsed * 26 / 12)
            
            if (biweekly_periods_elapsed > prev_biweekly_periods) {
              etf_values[t-1, i] <- etf_values[t-1, i] + etf_data$contribution[i]
              contribution_added <- TRUE
            }
          }
          
          # Apply geometric Brownian motion growth
          drift_term <- (log_drifts[i] - 0.5 * gbm_sigmas[i]^2) * dt
          diffusion_term <- gbm_sigmas[i] * sqrt(dt) * W[t, i]
          etf_values[t, i] <- etf_values[t-1, i] * exp(drift_term + diffusion_term)
        }
      }
      
      # Sum portfolio value
      portfolio_values[sim, ] <- rowSums(etf_values)
    }
    
    # Calculate summary statistics
    summary_stats <- data.frame(
      age = current_age + years,
      year = years,
      median = apply(portfolio_values, 2, median),
      mean = apply(portfolio_values, 2, mean),
      p10 = apply(portfolio_values, 2, quantile, probs = 0.1),
      p25 = apply(portfolio_values, 2, quantile, probs = 0.25),
      p75 = apply(portfolio_values, 2, quantile, probs = 0.75),
      p90 = apply(portfolio_values, 2, quantile, probs = 0.9)
    )
    
    # Calculate inflation-adjusted values
    summary_stats$median_real <- summary_stats$median / (1 + inflation_rate)^summary_stats$year
    summary_stats$mean_real <- summary_stats$mean / (1 + inflation_rate)^summary_stats$year
    
    # Calculate retirement target (age-adjusted)
    summary_stats$retirement_target <- calculate_retirement_target(
      summary_stats$age, 
      annual_expenses, 
      inflation_rate, 
      summary_stats$year
    )
    
    return(list(
      simulations = portfolio_values,
      summary = summary_stats
    ))
  }
  
  # Calculate age-adjusted retirement target
  calculate_retirement_target <- function(age, annual_expenses, inflation_rate, years) {
    life_expectancy <- 79
    retirement_years <- pmax(life_expectancy - age, 1)
    
    # Adjust multiplier based on expected retirement length
    # At age 49: 25× (30 years retirement)
    # Before 49: more than 25× 
    # After 49: less than 25×
    base_multiplier <- 25
    base_retirement_years <- 30
    
    multiplier <- base_multiplier * (retirement_years / base_retirement_years)
    multiplier <- pmin(multiplier, 35)  # Cap at 35×
    
    # Adjust for inflation
    inflated_expenses <- annual_expenses * (1 + inflation_rate)^years
    target <- multiplier * inflated_expenses
    
    return(target)
  }
  
  # Value boxes
  output$retirement_age_box <- renderValueBox({
    if (is.null(simulation_results$summary)) {
      valueBox(
        value = "N/A",
        subtitle = "Median Retirement Age",
        icon = icon("calendar"),
        color = "blue"
      )
    } else {
      # Find median retirement age
      summary_data <- simulation_results$summary
      retirement_achieved <- summary_data$median >= summary_data$retirement_target
      
      if (any(retirement_achieved)) {
        retirement_age <- round(summary_data$age[which(retirement_achieved)[1]], 1)
        color <- if (retirement_age < 50) "green" else if (retirement_age < 60) "yellow" else "red"
      } else {
        retirement_age <- "Not achieved"
        color <- "red"
      }
      
      valueBox(
        value = retirement_age,
        subtitle = "Median Retirement Age",
        icon = icon("calendar"),
        color = color
      )
    }
  })
  
  output$retirement_probability_box <- renderValueBox({
    if (is.null(simulation_results$data)) {
      valueBox(
        value = "N/A",
        subtitle = "Success Rate by 65",
        icon = icon("percentage"),
        color = "blue"
      )
    } else {
      # Calculate probability of retirement by age 65
      summary_data <- simulation_results$summary
      age_65_idx <- which.min(abs(summary_data$age - 65))
      
      if (length(age_65_idx) > 0) {
        target_at_65 <- summary_data$retirement_target[age_65_idx]
        portfolio_at_65 <- simulation_results$data[, age_65_idx]
        success_rate <- round(mean(portfolio_at_65 >= target_at_65) * 100, 1)
        
        color <- if (success_rate > 80) "green" else if (success_rate > 50) "yellow" else "red"
      } else {
        success_rate <- "N/A"
        color <- "blue"
      }
      
      valueBox(
        value = paste0(success_rate, "%"),
        subtitle = "Success Rate by Age 65",
        icon = icon("chart-line"),
        color = color
      )
    }
  })
  
  output$portfolio_value_box <- renderValueBox({
    if (is.null(simulation_results$summary)) {
      valueBox(
        value = "$0",
        subtitle = "Current Portfolio Value",
        icon = icon("dollar-sign"),
        color = "blue"
      )
    } else {
      current_value <- simulation_results$summary$median[1]
      
      valueBox(
        value = paste0("$", format(round(current_value), big.mark = ",")),
        subtitle = "Current Portfolio Value",
        icon = icon("dollar-sign"),
        color = "blue"
      )
    }
  })
  
  # Portfolio projection plot
  output$portfolio_projection <- renderPlotly({
    if (is.null(simulation_results$summary)) {
      plot_ly() %>% 
        layout(title = "Run simulation to see results")
    } else {
      summary_data <- simulation_results$summary
      
      # Sample every 12th row (annual) for cleaner visualization
      plot_data <- summary_data[seq(1, nrow(summary_data), by = 12), ]
      
      p <- plot_ly(plot_data, x = ~age) %>%
        add_trace(y = ~median, name = "Median (Nominal)", type = 'scatter', mode = 'lines',
                  line = list(color = 'blue', width = 2)) %>%
        add_trace(y = ~median_real, name = "Median (Real)", type = 'scatter', mode = 'lines',
                  line = list(color = 'green', width = 2)) %>%
        add_trace(y = ~p10, name = "10th Percentile", type = 'scatter', mode = 'lines',
                  line = list(color = 'red', width = 1, dash = 'dot')) %>%
        add_trace(y = ~p90, name = "90th Percentile", type = 'scatter', mode = 'lines',
                  line = list(color = 'orange', width = 1, dash = 'dot')) %>%
        add_trace(y = ~retirement_target, name = "Retirement Target", type = 'scatter', mode = 'lines',
                  line = list(color = 'purple', width = 2, dash = 'dash')) %>%
        layout(title = "Portfolio Value Projection",
               xaxis = list(title = "Age"),
               yaxis = list(title = "Portfolio Value ($)", tickformat = ",.0f"),
               hovermode = 'x unified',
               legend = list(orientation = 'h', y = -0.2))
      
      p
    }
  })
  
  # Probability distribution plot
  output$probability_distribution <- renderPlotly({
    if (is.null(simulation_results$data)) {
      plot_ly() %>% 
        layout(title = "Run simulation to see results")
    } else {
      # Show distributions at key ages
      summary_data <- simulation_results$summary
      key_ages <- c(40, 50, 60, 65)
      
      plot_list <- list()
      for (age_target in key_ages) {
        age_idx <- which.min(abs(summary_data$age - age_target))
        if (length(age_idx) > 0) {
          values <- simulation_results$data[, age_idx]
          plot_list[[length(plot_list) + 1]] <- list(
            x = values,
            name = paste("Age", age_target),
            type = 'histogram',
            histnorm = 'probability',
            opacity = 0.7
          )
        }
      }
      
      p <- plot_ly()
      for (trace in plot_list) {
        p <- add_trace(p, x = trace$x, name = trace$name, type = trace$type, 
                       histnorm = trace$histnorm, opacity = trace$opacity)
      }
      
      p %>% layout(
        title = "Portfolio Value Distribution at Key Ages",
        xaxis = list(title = "Portfolio Value ($)", tickformat = ",.0f"),
        yaxis = list(title = "Probability"),
        barmode = 'overlay'
      )
    }
  })
  
  # Retirement probability over time
  output$retirement_probability <- renderPlotly({
    if (is.null(simulation_results$data)) {
      plot_ly() %>% 
        layout(title = "Run simulation to see results")
    } else {
      summary_data <- simulation_results$summary
      
      # Calculate probability of meeting target at each age
      probabilities <- numeric(nrow(summary_data))
      for (i in 1:nrow(summary_data)) {
        probabilities[i] <- mean(simulation_results$data[, i] >= summary_data$retirement_target[i])
      }
      
      # Sample for plotting
      plot_indices <- seq(1, nrow(summary_data), by = 12)
      
      p <- plot_ly(x = summary_data$age[plot_indices], 
                   y = probabilities[plot_indices] * 100,
                   type = 'scatter', mode = 'lines+markers',
                   line = list(color = 'green', width = 2),
                   marker = list(size = 4)) %>%
        add_trace(y = rep(50, length(plot_indices)), 
                  name = "50% threshold",
                  line = list(color = 'orange', width = 1, dash = 'dash')) %>%
        layout(title = "Probability of Achieving Retirement Target",
               xaxis = list(title = "Age"),
               yaxis = list(title = "Probability (%)", range = c(0, 100)),
               showlegend = FALSE)
      
      p
    }
  })
  
  # Statistics table
  output$statistics_table <- DT::renderDataTable({
    if (is.null(simulation_results$summary)) {
      data.frame(Message = "Run simulation to see results")
    } else {
      summary_data <- simulation_results$summary
      
      # Create detailed statistics for key ages
      key_ages <- c(35, 40, 45, 50, 55, 60, 65)
      stats_table <- data.frame()
      
      for (age_target in key_ages) {
        age_idx <- which.min(abs(summary_data$age - age_target))
        if (length(age_idx) > 0 && age_target >= input$current_age) {
          portfolio_values <- simulation_results$data[, age_idx]
          target_value <- summary_data$retirement_target[age_idx]
          
          stats_table <- rbind(stats_table, data.frame(
            Age = age_target,
            `Years_from_Now` = age_target - input$current_age,
            `Median_Portfolio` = paste0("$", format(round(summary_data$median[age_idx]), big.mark = ",")),
            `Real_Value` = paste0("$", format(round(summary_data$median_real[age_idx]), big.mark = ",")),
            `Target_Value` = paste0("$", format(round(target_value), big.mark = ",")),
            `Success_Rate` = paste0(round(mean(portfolio_values >= target_value) * 100, 1), "%"),
            `10th_Percentile` = paste0("$", format(round(summary_data$p10[age_idx]), big.mark = ",")),
            `90th_Percentile` = paste0("$", format(round(summary_data$p90[age_idx]), big.mark = ","))
          ))
        }
      }
      
      names(stats_table) <- c("Age", "Years from Now", "Median Portfolio", "Real Value (Today's $)", 
                              "Retirement Target", "Success Rate", "10th Percentile", "90th Percentile")
      
      DT::datatable(stats_table, 
                    options = list(pageLength = 10, dom = 't'),
                    rownames = FALSE)
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
