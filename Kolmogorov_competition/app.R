#Shiny app for interactive visualization of Lotka-Volterra Predator-Prey Model
library(shiny)
library(tidyverse)
library(deSolve)
library(FME)
library(markdown)

source("~/Desktop/ggplot_themes/ggplot_themes.R")

# Set custom theme
theme_set(theme_arial_clean())

# Define UI for application that draws a plot
ui <- fluidPage(
  mathjax = TRUE,  # Ensure MathJax is enabled for LaTeX rendering
  # Application title
  titlePanel("Lotka-Volterra Predator-Prey Model"),
  
  # Sidebar with slider inputs for parameters
  sidebarLayout(
    sidebarPanel(
      sliderInput('r', 'Intrinsic Growth Rate (r):', min = 0, max = 5, value = 0, step = 0.1),
      sliderInput('K', 'Carrying capacity of environment:', min = 1, max = 1000, value = 10),
      sliderInput('a', 'strength of competition species a:', min = 0, max = 1, value = 0),
      sliderInput('b', 'strength of competition species b:', min = 0, max = 1, value = 0)
    ),
    
    # Show a plot of the results
    mainPanel(
      plotOutput("distPlot"),
      fluidRow(
        column(11,
               withMathJax(includeHTML("include.html"))  # Wrap includehtml in withMathJax for LaTeX math rendering
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Define a reactive expression for the model results
  km_results <- reactive({
    # Get the current parameters from input within this reactive context
    pars <- c(r = input$r, K = input$K, a = input$a, b = input$b)
    #pars <- c(r=0.1, K=0.2, a=0.1, b=0.11) for testing purposes
    # Lotka-Volterra model function with current parameters
    km_model <- function(pars, times = seq(0, 50, by = 0.25)) {
      # Initial state
      state <- c(N = 2, P = 1)
      
      # Derivative function
      deriv <- function(t, state, pars) {
        with(as.list(c(state, pars)), {
          d_N <- r * N *(1-N/K) -a*N*P
          d_P <- r * P *(1-P/K) -b*N*P
          return(list(c(d_N, d_P)))
        })
      }
      
      # Solve the differential equations
      ode(y = state, times = times, func = deriv, parms = pars)
    }
    
    # Call the model with updated parameters
    km_model(pars = pars)
  })
  
  # Render the plot
  output$distPlot <- renderPlot({
    # Get results from reactive expression
    km_data <- km_results() %>% 
      as.data.frame() %>% 
      gather(var, pop, -time) %>% 
      mutate(var = if_else(var == "N", "species a", "species b"))
    
    # Generate the plot with dynamic subtitle
    ggplot(km_data, aes(x = time, y = pop)) +
      geom_line(aes(color = var), linewidth=1.2) +
      scale_color_manual(values=c("darkseagreen", "darkgreen")) +
      labs(
        title = "",
        subtitle = paste("alpha =", input$alpha, "; beta =", input$beta, 
                         "; delta =", input$delta, "; gamma =", input$gamma),
        x = "Time", y = "Population Density"
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
