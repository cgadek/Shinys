
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
      sliderInput('alpha', 'Intrinsic Growth Rate (alpha):', min = 0.1, max = 5, value = 1),
      sliderInput('beta', 'Predation Rate (beta):', min = 0.01, max = 1, value = 0.1),
      sliderInput('delta', 'Prey Conversion Rate (delta):', min = 0.01, max = 1, value = 0.1),
      sliderInput('gamma', 'Predator Mortality (gamma):', min = 0.01, max = 1, value = 0.1)
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
  lv_results <- reactive({
    # Get the current parameters from input within this reactive context
    pars <- c(alpha = input$alpha, beta = input$beta, delta = input$delta, gamma = input$gamma)
    
    # Lotka-Volterra model function with current parameters
    lv_model <- function(pars, times = seq(0, 50, by = 0.25)) {
      # Initial state
      state <- c(x = 1, y = 2)
      
      # Derivative function
      deriv <- function(t, state, pars) {
        with(as.list(c(state, pars)), {
          d_x <- alpha * x - beta * x * y
          d_y <- delta * x * y - gamma * y
          return(list(c(d_x, d_y)))
        })
      }
      
      # Solve the differential equations
      ode(y = state, times = times, func = deriv, parms = pars)
    }
    
    # Call the model with updated parameters
    lv_model(pars = pars)
  })
  
  # Render the plot
  output$distPlot <- renderPlot({
    # Get results from reactive expression
    lv_data <- lv_results() %>% 
      as.data.frame() %>% 
      gather(var, pop, -time) %>% 
      mutate(var = if_else(var == "x", "Prey", "Predator"))
    
    # Generate the plot with dynamic subtitle
    ggplot(lv_data, aes(x = time, y = pop)) +
      geom_line(aes(color = var), linewidth=1.2) +
      scale_color_manual(values=c("brown", "goldenrod")) +
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