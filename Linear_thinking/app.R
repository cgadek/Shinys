library(shiny)
library(ggplot2)
library(markdown)

# Define UI for application that demonstrates linear regression
ui <- fluidPage(
  
  # Application title
  titlePanel("Linear thinking"),
  
  # Sidebar with sliders for slope and intercept
  sidebarLayout(
    sidebarPanel(
      sliderInput("beta",
                  "Slope (Regression Coefficient):",
                  min = -5,
                  max = 5,
                  value = 1, 
                  step = 0.1),
      sliderInput("intercept",
                  "Intercept:",
                  min = -10,
                  max = 10,
                  value = 0,
                  step = 0.5),
      fluidRow(
        column(11,
               withMathJax(includeHTML("include_sidebar.html"))  # Wrap include html in withMathJax for LaTeX math rendering
        )
      )
    ),
    
    # Show a plot of the generated data with regression line
    mainPanel(
      plotOutput("regressionPlot"),
      fluidRow(
        column(11,
               withMathJax(includeHTML("include.html"))  # Wrap include html in withMathJax for LaTeX math rendering
        )
      )
    )
  )
)

# Define server logic required to draw the plot
server <- function(input, output) {
  
  output$regressionPlot <- renderPlot({
    # Generate random x values
    x <- rnorm(n = 100, mean = 0, sd = 2)
    
    # Create y values based on the chosen slope and intercept, with some noise
    y <- input$intercept + input$beta * x + rnorm(100, sd = 2)
    
    # Create data frame for ggplot
    df <- data.frame(x = x, y = y)
    
    # Plot with ggplot2
    ggplot(df, aes(x = x, y = y)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +
      labs(
        title = "Interactive Linear Regression",
        subtitle = "Adjust the slope and intercept to see how the line changes",
        x = "Predictor (x)",
        y = "Response (y)"
      ) +
      theme_minimal() +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 16))+
      coord_cartesian(ylim = c(-20, 20), xlim = c(-10, 10)) 
     # geom_abline(intercept = input$intercept, slope = input$beta, color = "red", size = 1)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
