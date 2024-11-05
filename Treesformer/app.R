#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(geiger)
library(ape)  # For vcv function

# Define UI for application that draws a tree transformation plot
ui <- fluidPage(
  titlePanel("Phylogenetic Tree Transformation"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("n_tips",
                  "Generate a random pure birth tree with n species:",
                  min = 3,
                  max = 100,
                  value = 10),
      selectInput("type", 
                  "Tree type", 
                  choices = c("phylogram", "fan", "cladogram")),
      sliderInput("scalar",
                  "Transformation scalar",
                  min = 0,
                  max = 1,
                  value = 0.5),
      selectInput("trans", 
                  "Transformation type", 
                  choices = c("lambda", "delta", "kappa"))
    ),
    mainPanel(
      plotOutput("distPlot"),
      
      fluidRow(
        column(11,
               withMathJax(includeHTML("include.html"))
        ),
        column(12,
               h4("Phylogenetic Variance-Covariance Matrix"),
               tableOutput("matrix")
        )
      )
    )
  )
)

# Define server logic required to draw the tree transformation plot
server <- function(input, output) {
  
  # Reactive function to generate a simulated tree
  treesim <- reactive({
    sim.bdtree(n = input$n_tips)
  })
  
  
  
  # Reactive function to apply the selected transformation
  treetrans <- reactive({
    tree <- treesim()
    
    # Apply the transformation based on user selection
    if (input$trans == "lambda") {
      rescale(tree, model = "lambda", lambda = input$scalar)
    } else if (input$trans == "delta") {
      rescale(tree, model = "delta", delta = input$scalar)
    } else if (input$trans == "kappa") {
      rescale(tree, model = "kappa", kappa = input$scalar)
    }
  })
  # Reactive function to generate variance-covariance matrix
  vcvsim <- reactive({
    vcv(treetrans())
  })
  
  # Render the plot
  output$distPlot <- renderPlot({
    plot(treetrans(), edge.width = 2, type = input$type, show.tip.label = FALSE)
  })
  
  # Render the variance-covariance matrix as a table
  output$matrix <- renderTable({
    M <- vcvsim()
    rownames(M) <- treesim()$tip.label
    M
  }, rownames = TRUE, colnames = TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)