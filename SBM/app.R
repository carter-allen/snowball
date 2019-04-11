#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidygraph)
library(ggraph)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Stochastic Block Model Visualizer for Two Communities"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            titlePanel("Generate Random Network"),
            sliderInput("nodes",
                        "Number of nodes:",
                        min = 2,
                        max = 100,
                        value = 10),
            sliderInput("c1",
                        "Proportion of nodes within community:",
                        min = 0,
                        max = 1,
                        value = 0.5),
            sliderInput("p1",
                        "Probability of connection within community:",
                        min = 0,
                        max = 1,
                        value = 0.5),
            sliderInput("p2",
                        "Probability of connection between communities:",
                        min = 0,
                        max = 1,
                        value = 0.5),
            actionButton("action", label = "New Network"),
            
            # Button
            downloadButton("downloadData", "Download"),
            
            # Horizontal line ----
            tags$hr(),
            titlePanel("Upload Network"),
            # Input: Select a file ----
            fileInput("file1", "Choose .csv file corresponding to an adjecency matrix",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            checkboxInput("header", "Header", FALSE)
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("netPlot")
        )
        
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$netPlot <- renderPlot({
        refresh <- input$action
        n <- input$nodes
        p <- input$c1
        n1 <- round(p*n)
        n2 <- n-n1
        P <- matrix(c(input$p1,input$p2,input$p2,input$p1),nrow = 2,byrow = TRUE)
        file <- input$file1
        if(is.null(file))
        {
            graph <- play_blocks(n,c(n1,n2),P,FALSE,FALSE)
            ggraph(graph,layout = "kk") + 
                geom_node_point(size = 4) + 
                geom_edge_link(alpha = 0.50) + 
                theme_void()
        }
        else{
            graph <- as_tbl_graph(read.csv(input$file1$datapath,header = input$header),directed = FALSE)
            
            
            ggraph(graph,layout = "kk") + 
                geom_node_point(size = 4) + 
                geom_edge_link(alpha = 0.50) + 
                theme_void()
    }
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
