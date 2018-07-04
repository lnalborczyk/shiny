library(shinythemes)
library(shiny)

# Define UI
shinyUI(
    fluidPage(
        
        # theme
        theme = shinytheme("simplex"),
        
        # title
        titlePanel("Computing the corroboration index of a theory"),
        h5("Based on Meehl (1990)"),
        
        # layout
        sidebarLayout(
            
            # side panel
            sidebarPanel(
                width = 3,
                sliderInput("tolerance", "Tolerance", min = -100, max = 100, value = c(20, 40) ),
                sliderInput("spielraum", "Spielraum", min = -100, max = 100, value = c(0, 100) ),
                br(),
                numericInput("n", "Sample size", value = 20, step = 1, min = 10),
                numericInput("mu_data", "Population mean", value = 60, step = 0.5),
                numericInput("sd_data", "Population SD", min = 0, value = 5, step = 0.5),
                actionButton("sample_data", "New sample", width = "100%")
            ),
            
            # main panel
            mainPanel(
                width = 9, # main panel takes up 9 of 12 units
                
                # main plot
                plotOutput("distPlot", height = "500px"),
                
                # description
                br(),
                h4("Description"),
                p("The app draws a sample of a random variable y from a Gaussian
                    distribution, and illustrates how theory strength (tolerance)
                    and predictive accuracy jointly determine a corroboration index.")
                )
            )
        )
    )
