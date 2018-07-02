library(shiny)

# Define UI
shinyUI(
    fluidPage(
        
        # title
        titlePanel("Computing the corroboration index of a theory"),
        h5("Based on Meehl (1990)"),
        
        # css styles
        tags$head(
            tags$style(HTML('p{font-size: 16px}')),
            tags$style(HTML('li{font-size: 16px}')),
            tags$style(HTML('#mu_prior{background-color: rgba(27, 158, 119, 0.5);
                border-color: rgb(27, 158, 119)}')),
            tags$style(HTML('#sd_prior{background-color: rgba(27, 158, 119, 0.5);
                border-color: rgb(27, 158, 119)}')),
            tags$style(HTML('#sample_data{color: white;
                background-color: rgba(80, 80, 80, 0.8);
                border-color: black; border-width: 1.5px}')),
            tags$style(HTML('#likelihood{color: #d95f02; font-weight: bold}')),
            tags$style(HTML('#data{color: #e7298a; font-weight: bold}')),
            tags$style(HTML('#mean{color: #000000}'))
            ),
        
        # layout
        sidebarLayout(
            
            # side panel
            sidebarPanel(
                width = 3,
                sliderInput("tolerance", "tolerance", min = 0, max = 100, value = c(20, 40) ),
                sliderInput("spielraum", "spielraum", min = 0, max = 100, value = c(0, 100) ),
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
                    distribution, and illustrates how theory strength and predictive accuracy
                    jointly determine a corroboration index.")
                )
            )
        )
    )
