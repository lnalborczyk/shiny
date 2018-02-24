library(tidyverse)
library(shiny)

fluidPage(
  
    titlePanel(""),
    
    sidebarLayout(
        
        position = "right",
        
        sidebarPanel(
            h4("Effect size (d), sample size (n), and alpha level."),
            sliderInput("d", "d", min = 0, max = 2, value = 0.5, step = 0.01),
            sliderInput("n", "n", min = 1, max = 100, value = 20, step = 1),
            sliderInput("alpha", "alpha", min = 0, max = 1, value = 0.05, step = 0.01)
            ),
    
        mainPanel(
            plotOutput("Plot"),
            withMathJax(
                helpText(
                    "Distribution of p-values for an independant-samples t-test,
                    as a function of effect size (Cohen\'s d) and sample size."
                    )
                )
            )
      
      )
    )
