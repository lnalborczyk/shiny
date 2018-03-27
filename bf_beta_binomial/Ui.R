library(tidyverse)
library(shiny)

fluidPage(
  
    titlePanel("Bayes Factor comparing two beta-binomial models"),
    
    sidebarLayout(
        
        position = "right",
        
        sidebarPanel(
            h4(""),
            sliderInput("n", "sample size", min = 10, max = 200, value = 100, step = 1),
            sliderInput("theta", "theta", min = 0, max = 1, value = 0.4, step = 0.01),
            sliderInput("alpha1", "alpha prior model 1", min = 1, max = 20, value = 2, step = 0.1),
            sliderInput("beta1", "beta prior model 1", min = 1, max = 20, value = 2, step = 0.1),
            sliderInput("alpha2", "alpha prior model 2", min = 1, max = 20, value = 10, step = 0.1),
            sliderInput("beta2", "beta prior model 2", min = 1, max = 20, value = 9, step = 0.1)
            ),
    
        mainPanel(
            plotOutput("Plot", height = 800),
            withMathJax(
                helpText(
                    ""
                    )
                )
            )
      
      )
    )
