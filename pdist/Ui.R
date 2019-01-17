library(tidyverse)
library(shiny)

fluidPage(
  
    titlePanel(""),
    
    sidebarLayout(
        
        position = "right",
        
        sidebarPanel(
            h4("Effect size, sample size and significance level"),
            sliderInput("d", "Effect size", min = 0, max = 2, value = 0.5, step = 0.01),
            sliderInput("n", "Sample size", min = 1, max = 100, value = 20, step = 1),
            sliderInput("alpha", "Significance level (alpha)", min = 0, max = 1, value = 0.05, step = 0.01)
            ),
    
        mainPanel(
            plotOutput("Plot"),
            withMathJax(
                helpText(
                    "Distribution of p-values computed from a two-sample t-test,
                    for a given effect size (Cohen\'s d), sample size and significance level."
                    )
                )
            )
      
      )
    )
