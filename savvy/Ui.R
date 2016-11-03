library(shiny)

fluidPage(
  
  titlePanel("K-L prior (savvy prior)"),

  sidebarLayout(
    position = "right",
    sidebarPanel(
      h4("Number of parameters K and sample size n"),
      sliderInput("x", "K", min = 1, max = 100, value = 10, step = 1),
      sliderInput("y", "n", min = 1, max = 100, value = 10, step = 1)
    ),

    mainPanel(
      plotOutput("Plot"), withMathJax(helpText('z represents the (non-normalized) prior probability for a model with K parameters and n observations (Burnham & Anderson, 2004) $$z_{K,n} \\propto \\exp(\\frac{1}{2}K\\log (n)-K)$$'))
    )

  )
)
