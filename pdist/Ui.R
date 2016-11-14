library(shiny)

fluidPage(
  
  titlePanel(""),

  sidebarLayout(
    position = "right",
    sidebarPanel(
      h4("effect size (d) and sample size (n)"),
      sliderInput("d", "d", min = 0, max = 2, value = 0.5, step = 0.01),
      sliderInput("n", "n", min = 1, max = 100, value = 20, step = 1)
    ),

    mainPanel(
      plotOutput("Plot"), withMathJax(helpText('distribution of p-values for an independant-samples t-test, as a function of effect size (Cohen\'s d) and sample size'))
    )

  )
)
