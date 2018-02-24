library(shiny)

function(input, output, session) {
  
  output$Plot <- renderPlot({
    
      cohensd <- input$d  # set true effect size
      n <- input$n # sample size in each group
      alpha <- input$alpha
      
      nSims <- 1e4 # number of simulated experiments
      p <- numeric(nSims) # set up empty variable to store all simulated p-values
      
      for (i in 1:nSims) {
          
          x <- rnorm(n = n, mean = 0, sd = 1) # produce N simulated participants
          y <- rnorm(n = n, mean = 0 + cohensd, sd = 1) # produce N simulated participants
          z <- t.test(x, y) # perform a t-test for independant samples
          
          p[i] <- z$p.value # extract p-value
          
      }
      
      empirical_power <- (sum(p < alpha) / nSims)
      
      if (cohensd == 0) {empirical_power <- 0} # if H0, then power = 0
      
      data.frame(p = p) %>%
          ggplot(aes(x = p) ) +
          stat_density(fill = "grey60") +
          geom_vline(xintercept = alpha, size = 0.7, linetype = 3) +
          labs(x = "p-value", y = "") +
          theme_bw(base_size = 14, base_family = "Verdana") +
          ggtitle(
              paste0(
                  "p-value distribution for d = ", cohensd, " and n = ", n,
                  "\npower = ", round(empirical_power, digits = 2),
                  " (at alpha = ", alpha, ")") )
      
  })
  
}
