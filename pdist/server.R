library(shiny)

function(input, output, session) {
  
  output$Plot <- renderPlot({
    
      options(scipen = 999) # disable scientific notation for numbers
      
      cohensd <- input$d  # set true effect size
      n <- input$n # sample size in each group
      
      nSims <- 10000 # number of simulated experiments
      p <- numeric(nSims) # set up empty variable to store all simulated p-values
      
      for(i in 1:nSims){
          
          x <- rnorm(n = n, mean = 0, sd = 1) # produce N simulated participants
          y <- rnorm(n = n, mean = 0 + cohensd, sd = 1) # produce N simulated participants
          z <- t.test(x,y) # perform a t-test for independant samples
          
          p[i] <- z$p.value # extract p-value
          
      }
      
      empirical_power <- (sum(p < 0.05) / nSims)
      
      if(cohensd==0){empirical_power <- 0} # if H0, then power = 0
      
      hist(p, breaks = 20, ylab = "number of p-values", border = FALSE, 
          main = paste("p-value distribution with", 
              round(empirical_power * 100, digits = 0), "% power"), 
          xlim = c(0, 1), ylim = c(0, nSims), col = "steelblue")
      
      abline(h = nSims / 20, lty = 2)

  })
  
}
