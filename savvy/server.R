library(shiny)

function(input, output, session) {
  
  output$Plot <- renderPlot({
    
    f <- function(k,n){exp(0.5 * k * log(n) - k)}
    
    k <- seq(1, input$x, 1) 
    n <- seq(1, input$y, 1) 
    m <- length(k)
    p <- length(n)
    z <- array(0, dim = c(m, p) )

    for(i in 1:m)for(j in 1:p)z[i,j]=f(k[i],n[j])

    if(m!=1 & p!=1){ 
      persp(k, n, z, theta = 40, phi = 30, expand = .666, zlab ="z",
                           col = "steelblue2", border = "black", shade = .75, 
                           lphi = 45, ltheta = 135)}
    
    else{ 
    if(m==1){plot(t(z), type = "p", pch = 19, col = "steelblue", bty ="l", xlab = "n", ylab = "K")}
    else{plot(z, type = "p", pch = 19, col = "steelblue", bty ="l", xlab = "K", ylab = "n")}}

  })
  
}
