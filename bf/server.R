library(tidyverse)
library(ggpubr)
library(shiny)

function(input, output, session) {
  
  output$Plot <- renderPlot({
      
          n <- input$n
          theta <- input$theta
          a1 <- input$alpha1
          b1 <- input$beta1
          a2 <- input$alpha2
          b2 <- input$beta2
          
          # generating some data
          data <- rbinom(n, size = 1, prob = theta)
          
          # grid setup
          grid <- seq(from = 0, to = 1, length.out = 1e2)
          
          post <-
              data.frame(theta = seq(from = 0, to = 1, length.out = 1e2) ) %>%
              mutate(
                  # computing the likelihood of each data point
                  like = sapply(1:nrow(.), function(i) prod(dbinom(
                      data, size = 1, prob = theta[i], log = FALSE) ) ),
                  like = like / max(like),
                  # computing the marginal likelihood
                  # prior model 1
                  prior1 = dbeta(.$theta, a1, b1, log = FALSE),
                  prior1 = prior1 / max(prior1),
                  # for model 1
                  marg1 = like * prior1,
                  # prior model 2
                  prior2 = dbeta(.$theta, a2, b2, log = FALSE),
                  prior2 = prior2 / max(prior2),
                  # and for model 2
                  marg2 = like * prior2,
                  # incremental bf
                  bf = cumsum(marg1) / cumsum(marg2)
              ) %>%
              # replacing NaNs BFs by 1
              replace(., is.na(.), 1)
          
          # BF12 is the ratio of the marginals
          bf12 <- sum(post$marg1) / sum(post$marg2)
          
          # making first plot (prior and likelihood)
          p1 <- 
              post %>%
              select(-marg1, -marg2) %>%
              ggplot(aes() ) +
              # plotting likelihood function
              geom_line(aes(x = theta, y = like), inherit.aes = FALSE, size = 1) +
              # plotting prior1
              geom_line(
                  aes(x = theta, y = prior1),
                  linetype = "dashed", col = "steelblue", size = 1,
                  inherit.aes = FALSE) +
              # plotting prior2
              geom_line(
                  aes(x = theta, y = prior2),
                  linetype = "dashed", col = "orangered3", size = 1,
                  inherit.aes = FALSE) +
              theme_bw(base_size = 14, base_family = "Verdana") +
              ylab("") +
              xlab(expression(paste("Probability of heads ", theta) ) ) +
              ggtitle(
                  paste0(
                      "Likelihood function and prior predictions for\n", length(data), " coin flips and ",
                      sum(data), " heads"
                  )
              )
          
          # making second plot (marginal likelihoods)
          p2 <- 
              p <- 
              post %>%
              ggplot(aes(frame = len) ) +
              # plotting marg1
              geom_line(
                  aes(x = theta, y = marg1),
                  linetype = "dashed", col = "steelblue", size = 1,
                  inherit.aes = FALSE) +
              # plotting marg2
              geom_line(
                  aes(x = theta, y = marg2),
                  linetype = "dashed", col = "orangered3", size = 1,
                  inherit.aes = FALSE) +
              theme_bw(base_size = 14, base_family = "Verdana") +
              ylab("") +
              xlab(expression(paste("Probability of heads ", theta) ) ) +
              ggtitle("Marginal likelihood")
          
          # making third plot (cumulative marginals)
          p3 <- 
              post %>%
              mutate(cumsum1 = cumsum(marg1), cumsum2 = cumsum(marg2) ) %>%
              ggplot(aes(frame = len) ) +
              # plotting cumulative marginal for model1
              geom_line(
                  aes(x = theta, y = cumsum1),
                  linetype = "dashed", col = "steelblue", size = 1,
                  inherit.aes = FALSE) +
              # plotting cumulative marginal for model2
              geom_line(
                  aes(x = theta, y = cumsum2),
                  linetype = "dashed", col = "orangered3", size = 1,
                  inherit.aes = FALSE) +
              theme_bw(base_size = 14, base_family = "Verdana") +
              ylab("") +
              xlab(expression(paste("Probability of heads ", theta) ) ) +
              ggtitle("Cumulative marginal likelihood")
          
          # Making fourth plot - BF pizza plot
          
          p4 <- 
              post %>%
              tail(1) %>%
              mutate(
                  M1 = (bf) / (bf + 1),
                  M2 = 1 - M1) %>%
              gather(hyp, BF, M1:M2) %>%
              ggplot(aes(x = "", y = BF, fill = hyp) ) +
              geom_bar(width = 0.25, stat = "identity", show.legend = FALSE) +
              coord_polar(theta = "y") +
              geom_text(
                  aes(x = 0, y = 0, label = paste0("BF12 = ", round(bf, 2) ) ),
                  inherit.aes = FALSE, parse = FALSE, size = 5) +
              theme_void() +
              scale_fill_manual(values = c("steelblue", "orangered3") )
          
          #####################################################################################
          # Arranging the four plots
          #########################################################################
          
          ggpubr::ggarrange(p2, p3, p1, p4)
          
  })
  
}
