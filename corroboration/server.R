library(gridExtra)
library(ggplot2)
library(shiny)

shinyServer(function(input, output) {
    
    # react when user clicks "New Sample"
    data_sample <- eventReactive(c(input$sample_data), {
        
        y <- rnorm(input$n, input$mu_data, input$sd_data)
        
        mu <- mean(y)
        se <- sd(y) / sqrt(length(y) )
        
        dat_min <- min(y) - 0.2 * sd(y)
        dat_max <- max(y) + 0.2 * sd(y)
        
        xmin <- min(dat_min, qnorm(0.0001, mu, se) )
        xmax <- max(dat_max, qnorm(0.9999, mu, se) )
        
        return(list(y = y, xmin = xmin, xmax = xmax) )
        
        }, ignoreNULL = FALSE, ignoreInit = FALSE)
    
    # react when data_sample() changes (when user clicks 'New Sample')
    true_mean <- eventReactive(data_sample(), {return(input$mu_data)} )
    
    # draw plot
    output$distPlot <- renderPlot({
        
        # get reactive data
        data_sample <- data_sample()
        true_mean <- true_mean()

        # sample data
        y <- data_sample$y
        
        # retrieve tolerance and spielraum
        tolerance <- c(input$tolerance[1], input$tolerance[2])
        spielraum <- c(input$spielraum[1], input$spielraum[2])
        
        # update x-axis limits
        
        lim_low <- NULL
        lim_upp <- NULL
        xmin <- data_sample$xmin
        xmax <- data_sample$xmax
        
        # sample mean and standard error
        mu_data <- mean(y)
        se_data <- sd(y) / sqrt(length(y) )
        
        # sequence of x-values for generating probability distributions
        x <- seq(xmin, xmax, length.out = 400)
        lik <- dnorm(x, mu_data, se_data)

        # frequentist confidence intervals
        ci_low <- qnorm(0.025, mu_data, se_data)
        ci_upp <- qnorm(0.975, mu_data, se_data)
        
        # arrange dfs for prior, likelihood
        df_lik <- data.frame(x, type = 'lik', val = lik)

        df <- df_lik
        df$type <- factor(df$type, levels = c('lik'), labels = c('Likelihood') )
        
        # dfs for confidence intervals, sample data, and population mean
        fc <- 0.12  # factor for positioning CIs, sample data, and pop mean
        
        df_ci <- data.frame(x = mu_data, xmin = ci_low, xmax = ci_upp, y = -1 * fc)
        df_sd <- data.frame(x = y, y = -2 * fc)

        df_tolerance <- data.frame(xmin = tolerance[1], xmax = tolerance[2], y = fc)
        
        ########################################################################
        # computing the corroboration index
        ######################################################
        
        spiel <- max(spielraum) - min(spielraum) # width of spielraum
        interval <- max(tolerance) - min(tolerance) # width of tolerance
        
        relinterval <- interval / spiel # theory tolerance
        
        intolerance <- 1 - relinterval # theory intolerance
        
        if (mu_data < tolerance[2] & mu_data > tolerance[1]) {
            
            deviation <- 0
            
        } else if (mu_data < tolerance[1]) {
            
            deviation <- tolerance[1] - mu_data
            
        } else if (mu_data > tolerance[2]) {
            
            deviation <- mu_data - tolerance[2]
            
        }
        
        relative_error <- deviation / spiel
        closeness <- 1 - relative_error
        
        ci <- closeness * intolerance # corroboration index
        
        #############################################
        # plot settings
        ###########################
        
        size_labs <- 5.5
        ci_size <- 1.6
        
        # plot
        p1 <-
            ggplot(df) +
            # plotting the mean of the sample
            geom_vline(xintercept = mu_data, linetype = 2, color = "#e7298a") +
            # plotting the data
            geom_point(
                data = df_sd, aes(x, y = 0),
                size = 4.2, stroke = 0, alpha = 0.4, color = "#e7298a"
                ) +
            geom_point(
                data = df_sd, aes(x, y = 0),
                shape = 1, stroke = 1.2, size = 4.2, alpha = 0.9, color = "#e7298a"
                ) +
            geom_density(
                data = df_sd, aes(x = x), inherit.aes = FALSE,
                alpha = 0.3, color = "#e7298a", fill = "#e7298a"
            ) +
            # plotting the theory tolerance
            geom_errorbarh(
                data = df_tolerance,
                aes(xmin = xmin, xmax = xmax, y = y), inherit.aes = FALSE,
                height = 0.01, size = 1#, size = ci_size#, color = "#d95f02"
            ) +
            # setting the x-axis as the spielraum
            coord_cartesian(xlim = spielraum, ylim = c(0, 4 * fc) ) +
            scale_x_continuous(expand = c(0, 0) ) +
            xlab(NULL) + ylab(NULL) +
            # plotting the corroboraiton index
            ggtitle(paste0("Corroboration Index = ", round(ci, 3) ) ) +
            # aesthetics
            theme(
                panel.grid = element_blank(),
                text = element_text(size = 16),
                axis.title = element_text(size = 16),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                panel.background = element_rect(fill = "grey94"),
                axis.title.x = element_text(margin = margin(.3, 0, 0, 0, unit = 'cm') ),
                legend.title = element_blank(),
                legend.justification = c(0, 1),
                legend.box.spacing = unit(1.2, "pt"),
                legend.text = element_text(size = 16, face = "bold"),
                legend.background = element_blank(),
                plot.margin = unit(c(5.5, 65.5, 5.5, 5.5), "pt")
                )
        
        # allow plotting outside main plot region
        g1 <- ggplotGrob(p1)
        g1$layout$clip[g1$layout$name == "panel"] <- "off"
        
        # output plot
        grid.arrange(g1)
    })
})
