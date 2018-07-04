library(gridExtra)
library(ggplot2)
library(shiny)

shinyServer(function(input, output) {
    
    # react when user clicks on "new sample"
    data_sample <- eventReactive(c(input$sample_data), {
        
        y <- rnorm(input$n, input$mu_data, input$sd_data)
        
        return(list(y = y) )
        
        }, ignoreNULL = FALSE, ignoreInit = FALSE)
    
    # draw plot
    output$distPlot <- renderPlot({
        
        # get reactive data
        data_sample <- data_sample()

        # sample data
        y <- data_sample$y
        
        # retrieve tolerance and spielraum
        tolerance <- c(input$tolerance[1], input$tolerance[2])
        spielraum <- c(input$spielraum[1], input$spielraum[2])
        
        # update x-axis limits
        xmin <- spielraum[1]
        xmax <- spielraum[2]
        
        # sample mean
        mu_data <- mean(y)

        # sample data
        df_sd <- data.frame(x = y, y = 0)

        # creating a dataframe for subsequent plotting
        df_tolerance <- data.frame(xmin = tolerance[1], xmax = tolerance[2], y = 0)
        
        ###################################################################
        # computing the corroboration index
        ######################################################
        
        spiel <- max(spielraum) - min(spielraum) # width of spielraum
        interval <- max(tolerance) - min(tolerance) # width of tolerance
        
        relinterval <- interval / spiel # theory relative tolerance
        
        intolerance <- 1 - relinterval # theory relative intolerance
        
        if (mu_data < tolerance[2] & mu_data > tolerance[1]) {
            
            deviation <- 0 # set deviation to zero if sample mean is in the predicted interval
            
        } else if (mu_data < tolerance[1]) {
            
            deviation <- tolerance[1] - mu_data
            
        } else if (mu_data > tolerance[2]) {
            
            deviation <- mu_data - tolerance[2]
            
        }
        
        relative_error <- deviation / spiel
        closeness <- 1 - relative_error
        
        ci <- closeness * intolerance # corroboration index
        
        #######################################
        # plotting
        ###########################
        
        p1 <-
            ggplot(df_sd) +
            # plotting the tolerance interval
            geom_rect(
                aes(xmin = tolerance[1], xmax = tolerance[2], ymin = -Inf, ymax = Inf),
                fill = "grey64", alpha = 0.01
                ) +
            geom_vline(xintercept = tolerance[1], linetype = 3, size = 0.75, color = "grey64") +
            geom_vline(xintercept = tolerance[2], linetype = 3, size = 0.75, color = "grey64") +
            # plotting the mean of the sample
            geom_vline(xintercept = mu_data, linetype = 2, size = 0.5, color = "#e7298a") +
            # plotting the data
            # geom_dotplot(
            #     data = df_sd, aes(x, y = 0),
            #     method = "histodot",
            #     color = "#e7298a", fill = "#e7298a"
            #     ) +
            geom_point(
                data = df_sd, aes(x, y = 0),
                size = 4.2, stroke = 0, alpha = 0.4, color = "#e7298a"
                ) +
            geom_point(
                data = df_sd, aes(x, y = 0),
                shape = 1, stroke = 1.2, size = 4.2, alpha = 0.9, color = "#e7298a"
                ) +
            # plotting the density
            geom_density(
                data = df_sd, aes(x = x),
                alpha = 0.25, color = "#e7298a", fill = "#e7298a", trim = TRUE
            ) +
            # setting the x-axis as the spielraum
            coord_cartesian(xlim = spielraum) +
            # axis labels
            xlab(NULL) + ylab(NULL) +
            # plotting the corroboration index as title
            ggtitle(paste0("Corroboration Index = ", round(ci, 3) ) ) +
            # aesthetics
            theme_grey(base_size = 16) +
            theme(
                panel.grid = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank()
                )
        
        # allow plotting outside main plot region
        g1 <- ggplotGrob(p1)
        g1$layout$clip[g1$layout$name == "panel"] <- "off"
        
        # output plot
        grid.arrange(g1)
    })
})
