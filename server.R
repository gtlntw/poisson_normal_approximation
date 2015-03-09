library(shiny)
# library(shinyIncubator)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {  
        process <- reactive({
                if(input$run==0) return(NULL)
                
                ## get input parameters
                lambda = isolate(input$lambda)
                a = isolate(input$a)
                ineq.sign = isolate(switch(input$upr, 
                                           "lt" = "<", "leq" = "<=",
                                           "gt" = ">", "geq" = ">="))
                
                ## calculate binomial exact probability
                prob.pois = isolate(switch(input$upr,
                                            "lt" = ppois(a-1, lambda=lambda),
                                            "leq" = ppois(a, lambda=lambda), 
                                            "gt" = 1-ppois(a, lambda=lambda),
                                            "geq" = 1-ppois(a-1, lambda=lambda)))
                prob.pois = round(prob.pois, 4)
                
                ## calculate normal approx. probability
                ## note the usage of continuity correction factor of 0.5 / -0.5
                mu = lambda
                sigma = round(sqrt(lambda), 2)
                prob.norm = isolate(switch(input$upr,
                                           "leq" = pnorm(a+0.5, mu, sigma), 
                                           "gt" = 1 - pnorm(a+0.5, mu, sigma),
                                           "lt" = pnorm(a-0.5, mu, sigma),
                                           "geq" = 1 - pnorm(a-0.5, mu, sigma)))
                prob.norm = round(prob.norm, 4)
                
                ## make a matrix
                M = matrix(c(lambda, NA, prob.pois), nrow=3)
                M = cbind(M, c(mu, sigma, prob.norm))
                M = cbind(M, c("mean", "std", "---"))
                
                probName = paste0(paste("P(X", ineq.sign, a), ")")
                rownames(M) = c("λ", "---", probName)
                colnames(M) = c("Poisson Exact", "Normal Approx.", "---")
                
                ## collect above processed data into a list
                out = list()
                out$M = M; out$a = a; out$lambda=lambda; out$sigma=sigma;
                
                ## return
                out
        })
        
        output$tab <- renderTable({
                if(input$run==0) return(NULL)
                
                ## call process() to get M
                process()$M
        })
        
        output$bnplot = renderPlot({
                if(input$run==0) return(NULL)
                
                ## call process()
                proc = process()
                a = proc$a
                lambda = proc$lambda
                sigma = proc$sigma
                
                ## plot 
                big.size = element_text(size=13)
                bold.text = element_text(face='bold')
                
                #auto adjust the range of the plot based on lambda: mean +- 4*sd
                plt = ggplot(transform(data.frame(x=max(floor(lambda-4*sigma), 0):floor(lambda+4*sigma)), y=dpois(x, lambda=lambda)), aes(x, y)) + 
                      geom_bar(stat="identity", fill=hcl(h=195,l=65,c=100),color="black") + 
                      stat_function(fun=dnorm, args=list(mean = lambda, sd = sigma), color=hcl(h=15,l=65,c=100), size=1.2) +
                      theme_bw() + 
                      theme(axis.text.x=big.size, axis.text.y=big.size, 
                            title=bold.text) + 
                      labs(x = "", y = "",
                           title="Poisson Distribution with Bell Curve")
                
                ## add vertical line x=a
                plt = plt + geom_vline(xintercept = a, color="darkblue", 
                                       size=1.2)
                
                ## draw plt
                print(plt)
        })
})