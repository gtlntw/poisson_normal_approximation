shinyUI(fluidPage(
  titlePanel("Normal Approximation to the Poisson Distribution"),
  
  withMathJax(),  
  sidebarLayout(position = "left",
        sidebarPanel( 
                p("The Poisson distribution takes one parameters:", 
                        tags$ul(
                                tags$li("λ: expected number of successes or mean of the Poisson distribution")
                        ),
                   "If λ is greater than about 10, then the normal distribution with mean λ and variance λ 
                    is a good approximation. We can then use the normal 
                    distribution to approximate the Poisson probabilities."),
                numericInput(inputId = "lambda", label = h5("Enter a positive integer for λ:"), 
                            value=1, min=0,  step = 1),
                selectInput(inputId = "upr",
                label = h5("Calculate P(X < a), P(X <= a),
                                       P(X > a) or P(X >= a): "),
                            choices = c("<" = "lt", "<=" = "leq",
                                        ">" = "gt", ">=" = "geq"),
                            selected = "lt"),
                numericInput("a", label = h5("Enter a (a = 0, 1, 2, ...): "),
                          value = 10),                
                actionButton("run", "Run")
                
        ),
        
        mainPanel(                
                tableOutput("tab"), 
#                 br(),
                plotOutput("bnplot")
        ))
  
))