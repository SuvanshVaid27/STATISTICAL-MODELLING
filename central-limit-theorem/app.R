library(shiny)
library(ggplot2)


#shiny UI
ui <- fluidPage(
    
    titlePanel("Central limit theorem demonstration"),
    
    fluidRow(
        column(
            width = 12,
            textOutput("intro"),
            br(),
            br()
        )
    ),
    
    
    sidebarLayout(
        sidebarPanel(
            helpText("Select distribution type, number of simulations and sample size"),
            selectInput("distribution", "Distribution:", 
                        c("Die roll", "Coin flip", "Exponential", "Poisson")),
            sliderInput("simulations", "Number of simulations:", 100, 10000, 100, step = 100),
            sliderInput("samples", "Number of samples:", 1, 40, 10, step = 1)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("plot")
        )
    )
)

# shiny Server
server <- function(input, output) {
    
    output$intro <- renderText(
        
        paste("This is a small demonstration of the most important concept in inferential statistics i.e. the Central limit theorem. This theorem states that if you have a population with mean μ and standard deviation σ and take sufficiently large random samples from the population with replacement, then the distribution of the sample means (and the summation distribution) will be approximately normally distributed. This will hold true regardless of whether the source population is normal or skewed, provided the sample size is sufficiently large (usually n > 30).")
    )
    
    output$plot  <- renderPlot({
        nosim <- input$simulations
        n <- input$samples
        
        if(input$distribution == "Die roll") {
            set.seed(2604)
            sim <- sample(1:6, nosim * n, replace = TRUE)
            sims <- matrix(sim, nosim)
            mu <- 3.5
            s <-  1.71
            SE <-  s/sqrt(n)
            func <- function(x, n) (mean(x) - mu) / SE
            dat <- data.frame(x = apply(sims, 1, func, n))
        }
        else if(input$distribution == "Coin flip") {
            set.seed(2604)
            sim <- sample(0:1, nosim * n, replace = TRUE)
            sims <- matrix(sim, nosim)
            mu <- 0.5
            s <-  0.5*(1-0.5)
            SE <-  s/sqrt(n)
            func <- function(x, n) (mean(x) - mu) / SE
            dat <- data.frame(x = apply(sims, 1, func, n))
        }
        else if(input$distribution == "Exponential") {
            set.seed(2604)
            sim <- rexp(n*nosim)
            sims <- matrix(sim, nosim)
            mu <- 1.
            s <-  1.
            SE <-  s/sqrt(n)
            func <- function(x, n) (mean(x) - mu) / SE
            dat <- data.frame(x = apply(sims, 1, func, n))
        }
        else if(input$distribution == "Poisson") {
            set.seed(2604)
            sim <- rpois(n*nosim, lambda = 1)
            sims <- matrix(sim, nosim)
            mu <- 1
            s <-  sqrt(mu)
            SE <-  s/sqrt(n)
            func <- function(x, n) (mean(x) - mu) / SE
            dat <- data.frame(x = apply(sims, 1, func, n))
        }
        g <- ggplot(dat, aes(x = x)) + 
            geom_histogram(alpha = .20, bins = 100, colour = "black", aes(y = ..density..)) +
            labs(title = "Distribution of Sample Means", x='sample mean' )
        g
    })

}

# Shiny app
shinyApp(ui, server)
