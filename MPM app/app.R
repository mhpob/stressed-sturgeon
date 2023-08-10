#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Atlantic sturgeon Leslie matrix"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("lifespan",
                  "Lifespan:",
                  min = 30,
                  max = 70,
                  value = 60),
      sliderInput("maturity",
                  "Age at maturity:",
                  min = 5,
                  max = 25,
                  value = 16),
      sliderInput("surv_1p",
                  "Survival (Age 1+):",
                  min = 0,
                  max = 1,
                  step = 0.01,
                  value = 0.93),
      dateInput("spawn_date",
                "Spawn date:",
                format = 'mm-dd',
                value = '2023-06-15'),
      sliderInput("spawn_int",
                  "Spawn interval:",
                  min = 0,
                  max = 10,
                  step = 0.5,
                  value = 4.5),
      sliderInput("fec_interval",
                  "Fecundity function intercept:",
                  min = -5e6,
                  max = 1e6,
                  value = -1304704),
      sliderInput("fec_slope",
                  "Fecundity function slope:",
                  min = 0,
                  max = 5e6,
                  value = 111909)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h2('Model-solved YOY survival'),
      textOutput("vital_rates"),
      
      h2('Elasticity'),
      plotOutput("elas")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  library(Matrix)
  row1_fun <- function(age,
                       spawn_int = input$spawn_int,
                       spawn_date = input$spawn_date,
                       maturity = input$maturity,
                       surv_1p = input$surv_1p,
                       fec_intercept = input$fec_interval,
                       fec_slope = input$fec_slope){
    # This is calculated from the fecundity function (Table 1),
    # divided by spawning interval (e.g., three years for shortnose sturgeon)...
    
    fecund_func <- function(age, maturity, fec_intercept, fec_slope){
      if(age < maturity){
        0
      }else{
        fec_intercept + fec_slope * age
      }
    }
    step1 <- fecund_func(age, maturity, fec_intercept, fec_slope) / spawn_int
    
    # discounted by the mortality of females between the 1 January census and the
    # spawning date...
    discount <- function(spawn_date, surv_1p){
      if(is.character(spawn_date)|class(spawn_date) == 'Date'){
        n_days <- as.numeric(
          difftime(
            as.Date(spawn_date),
            as.Date('2023-01-01'),
            units = 'days'
          )
        )
      }else{
        n_days <- spawn_date - 1
      }
      
      pct_yr <- n_days / 365
      
      exp(log(surv_1p) * pct_yr)
    }
    
    step2 <- step1 * discount(spawn_date, surv_1p)
    
    # and then divided by two to count only those eggs that can mature into females.
    step2 / 2
  }
  
  # Make population matrix; sparse, so the below define where non-zero values are
  popmat <- function(lifespan = input$lifespan,
                     maturity = input$maturity,
                     spawn_int = input$spawn_int,
                     spawn_date = input$spawn_date,
                     surv_1p = input$surv_1p,
                     fec_intercept = input$fec_interval,
                     fec_slope = input$fec_slope){
    sparseMatrix(
      i = c(
        # Egg production in Row 1 in cols from maturity to death
        rep(1, times = (lifespan - maturity) + 1),
        # survivorship in off diagonal
        2:(lifespan + 1)
      ),
      j = c(
        # Egg production in Row 1 in cols from maturity to death
        (maturity + 1):(lifespan + 1),
        # survival rate in off diagonal
        1:lifespan
      ),
      
      x = c(
        # fill in egg production
        sapply(maturity:lifespan,
               row1_fun,
               spawn_int,
               spawn_date,
               maturity,
               surv_1p,
               fec_intercept,
               fec_slope),
        # and survival rate
        rep(surv_1p, lifespan)
      ),
      # name the axes
      dimnames = list(0:lifespan, 0:lifespan)
    )
  }
  # solve for lambda = 1
  lambda_solve <- function(pop = popmat()){
    obj_fun <- function(x, popmat){
      popmat[2, 1] <- x
      norm(det(popmat - Diagonal(nrow(popmat))), '2')
    }
    
    res <- optimize(obj_fun, interval = c(0, 1), 
                    tol = 1e-10, popmat = pop)
    
    pop[2, 1] <- res$minimum
    
    list(
      pop = pop,
      surv_yoy = res$minimum
    )
  }
  output$vital_rates <- renderPrint(
    lambda_solve()$surv_yoy
  )
  
  # Elasticity
  elasticity <- function(pop_mat, zero = FALSE){
    ## hacking popbio sensitivity and elasticity
    ev <- eigen(pop_mat)
    lmax <- which.max(Re(ev$values))
    W <- ev$vectors
    w <- abs(Re(W[, lmax]))
    V <- try(Conj(solve(W)), silent = TRUE)
    if ('try-error' %in% class(V)) {
      message("Warning: matrix is singular")
      s <- pop_mat * NA
    }else {
      v <- abs(Re(V[lmax, ]))
      s <- v %o% w
      # if (zero) {
      #   s[pop_mat == 0] <- 0
      # }
      dimnames(s) <- dimnames(pop_mat)
    }
    
    elas <- s * pop_mat/popbio::lambda(pop_mat)
    elas
  }
  
  
  pop_elas_df <- function(popmat = lambda_solve()$pop){
    pop_elas <-
      popmat |>
      elasticity() |>
      summary() |>
      data.frame()
    
    data.frame(
      age = c(pop_elas[pop_elas$i == 1, 'j'],
              pop_elas[pop_elas$i != 1, 'j']),
      type = c(rep('fecundity',
                   length(pop_elas[pop_elas$i == 1, 'j'])),
               rep('survival',
                   length(pop_elas[pop_elas$i != 1, 'j']))),
      value = c(pop_elas[pop_elas$i == 1, 'x'],
                pop_elas[pop_elas$i != 1, 'x'])
    )
    
  }
  
  library(ggplot2)
  output$elas <- renderPlot(
    
    ggplot() +
      geom_col(data = pop_elas_df(),
               aes(x = age-1, y = value * 100, fill = type),
               position = position_dodge2(preserve = 'single')) +
      labs(x = 'Age', y = 'Elasticity (%)', color = NULL) +
      theme_bw() +
      theme(legend.position = c(0.8, 0.8))
    
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
