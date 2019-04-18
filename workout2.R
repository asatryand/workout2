library(shiny)
library(ggplot2)



ui <- fluidPage(
   
   titlePanel("Saving and Investing Modalities"),
   
   
   fluidRow(
     column(4,
            sliderInput("initial",
                        "Initial Amount",
                        min = 0,
                        max = 100000,
                        value = 1000,
                        step = 500,
                        pre = "$", 
                        sep = ",",
                        animate = TRUE)),
     column(4,
            sliderInput("rr",
                        "Return Rate (in %)",
                        min = 0,
                        max = 20,
                        value = 5,
                        step = 0.1)),
     
     column(4,
            sliderInput("years",
                        "Years",
                        min = 0,
                        max = 50,
                        value = 20,
                        step = 1))
   ),
   
   
   fluidRow(
     column(4,
            sliderInput("anncontrib",
                        "Annual Contribution",
                        min = 0,
                        max = 50000,
                        value = 2000,
                        step = 500,
                        pre = "$", 
                        sep = ",",
                        animate = TRUE)),
     
     column(4,
            sliderInput("growth_rate",
                        "Growth Rate (in %)",
                        min = 0,
                        max = 20,
                        value = 2,
                        step = 0.1)),
     
     column(4,
            selectInput("fct",
                        "Facet?",
                        c("No", "Yes")))
   ),
   
    
   
    h4("Timeline"),
    plotOutput("investing"),
        
    h4("Balances"),
    tableOutput("table")
  
      
)

server <- function(input, output) {
  
  
  output$table <- renderTable({
    
    future_value <- function(amount, rate, years) {
      right = (1 + rate)^years
      
      return(amount * right)
    }
    
    annuity <- function(contrib, rate, years) {
      right = ((1 + rate)^years) - 1
      down = right / rate
      return(down * contrib)
    }
    
    growing_annuity <- function(contrib, rate, growth, years) {
      right <- ((1 + rate)^years) - ((1 + growth)^years)
      down <- right / (rate - growth)
      return(contrib * down)
    }
    my_vector1 <- c(input$initial)
    my_vector2 <- c(input$initial)
    my_vector3 <- c(input$initial)
    
    for (x in 1:input$years) {
      my_vector1[x] <- future_value(input$initial, input$rr/100, x)
      my_vector2[x] <- annuity(input$anncontrib, input$rr/100, x) + future_value(input$initial, input$rr/100, x)
      my_vector3[x] <- growing_annuity(input$anncontrib, input$rr/100, input$growth_rate/100, x) + future_value(input$initial, input$rr/100, x)
    }
    
    
    modalities <- data.frame(
      year = 0:input$years, 
      no_contrib = c(input$initial, my_vector1), 
      fixed_contrib = c(input$initial, my_vector2), 
      growing_contrib = c(input$initial, my_vector3))
    
    
    
  })
  
  output$investing <- renderPlot({
    
    future_value <- function(amount, rate, years) {
      right = (1 + rate)^years
      
      return(amount * right)
    }
    
    annuity <- function(contrib, rate, years) {
      right = ((1 + rate)^years) - 1
      down = right / rate
      return(down * contrib)
    }
    
    growing_annuity <- function(contrib, rate, growth, years) {
      right <- ((1 + rate)^years) - ((1 + growth)^years)
      down <- right / (rate - growth)
      return(contrib * down)
    }
    my_vector1 <- c(input$initial)
    my_vector2 <- c(input$initial)
    my_vector3 <- c(input$initial)
    
    for (x in 1:input$years) {
      my_vector1[x] <- future_value(input$initial, input$rr/100, x)
      my_vector2[x] <- annuity(input$anncontrib, input$rr/100, x) + future_value(input$initial, input$rr/100, x)
      my_vector3[x] <- growing_annuity(input$anncontrib, input$rr/100, input$growth_rate/100, x) + future_value(input$initial, input$rr/100, x)
    }
    
    
    modalities <- data.frame(
      year = 0:input$years, 
      no_contrib = c(input$initial, my_vector1), 
      fixed_contrib = c(input$initial, my_vector2), 
      growing_contrib = c(input$initial, my_vector3))
    
  
    if (input$fct == "Yes") {
      
      plor1 <- c()
      
      for (x in 1:input$years) {
        plor1[x] <- future_value(input$initial, input$rr/100, x)
      }
      
      new_modalities1 <- data.frame(
        type = rep("no_contrib", input$years + 1),
        year = c(0:input$years), 
        result = c(input$initial, plor1) 
      )
      
      plor2 <- c()
      
      for (x in 1:input$years) {
        plor2[x] <- annuity(input$anncontrib, input$rr/100, x) + future_value(input$initial, input$rr/100, x)
      }
      
      new_modalities2 <- data.frame(
        type = rep("fixed_contrib", input$years + 1),
        year = c(0:input$years),
        result = c(input$initial, plor2)
      )
      
      plor3 <- c()
      
      for (x in 1:input$years) {
        plor3[x] <- growing_annuity(input$anncontrib, input$rr/100, input$growth_rate/100, x) + future_value(input$initial, input$rr/100, x)
      }
      
      new_modalities3 <- data.frame(
        type = rep("growing_contrib", input$years + 1),
        year = c(0:input$years),
        result = c(input$initial, plor3)
      )
      
      total <- rbind(new_modalities1, new_modalities2, new_modalities3)
      
      ggplot(total, aes(x = total$year, y = total$result, color = type)) + geom_line() + scale_colour_manual(values=c("red","green","blue")) + 
        facet_wrap(~ type) + geom_ribbon(aes(ymin = 0, ymax = total$result, fill = type)) + xlab("year") + ylab("value") + ggtitle("Three modes of investing")
      
    } else {
      
      ggplot(modalities, aes(modalities$year, modalities$growing_contrib, colour = "growing_contrib")) + geom_line() +
        geom_line(aes(modalities$year, modalities$fixed_contrib, colour = "fixed_contrib")) +
        geom_line(aes(modalities$year, modalities$no_contrib, colour = "no_contrib")) +
        xlab("year") +
        ylab("value") + scale_colour_manual(values=c("green","blue","red")) + ggtitle("Three modes of investing")
    }
    
    
  })
  
  
}



shinyApp(ui = ui, server = server)

