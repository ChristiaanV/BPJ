library(shiny)
library(tidyverse)
library(shinydashboard)

SimulateInventory <- function (Demand, ROP, lead) {
  n = length(Demand)
  if(length(ROP == 1)) {
    ROP = rep(ROP, n)
  }
  
  inv.purchased <- rep(0, n + lead)
  inv <- rep(NA, n)
  inv[1] <- (ROP[1] + ROP[lead])/2 - Demand[1]
  inv.purchased[lead+1] <- ROP[min(lead + 1, n)]
  
  for (i in 2:n) {
    inv[i] <-  max(0, inv[i-1] - Demand[i] + inv.purchased[i])
    
    if ((inv[i] <= ROP[i]) & (sum(inv.purchased[i:length(inv.purchased)]) == 0)) {
      inv.purchased[i + lead -1] <- ROP[min(floor(((i + lead)+(i + (2*lead)))/2), n)]
    }
  }
  df <- data.frame(Period = 0:n, Inventory = c((ROP[1] + ROP[lead])/2, inv))
  df
}

res <- function(x, y) {
  LinearModel <- lm(y~x)
  -LinearModel$residuals
}

body <-  dashboardBody(   
  
  tags$head(tags$style(HTML('.skin-blue .left-side, .skin-blue .main-sidebar, .skin-blue .wrapper {background-color: #ecf0f5;}'))),
  
  box(
    title = "General Parameters",
    width = 12,
    column(
      width = 6,
      numericInput("DemandVar", "Demand Variance", 30, min = 0),
      numericInput("DemandAvg", "Average Demand per Period", 200, min = 0)
    ),
    column(
      width = 6,
      numericInput("Periods", "Periods", 50, min = 1),
      numericInput("Sims", "Simulations", 1000, min = 1, max = 10000)
    )
  ),
  box(
    title = "Constant Demand",
    width = 6,
    div(style = "height:74px;width:100%"),
    plotOutput("ConstantDemand"),
    htmlOutput("o1"),
    htmlOutput("o2")
  ),
  box(
    title = "Linear Growing Demand",
    width = 6,
    numericInput("DemandGrowth", "Demand growth per period", 4, min = 0),
    plotOutput("LinearDemand"),
    htmlOutput("o3"),
    htmlOutput("o4"),
    br(),
    br(),
    plotOutput("LinearDemand2"),
    htmlOutput("o5"),
    htmlOutput("o6")
  ),
  box(
    title = "Inventory Level Simulation Parameters",
    width = 12,
    column(
      width = 6,
      numericInput("Lead", "Lead Time", min = 2, value = 5)
    ),
    column(
      width = 6,
      numericInput("SS", "Safety Stock Percentage", min = 0, max = 100, value = 10)
    )
  ),
  box(
    title = "Inventory Simulation with Constant Demand",
    width = 6,
    plotOutput("plotConstInv"),
    htmlOutput("o7"),
    htmlOutput("o8"),
    htmlOutput("o9"),
    htmlOutput("o10")
  ),
  box(
    title = "Inventory Simulation with Linear Growing Demand",
    width = 6,
    plotOutput("plotLinInv"),
    htmlOutput("o11"),
    htmlOutput("o12"),
    htmlOutput("o13"),
    htmlOutput("o14")
  )
)

sidebar <- dashboardSidebar(disable = TRUE)
header <- dashboardHeader(title = "BPJ - Pull inventory simulation with linear growth", titleWidth = 550,
                          tags$li(actionLink("openModal", label = "", icon = icon("github"), onclick ="location.href='https://github.com/ChristiaanV/BPJ';"),class = "dropdown"))
ui <- dashboardPage(header, sidebar, body)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$o1 <- renderText(paste("Standard Deviation =",round(sd(getConstantDemand()), 2)))
  output$o2 <- renderText({
    sims <- input$Sims
    
    periods <- input$Periods
    avgDemand <- input$DemandAvg
    varDemand <- input$DemandVar
    DemandGrowth <- input$DemandGrowth
    
    simsd <- rep(NA, sims)
    for (i in 1:sims) {
      simsd[i] <- sd(rnorm(periods, avgDemand, varDemand))
    }
    
    paste("Average Standard Deviation For", sims, "Simulations =", round(mean(simsd),2))
    
  })  
  
  output$o3 <- renderText(paste("Standard Deviation =",round(sd(getLinearDemand()), 2)))
  output$o4 <- renderText({
    sims <- input$Sims
    
    periods <- input$Periods
    avgDemand <- input$DemandAvg
    varDemand <- input$DemandVar
    DemandGrowth <- input$DemandGrowth
    
    simsd <- rep(NA, sims)
    for (i in 1:sims) {
      simsd[i] <- sd(rnorm(periods, avgDemand, varDemand) + (seq(periods) * DemandGrowth) - (periods*DemandGrowth/2))
    }
    
    paste("Average Standard Deviation For", sims, "Simulations =", round(mean(simsd),2))
    
  })  
  
  output$o5 <- renderText(paste("Standard Deviation =",round(sd(res(1:input$Periods,getLinearDemand())), 2)))
  output$o6 <- renderText({
    sims <- input$Sims
    
    periods <- input$Periods
    avgDemand <- input$DemandAvg
    varDemand <- input$DemandVar
    DemandGrowth <- input$DemandGrowth
    
    simsd <- rep(NA, sims)
    for (i in 1:sims) {
      simsd[i] <- sd(rnorm(periods, avgDemand, varDemand))
    }
    
    paste("Average Standard Deviation For", sims, "Simulations =", round(mean(simsd),2))
    
  })
  
  
  getMax <- reactive({
    periods <- input$Periods
    avgDemand <- input$DemandAvg
    varDemand <- input$DemandVar
    DemandGrowth <- input$DemandGrowth
    
    # 5 sigma has a probabilty of 1 in 1744278 * 2 of being too low for the first or last value
    result <- avgDemand + abs((DemandGrowth * periods * 0.5)) + (5 * varDemand)
    
    return(result)
  })
  
  
  getConstantDemand <- reactive({
    periods <- input$Periods
    avgDemand <- input$DemandAvg
    varDemand <- input$DemandVar
    rnorm(periods, avgDemand, varDemand)
  })
  output$ConstantDemand <- renderPlot({
    avgDemand <- input$DemandAvg
    periods <- input$Periods
    
    df <- data.frame(Period = seq(periods), Demand = getConstantDemand())
    ggplot(df, aes(x = Period, y = Demand)) +
      geom_point() +
      scale_y_continuous(limits = c(0, max(getMax()))) +
      theme_minimal() +
      geom_hline(yintercept = avgDemand, color = "blue", size = 1) +
      ylab("Demand [Units/Period]") +
      labs(title = "Simulated demand levels over time with no increase in demand")
  }) 
  
  getConstantSim <- reactive({
    Lead <- input$Lead
    SS <- input$SS / 100
    Demand <- getConstantDemand()
    DemandAvg <- input$DemandAvg
    ROP <- Lead * (1 + SS) * DemandAvg
    SimulateInventory(Demand, ROP, Lead)
  })
  
  output$plotConstInv <- renderPlot({
    Lead <- input$Lead
    SS <- input$SS / 100
    DemandAvg <- input$DemandAvg
    ROP <- Lead * (1 + SS) * DemandAvg
    
    getConstantSim() %>%
      ggplot(aes(x = Period, y = Inventory)) +
      geom_line() +
      theme_minimal() +
      geom_hline(aes(yintercept = ROP, color = "ROP")) + 
      theme(legend.title = element_blank()) +
      ylab("Inventory Level [Units]") +
      labs(title = "Simulated inventory levels over time with no increase in demand")
  })
  
  getConstSimSim <- reactive({
    sims <- input$Sims
    Lead <- input$Lead
    SS <- input$SS / 100
    DemandAvg <- input$DemandAvg
    varDemand <- input$DemandVar
    periods <- input$Periods
    ROP <-  Lead * (1 + SS) * DemandAvg
    
    simSO <- rep(NA, sims)
    simAvgInv <- rep(NA, sims)
    for (i in 1:sims) {
      Demand <- rnorm(periods, DemandAvg, varDemand)
      sim <- SimulateInventory(Demand, ROP, Lead)
      simSO[i] <- length(sim$Inventory[sim$Inventory == 0])
      simAvgInv[i] <- mean(sim$Inventory)
    }
    
    return(c(SO = mean(simSO), AvgInv = mean(simAvgInv)))
  })
  
  output$o7 <- renderText(paste("Average Inventory Level =", round(mean(getConstantSim()$Inventory),2)))  
  output$o8 <- renderText(paste("Stockout Count  =", round(length(getConstantSim()$Inventory[getConstantSim()$Inventory == 0]),2)))
  output$o9 <- renderText(paste("Average Simulated Average Inventory Level For", input$Sims, "Simulations =", round(mean(getConstSimSim()["AvgInv"]),2)))  
  output$o10 <- renderText(paste("Average Simulated Stockout Count For", input$Sims, "Simulations =", round(mean(getConstSimSim()["SO"]),2)))  
  
  getLinearDemand <- reactive({
    periods <- input$Periods
    avgDemand <- input$DemandAvg
    varDemand <- input$DemandVar
    DemandGrowth <- input$DemandGrowth
    
    rnorm(periods, avgDemand, varDemand) + (seq(periods) * DemandGrowth) - (periods*DemandGrowth/2)
    
  })
  output$LinearDemand <- renderPlot({
    periods <- input$Periods
    
    df <- data.frame(Period = seq(periods), 
                     Demand = getLinearDemand())
    
    ggplot(df, aes(x = Period, y = Demand)) +
      geom_point() +
      scale_y_continuous(limits = c(0, max(getMax()))) +
      theme_minimal() +
      geom_smooth(method = "lm", se = F) +
      ylab("Demand [Units/Period]") +
      labs(title = "Simulated demand levels over time with linear increase in demand")
  })
  output$LinearDemand2 <- renderPlot({
    periods <- input$Periods
    df <- data.frame(Period = seq(periods), 
                     Demand = res(1:periods, getLinearDemand()))
    
    ggplot(df, aes(x = Period, y = Demand)) +
      geom_point() +
      theme_minimal() +
      ylab("Residuals of Demand Model [Units/Period]") +
      labs(title = "Linear model residual values of simulated demand levels over time with linear increase in demand")
  }) 
  getLinearSim <- reactive({
    Lead <- input$Lead
    SS <- input$SS / 100
    Demand <- getLinearDemand()
    DemandAvg <- input$DemandAvg
    periods <- input$Periods
    DemandGrowth <- input$DemandGrowth
    ROP <- Lead * (1 + SS) * ((seq(periods) * DemandGrowth) - (periods*DemandGrowth/2) + DemandAvg)
    
    SimulateInventory(Demand, ROP, Lead)
  })
  output$plotLinInv <- renderPlot({
    Lead <- input$Lead
    SS <- input$SS / 100
    DemandAvg <- input$DemandAvg
    periods <- input$Periods
    DemandGrowth <- input$DemandGrowth
    ROP <- Lead * (1 + SS) * ((seq(periods) * DemandGrowth) - (periods*DemandGrowth/2) + DemandAvg)
    
    getLinearSim() %>%
      ggplot(aes(x = Period, y = Inventory)) +
      geom_line() +
      theme_minimal() +
      geom_line(aes(x = c(NA,seq(periods)),y = c(NA,ROP), color = "ROP")) + 
      theme(legend.title = element_blank()) +
      ylab("Inventory Level [Units]") +
      labs(title = "Simulated inventory levels over time with linear increase in demand")
  })
  
  
  output$o11 <- renderText(paste("Average Inventory Level =", round(mean(getLinearSim()$Inventory),2)))  
  output$o12 <- renderText(paste("Stockout Count  =", round(length(getLinearSim()$Inventory[getLinearSim()$Inventory == 0]),2)))
  output$o13 <- renderText(paste("Average Simulated Average Inventory Level For", input$Sims, "Simulations =", round(mean(getLinSimSim()["AvgInv"]),2)))  
  output$o14 <- renderText(paste("Average Simulated Stockout Count For", input$Sims, "Simulations =", round(mean(getLinSimSim()["SO"]),2)))  
  
  getLinSimSim <- reactive({
    sims <- input$Sims
    Lead <- input$Lead
    SS <- input$SS / 100
    DemandAvg <- input$DemandAvg
    varDemand <- input$DemandVar
    periods <- input$Periods
    DemandGrowth <- input$DemandGrowth
    ROP <- Lead * (1 + SS) * ((seq(periods) * DemandGrowth) - (periods*DemandGrowth/2) + DemandAvg)
    
    simSO <- rep(NA, sims)
    simAvgInv <- rep(NA, sims)
    for (i in 1:sims) {
      Demand <- rnorm(periods, DemandAvg, varDemand) + (seq(periods) * DemandGrowth) - (periods*DemandGrowth/2)
      sim <- SimulateInventory(Demand, ROP, Lead)
      simSO[i] <- length(sim$Inventory[sim$Inventory == 0])
      simAvgInv[i] <- mean(sim$Inventory)
    }
    
    return(c(SO = mean(simSO), AvgInv = mean(simAvgInv)))
  })
  
}

shinyApp(ui = ui, server = server)

