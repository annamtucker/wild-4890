# shiny app to explore lotka-volterra predator-prey dynamics
# uses predprey_models.md in second tab to display equations
# WILD 4890 spring 18

library(shiny)

# Define UI for application 
ui <- fluidPage(
  
  # Application title
  titlePanel("Lotka-Volterra Predator-Prey Model"),
  
  sidebarLayout(
    sidebarPanel(
      
      # Number of years
      numericInput("t", min = 5, max = 250, value = 100, step = 5,
                  label = "Number of years"),
      
      sliderInput("Cinit", min = 0, max = 100, value = 20, step = 1,
                  label = "Initial number of predators"),
      
      sliderInput("Vinit", min = 0, max = 5000, value = 1000, step = 10,
                  label = "Initial number of prey"),
      
      # prey growth rate
      numericInput("R", min = 0, max = 3, value = 0.25,step = 0.01,
                  label = "Prey growth rate (R)"),
      
      # pred starvation rate
      numericInput("q", min = 0, max = 1, value = 0.08, step = 0.001,
                   label = "Predator starvation rate (q)"),
      
      # pred conversion efficiency
      numericInput("f", min = 0, max = 1, value = 0.006, step = 0.001,
                   label = "Predator conversion efficiency (f)"),
      
      # attack rate
      numericInput("a", min = 0, max = 1, value = 0.01, step = 0.001,
                   label = "Predator attack rate (a)"),
      
      # prey carrying capacity
      checkboxInput("preyK", label = "Add prey density-dependence", value = F),
      numericInput("Kv", min = 10, max = 100000000, step = 10,value = 2000,
                   label = "Prey carrying capacity (Kv)"),
      
      # predator carrying capacity
      checkboxInput("predK", label = "Add predator density-dependence", value = F),
      numericInput("Kc", min = 10, max= 10000000, step = 10, value = 100,
                   "Predator carrying capacity (Kc)"),
      
      # prey refugia
      checkboxInput("refugia", label = "Add prey refuges", value = F),
      numericInput("minprey", label = "Minimum prey population size", value = 10)
      
    ),

    mainPanel(

      tabsetPanel(type = "tabs",
                  tabPanel("Plots",
                  # Plot results of simulations
                  fluidPage(
                    fluidRow(column(6, plotOutput("plot1")),
                             column(6, plotOutput("plot1a"))),
                    fluidRow(column(12, style = "background-color:white;",
                                    div(style = "height:50px;"))),
                    fluidRow(column(6, plotOutput("plot2")),
                             column(6, plotOutput("plot3"))))),
                  tabPanel("Equations",
                           includeHTML("models.html")))
    )
  )
)

# Define server logic 
server <- function(input, output){
  library(tidyverse)
  library(cowplot)
  library(markdown)
  library(knitr)
  
  simdat <- reactive({
    
    R = input$R
    q = input$q
    f = input$f
    a = input$a
    Kv = input$Kv
    Kc = input$Kc
    t = input$t
    
    C = rep(NA, t)
    C[1] = input$Cinit
    V = rep(NA, t)
    V[1] = input$Vinit
    
    #FFF
    if(!input$preyK & !input$predK & !input$refugia){
      for(i in 2:t){
        V[i] = ifelse(V[i-1] + R*V[i-1] - a*C[i-1]*V[i-1] > 0, 
                      V[i-1] + R*V[i-1] - a*C[i-1]*V[i-1], 0)
        C[i] = ifelse(C[i-1] + a*f*V[i-1]*C[i-1] - q*C[i-1] > 0, 
                      C[i-1] + a*f*V[i-1]*C[i-1] - q*C[i-1], 0)
      }
    }
    
    #FFT
    else if(!input$preyK & !input$predK & input$refugia){
      for(i in 2:t){
        V[i] = ifelse(V[i-1] + R*V[i-1] - a*C[i-1]*V[i-1] > input$minprey, 
                      V[i-1] + R*V[i-1] - a*C[i-1]*V[i-1], input$minprey)
        C[i] = ifelse(C[i-1] + a*f*V[i-1]*C[i-1] - q*C[i-1] > 0, 
                      C[i-1] + a*f*V[i-1]*C[i-1] - q*C[i-1], 0)
      }
    }
    
    
    #TFF
    else if(input$preyK & !input$predK & !input$refugia){
      for(i in 2:t){
        V[i] = ifelse(V[i-1] + R*V[i-1]*((Kv-V[i-1])/Kv) - a*C[i-1]*V[i-1] > 0, 
                      V[i-1] + R*V[i-1]*((Kv-V[i-1])/Kv) - a*C[i-1]*V[i-1], 0)
        C[i] = ifelse(C[i-1] + a*f*V[i-1]*C[i-1] - q*C[i-1] > 0, 
                      C[i-1] + a*f*V[i-1]*C[i-1] - q*C[i-1], 0)
      }
    }
    
    #TFT
    else if(input$preyK & !input$predK & input$refugia){
      for(i in 2:t){
        V[i] = ifelse(V[i-1] + R*V[i-1]*((Kv-V[i-1])/Kv) - a*C[i-1]*V[i-1] > input$minprey, 
                      V[i-1] + R*V[i-1]*((Kv-V[i-1])/Kv) - a*C[i-1]*V[i-1], input$minprey)
        C[i] = ifelse(C[i-1] + a*f*V[i-1]*C[i-1] - q*C[i-1] > 0, 
                      C[i-1] + a*f*V[i-1]*C[i-1] - q*C[i-1], 0)
      }
    }
    
    #TTF
    else if(input$preyK & input$predK & !input$refugia){
      for(i in 2:t){
        V[i] = ifelse(V[i-1] + R*V[i-1]*((Kv-V[i-1])/Kv) - a*C[i-1]*V[i-1] > 0, 
                      V[i-1] + R*V[i-1]*((Kv-V[i-1])/Kv) - a*C[i-1]*V[i-1], 0)
        C[i] = ifelse(C[i-1] + a*f*V[i-1]*C[i-1]*((Kc-C[i-1])/Kc) - q*C[i-1] > 0, 
                      C[i-1] + a*f*V[i-1]*C[i-1]*((Kc-C[i-1])/Kc) - q*C[i-1], 0)
      }
    }
    
    #FTF
    else if(!input$preyK & input$predK & !input$refugia){
      for(i in 2:t){
        V[i] = ifelse(V[i-1] + R*V[i-1] - a*C[i-1]*V[i-1] > 0, 
                      V[i-1] + R*V[i-1] - a*C[i-1]*V[i-1], 0)
        C[i] = ifelse(C[i-1] + a*f*V[i-1]*C[i-1]*((Kc-C[i-1])/Kc) - q*C[i-1] > 0, 
                      C[i-1] + a*f*V[i-1]*C[i-1]*((Kc-C[i-1])/Kc) - q*C[i-1], 0)
      }
    }
    
    #FTT
    else if(!input$preyK & input$predK & input$refugia){
      for(i in 2:t){
        V[i] = ifelse(V[i-1] + R*V[i-1] - a*C[i-1]*V[i-1] > 10, 
                      V[i-1] + R*V[i-1] - a*C[i-1]*V[i-1], 10)
        C[i] = ifelse(C[i-1] + a*f*V[i-1]*C[i-1]*((Kc-C[i-1])/Kc) - q*C[i-1] > 0, 
                      C[i-1] + a*f*V[i-1]*C[i-1]*((Kc-C[i-1])/Kc) - q*C[i-1], 0)
      }
    }

    #TTT
    else if(input$preyK & input$predK & input$refugia){
      for(i in 2:t){
        V[i] = ifelse(V[i-1] + R*V[i-1]*((Kv-V[i-1])/Kv) - a*C[i-1]*V[i-1] > input$minprey, 
                      V[i-1] + R*V[i-1]*((Kv-V[i-1])/Kv) - a*C[i-1]*V[i-1], input$minprey)
        C[i] = ifelse(C[i-1] + a*f*V[i-1]*C[i-1]*((Kc-C[i-1])/Kc) - q*C[i-1] > 0, 
                      C[i-1] + a*f*V[i-1]*C[i-1]*((Kc-C[i-1])/Kc) - q*C[i-1], 0)
      }
    }
    
    data.frame(V, C, Time = c(1:length(V)))
  })
  

  output$plot1 <- renderPlot({

    dat <- simdat()
    sf = max(dat$V)/max(dat$C)
    
    if(!input$preyK & !input$predK){
      
      plot <- dat %>% 
        mutate(Cplot = C*sf) %>% 
        select(V, Cplot, Time) %>% 
        gather(type, N, 1:2) %>% 
        ggplot(aes(x = Time, y = N, col = type)) +
        scale_y_continuous("Prey population size", 
                           sec.axis = sec_axis(~./sf, name = "Predator population size")) +
        geom_line(lwd = 2) +
        scale_color_manual(labels = c("Predator", "Prey"), name = "",
                             values = c("dodgerblue4", "palegreen3")) +
        theme(legend.position = "top",
              axis.title = element_text(size = 14),
              legend.text = element_text(size = 14)) +
        ggtitle("Population size over time")
    }
    
    if(input$preyK & !input$predK){
      plot <- simdat() %>%
        mutate(Cplot = C*sf) %>% 
        select(V, Cplot, Time) %>% 
        gather(type, N, 1:2) %>% 
        ggplot(aes(x = Time, y = N, col = type)) +
        scale_y_continuous("Prey population size", 
                           sec.axis = sec_axis(~./sf, name = "Predator population size")) +
        geom_line(lwd = 2) +
        geom_hline(yintercept = input$Kv, lty = 2, col = "palegreen3") +
        scale_color_manual(labels = c("Predator", "Prey"), name = "",
                           values = c("dodgerblue4", "palegreen3")) +
        theme(legend.position = "top",
              axis.title = element_text(size = 14),
              legend.text = element_text(size = 14)) +
        ggtitle("Population size over time")
    }
    
    if(!input$preyK & input$predK){
      plot <-  simdat() %>%
        mutate(Cplot = C*sf) %>% 
        select(V, Cplot, Time) %>% 
        gather(type, N, 1:2) %>% 
        ggplot(aes(x = Time, y = N, col = type)) +
        scale_y_continuous("Prey population size", 
                           sec.axis = sec_axis(~./sf, name = "Predator population size")) +
        geom_hline(yintercept = input$Kc*100, lty = 2, col = "dodgerblue4") +
        geom_line(lwd = 2) +
        scale_color_manual(labels = c("Predator", "Prey"), name = "",
                           values = c("dodgerblue4", "palegreen3")) +
        theme(legend.position = "top",
              axis.title = element_text(size = 14),
              legend.text = element_text(size = 14)) +
        ggtitle("Population size over time")
    }
    
    if(input$preyK & input$predK){
      plot <- simdat() %>%
        mutate(Cplot = C*sf) %>% 
        select(V, Cplot, Time) %>% 
        gather(type, N, 1:2) %>% 
        ggplot(aes(x = Time, y = N, col = type)) +
        scale_y_continuous("Prey population size", 
                           sec.axis = sec_axis(~./sf, name = "Predator population size")) +
        geom_hline(yintercept = input$Kv, lty = 2, col = "palegreen3") +
        geom_hline(yintercept = input$Kc*100, lty = 2, col = "dodgerblue4") +
        geom_line(lwd = 2) +
        scale_color_manual(labels = c("Predator", "Prey"), name = "",
                           values = c("dodgerblue4", "palegreen3")) +
        theme(legend.position = "top",
              axis.title = element_text(size = 14),
              legend.text = element_text(size = 14)) +
        ggtitle("Population size over time")
    }
    plot

  })
  
  output$plot1a <- renderPlot({
    
    if(!input$predK){
      plot1 <- simdat() %>%
        ggplot(aes(x = Time, y = C)) +
        ylab("Population size") +
        geom_line(lwd = 2, col = "dodgerblue4") +
        theme(axis.title = element_text(size = 14),
              legend.text = element_text(size = 14)) +
        ggtitle("Predator")
    }
    
    if(input$predK){
      plot1 <- simdat() %>%
        ggplot(aes(x = Time, y = C)) +
        ylab("Population size") +
        geom_hline(yintercept = input$Kc, lty = 2, col = "dodgerblue4") +
        geom_line(lwd = 2, col = "dodgerblue4") +
        theme(axis.title = element_text(size = 14),
              legend.text = element_text(size = 14)) +
        ggtitle("Predator")
    }
    
    if(!input$preyK){
      plot2 <- simdat() %>%
        ggplot(aes(x = Time, y = V)) +
        ylab("Population size") +
        geom_line(lwd = 2, col = "palegreen3") +
        theme(axis.title = element_text(size = 14),
              legend.text = element_text(size = 14)) +
        ggtitle("Prey")
    }
    
    if(input$preyK){
      plot2 <- simdat() %>%
        ggplot(aes(x = Time, y = V)) +
        ylab("Population size") +
        geom_hline(yintercept = input$Kv, lty = 2, col = "palegreen3") +
        geom_line(lwd = 2, col = "palegreen3") +
        theme(axis.title = element_text(size = 14),
              legend.text = element_text(size = 14)) +
        ggtitle("Prey")
    }
    
    plot_grid(plot1, plot2, ncol = 1)
    
  })
  
  output$plot2 <- renderPlot({

    dat <- simdat()
    
    minC = round(min(dat$C))
    maxC = round(max(dat$C))
    minV = round(min(dat$V))
    maxV = round(max(dat$V))
    
    ggplot(dat, aes(x = V, y = C)) +
      geom_rect(data = dat[1,], ymin = 0, ymax = input$R/input$a, xmin = -10, xmax = input$q/(input$a*input$f),
                aes(fill = "4"), alpha = 0.75) +
      geom_rect(data = dat[1,], ymin = input$R/input$a, ymax = maxC+10, xmin = -10, xmax = input$q/(input$a*input$f),
                aes(fill = "1"), alpha = 0.75) +
      geom_rect(data = dat[1,], ymin = 0, ymax = input$R/input$a, xmin = input$q/(input$a*input$f), xmax = maxV+500,
                aes(fill = "3"), alpha = 0.75) +
      geom_rect(data = dat[1,], ymin = input$R/input$a, ymax = maxC+10, xmin = input$q/(input$a*input$f), xmax = maxV+500,
                aes(fill = "2"), alpha = 0.75) +
      geom_hline(yintercept = input$R/input$a, lty = 2, lwd = 1) +
      geom_vline(xintercept = input$q/(input$a*input$f), lty = 2, lwd = 1) +
      geom_path(lwd = 2, arrow = arrow(type = "closed"))+
      xlab("Prey population size") +
      ylab("Predator population size") +
      theme(axis.title = element_text(size = 14),
            legend.position = "top") +
      scale_fill_manual(values = c("1" = "gray80", 
                                   "2" = "dodgerblue4",
                                   "3" = "#0b7267",
                                   "4" = "palegreen4"),
                        labels = c("both decreasing",
                                   "predator increasing\nprey decreasing",
                                   "both increasing",
                                   "prey increasing\npredator decreasing"),
                        name = "") +
      guides(fill = guide_legend(override.aes = list(alpha = 0.5)))
    
  })
  
  output$plot3 <- renderPlot({
    
    pdat = data.frame(V = c(0:10), C = c(0:10))
    
    ggplot(pdat, aes(x = V, y = C)) +
      geom_point(alpha = 0) +
      geom_hline(yintercept = 5, lty = 2, lwd = 2) +
      geom_vline(xintercept = 5, lty = 2, lwd = 2) +
      xlab("Prey population size") +
      ylab("Predator population size") +
      theme(axis.ticks = element_blank(),
            axis.text = element_blank()) +
      geom_segment(x = 2, xend = 2, y = 6.5, yend = 3.5, lwd= 2,
                   arrow = arrow(type = "open"), col = "dodgerblue4") +
      geom_segment(x = 2, xend = 2, y = 6.5, yend = 3.5, lwd= 2, aes(col = "1")) +
      geom_segment(x = 8, xend = 8, y = 3.5, yend = 6.5, lwd= 2,
                   arrow = arrow(type = "open"), col = "dodgerblue4") +
      geom_segment(x = 4, xend = 6, y = 2, yend = 2, lwd= 2,
                   arrow = arrow(type = "open"), col = "palegreen3") +
      geom_segment(x = 4, xend = 6, y = 2, yend = 2, lwd= 2, aes(col = "2")) +
      geom_segment(x = 6, xend = 4, y = 8, yend = 8, lwd= 2,
                   arrow = arrow(type = "open"), col = "palegreen3") +
      ggtitle("Zero net growth isoclines") +
      theme(legend.position = "top",
            axis.title = element_text(size = 14),
            legend.text = element_text(size = 14)) +
      scale_color_manual(name="", values = c("1" = "dodgerblue4",
                                             "2" = "palegreen3"),
                         labels = c("Predator", "Prey")) 

  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)


# Deploy to shinyapps.io
#require(rsconnect)
#setAccountInfo(name='amtucker',
#               token='2856B2DDC86791E8EA770446491E2F79',
#               secret='TKnHpFfthgC2j+IV0dHJrekpW8WLH5ejV8x2rt6J')
#setwd("C:/Users/amt0046/Dropbox/Spring 2017 Teaching/WILD 4890 Pop Dynamics/lab materials/pred_prey")
#deployApp()


