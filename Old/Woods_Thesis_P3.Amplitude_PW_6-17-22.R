#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(dygraphs)
library(tidyverse)
library(readxl)
library(RcppRoll)
library(RColorBrewer)
library(writexl)

ui <- fluidPage(
  
  titlePanel("Stretch Activation and Fatigue - Woods' Masters Thesis Analysis"),
  
  sidebarLayout(
    
    sidebarPanel(
      fileInput(inputId = "file", 
                label = "Select a File"),  
      actionButton("load_file", "Load File"),
  ), 
    mainPanel(
      dygraphOutput("interactive_plot"),
      
      conditionalPanel(
        condition = "input.tabselected == 1",
        actionButton("set_phase_3", "Set Phase 3 Boundaries")
      ),
      
      conditionalPanel(
        condition = "input.tabselected == 2",
        actionButton("phase2_4", "Set Boundaries")
      ),
      
      tabsetPanel(
        tabPanel("Amplitude",
                 value = 1,
                 conditionalPanel(condition = "input.actionButton == 'set_phase_3'",
                   plotOutput("phase_3"),
                   tableOutput("datatable"),
                   verbatimTextOutput("Fsa"), 
                   verbatimTextOutput("Fsa_time"),
                   verbatimTextOutput("Baseline_Tension"),
                   verbatimTextOutput("Phase_3_Boundary_Time"),
                   downloadButton("downloadPlot", "Download Plot"),
                   downloadButton("downloadTable", "Download Datatable")
                ),
        ),
        tabPanel("Rate Fittings",
                 value = 2,
                 conditionalPanel(condition = "input.actionButton == 'phase2_4'",
                   plotOutput("fit"),
                   tableOutput("rates"),
                   verbatimTextOutput("a2"),
                   verbatimTextOutput("r2"),
                   verbatimTextOutput("a3"),
                   verbatimTextOutput("r3"),
                   verbatimTextOutput("a4"),
                   verbatimTextOutput("r4"),
                   downloadButton("downloadPlot2", "Download Plot"),
                   downloadButton("downloadTabe2", "Download Datatable")
                                  )
                  ),
                  ),
              )
               )
                ) 

server <- function(input, output) {
    
    user <- reactiveValues() 
    
    observeEvent(input$load_file, {
        
        user$data <- read_excel(input$file$datapath, skip = 29)
    })


    output$interactive_plot <- renderDygraph({
        validate(need(user$data, "Please upload data to begin"))
        df <- data.frame(Seconds = user$data$Time,
                         Force = user$data$Force_One)
        dygraph(df, xlab = "Seconds", ylab = "Force") %>% 
            dyRangeSelector()
    })
   
    
     ## Phase 3 Amplitude Code
    observeEvent(input$set_phase_3, {
        req(user$data) 
        if(!is.null(input$interactive_plot_date_window)){
          
            user$phase_3_boundaries <-  c(input$interactive_plot_date_window[[1]],
                                          input$interactive_plot_date_window[[2]])
            
            user$data$force_one_smooth <- RcppRoll::roll_meanl(x = user$data$Force_One, n = 16)
            
            user$phase_3_data <- user$data %>% 
              filter(Time >= user$phase_3_boundaries[[1]] & Time <= user$phase_3_boundaries[[2]])
            
            user$phase_3_max_force <- max(user$phase_3_data$force_one_smooth)[[1]]
            
            user$phase_3_max_x_index <- user$data[which(user$data$force_one_smooth == user$phase_3_max_force), ]
            
            user$phase_3_total_time <- user$phase_3_boundaries[[2]] + 0.1
            
            user$parameters <- list(user$phase_3_boundaries[[1]],
                               user$phase_3_boundaries[[2]],
                               user$phase_3_max_force,
                               round(user$phase_3_max_force, 6)*1000,
                               user$phase_3_max_x_index$Time)
            
            m <- list("Phase 3 Boundary 1",
                      "Phase 3 Boundary 2",
                      "Phase 3 Max Force, mN",
                      "Phase 3 Max Force, mN*1000",
                      "Phase 3 Max Index")
            
            names(user$parameters) <- m
            
            colorz <- RColorBrewer::brewer.pal(8, "Dark2")
            
            df <- filter(user$data, Time <= user$phase_3_total_time) 
            
            user$plot <- ggplot() +
              
              geom_line(data = df,
                        aes(x = Time,
                            y = Force_One)) +
              
              geom_line(data = df,
                        aes(x = Time,
                            y = force_one_smooth),
                        size = 1,
                        color = colorz[[1]]) +
              
              geom_errorbarh(aes(xmin = user$phase_3_boundaries[[1]],
                                 xmax = user$phase_3_boundaries[[2]],
                                 y = user$phase_3_max_force),
                             height = 0.001,
                             color = colorz[[2]],
                             size = 1) +
              
              geom_point(aes(x = user$phase_3_max_x_index$Time,
                             y = user$phase_3_max_force),
                         color = colorz[[2]],
                         size = 4) +
              
              theme_linedraw(20)
        }
    })
    
    output$Fsa <- renderPrint({
        req(user$phase_3_max_force)
        cat("Stretch-Activated Force: ", round(user$phase_3_max_force, 6)*1000, " mN")
    })
  
    output$Fsa_time <- renderPrint({
      req(user$phase_3_max_x_index$Time)
      cat("Fsa Time:", user$phase_3_max_x_index$Time, "seconds")
    })
    
    output$Phase_3_Boundary_Time <- renderPrint({
      req(user$phase_3_data$Time)
      cat("Phase 3 Boundaries - Time", user$phase_3_boundaries, "seconds") 
    })
   
    output$phase_3 <- renderPlot({
      req(user$plot)
      user$plot
    })
    
    output$downloadPlot <- downloadHandler(
        filename = function() {
          paste("Woods_Fiberx_Conditionx_ggplot", '.png', sep = '')
        },
        content = function(file) {
          ggsave(filename = file, plot = user$plot)
          }
    )
    
    output$downloadTable <- downloadHandler(
      filename = function() {
        paste("Woods_Fiberx_Condition_P3_Parameters", '.csv', sep = '')
      },
      content = function(file) {
        write.csv(user$parameters, file = file)
      }
    )
}

shinyApp(ui = ui, server = server)
