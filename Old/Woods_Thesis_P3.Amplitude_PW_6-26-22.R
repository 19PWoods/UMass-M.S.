## Masters Thesis: Stretch activation and fatigue 
## Philip C. Woods
## Created: 3/11/22
## Last updated: 6/25/22


library(shiny)
library(dygraphs)
library(tidyverse)
library(readxl)
library(RcppRoll)
library(RColorBrewer)
library(writexl)
library(minpack.lm)
library(ggpubr)
library(broom)
theme_set(theme_classic())

get_seperate_phases <- function(mdl_tidy, time0){
  opt_a <- filter(mdl_tidy, term == 'a')
  opt_b <- filter(mdl_tidy, term == 'b')
  opt_c <- filter(mdl_tidy, term == 'c')
  opt_d <- filter(mdl_tidy, term == 'd')
  opt_e <- filter(mdl_tidy, term == 'e')
  opt_g <- filter(mdl_tidy, term == 'g')
  
  p2 <- opt_a$estimate * exp(-opt_b$estimate * time0)
  p3 <- opt_c$estimate * (1 - exp(-opt_d$estimate * time0))
  p4 <- opt_e$estimate * exp(-opt_g$estimate * time0)
  
  phase2 <- data.frame(time0 = time0,
                       Force_One = p2,
                       phase = '2')
  
  phase3<- data.frame(time0 = time0,
                      Force_One = p3,
                      phase = '3')
  phase4 <- data.frame(time0 = time0,
                       Force_One = p4,
                       phase = '4')
  
  rbind(phase2,phase3,phase4)
}


ui <- fluidPage(
  
  ## Conditional panel section, including all action buttons
  titlePanel("Stretch Activation and Fatigue - Woods' Masters Thesis"),
    sidebarPanel(
      fileInput(inputId = "file",
                label = "Select a file"),
      actionButton("load_file", 
                   "Load File"),
      
      conditionalPanel(
        condition = "input.tabselected==1",
        actionButton("set_phase_3", 
                     "Set Phase 3"),
        downloadButton("download_amp", 
                       "Download Amplitude"),
        downloadButton("download_amp_values", 
                       "Download Phase 3 Values")),

      conditionalPanel(
        condition = "input.tabselected==2",
        actionButton("set_rate_phases", 
                     "Set Phases 2-4"),
        downloadButton("download_rate", 
                       "Download Rates"),
        downloadButton("download_rate_values",
                       "Download Rate Values"))
      ),
  
  # mainPanel (what will show up in center after actionbuttons are clicked)
   mainPanel(
    dygraphOutput("interactive_plot"),
    
    tabsetPanel(id = "tabselected",
      tabPanel("Amplitude", value = 1,
               plotOutput("phase_3"),
               tableOutput("amp_datatable")),
      
      tabPanel("Rates", value = 2,
               plotOutput("fit"),
               plotOutput("fit_split"),
               tableOutput("rates"))
    )
   ) 
)

server <- function(input, output){
 
  ## creating reactiveValues that will be stored following certain actions 
  user <- reactiveValues() 
  
  
  ## loading in file and ploting dygraph
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
  
  
  ## Instructions on setting Phase 3 amplitude after clicking Set Phase 3 button
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
      
      user$amp_parameters <- list(user$phase_3_boundaries[[1]],
                              user$phase_3_boundaries[[2]],
                              user$phase_3_max_force,
                              round(user$phase_3_max_force, 6)*1000,
                              user$phase_3_max_x_index$Time)
      
      m <- list("Phase 3 Boundary 1",
                "Phase 3 Boundary 2",
                "Phase 3 Max Force, mN",
                "Phase 3 Max Force, mN*1000",
                "Phase 3 Max Index")
      
      names(user$amp_parameters) <- m
      
      colorz <- RColorBrewer::brewer.pal(8, "Dark2")
      
      df1 <- filter(user$data, Time <= user$phase_3_total_time) 
      
      user$plot_amp <- ggplot() +
        
        geom_line(data = df1,
                  aes(x = Time,
                      y = Force_One)) +
        
        geom_line(data = df1,
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
   
  
  ## Rate fitting code following selection of "Set Phases 2-4" button 
  observeEvent(input$set_rate_phases, {
    req(user$data)
    if(!is.null(input$interactive_plot_date_window)) {
      
      user$rate_phases_boundaries <-  c(input$interactive_plot_date_window[[1]],
                                    input$interactive_plot_date_window[[2]])
      
      user$rate_phases_data <- user$data %>% 
        filter(Time >= user$rate_phases_boundaries[[1]], 
               Time <= user$rate_phases_boundaries[[2]]) %>% 
        mutate(time0 = Time - Time[1], .before = Force_One)
      
      phase2 <- user$rate_phases_data %>% 
        filter(time0 <= time0[16])
      
      phase2_linfit <- lm(log10(phase2$Force_One) ~ phase2$time0)
      
      phase2$lm <- predict(phase2_linfit)
      
      phase2_model <- nlsLM(Force_One ~ (a*exp(-b*time0)),
                               data = phase2,
                               start = list(a = (10^phase2_linfit$coefficients[[1]]),
                                            b = (-phase2_linfit$coefficients[[2]])/(log10(exp(1)))),
                               control = nls.control(maxiter = 100))
      
      phase2_mdl_summary <- broom::tidy(phase2_model)

      user$grd <- list(a = phase2_mdl_summary$estimate[[1]],
                  b = phase2_mdl_summary$estimate[[2]],
                  c = tail(user$rate_phases_data$Force_One, n=1),
                  d = phase2_mdl_summary$estimate[[2]]/2,
                  e = phase2_mdl_summary$estimate[[1]],
                  g = phase2_mdl_summary$estimate[[2]]/4)

      mdl <- nlsLM(Force_One ~ (a*exp(-b*time0))+
                     (c*(1.0-exp(-d*time0))) +
                     (e*exp(-g*time0)),
                   data = user$rate_phases_data,
                   start = user$grd,
                   control = nls.control(maxiter = 1000))

      user$rate_phases_data$fit <- predict(mdl)

      user$mdl_tidy <- broom::tidy(mdl)

      user$plot_rates <- ggplot(data = user$rate_phases_data,
                                aes(x = time0, y = Force_One)) +

        geom_point() +

        geom_line(aes(y = fit), 
                  size = 0.8, 
                  col = 'red') +

        ggtitle("Fit")


      fits_seperated <- get_seperate_phases(user$mdl_tidy,
                                            user$rate_phases_data$time0)

      user$plot_rates_seperated <- ggplot() +
        geom_line(data = fits_seperated,
                  aes(x = time0, 
                      y = Force_One, 
                      color = phase)) +
        geom_line(data = user$rate_phases_data,
                  aes(x = time0, y = fit), 
                  size = 0.8, 
                  col =  "red") +
        ggtitle("Fit Seperated")
      
      user$plot_rates_comb <- ggarrange(user$plot_rates,
                                   user$plot_rates_seperated, 
                                   ncol=1)
      
      user$rate_parameters <- list(user$grd,
                                   user$mdl_tidy)
      
     names(user$rate_parameters) <- list("Starting Parameters",
                                         "Model")
      
    }
  })
 
  ## Output code
  
   # Phase 3 Amplitude
  
  output$phase_3 <- renderPlot({
    req(user$plot_amp)
    user$plot_amp
  }) 
  
  output$amp_datatable <- renderTable({
    req(user$amp_parameters)
    user$amp_parameters
  }) 
  
  output$download_amp <- downloadHandler(
    filename = function() {
      paste("Woods_Fiberx_Conditionx_P3_ggplot", '.png', sep = '')
    },
    content = function(file) {
      ggsave(filename = file, plot = user$plot_amp)
    }
  )
  
  output$download_amp_values <- downloadHandler(
    filename = function() {
      paste("Woods_Fiberx_Conditionx_P3_Parameters", '.csv', sep = '')
    },
    content = function(file) {
      write.csv(user$amp_parameters, file = file)
    }
  )
 
  
  # Rate fittings 
  
  output$fit <- renderPlot({
    req(user$plot_rates)
    user$plot_rates
  })

  output$fit_split <- renderPlot({
    req(user$plot_rates_seperated)
    user$plot_rates_seperated
  })
  
  output$rates <- renderTable({
    req(user$mdl_tidy)
    user$mdl_tidy
  }) 
  
  output$download_rate <- downloadHandler(
    filename = function() {
      paste("Woods_Fiberx_Conditionx_Rates_ggplot", '.pdf', sep ='')
    },
    content = function(file) {
      ggsave(filename = file, plot = user$plot_rates_comb)
    }
  )
  
  output$download_rate_values <- downloadHandler(
    filename = function() {
      paste("Woods_Fiberx_Conditionx_Rates_Parameters", '.xlsx', sep = '')
    },
    content = function(file) {
      write.csv(user$rate_parameters, file = file)
    }
  )
}

shinyApp(ui = ui, server = server)