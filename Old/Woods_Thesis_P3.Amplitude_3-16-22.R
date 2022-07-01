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

###########################################################################################
# Define UI for application that draws a histogram (First component: Front end, accepts user input values)
ui <- fluidPage(
    navbarPage(
        "Stretch Activation and Fatigue - Woods Master's Thesis Analysis",
        tabPanel("Navbar 1",
                sidebarPanel(
                    fileInput(inputId = "file", 
                          label = "Select a File"),  
                    actionButton("load_file", "Load File"),               # load_file will be sent to the server
                    actionButton("set_phase_3", "Set Phase 3 Boundaries") #set_phase_3 will be sent to the server
                             ), #sidebarPanel
            
                # Show a plot of the generated distribution
                mainPanel(
                   dygraphOutput("interactive_plot"),             # dygraphOutput is generated from the server
                   plotOutput("phase_3"),                         # plotOutput is generated from the server
                   verbatimTextOutput("Fsa"), 
                   verbatimTextOutput("Fsa_time"),                 # Fsa time is generated from server# Fsa is generated from the server
                   verbatimTextOutput("Baseline_Tension"),        # Baseline tension is generated from the server
                   verbatimTextOutput("Phase_3_Boundary_Time"),   # Phase 3 Time Boundaries is generated from server
                ) #mainPanel
            
        ), # Navbar 1, tabPanel
    
       # tabPanel("Navbar 2", "This panel is intentionally left blank") # Ideally this tab I would like to use to fit rates, not entirely sure how feasible this is
    
       ) #navbarPage
    ) #fluid page

#####################################################################################
# Define server logic (Second component - Performs the processing of the data (Back end: Processes input values to put results on website ))
server <- function(input, output) {
    
    user <- reactiveValues() # creates a reactive R object that allows data to be assigned to from UI
    
    # Reading in Data
    # Observe event means, when the "load_file" button is clicked do these things. Changed to 
    observeEvent(input$load_file, {
        # assign the data to the "user" reactive value to the data can be read in and accessed
        user$data <- read_excel(input$file$datapath, skip = 29)
    })

    # draws the interactive graph to select phase 3 boundaries
    output$interactive_plot <- renderDygraph({
        # don't draw graph unless data is loaded
        validate(need(user$data, "Please upload data to begin"))
        
        # take the data you need from raw file. Time and Force One
        df <- data.frame(Seconds = user$data$Time,
                         Force = user$data$Force_One)
        
        # Graph the data with dygraphs
        dygraph(df, xlab = "Seconds", ylab = "Force") %>% 
            dyRangeSelector()
    })
    
    # observe event again. Means when the "set_phase_3" button is clicked do this stuff
    observeEvent(input$set_phase_3, {
        req(user$data) # Data is required. dont do anything unless we have this data. 
        # if the interactive plot sliders have not values do not do anything. 
        if(!is.null(input$interactive_plot_date_window)){
        
            # get the user specificed phase 3 boundaries    
            # interactive_plot_date_window[[1]] is the left side
            # interactive_plot_date_window[[2]] is the right
            user$phase_3_boundaries <-  c(input$interactive_plot_date_window[[1]],
                                          input$interactive_plot_date_window[[2]])
         
            # smooth the data with a running mean using 16 datapoints (2ms at 8kHz)
            # RcppRoll is an R package that allows for fast rolling window calcuation implemented in C++
            user$data$force_one_smooth <- RcppRoll::roll_meanl(x = user$data$Force_One, n = 16)
            
            # make a copy of raw data.
            # only copy data a part of user selected phase 3
            user$phase_3_data <- user$data %>% filter(Time >= user$phase_3_boundaries[[1]] & Time <= user$phase_3_boundaries[[2]])
            
            # calculate max force
            user$phase_3_max_force <- max(user$phase_3_data$force_one_smooth)[[1]]
            
            # get the correspoding x-axis value that matches where the max value occurs in the y-dimension
            user$phase_3_max_x_index <- user$data[ which(user$data$force_one_smooth == user$phase_3_max_force), ]
        }
    })
    
    # make a graph overlaying the max force and running mean over the raw data
    output$phase_3 <- renderPlot({
        # require this data before doing anything
        req(user$phase_3_max_force) 
        # get some nice colors
        colorz <- RColorBrewer::brewer.pal(8, "Dark2") 
        # plot some arbitrary subset of the data so we can see analysis
        # change to whatever you want
        total_time <- user$phase_3_boundaries[[2]]+0.1 
        # filter dataset to just the time you want using the total_time object above
        df <- filter(user$data, Time <= total_time)
        
        # just a ggplot
        ggplot()+
            # raw data
            geom_line(data = df, 
                      aes(x = Time, 
                          y = Force_One))+
            # smooth data
            geom_line(data = df, 
                      aes(x = Time, 
                          y = force_one_smooth),
                      size = 1,
                      color = colorz[[1]])+
            # horizontal errobars to mark where user selected beginning/end of phase 3
            geom_errorbarh(aes(xmin = user$phase_3_boundaries[[1]], 
                               xmax = user$phase_3_boundaries[[2]], 
                               y = user$phase_3_max_force),
                           height = 0.001,
                           color = colorz[[2]],
                           size = 1)+
            # put a point at where the max occurs
            geom_point(aes(x = user$phase_3_max_x_index$Time, 
                           y = user$phase_3_max_force), 
                       color = colorz[[2]],
                       size = 4)+
            theme_linedraw(20)
    })
    
    # print the max force to the UI
    output$Fsa <- renderPrint({
        req(user$phase_3_max_force)
        cat("Stretch-Activated Force: ", round(user$phase_3_max_force, 6)*1000, " mN")
    })
  
   # print time for max force to the UI
    output$Fsa_time <- renderPrint({
      req(user$phase_3_max_x_index$Time)
      cat("Fsa Time:", user$phase_3_max_x_index$Time, "seconds")
    })
    
    # print time for selected phase 3 boundaries to UI
    output$Phase_3_Boundary_Time <- renderPrint({
      req(user$phase_3_data$Time)
      cat("Phase 3 Boundaries - Time", user$phase_3_boundaries, "seconds") 
    })
}
##############################################################################
# Run the application (Third component - fuses Part 1 and 2 together)
shinyApp(ui = ui, server = server)
