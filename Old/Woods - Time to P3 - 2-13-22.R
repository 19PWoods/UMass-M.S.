## Attempt to analyze magnitude and rate of P3
## Masters Thesis: Stretch activation and fatigue 
## Philip C. Woods
## Created: 2/13/22
## Last updated: 3/10/22 by PW & Brent Scott
##################################################################

rm(list = ls())
if(!is.null(dev.list())) dev.off()
cat("\014") 

#set the working directory to the folder where the corresponding fiber runs are kept

library(tidyverse)
library(readxl)
library(dygraphs)
library(RcppRoll)

#################################################################
# Reading in files (new, Brent Scott way)

#my_data <- file.choose() #open windows dialgue to choose file name manually

my_files <- list.files(pattern = "Run") # list files in working directory. pattern is an optional function to only return files that have Run in the name
my_data <- map(my_files, ~read_excel(.x, skip = 29)) # this reads in data as tibble but confused as to how
names(my_data) <- my_files # applies the names of the files to my data

###################################################################
# Reading in files (old, Phil way)

#Relaxing <-read_excel("Run 1 - Relaxing.xlsx", skip = 29)
#Fatigue6.8 <- read_excel("Run 2 - Fatigue 6.8.xlsx", skip = 29)
#Fatigue6.6 <- read_excel("Run 3 - Fatigue 6.6.xlsx", skip = 29)
#Fatigue4.5 <- read_excel("Run 4 - Fatigue 4.5.xlsx", skip = 29)
#Active <- read_excel("Run 5 - Active.xlsx", skip = 29)
#Active2.0 <- read_excel("Run 6 - Active 2.0.xlsx", skip = 29)


#################################################################
## Fatigue pCa 6.8 - Run 2

# SA & SD data frames (Old way, PW)
#sa_run_2 <- Fatigue6.8[1:2032,c(1,3)]
#sd_run_2 <- Fatigue6.8[19032:25000,c(1,3)]


## SA vector, filtered with dplyr package
sa_run_2_brent <- 
  my_data$Run2.xlsx %>% 
  dplyr::filter(Time <= 0.7) %>% # fitlering the data based on Time column for everythin less than 2 seconds
  dplyr::select(Time, Force_One) # choosing/selecting columns based on names 

## Interactive Graph of SA vector
plot1 <- dygraph(data = sa_run_2_brent,
        xlab = "Time") %>%
  dyRangeSelector(height = 30) # dyRangeSelector adds selections tool to bottom of graph
  
plot1
  
## Generic ggplot of SA vector
#plot1 <- ggplot(data = sa_run_2_brent)+
  #geom_line(aes(x = Time, y = Force_One))

sa_run_smooth <- RcppRoll::roll_meanl(x = sa_run_2_brent$Force_One, n = 16)
df <- data.frame(Time = sa_run_2_brent$Time,
                 Force = sa_run_smooth)

plot2 <- dygraph(data = df,
                 xlab = "Time",
                 ylab = "Force") %>%
  dyRangeSelector(height = 30)

plot2

 #plot2 <- ggplot(data = df)+
  #geom_line(aes(x = Time, y = Force))



#SA Graph (Old, PW way)
#sa_f6.8 <- ggplot(data = sa_run_2, aes(x=Time, y=Force_One))+
  #geom_line(col = "black")+
  #scale_x_continuous(breaks = pretty(sa_run_2$Time, n=10))

# SD graph (Old, PW way)
#sd_f6.8 <- ggplot(data = sd_run_2, aes(x=Time, y=Force_One))+
  #geom_line(col = "black")+
  #scale_x_continuous(breaks = pretty(sd_run_2$Time, n=10))









