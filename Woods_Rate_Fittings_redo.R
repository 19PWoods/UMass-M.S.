library(tcltk)
library(tidyverse)
library(readxl)
library(dygraphs)
library(RcppRoll)
library(minpack.lm)
library(writexl)
library(ggpubr)
library(broom)
theme_set(theme_classic())

my_forumula <- Force_One ~ (a*exp(-b*time0))+ 
  (c*(1.0-exp(-d*time0))) + 
  (e*exp(-g*time0))

get_seperate_phases <- function(model_tidy, time0){
  opt_a <- filter(model_tidy, term == 'a')
  opt_b <- filter(model_tidy, term == 'b')
  opt_c <- filter(model_tidy, term == 'c')
  opt_d <- filter(model_tidy, term == 'd')
  opt_e <- filter(model_tidy, term == 'e')
  opt_g <- filter(model_tidy, term == 'g')
  
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

# Read in Data--------------------
setwd(tk_choose.dir("Choose X"))
my_files <- list.files(pattern = "Run")
my_data <- map(my_files, ~ read_excel(.x, skip = 29) %>% dplyr::select(Time, Force_One))
names(my_data) <- my_files

# Starting Paramters -------------------

# Fat pCa 5.1 Fit--------------------


# Fat pCa 4.5 Fit----------------------

# Active Fit -----------------------