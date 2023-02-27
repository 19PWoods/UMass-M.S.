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
my_files <- list.files()
my_data <- map(my_files, ~ read_excel(.x, skip = 29) %>% 
                 dplyr::select(Time, Force_One) %>% 
                 dplyr::filter(Time < 1.5))
names(my_data) <- my_files

# Starting Paramters -------------------
I_active <- list(a = 0.0835,
                 b = 102.3,
                 c = 0.01773,
                 d = 9.72,
                 e = 0.0214,
                 g = 2.37)

IIA_active <- list(a = 0.02145,
                   b = 331,
                   c = 0.01147,
                   d = 54.8,
                   e = 0.0235,
                   g = 11.01)
IIA_fat4.5 <- list(a = 0.01001,
                   b = 335,
                   c = 0.00952,
                   d = 22.4,
                   e = 0.0362,
                   g = 5.97)
IIA_fat5.1 <- list(a = 0.00948,
                   b = 235,
                   c = 0.00427,
                   d = 11.4,
                   e = 0.0339,
                   g = 3.98)

IIX_active <- list(a = 0.01074,
                   b = 826,
                   c = 0.0103,
                   d = 128.1,
                   e = 0.0208,
                   g = 21.8)
IIX_fat4.5 <- list(a = 0.00851,
                   b = 620,
                   c = 0.01311,
                   d = 88,
                   e = 0.0263,
                   g = 12.6)
IIX_fat5.1 <- list(a = 0.00785,
                   b = 506,
                   c = 0.00949,
                   d = 92.6,
                   e = 0.0205,
                   g = 12.7)

IIB_active <- list(a = 0.0194,
                   b = 839,
                   c = 0.0274,
                   d = 366,
                   e = 0.0346,
                   g = 50.8)
IIB_fat4.5 <- list(a = 0.0219,
                   b = 808,
                   c = 0.0298,
                   d = 418,
                   e = 0.0365,
                   g = 37.2)
IIB_fat5.1 <- list(a = 0.0128,
                   b = 1217,
                   c = 0.0205,
                   d = 394,
                   e = 0.0197,
                   g = 38.9)

# Fat pCa 5.1 Fit--------------------
dygraph(my_data$Fat_5.1.xlsx)

fat4.5_data <- my_data$Fat_5.1.xlsx %>% 
  filter(Time > 0.067375 & Time < 0.25)



# Fat pCa 4.5 Fit----------------------


# Active Fit -----------------------