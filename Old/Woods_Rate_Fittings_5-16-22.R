## Rate Fittings
## Masters Thesis: Stretch activation and fatigue 
## Created: 5/16/22 by Philip C. Woods
## Last updated: 5/16/22

# Clear work space, load libraries and set working directory

rm(list = ls())
if(!is.null(dev.list())) dev.off()
cat("\014") 

library(tcltk)
library(tidyverse)
library(readxl)
library(dygraphs)
library(RcppRoll)
library(minpack.lm)
library(writexl)
library(ggpubr)
library(broom)
library(writexl)
theme_set(theme_classic())

setwd(tk_choose.dir("Choose X"))


### read data in
# my_files <- list.files(pattern = "Run")
# my_data <- map(my_files, ~ read_excel(.x, skip = 22) %>%
#                  dplyr::select(Time, Force_One) %>% 
#                  dplyr::filter(Time >= Time[[which(Force_One == max(Force_One))]], 
#                                Time >= 0.25)
# )
# names(my_data) <- my_files

my_files <- list.files(pattern = "Run")

phase2 <- map_df(my_files, ~ read_excel(.x, skip = 22) %>% 
  dplyr::mutate(filename = .x) %>%
  dplyr::select(filename, Time, Force_One) %>% 
  dplyr::filter(Time >= 0.004))

my_data <- map_df(my_files, ~ read_excel(.x, skip = 22) %>%
                 dplyr::mutate(filename = .x) %>%
                 dplyr::select(filename, Time, Force_One) %>% 
                 dplyr::filter(Time >= Time[[which(Force_One == max(Force_One))]], 
                               Time >= 0.25))
my_data_nest <-
  my_data %>%
  nest(data = c(Time, Force_One))

phase2.nest <- 
  phase2 %>% 
  nest(data = c(Time, Force_One))

##..................................

 
lin.mod <- function(data) {
  lm(log(Force_One) ~ Time)
}

phase.2.fit <- phase2.nest %>% 
  mutate(model = map(phase2, lin.mod))









