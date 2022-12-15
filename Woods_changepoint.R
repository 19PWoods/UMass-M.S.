library(tcltk)
library(tidyverse)
library(readxl)
library(changepoint)


setwd("C:/Users/Phil/Dropbox/Thesis- Stretch Activation/Data/Woods - Master's Thesis/Project/Mouse 8/Baseline")
my_files <- list.files(pattern = "Run")
my_data <- map(my_files, ~ read_excel(.x, skip = 29) %>%
                 dplyr::select(Time, Force_One) %>% 
                 filter(Time <0.5))
names(my_data) <- my_files


## my data
df <- my_data$Run5.xlsx %>% 
  filter(Time<0.2)
plot(df)

df.pm <- cpt.mean(df$Force_One,method = "PELT")
plot(df.pm, type = "l", cpt.col = "blue", cpt.width = 4)

df.pm <- cpt.meanvar(df$Force_One,method = "PELT")
plot(df.pm, type = "l", cpt.col = "blue", cpt.width = 4)

## example data from article
set.seed(10) 
m.data <- c(rnorm(100, 0, 1), rnorm(100, 1, 1), rnorm(100, 0, 1), + rnorm(100, 0.2, 1)) 
ts.plot(m.data, xlab = "Index")

m.pelt <- cpt.mean(m.data, method = "PELT")
plot(m.pelt, type = "l", cpt.col = "blue", cpt.width = 4)
cpts(m.pelt)

m.pelt.man <- cpt.mean(m.data, penalty = "Manual", pen.value = "1.5 * log(n)",
                       method = "PELT")
plot(m.pelt.man, type = "l", cpt.col = "blue", cpt.width = 4)
cpts(m.pelt.man)
