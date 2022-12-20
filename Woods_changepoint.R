library(tidyverse)
library(readxl)
library(changepoint)


setwd("C:/Users/Phil/Dropbox/Thesis- Stretch Activation/Data/Woods - Master's Thesis/Project/Mouse 8/Fiber 9/Baseline")
my_files <- list.files(pattern = "Run")
my_data <- map(my_files, ~ read_excel(.x, skip = 29) %>%
                 dplyr::select(Time, Force_One) %>% 
                 filter(Time <0.15))
names(my_data) <- my_files


## my data
df <- my_data$Run5.xlsx %>% 
  filter(Time<0.2)
plot(df)

# df.avg <- cpt.mean(df$Force_One,method = "BinSeg", Q =5)
# plot(df.avg, type = "l", cpt.col = "blue", cpt.width = 4)
# 
# df.var <- cpt.var(df$Force_One, method = "PELT")
# plot(df.var, type = "l", cpt.col = "blue", cpt.width = 4)
# 
# df.avgvar <- cpt.meanvar(df$Force_One,method = "PELT")
# plot(df.avgvar, type = "l", cpt.col = "blue", cpt.width = 4)

df.avgvar2 <- cpt.meanvar(df$Force_One,method = "BinSeg", Q=5)
plot(df.avgvar2, type = "l", cpt.col = "blue", cpt.width = 4)


