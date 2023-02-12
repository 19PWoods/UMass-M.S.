library(tidyverse)
library(readxl)
library(changepoint)
library(EnvCpt)
library(mcp)
library(minpack.lm)
library(segmented)
# library(segmented)


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



## changepoint() package -------------------------------------
df.avgvar2 <- cpt.meanvar(df$Force_One ,method = "BinSeg", Q=5)
plot(df.avgvar2, type = "l", cpt.col = "blue", cpt.width = 4)

df.avgvar2 <- cpt.meanvar(df$Force_One ,method = "PELT", Q=5)
plot(df.avgvar2, type = "l", cpt.col = "blue", cpt.width = 4)

# df.avg <- cpt.mean(df$Force_One,method = "BinSeg", Q =5)
# plot(df.avg, type = "l", cpt.col = "blue", cpt.width = 4)
# 
# df.var <- cpt.var(df$Force_One, method = "PELT")
# plot(df.var, type = "l", cpt.col = "blue", cpt.width = 4)
# 
# df.avgvar <- cpt.meanvar(df$Force_One,method = "PELT")
# plot(df.avgvar, type = "l", cpt.col = "blue", cpt.width = 4)





## mcp() package -----------------------------------------------------------

# fit_mcp <- mcp(mdl, data = df, par_x = "Time")


## EnvCpt() package -------------------------
fit_envcpt <- envcpt(df$Force_One)
fit_envcpt$summary
plot(fit_envcpt)

AIC(fit_envcpt)
which.min(AIC(fit_envcpt))
fit_envcpt$trendcpt

BIC(fit_envcpt)
which.min(BIC(fit_envcpt))
fit_envcpt$trendcpt


plot(fit_envcpt, type = "fit")
plot(fit_envcpt, type = "aic")
plot(fit_envcpt, type = "bic")



## Segmented() package ------------------------------------
my_data <- df %>% 
  filter(Time > 0.067)

plot(my_data)

mdl <- nlsLM(Force_One ~ (a*exp(-b*Time))+
               (c*(1.0-exp(-d*Time))) +
               (e*exp(-g*Time)),
             data = df,
             start = list(a = 0.015,
                          b = 200,
                          c = 0.01,
                          d = 100,
                          e = 30,
                          g = 0.008),
             control = nls.control(maxiter = 100))

fit_segmented <- segmented(mdl, seg.Z = ~ Time, npsi = 5)
