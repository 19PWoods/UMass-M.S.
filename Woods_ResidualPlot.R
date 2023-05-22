library(tcltk)
library(tidyverse)
library(readxl)
library(dygraphs)
library(minpack.lm)
library(writexl)
library(ggpubr)
library(broom)
theme_set(theme_classic())

triple.form <- Force_One ~ (a*exp(-b*time0))+ 
  (c*(1.0-exp(-d*time0))) + 
  (e*exp(-g*time0))

dbl.form <- Force_One ~ (a*exp(-b*time0))+ 
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

setwd(tk_choose.dir("Choose X"))
my_files <- list.files(pattern = ".xlsx")
my_data <- map(my_files, ~ read_excel(.x, skip = 29) %>%
                 dplyr::select(Time, Force_One))
names(my_data) <- my_files

### Starting Parameters -------------------------------------------
# I_active <- list(a = 0.0835,
#                  b = 102.3,
#                  c = 0.01773,
#                  d = 9.72,
#                  e = 0.0214,
#                  g = 2.37)

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

### Fatigue 5.1 -----------------------------------------------------------

dygraph(my_data$Fat_5.1.xlsx)

fat5.1_data <- my_data$Fat_5.1.xlsx %>% 
  filter(Time > 0.06735 & Time < 0.3) %>% 
  mutate(time0 = Time - Time[[1]], .before = Force_One) %>% 
  select(-Time)

fat5.1_model <- nlsLM(triple.form,
                      data = fat5.1_data,
                      start = IIA_fat5.1,
                      control = nls.control(maxiter = 100))

fat5.1_data$fit <- predict(fat5.1_model)

(fat5.1_gg <- ggplot(data = fat5.1_data, aes(x = time0, y = Force_One)) +
    geom_point()+
    geom_line(aes(y = fit), size = 0.8, col = "red") +
    ggtitle("Fatigue 5.1")
)

fat5.1_mdl_tidy <- tidy(fat5.1_model)

fat5.1_seperate <- get_seperate_phases(fat5.1_mdl_tidy, fat5.1_data$time0)

(fat5.1_ggall <- ggplot() +
    geom_line(data = fat5.1_seperate, 
              aes(x = time0, y = Force_One, color = phase)) +
    geom_line(data = fat5.1_data, 
              aes(x = time0, y = fit), size = 0.8, col =  "red") +
    ggtitle("Fatigue 5.1 Seperated")
)


fat5.1.resi <- resid(fat5.1_model)

plot(fat5.1_data$time0, fat5.1.resi,
     ylab = "Residuals", xlab = "Time",
     main = "Fat 5.1 Triple Fit")

abline(0,0)

qqnorm(fat5.1.resi)
qqline(fat5.1.resi)



fat5.1_dblmdl <- nlsLM(dbl.form,
                       data = fat5.1_data,
                       start = list(a = 0.02145,
                                    b = 331,
                                    e = 0.0235,
                                    g = 11.01),
                       control = nls.control(maxiter = 100))

fat5.1.dbl.res <- resid(fat5.1_dblmdl)

fat5.1_data$dblmdl <- predict(fat5.1_dblmdl)

(fat5.1_ggdbl <- ggplot() +
    geom_point(data = fat5.1_data, aes(x = time0, y = Force_One)) +
    geom_line(data = fat5.1_data, 
              aes(x = time0, y = dblmdl), size = 0.8, col =  "red") +
    ggtitle("Fatigue 5.1 Seperated")
)

plot(fat5.1_data$time0, fat5.1.dbl.res,
     ylab = "Residuals", xlab = "Time",
     main = "Fat 5.1 Dbl Fit")

abline(0,0)

qqnorm(fat5.1.dbl.res)
qqline(fat5.1.dbl.res)

## Fatigue 4.5 -----------------------------------------------------------
dygraph(my_data$Fat_4.5.xlsx)

fat4.5_data <- my_data$Fat_4.5.xlsx %>% 
  filter(Time > 0.06735 & Time < 0.4) %>% 
  mutate(time0 = Time - Time[[1]], .before = Force_One) %>% 
  select(-Time)

fat4.5_model <- nlsLM(triple.form,
                      data = fat4.5_data,
                      start = IIA_fat4.5,
                      control = nls.control(maxiter = 100))

fat4.5_data$fit <- predict(fat4.5_model)

(fat4.5_gg <- ggplot(data = fat4.5_data, aes(x = time0, y = Force_One)) +
    geom_point()+
    geom_line(aes(y = fit), size = 0.8, col = "red") +
    ggtitle("Fatigue 4.5")
)

fat4.5_mdl_tidy <- tidy(fat4.5_model)

fat4.5_seperate <- get_seperate_phases(fat4.5_mdl_tidy, fat4.5_data$time0)

(fat4.5_ggall <- ggplot() +
    geom_line(data = fat4.5_seperate, 
              aes(x = time0, y = Force_One, color = phase)) +
    geom_line(data = fat4.5_data, 
              aes(x = time0, y = fit), size = 0.8, col =  "red") +
    ggtitle("Fatigue 4.5 Seperated")
)

fat4.5.resi <- resid(fat4.5_model)

plot(fat4.5_data$time0, fat4.5.resi,
     ylab = "Residuals", xlab = "Time",
     main = "Fat 4.5 Triple Fit")

abline(0,0)

qqnorm(fat4.5.resi)
qqline(fat4.5.resi)



fat4.5_dblmdl <- nlsLM(dbl.form,
                       data = fat4.5_data,
                       start = list(a = 0.02145,
                                    b = 331,
                                    e = 0.0235,
                                    g = 11.01),
                       control = nls.control(maxiter = 100))

fat4.5.dbl.res <- resid(fat4.5_dblmdl)

fat4.5_data$dblmdl <- predict(fat4.5_dblmdl)

(fat4.5_ggdbl <- ggplot() +
    geom_point(data = fat4.5_data, aes(x = time0, y = Force_One)) +
    geom_line(data = fat4.5_data, 
              aes(x = time0, y = dblmdl), size = 0.8, col =  "red") +
    ggtitle("Fatigue 4.5 Seperated")
)

plot(fat4.5_data$time0, fat4.5.dbl.res,
     ylab = "Residuals", xlab = "Time",
     main = "Fat 4.5 Dbl Fit")

abline(0,0)

qqnorm(fat4.5.dbl.res)
qqline(fat4.5.dbl.res)








