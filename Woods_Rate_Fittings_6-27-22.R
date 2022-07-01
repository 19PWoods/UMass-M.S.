## Rate Fittings 
## Masters Thesis: Stretch activation and fatigue 
## Philip C. Woods
## Created: 5/23/22
## Last updated: 5/24/22


## Setting up script -----------------------------------------------------------

# clearing workspace
rm(list = ls())
if(!is.null(dev.list())) dev.off()
cat("\014") 

# packages to load
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

# formula for fits
my_forumula <- Force_One ~ (a*exp(-b*time0))+ 
  (c*(1.0-exp(-d*time0))) + 
  (e*exp(-g*time0))

# function to graph each parameter seperately
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

# end_time <- function(.x, Time){
#   p1 <- plot(Time, Force_One)
#   
#   print(ttkentry("Enter Phase 4 Cutoff"))
# }

## read data in-----------------------------------------------------------------
setwd(tk_choose.dir("Choose X"))

# Brent Messing around
# read_fiber <- function(file){
#   read_excel(file, skip = 29) %>%
#   dplyr::select(Time, Force_One)
# }
# 
# my_data11 <- map(my_files, read_fiber)

my_files <- list.files(pattern = "Run")
my_data <- map(my_files, ~ read_excel(.x, skip = 29) # tilde in map refers to annonomous function
               #
                 # dplyr::filter(Time >= Time[[which.max(Force_One)]], Time <= 1) %>% 
                 # dplyr::mutate(time0 = Time - Time[[1]], .before = Force_One) %>%
                 dplyr::select(Time, Force_One))

names(my_data) <- my_files

## Run 2: Fatigue pCa 5.2 ------------------------------------------------------

dygraph(my_data$Run2.xlsx)

r2_phase2 <- my_data$Run2.xlsx %>% 
  filter(time0 <= 0.0086)

r2_lm <- lm(log10(r2_phase2$Force_One) ~ r2_phase2$time0)

r2_phase2$lm <- predict(r2_lm)

r2_phase2_model <- nlsLM(Force_One ~ (a*exp(-b*time0)),
                         data = r2_phase2,
                         start = list(a = (10^r2_lm$coefficients[[1]]),
                                      b = (-r2_lm$coefficients[[2]])/(log10(exp(1)))),
                         control = nls.control(maxiter = 100))

r2_phase2_model_summary <- broom::tidy(r2_phase2_model)

grd2 <- list(a = r2_phase2_model_summary$estimate[[1]],
             b = r2_phase2_model_summary$estimate[[2]],
             c = tail(my_data$Run2.xlsx$Force_One, n=1),
             d = r2_phase2_model_summary$estimate[[2]]/2,
             e = r2_phase2_model_summary$estimate[[1]],
             g = r2_phase2_model_summary$estimate[[2]]/4)

run2_model <- nlsLM(my_forumula,
                     data = my_data$Run2.xlsx,
                     start = grd2,
                     control = nls.control(maxiter = 100)) 

my_data$Run2.xlsx$fit <- predict(run2_model)

(run2.graph <- ggplot(data = my_data$Run2.xlsx, aes(x = time0, y = Force_One)) +
    geom_point()+
    geom_line(aes(y = fit), size = 0.8, col = "red") +
    ggtitle("Run2")
)

run2_model_tidy <- tidy(run2_model)

run2_seperate <- get_seperate_phases(run2_model_tidy, my_data$Run2.xlsx$time0)

(run2_all <- ggplot() +
  geom_line(data = run2_seperate, 
            aes(x = time0, y = Force_One, color = phase)) +
  geom_line(data = my_data$Run2.xlsx, 
            aes(x = time0, y = fit), size = 0.8, col =  "red") +
  ggtitle("Run 2 Seperated")
)

## Run 3: Fatigue pCa 5.1 ------------------------------------------------------

dygraph(my_data$Run3.xlsx)

r3 <- my_data$Run3.xlsx %>% 
  filter(Time >=0.067375, Time <= 0.180375) %>% 
  mutate(time0 = Time - Time[[1]], .before = Force_One) %>% 
  select(-Time)

dygraph(r3)

r3_phase2 <- r3 %>% 
  filter(time0 >= 0.0, time0 <= 0.00275)

r3_lm <- lm(log10(r3_phase2$Force_One) ~ r3_phase2$time0)

r3_phase2$lm <- predict(r3_lm)

r3_phase2_model <- nlsLM(Force_One ~ (a*exp(-b*time0)),
                         data = r3_phase2,
                         start = list(a = (10^r3_lm$coefficients[[1]]),
                                      b = (-r3_lm$coefficients[[2]])/(log10(exp(1)))),
                         control = nls.control(maxiter = 100))

r3_phase2_model_summary <- broom::tidy(r3_phase2_model)

grd3 <- list(a = r3_phase2_model_summary$estimate[[1]],
             b = r3_phase2_model_summary$estimate[[2]],
             c = tail(r3$Force_One, n=1),
             d = r3_phase2_model_summary$estimate[[2]]/2,
             e = r3_phase2_model_summary$estimate[[1]],
             g = r3_phase2_model_summary$estimate[[2]]/4)

run3_model <- nlsLM(my_forumula,
                    data = r3,
                    start = grd3,
                    control = nls.control(maxiter = 100)) 

my_data$Run3.xlsx$fit <- predict(run3_model)

(run3.graph <- ggplot(data = my_data$Run3.xlsx, aes(x = time0, y = Force_One)) +
    geom_point()+
    geom_line(aes(y = fit), size = 0.8, col = "red") +
    ggtitle("Run3")
)

run3_model_tidy <- tidy(run3_model)

run3_seperate <- get_seperate_phases(run3_model_tidy, my_data$Run3.xlsx$time0)

(run3_all <- ggplot() +
    geom_line(data = run3_seperate, 
              aes(x = time0, y = Force_One, color = phase)) +
    geom_line(data = my_data$Run3.xlsx, 
              aes(x = time0, y = fit), size = 0.8, col =  "red") +
    ggtitle("Run 3 Seperated")
)

## Run 4: Fatigue pCa 4.5 ------------------------------------------------------

dygraph(my_data$Run4.xlsx)

r4_phase2 <- my_data$Run4.xlsx %>% 
  filter(time0 <= 0.0086)

r4_lm <- lm(log10(r4_phase2$Force_One) ~ r4_phase2$time0)

r4_phase2$lm <- predict(r4_lm)

r4_phase2_model <- nlsLM(Force_One ~ (a*exp(-b*time0)),
                         data = r4_phase2,
                         start = list(a = (10^r4_lm$coefficients[[1]]),
                                      b = (-r4_lm$coefficients[[2]])/(log10(exp(1)))),
                         control = nls.control(maxiter = 100))

r4_phase2_model_summary <- broom::tidy(r4_phase2_model)

grd4 <- list(a = r4_phase2_model_summary$estimate[[1]],
             b = r4_phase2_model_summary$estimate[[2]],
             c = tail(my_data$Run4.xlsx$Force_One, n=1),
             d = r4_phase2_model_summary$estimate[[2]]/2,
             e = r4_phase2_model_summary$estimate[[1]],
             g = r4_phase2_model_summary$estimate[[2]]/4)

run4_model <- nlsLM(my_forumula,
                    data = my_data$Run4.xlsx,
                    start = grd4,
                    control = nls.control(maxiter = 100)) 

my_data$Run4.xlsx$fit <- predict(run4_model)

(run4.graph <- ggplot(data = my_data$Run4.xlsx, aes(x = time0, y = Force_One)) +
    geom_point()+
    geom_line(aes(y = fit), size = 0.8, col = "red") +
    ggtitle("Run4")
)

run4_model_tidy <- tidy(run4_model)

run4_seperate <- get_seperate_phases(run4_model_tidy, my_data$Run4.xlsx$time0)

(run4_all <- ggplot() +
    geom_line(data = run4_seperate, 
              aes(x = time0, y = Force_One, color = phase)) +
    geom_line(data = my_data$Run4.xlsx, 
              aes(x = time0, y = fit), size = 0.8, col =  "red") +
    ggtitle("Run 4 Seperated")
)

## Run 5: Active ---------------------------------------------------------------

dygraph(my_data$Run5.xlsx)

r5_phase2 <- my_data$Run5.xlsx %>% 
  filter(time0 <= 0.0086)

r5_lm <- lm(log10(r5_phase2$Force_One) ~ r5_phase2$time0)

r5_phase2$lm <- predict(r5_lm)

r5_phase2_model <- nlsLM(Force_One ~ (a*exp(-b*time0)),
                         data = r5_phase2,
                         start = list(a = (10^r5_lm$coefficients[[1]]),
                                      b = (-r5_lm$coefficients[[2]])/(log10(exp(1)))),
                         control = nls.control(maxiter = 100))

r5_phase2_model_summary <- broom::tidy(r5_phase2_model)

grd5 <- list(a = r5_phase2_model_summary$estimate[[1]],
             b = r5_phase2_model_summary$estimate[[2]],
             c = tail(my_data$Run5.xlsx$Force_One, n=1),
             d = r5_phase2_model_summary$estimate[[2]]/2,
             e = r5_phase2_model_summary$estimate[[1]],
             g = r5_phase2_model_summary$estimate[[2]]/4)

run5_model <- nlsLM(my_forumula,
                    data = my_data$Run5.xlsx,
                    start = grd5,
                    control = nls.control(maxiter = 100)) 

my_data$Run5.xlsx$fit <- predict(run5_model)

(run5.graph <- ggplot(data = my_data$Run5.xlsx, aes(x = time0, y = Force_One)) +
    geom_point()+
    geom_line(aes(y = fit), size = 0.8, col = "red") +
    ggtitle("Run5")
)

run5_model_tidy <- tidy(run5_model)

run5_seperate <- get_seperate_phases(run5_model_tidy, my_data$Run5.xlsx$time0)

(run5_all <- ggplot() +
    geom_line(data = run5_seperate, 
              aes(x = time0, y = Force_One, color = phase)) +
    geom_line(data = my_data$Run5.xlsx, 
              aes(x = time0, y = fit), size = 0.8, col =  "red") +
    ggtitle("Run 5 Seperated")
)

## Run 6: Active Remeasure -----------------------------------------------------

dygraph(my_data$Run6.xlsx)

r6_phase2 <- my_data$Run6.xlsx %>% 
  filter(time0 <= 0.0086)

r6_lm <- lm(log10(r6_phase2$Force_One) ~ r6_phase2$time0)

r6_phase2$lm <- predict(r6_lm)

r6_phase2_model <- nlsLM(Force_One ~ (a*exp(-b*time0)),
                         data = r6_phase2,
                         start = list(a = (10^r6_lm$coefficients[[1]]),
                                      b = (-r6_lm$coefficients[[2]])/(log10(exp(1)))),
                         control = nls.control(maxiter = 100))

r6_phase2_model_summary <- broom::tidy(r6_phase2_model)

grd6 <- list(a = r6_phase2_model_summary$estimate[[1]],
             b = r6_phase2_model_summary$estimate[[2]],
             c = tail(my_data$Run6.xlsx$Force_One, n=1),
             d = r6_phase2_model_summary$estimate[[2]]/2,
             e = r6_phase2_model_summary$estimate[[1]],
             g = r6_phase2_model_summary$estimate[[2]]/4)

run6_model <- nlsLM(my_forumula,
                    data = my_data$Run6.xlsx,
                    start = grd6,
                    control = nls.control(maxiter = 100)) 

my_data$Run6.xlsx$fit <- predict(run6_model)

(run6.graph <- ggplot(data = my_data$Run6.xlsx, aes(x = time0, y = Force_One)) +
    geom_point()+
    geom_line(aes(y = fit), size = 0.8, col = "red") +
    ggtitle("Run6")
)

run6_model_tidy <- tidy(run6_model)

run6_seperate <- get_seperate_phases(run6_model_tidy, my_data$Run6.xlsx$time0)

(run6_all <- ggplot() +
    geom_line(data = run6_seperate, 
              aes(x = time0, y = Force_One, color = phase)) +
    geom_line(data = my_data$Run6.xlsx, 
              aes(x = time0, y = fit), size = 0.8, col =  "red") +
    ggtitle("Run 6 Seperated")
)

## Saving ----------------------------------------------------------------------

p <- list(plot1 = ggarrange(run2.graph, run2_all, ncol = 1),
           plot2 = ggarrange(run3.graph, run3_all, ncol = 1),
           plot3 = ggarrange(run4.graph, run4_all, ncol = 1),
           plot4 = ggarrange(run5.graph, run5_all, ncol = 1),
           plot5 = ggarrange(run6.graph, run6_all, ncol = 1)
           )

sp <- list(grd2,
           grd3,
           grd4,
           grd5,
           grd6)

names(sp) <- c('Run 2',
               'Run 3',
               'Run 4',
               'Run 5',
               'Run 6')

models <- list(run2_model_tidy,
               run3_model_tidy,
               run4_model_tidy,
               run5_model_tidy,
               run6_model_tidy)

names(models) <- c('Run 2 - Fatigue pCa 5.2',
                   'Run 3 - Fatigue pCa 5.1',
                   'Run 4 - Fatigue pCa 4.5',
                   'Run 5 - Activating',
                   'Run 6 - Activating Remeasured')

ggexport(p, filename = "Woods_Fiberx_Plots.pdf")
writexl::write_xlsx(models, path = 'Woods_Fiberx_Fits.xlsx')
capture.output(sp, file = 'Woods_Fiberx_StartingParameters.txt')
