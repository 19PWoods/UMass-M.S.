## Rate Fittings - Stretch Activation and Fatigue Study 
## Woods Master's Thesis
## Created 4/13/22 by PW
## Last updated: 4/24/22 by PW

# script setup -----
rm(list = ls())
if(!is.null(dev.list())) dev.off()
cat("\014") 

library(tcltk)
library(tidyverse)
library(readxl)
library(dygraphs)
library(RcppRoll)
library(minpack.lm)
library(ggpubr)
library(broom)
library(writexl)


#theme_set(theme_bw())
theme_set(theme_classic())


my_formula <- Force_One ~ (a2*exp(-r2*Time0))+
  (a3*(1.0-exp(-r3*Time0))) +
  (a4*exp(-r4*Time0))

# reading in data ----

setwd(tk_choose.dir("Choose X", caption = "Select directory"))

my_files <- list.files(pattern = "Run") 
my_data <- map(my_files, ~ read_excel(.x, skip = 23))
names(my_data) <- my_files

# organizing files ----

cond2 <- my_data$Run2.xlsx %>% 
  select(Time, Force_One) %>% 
  filter(Time >= 0.1016, Time <= 0.19)

cond3 <- my_data$Run3.xlsx %>% 
  select(Time, Force_One) %>% 
  filter(Time >= 0.1018, Time <= 0.22)

cond4 <- my_data$Run4.xlsx %>% 
  select(Time, Force_One) %>% 
  filter(Time >= 0.1018, Time <= 0.22)

cond5 <- my_data$Run5.xlsx %>% 
  select(Time, Force_One) %>% 
  filter(Time >= 0.1018, Time <= 0.27)

cond6 <- my_data$Run6.xlsx %>% 
  select(Time, Force_One) %>% 
  filter(Time >= 0.1018, Time <= 0.3)

# Condition 2: Fatigue pCa 6.8 ----

# further filtering of dataframe
cond2$Time0 <- cond2$Time - cond2$Time[[1]]

cond2 <- cond2 %>% 
  dplyr::relocate(Time0, .before = Time) %>% 
  dplyr::relocate(Force_One, .before = Time) %>% 
  select(-Time)

dygraph(cond2)

# separate phases
c2.phase2 <- cond2 %>% 
  filter(Time0 <= 0.0086)
c2.phase3 <- cond2 %>% 
  filter(Time0 >= 0.0086,Time0 <= 0.0555)
c2.phase4 <- cond2 %>% 
  filter(Time0 >= 0.0555,Time0 <= 0.08)

# linear models
lm.c2.p2 <- lm(log10(c2.phase2$Force_One) ~ c2.phase2$Time0)
lm.c2.p3 <- lm(log10(c2.phase3$Force_One) ~ c2.phase3$Time0)
lm.c2.p4 <- lm(log10(c2.phase4$Force_One) ~ c2.phase4$Time0)

# c2.phase2$log <- log10(c2.phase2$Force_One)
# c2.phase3$log <- log10(c2.phase3$Force_One)
# c2.phase4$log <- log10(c2.phase4$Force_One)
# 
# # plots of seperate phases vs. log of seperate phases (troubleshooting)
# (p2 <- ggplot(data = c2.phase2, aes(x = Time0, y = Force_One)) +
#   geom_point())
# (p2.log <- ggplot(data = c2.phase2, aes(x = Time0, y = log)) +
#     geom_point())
# 
# (p3 <- ggplot(data = c2.phase3, aes(x = Time0, y = Force_One)) +
#     geom_point())
# (p3.log <- ggplot(data = c2.phase3, aes(x = Time0, y = log)) +
#     geom_point())
# 
# (p4 <- ggplot(data = c2.phase4, aes(x = Time0, y = Force_One)) +
#     geom_point())
# (p4.log <- ggplot(data = c2.phase4, aes(x = Time0, y = log)) +
#     geom_point())

# # plotting lin model to phase 2 (troubleshooting)
# c2.phase2$lin.fit <- predict(lm.c2.p2)
# 
# (plot.7 <- ggplot(data = c2.phase2, aes(x = Time0, y = log)) +
#     geom_point() +
#     geom_line(aes(y = lin.fit), size = 0.8, col = "red") +
#     ggtitle("Phase 2 linear fit")
# )
# 
# grd11 <-list(a = 10^(lm.c2.p2$coefficients[[1]]),
#              b = (-lm.c2.p2$coefficients[[2]])/(log10(exp(1)))
#              )
# 
# phil <- data.frame(x = c2.phase2$Time0,
#                    y = c2.phase2$Force_One)
# 
# mark <- nlsLM(y ~ a* exp(-b * x),
#               data = phil,
#               start = grd11,
#               control = nls.control(maxiter = 100))
# 
# 
# 
# phil$predict <- predict(mark)
# 
# 
# (phil.predict <- ggplot(data = phil, aes(x = x, y = y)) + 
#     geom_point() + 
#     geom_line(aes(y = predict), size = 0.8, col = "red") +
#     ggtitle("Condition 2")
# )

# c2.phase3$lin.fit <- predict(lm.c2.p3)
# 
# (plot.8 <- ggplot(data = c2.phase3, aes(x = Time0, y = Force_One)) +
#     geom_point() +
#     geom_line(aes(y = lin.fit), size = 0.8, col = "red") +
#     ggtitle("Phase 3 linear fit")
# )
# 
# c2.phase4$lin.fit <- predict(lm.c2.p4)
# 
# (plot.9 <- ggplot(data = c2.phase4, aes(x = Time0, y = Force_One)) +
#     geom_point() +
#     geom_line(aes(y = lin.fit), size = 0.8, col = "red") +
#     ggtitle("Phase 4 linear fit")
# )

# grid of starting parameters
grd2 <- list(a2 = 10^(lm.c2.p2$coefficients[[1]]),
            r2 = (-lm.c2.p2$coefficients[[2]])/(log10(exp(1))),
            a3 = 10^(lm.c2.p3$coefficients[[1]]),
            r3 = (-lm.c2.p3$coefficients[[2]])/(log10(exp(1))),
            a4 = 10^(lm.c2.p4$coefficients[[1]]),
            r4 = (-lm.c2.p4$coefficients[[2]])/(log10(exp(1)))
)

# model
cond2.model <- nlsLM(my_formula,
                    data = cond2,
                    start = grd2,
                    control = nls.control(maxiter = 1000))
# predicted fit
cond2$curve <- predict(cond2.model)

# data w/ fit
(plot.2 <- ggplot(data = cond2, aes(x = Time0, y = Force_One)) + 
    geom_point() + 
    geom_line(aes(y = curve), size = 0.8, col = "red") +
    ggtitle("Condition 2")
)

# Condition 3: Fatigue pCa 6.6 ----

cond3$Time0 <- cond3$Time - cond3$Time[[1]]

cond3 <- cond3 %>% 
  dplyr::relocate(Time0, .before = Time) %>% 
  dplyr::relocate(Force_One, .before = Time) %>% 
  select(-Time)

dygraph(cond3)

# separate phases
c3.phase2 <- cond3 %>% 
  filter(Time0 <= 0.02)
c3.phase3 <- cond3 %>% 
  filter(Time0 >= 0.02,Time0 <= 0.06)
c3.phase4 <- cond3 %>% 
  filter(Time0 >= 0.06,Time0 <= 0.1)

# linear models
lm.c3.p2 <- lm(log10(c3.phase2$Force_One) ~ c3.phase2$Time0)
lm.c3.p3 <- lm(log10(c3.phase3$Force_One) ~ c3.phase3$Time0)
lm.c3.p4 <- lm(log10(c3.phase4$Force_One) ~ c3.phase4$Time0)

# grid of starting parameters
grd3 <- list(a2 = 10^(lm.c3.p2$coefficients[[1]]),
             r2 = (-lm.c3.p2$coefficients[[2]])/(log(exp(1))),
             a3 = 10^(lm.c3.p3$coefficients[[1]]),
             r3 = (-lm.c3.p3$coefficients[[2]])/(log(exp(1))),
             a4 = 10^(lm.c3.p4$coefficients[[1]]),
             r4 = (-lm.c3.p4$coefficients[[2]])/(log(exp(1)))
)

# model
cond3.model <- nlsLM(my_formula,
                     data = cond3,
                     start = grd3,
                     control = nls.control(maxiter = 100))
# predicted fit
cond3$curve <- predict(cond3.model)

# data w/ fit
(plot.3 <- ggplot(data = cond3, aes(x = Time0, y = Force_One)) + 
    geom_point() + 
    geom_line(aes(y = curve), size = 0.8, col = "red") +
    ggtitle("Condition 3")
)

# Condition 4: Fatigue pCa 4.5 ----

cond4$Time0 <- cond4$Time - cond4$Time[[1]]

cond4 <- cond4 %>% 
  dplyr::relocate(Time0, .before = Time) %>% 
  dplyr::relocate(Force_One, .before = Time) %>% 
  select(-Time)

dygraph(cond4)

# separate phases
c4.phase2 <- cond4 %>% 
  filter(Time0 <= 0.0236)
c4.phase3 <- cond4 %>% 
  filter(Time0 >= 0.0236,Time0 <= 0.07)
c4.phase4 <- cond4 %>% 
  filter(Time0 >= 0.07,Time0 <= 0.11)

# linear models
lm.c4.p2 <- lm(log10(c4.phase2$Force_One) ~ c4.phase2$Time0)
lm.c4.p3 <- lm(log10(c4.phase3$Force_One) ~ c4.phase3$Time0)
lm.c4.p4 <- lm(log10(c4.phase4$Force_One) ~ c4.phase4$Time0)

# grid of starting parameters
grd4 <- list(a2 = 10^(lm.c4.p2$coefficients[[1]]),
             r2 = (-lm.c4.p2$coefficients[[2]])/(log(exp(1))),
             a3 = 10^(lm.c4.p3$coefficients[[1]]),
             r3 = (-lm.c4.p3$coefficients[[2]])/(log(exp(1))),
             a4 = 10^(lm.c4.p4$coefficients[[1]]),
             r4 = (-lm.c4.p4$coefficients[[2]])/(log(exp(1)))
)

# model
cond4.model <- nlsLM(my_formula,
                     data = cond4,
                     start = grd4,
                     control = nls.control(maxiter = 100))
# predicted fit
cond4$curve <- predict(cond4.model)

# data w/ fit
(plot.4 <- ggplot(data = cond4, aes(x = Time0, y = Force_One)) + 
    geom_point() + 
    geom_line(aes(y = curve), size = 0.8, col = "red") +
    ggtitle("Condition 4")
)

# Condition 5: Active ----

cond5$Time0 <- cond5$Time - cond5$Time[[1]]

cond5 <- cond5 %>% 
  dplyr::relocate(Time0, .before = Time) %>% 
  dplyr::relocate(Force_One, .before = Time) %>% 
  select(-Time)

dygraph(cond5)

# separate phases
c5.phase2 <- cond5 %>% 
  filter(Time0 <= 0.026)
c5.phase3 <- cond5 %>% 
  filter(Time0 >= 0.026,Time0 <= 0.09)
c5.phase4 <- cond5 %>% 
  filter(Time0 >= 0.09,Time0 <= 0.15)

# linear models
lm.c5.p2 <- lm(log10(c5.phase2$Force_One) ~ c5.phase2$Time0)
lm.c5.p3 <- lm(log10(c5.phase3$Force_One) ~ c5.phase3$Time0)
lm.c5.p4 <- lm(log10(c5.phase4$Force_One) ~ c5.phase4$Time0)

# grid of starting parameters
grd5 <- list(a2 = 10^(lm.c5.p2$coefficients[[1]]),
             r2 = (-lm.c5.p2$coefficients[[2]])/(log(exp(1))),
             a3 = 10^(lm.c5.p3$coefficients[[1]]),
             r3 = (-lm.c5.p3$coefficients[[2]])/(log(exp(1))),
             a4 = 10^(lm.c5.p4$coefficients[[1]]),
             r4 = (-lm.c5.p4$coefficients[[2]])/(log(exp(1)))
)

# model
cond5.model <- nlsLM(my_formula,
                     data = cond5,
                     start = grd5,
                     control = nls.control(maxiter = 100))
# predicted fit
cond5$curve <- predict(cond5.model)

# data w/ fit
(plot.5 <- ggplot(data = cond5, aes(x = Time0, y = Force_One)) + 
    geom_point() + 
    geom_line(aes(y = curve), size = 0.8, col = "red") +
    ggtitle("Condition 5")
)

# Condition 6: Active 2.0 ----

cond6$Time0 <- cond6$Time - cond6$Time[[1]]

cond6 <- cond6 %>% 
  dplyr::relocate(Time0, .before = Time) %>% 
  dplyr::relocate(Force_One, .before = Time) %>% 
  select(-Time)

dygraph(cond6)

# separate phases
c6.phase2 <- cond6 %>% 
  filter(Time0 <= 0.04)
c6.phase3 <- cond6 %>% 
  filter(Time0 >= 0.04,Time0 <= 0.1)
c6.phase4 <- cond6 %>% 
  filter(Time0 >= 0.1,Time0 <= 0.16)

# linear models
lm.c6.p2 <- lm(log10(c6.phase2$Force_One) ~ c6.phase2$Time0)
lm.c6.p3 <- lm(log10(c6.phase3$Force_One) ~ c6.phase3$Time0)
lm.c6.p4 <- lm(log10(c6.phase4$Force_One) ~ c6.phase4$Time0)

# grid of starting parameters
grd6 <- list(a2 = 10^(lm.c6.p2$coefficients[[1]]),
             r2 = (-lm.c6.p2$coefficients[[2]])/(log(exp(1))),
             a3 = 10^(lm.c6.p3$coefficients[[1]]),
             r3 = (-lm.c6.p3$coefficients[[2]])/(log(exp(1))),
             a4 = 10^(lm.c6.p4$coefficients[[1]]),
             r4 = (-lm.c6.p4$coefficients[[2]])/(log(exp(1)))
)

# model
cond6.model <- nlsLM(my_formula,
                     data = cond6,
                     start = grd6,
                     control = nls.control(maxiter = 100))
# predicted fit
cond6$curve <- predict(cond6.model)

# data w/ fit
(plot.6 <- ggplot(data = cond6, aes(x = Time0, y = Force_One)) + 
    geom_point() + 
    geom_line(aes(y = curve), size = 0.8, col = "red") +
    ggtitle("Condition 6")
)

# Saving Data using ggpubr package ----

# creating variable of all plots

p <- ggpubr::ggarrange(plot.2, plot.3, plot.4, plot.5, plot.6,
               ncol = 2,
               nrow = 3)

cond2.model.tidy <- tidy(cond2.model)
cond3.model.tidy <- tidy(cond3.model)
cond4.model.tidy <- tidy(cond4.model)
cond5.model.tidy <- tidy(cond5.model)
cond6.model.tidy <- tidy(cond6.model)

m <- list(cond2.model.tidy, 
         cond3.model.tidy, 
         cond4.model.tidy, 
         cond5.model.tidy,
         cond6.model.tidy)

names(m) <- c("Condition 2", 
              "Condition 3", 
              "Condition 4", 
              "Condition 5", 
              "Condition 6")

# exporting data
ggexport(p, filename = "Woods_Fiberx_Plots.pdf")
writexl::write_xlsx(m,
                    path = 'Woods_Fiberx_Fits.xlsx'
                    )