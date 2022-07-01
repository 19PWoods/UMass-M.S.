## Attempt to analyze magnitude and rate of P3
## Masters Thesis: Stretch activation and fatigue 
## Philip C. Woods
## Created: 4/13/22
## Last updated: 5/23/22

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


# Reading in files (new, Brent Scott way) -------------------------------

#my_data <- file.choose() #open windows dialgue to choose file name manually

my_files <- list.files(pattern = "Run") # list files in working directory. pattern is an optional function to only return files that have Run in the name
my_data <- map(my_files, ~read_excel(.x, skip = 49)) # this reads in data as tibble but confused as to how
names(my_data) <- my_files # applies the names of the files to my data



# Testing - Running average on Baseline -------------------------------
## Section applies running average to baseline tension 

sa_baseline <- 
  my_data$Run5.xlsx %>% # pick whichever run you want
  dplyr::filter(Time <= 0.06) %>% # fitler data by time
  dplyr::select(Time, Force_One) # only show Time + Force_One column

sa_baseline_smooth <- RcppRoll::roll_mean(x = sa_baseline$Force_One, n=10)

base.plot <- base.plot <- ggplot()+
  geom_line(
    data = sa_baseline,
    aes(x = Time,
        y = Force_One),
  )




# Fatigue pCa 6.8 - Run 4 -----------------------------------------------
## Section plays with dplyr + dygraph packages. Also applies running average to entire data

## SA vector, filtered with dplyr package
sa_run_4_brent <- 
  my_data$Run5.xlsx %>% 
  dplyr::filter(Time <= 0.7) %>% 
  dplyr::select(Time, Force_One)  

## Interactive Graph of SA vector
plot1_6.8 <- dygraph(data = sa_run_4_brent,
        xlab = "Time") %>%
  dyRangeSelector(height = 30) # dyRangeSelector adds selections tool to bottom of graph
  
plot1_6.8
  
## Interactive Graph of SA vector w/ running mean
sa_run_smooth <- RcppRoll::roll_meanl(x = sa_run_4_brent$Force_One, n = 16)
df <- data.frame(Time = sa_run_4_brent$Time,
                 Force = sa_run_smooth)

plot4_6.8 <- dygraph(data = df,
                 xlab = "Time",
                 ylab = "Force") %>%
  dyRangeSelector(height = 30)

plot4_6.8



# Rate fittings - Woods Data----------------------------------------------
## Initial rate fitting attempt using equation from Straight et al. (4019) on Mouse 1 data for Phil Woods Thesis

setwd(tk_choose.dir("Choose X"))

my_files <- list.files(pattern = "Run") 
my_data <- map(my_files, ~read_excel(.x, skip = 49))
names(my_data) <- my_files

# Filter data for only Phase 4-4
sa_run_6_fits <- 
  my_data$Run6.xlsx %>% 
  #dplyr::filter(Time >= 0.065145, Time <=0.1) %>% 
  dplyr::select(Time, Force_One)

dygraph(sa_run_6_fits) %>% 
  dyRangeSelector()

# separate into Phases

phase4 <- sa_run_4_fits %>% 
  dplyr::filter(Time >= 0.065145,Time <= 0.068145)

phase4.mut<- mutate(phase4, Time.4 = seq(0,0.003,0.000145))
transmute(phase4.mut,
          phase4.mut$Time.4,
          phase4.mut$Force_One)

phase3 <- sa_run_4_fits %>% 
  dplyr::filter(Time >= 0.068145,Time <= 0.0745)

phase3.mut <- mutate(phase3,Time.4 = seq(0,0.006375,0.000145))


transmute(phase3.mut,
          phase3.mut$Time.4,
          phase3.mut$Force_One)


phase4 <- sa_run_4_fits %>% 
  dplyr::filter(Time >= 0.0745,Time <= 0.1)

phase4.mut <- mutate(phase4, Time.4 = seq(0,0.045500,0.000145))

transmute(phase4.mut,
          phase4.mut$Time.4,
          phase4.mut$Force_One)

# Linear equation for parameter boundaries

phase4.fit <- lm(Force_One ~ Time4, data = phase4.mut)

summary(phase4.fit)

#Intercept = 0.14184, Time = - 1.89067

phase3.fit <- lm(Force_One ~ Time4, data = phase3.mut)

summary(phase3.fit)

# Intercept = -0.018150, Time = 0.487441

phase4.fit <- lm(Force_One ~ Time4, data = phase4.mut)

summary(phase4.fit)

# Intercept = 0.0430353, ime = - 0.0838694

# Define starting parameters attempt 1
grd1 <- list(a = 1,
             b = 1,
             c = 1,
             d = 4,
             e = 1,
             g = 14.5)


df <- data.frame(t = sa_run_4_fits$Time,
                 y = sa_run_4_fits$Force_One) %>% 
  mutate()



#3 part exponential equation
f <- df$y ~ (a*exp(-b*df$t))+ 
  (c*(1.0-exp(-d*df$t))) + 
  (e*exp(-g*df$t))

# 3-part exponential fit using a,c,e,b,d,g (similar to instructions sent from Doug)
fit.master <- nlsLM(f,
                    data = df,
                    nls.control(maxiter = 100))

plz <- nlsLM()



# Rate fittings - Chad's data (raw)--------------------------------------------
## Attempt fitting equation to chad's raw data

setwd(tk_choose.dir("Choose X"))

##### read data in
# my_files <- list.files(pattern = "Run") 
# my_data <- map(my_files, ~ read_excel(.x, skip = 43))
# names(my_data) <- my_files

my_data <- read_excel("Run4.xlsx", skip = 43) %>% 
  select(Time, Force_One) #%>% 
  #filter(Time >= 0.1018, Time <=0.45)

run4 <- my_data %>% 
  filter(Time >= 0.1018, Time <=0.45)

dygraph(run4)

##### crop data to each phase

# just phase 4
phase.4 <- my_data %>%
  dplyr::filter(Time >= 0.1018, Time <=0.1174) %>% 
  dplyr::select(Time, Force_One)

# just phase 3
phase.3 <- my_data %>%
  dplyr::filter(Time >= 0.1174, Time <=0.1794) %>%
  dplyr::select(Time, Force_One)

#just phase 4
phase.4 <- my_data %>%
  dplyr::filter(Time >= 0.1794, Time <=0.45) %>%
  dplyr::select(Time, Force_One)


##### linear fits for each parameter 

lm4<- lm(log(phase.4$Force_One) ~ phase.4$Time)
coef(phase.4.fit)

lm3 <- lm(log(phase.3$Force_One) ~ phase.3$Time)
coef(phase.3.fit)

lm4 <- lm(log(phase.4$Force_One) ~ phase.4$Time)
coef(phase.4.fit)

##### NL formula

my_formula <- Force_One ~ (a*exp(-b*time0))+ 
  (c*(1.0-exp(-d*time0))) + 
  (e*exp(-g*time0))

run4$time0 <- run4$Time - run4$Time[[1]] 

grd <- list(a = lm4$coefficients[[1]],
            b = lm4$coefficients[[4]],
            c = lm3$coefficients[[1]],
            d = lm3$coefficients[[4]],
            e = lm4$coefficients[[1]],
            g = lm4$coefficients[[4]])

##### fit

run4.model <- nlsLM(my_formula,
                data = run4,
                start = grd,
                control = nls.control(maxiter = 100))

summary(run4.model)

run4$curve <- predict(run4.model)

#### plotting data w/ fit
(plot.4 <- ggplot(data = run4, aes(x = time0, y = Force_One)) + 
  geom_point() + 
  geom_line(aes(y = curve), size = 0.8, col = "red") +
  ggtitle("Run 4 - 6mM")
)


# Rate fitting - Chad's data (already fitted) ------------------------------------------------
## Attempt fitting chad's already fitting data (from excel) to check if code is wrong
## Something is going wrong when data is broken up and fit with linear models.



setwd(tk_choose.dir("Choose X"))

my_data <- read_excel("step.stim.xlsx")

# filtered vector data for 1
# ten.1 <- my_data %>%
#   dplyr::select(Time, Tension_1)

ten.4 <- my_data %>%
  dplyr::select(Time, Tension_4)

# ten.3 <- my_data %>%
#   dplyr::select(Time, Tension_3)
# 
# ten.4 <- my_data %>%
#   dplyr::select(Time, Tension_4)

 
#### Tension 4

df4 <- data.frame(t = ten.4$Time,
                 y = ten.4$Tension_4)

dygraph(df4)

#### separation into phases

phase4 <- df4 %>% 
  filter(t <= 0.0065)

phase3 <- df4 %>% 
  filter(t >= 0.014, t <= 0.07)

phase4 <- df4 %>% 
  filter(t >= 0.14) 


### Phase 4

phase4$log <- log10(phase4$y)
#phase4$ln <- log(phase4$y)

phase4.linfit <- lm(phase4$log ~ phase4$t)
phase4$fit <- predict(phase4.linfit)

(phase4.plot <- ggplot(data = phase4, aes(x = t, y = log)) +
    geom_point()+
    geom_line(aes(y = fit), size = 0.8, col = "red") +
    ggtitle("Phase 4 Linear Fit")
)

grd4 <- list(a = 10^phase4.linfit$coefficients[[1]],
             b = (-phase4.linfit$coefficients[[4]])/(log10(exp(1)))
             )

phase4.nls <- nls(y ~ a* exp(-b * t),
                    data = phase4,
                    start = grd4,
                    control = nls.control(maxiter = 100)
                    )

phase4$nls <- predict(phase4.nls)

(phase4.model <- ggplot(data = phase4, aes(x = t, y = y)) +
    geom_point()+
    geom_line(aes(y = nls), size = 0.8, col = "red") +
    ggtitle("Phase 4 Nls Fit")
)

### Phase 3 
dygraph(phase3)

phase3$log <- log10(phase3$y)

phase3.linfit <- lm(phase3$log ~ phase3$t)
phase3$fit <- predict(phase3.linfit)

(phase3.plot <- ggplot(data = phase3, aes(x = t, y = log)) +
    geom_point()+
    geom_line(aes(y = fit), size = 0.8, col = "red") +
    ggtitle("Phase 3 Linear Fit")
)


# abby test 
phase3$e <- (1/phase3$y)

w <- data.frame(x = phase3$t,
                y = phase3$e)

dygraph(w)

(abby <- ggplot(data = phase3, aes(x = t)) +
    geom_point(aes(y = y), col = "blue") +
    geom_point(aes(y = log), col = "red") +
    coord_flip()
  )

####
grd3 <- list(a = 10^phase4.linfit$coefficients[[1]],
             b = (-phase3.linfit$coefficients[[4]])/(log10(exp(1)))
)

phase3.nls <- nls(y ~ (1-(a* exp(-b * t))),
                    data = phase3,
                    start = grd3,
                    control = nls.control(maxiter = 100)
)

phase3$nls <- predict(phase3.nls)

df <- data.frame(x = phase3$t,
                 y = phase3$y)


(phase3.model <- ggplot(data = phase3, aes(x = t, y = y)) +
    geom_point()+
    geom_line(aes(y = nls), size = 0.8, col = "red") +
    ggtitle("Phase 3 Nls Fit")
)

#### Phase 4

phase4$log <- log10(phase4$y)

phase4.linfit <- lm(phase4$log ~ phase4$t)
phase4$fit <- predict(phase4.linfit)

(phase4.plot <- ggplot(data = phase4, aes(x = t, y = log)) +
    geom_point()+
    geom_line(aes(y = fit), size = 0.8, col = "red") +
    ggtitle("Phase 4 Linear Fit")
)

grd4 <- list(a = 10^phase4.linfit$coefficients[[1]],
             b = (-phase4.linfit$coefficients[[4]])/(log10(exp(1)))
)

phase4.nls <- nls(y ~ a* exp(-b * t),
                    data = phase4,
                    start = grd4,
                    control = nls.control(maxiter = 100)
)

phase4$nls <- predict(phase4.nls)

(phase4.model <- ggplot(data = phase4, aes(x = t, y = y)) +
    geom_point()+
    geom_line(aes(y = nls), size = 0.8, col = "red") +
    ggtitle("Phase 4 Nls Fit")
)


### All

grd.all <- list(a = 10^phase4.linfit$coefficients[[1]],
             b = (-phase4.linfit$coefficients[[4]])/(log10(exp(1))),
             c = 10^phase3.linfit$coefficients[[1]],
             d = (-phase3.linfit$coefficients[[4]])/(log10(exp(1))),
             e = 10^phase4.linfit$coefficients[[1]],
             g = (-phase4.linfit$coefficients[[4]])/(log10(exp(1)))
             )


# equation
f4 <- df4$y ~ (a*exp(-b*df4$t))+ 
  (c*(1.0-exp(-d*df4$t))) + 
  (e*exp(-g*df4$t))

# starting parameters from excel sheet itself
# grd4 <- list(a = 1,
#              b = 14.5,
#              c = 1,
#              d = 1,
#              e = 1,
#              g = 8)

fit.ten.4 <- nlsLM(f4,
                   data = df4,
                   start = list(a = 1,
                                b = 100,
                                c = 1,
                                d = 10,
                                e = 1,
                                g = 5),
                   control = nls.control(100))

summary(fit.ten.4)

df4$fit <- predict(fit.ten.4)

(plot.4 <- ggplot(data = df4, aes(x = t, y = y)) + 
    geom_point() + 
    geom_line(aes(y = fit), size = 0.8, col = "red") +
    ggtitle("Condition 4")
)


# Rate fitting - Guess 3, calculate 3 (already fitted)--------
## 

setwd(tk_choose.dir("Choose X"))

my_data <- read_excel("step.stim.xlsx")
theme_set(theme_classic())

# Tension 1 Trace.......................................

ten.1 <- my_data %>%
   dplyr::select(Time, Tension_1)

df1 <- data.frame(t = ten.1$Time,
                  y = ten.1$Tension_1)

dygraph(df1)

ten.1.p4 <- df1 %>% 
  filter(t <= 0.0065)

ten.1.p4.lin <- lm(log(ten.1.p4$y) ~ ten.1.p4$t)
ten.1.p4$linfit <- predict(ten.1.p4.lin)


gird1 <- list(a = 10^ten.1.p4.lin$coefficients[[1]],
             b = (-ten.1.p4.lin$coefficients[[4]])/(log10(exp(1)))
)

t1.p4 <- nlsLM(y ~ a* exp(-b * t),
               data = ten.1.p4,
               start = gird1,
               control = nls.control(maxiter = 100)
)

ten.1.p4$model <- predict(t1.p4)

(ten.1.graph <- ggplot(data = ten.1.p4, aes(x = t, y = y)) +
    geom_point()+
    geom_line(aes(y = model), size = 0.8, col = "red") +
    ggtitle("Phase 4 Nls Fit")
)


# creation of grd for all parameters
ten.1.p4.summary <- broom::tidy(t1.p4)

grd1 <- list(a = ten.1.p4.summary$estimate[[1]],
            b = ten.1.p4.summary$estimate[[4]],
            c = tail(ten.1$Tension_1, n=1),
            d = ten.1.p4.summary$estimate[[4]]/4,
            e = ten.1.p4.summary$estimate[[1]],
            g = ten.1.p4.summary$estimate[[4]]/4
)

my_formula1 <- Tension_1 ~ (a*exp(-b*Time))+ 
  (c*(1.0-exp(-d*Time))) + 
  (e*exp(-g*Time))

model.1 <- nlsLM(my_formula1,
                 data = ten.1,
                 start = grd1,
                 control = nls.control(maxiter = 100)
)

ten.1$fit <- predict(model.1)

(phil.1 <- ggplot(data = ten.1, aes(x = Time, y = Tension_1)) +
    geom_point()+
    geom_line(aes(y = fit), size = 0.8, col = "red") +
    ggtitle("Non-linear model")
)


### Tension 4 Trace......................................
ten.4 <- my_data %>%
  dplyr::select(Time, Tension_4)

df4 <- data.frame(t = ten.4$Time,
                  y = ten.4$Tension_4)
dygraph(df4)

ten.4.p4 <- df4 %>% 
  filter(t <= 0.0065)

ten.4.p4.lin <- lm(log(ten.4.p4$y) ~ ten.4.p4$t)
ten.4.p4$linfit <- predict(ten.4.p4.lin)


gird4 <- list(a = 10^ten.4.p4.lin$coefficients[[1]],
             b = (-ten.4.p4.lin$coefficients[[4]])/(log10(exp(1)))
)

t4.p4 <- nlsLM(y ~ a* exp(-b * t),
                  data = ten.4.p4,
                  start = gird4,
                  control = nls.control(maxiter = 100)
)

ten.4.p4$model <- predict(t4.p4)

(ten.4.graph <- ggplot(data = ten.4.p4, aes(x = t, y = y)) +
    geom_point()+
    geom_line(aes(y = model), size = 0.8, col = "red") +
    ggtitle("Phase 4 Nls Fit")
)


# creation of grd for all parameters
ten.4.p4.summary <- broom::tidy(t4.p4)

grd4 <- list(a = ten.4.p4.summary$estimate[[1]],
            b = ten.4.p4.summary$estimate[[4]],
            c = tail(ten.4$Tension_4, n=1),
            d = ten.4.p4.summary$estimate[[4]]/4,
            e = ten.4.p4.summary$estimate[[1]],
            g = ten.4.p4.summary$estimate[[4]]/4
)

my_formula4 <- Tension_4 ~ (a*exp(-b*Time))+ 
  (c*(1.0-exp(-d*Time))) + 
  (e*exp(-g*Time))

model.4 <- nlsLM(my_formula4,
                 data = ten.4,
                 start = grd4,
                 control = nls.control(maxiter = 100)
)

ten.4$fit <- predict(model.4)

(phil.4 <- ggplot(data = ten.4, aes(x = Time, y = Tension_4)) +
    geom_point()+
    geom_line(aes(y = fit), size = 0.8, col = "red") +
    ggtitle("Non-linear model")
)

### Tension 3 Trace...........................................

ten.3 <- my_data %>%
  dplyr::select(Time, Tension_3)

df3 <- data.frame(t = ten.3$Time,
                  y = ten.3$Tension_3)
dygraph(df3)

ten.3.ph4 <- df3 %>% 
  filter(t <= 0.0065)

ten.3.ph4.linfit <- lm(log(ten.3.ph4$y) ~ ten.3.ph4$t)
ten.3.ph4$lin <- predict(ten.3.ph4.linfit)


gird3 <- list(a = 10^ten.3.ph4.linfit$coefficients[[1]],
             b = (-ten.3.ph4.linfit$coefficients[[4]])/(log10(exp(1)))
)

ten.3.phas4.model <- nlsLM(y ~ a* exp(-b * t),
                           data = ten.3.ph4,
                           start = gird3,
                           control = nls.control(maxiter = 100)
)

ten.3.ph4$model <- predict(ten.3.phas4.model)

(ten3.phase4 <- ggplot(data = ten.3.ph4, aes(x = t, y = y)) +
    geom_point()+
    geom_line(aes(y = model), size = 0.8, col = "red") +
    ggtitle("Phase 4 Nls Fit")
)

# creation of grd for all parameters
ten.3.ph4.model.tidy <- broom::tidy(ten.3.phas4.model)

grd3 <- list(a = ten.3.ph4.model.tidy$estimate[[1]],
            b = ten.3.ph4.model.tidy$estimate[[4]],
            c = tail(ten.3$Tension_3, n=1),
            d = ten.3.ph4.model.tidy$estimate[[4]]/4,
            e = ten.3.ph4.model.tidy$estimate[[1]],
            g = ten.3.ph4.model.tidy$estimate[[4]]/4
)

my_formula3 <- Tension_3 ~ (a*exp(-b*Time))+ 
  (c*(1.0-exp(-d*Time))) + 
  (e*exp(-g*Time))

model.3 <- nlsLM(my_formula3,
                 data = ten.3,
                 start = grd3,
                 control = nls.control(maxiter = 100)
)

ten.3$fit <- predict(model.3)

(phil.3 <- ggplot(data = ten.3, aes(x = Time, y = Tension_3)) +
    geom_point()+
    geom_line(aes(y = fit), size = 0.8, col = "red") +
    ggtitle("Non-linear model")
)


### Tension 4 trace..............................................
ten.4 <- my_data %>%
 dplyr::select(Time, Tension_4)

df4 <- data.frame(t = ten.4$Time,
                  y = ten.4$Tension_4)
dygraph(df4)

ten.4.ph4 <- df4 %>% 
  filter(t <= 0.0085)

ten.4.ph4.linfit <- lm(log(ten.4.ph4$y) ~ ten.4.ph4$t)
ten.4.ph4$lin <- predict(ten.4.ph4.linfit)


gird4 <- list(a = 10^ten.4.ph4.linfit$coefficients[[1]],
             b = (-ten.4.ph4.linfit$coefficients[[4]])/(log10(exp(1)))
)

ten.4.phas4.model <- nlsLM(y ~ a* exp(-b * t),
                  data = ten.4.ph4,
                  start = gird4,
                  control = nls.control(maxiter = 100)
)

ten.4.ph4$model <- predict(ten.4.phas4.model)

(ten4.phase4 <- ggplot(data = ten.4.ph4, aes(x = t, y = y)) +
    geom_point()+
    geom_line(aes(y = model), size = 0.8, col = "red") +
    ggtitle("Phase 4 Nls Fit")
)

# creation of grd for all parameters
ten.4.ph4.model.tidy <- broom::tidy(ten.4.phas4.model)

grd4 <- list(a = ten.4.ph4.model.tidy$estimate[[1]],
            b = ten.4.ph4.model.tidy$estimate[[4]],
            c = tail(ten.4$Tension_4, n=1),
            d = ten.4.ph4.model.tidy$estimate[[4]]/4,
            e = ten.4.ph4.model.tidy$estimate[[1]],
            g = ten.4.ph4.model.tidy$estimate[[4]]/4
)

my_formula4 <- Tension_4 ~ (a*exp(-b*Time))+ 
  (c*(1.0-exp(-d*Time))) + 
  (e*exp(-g*Time))

model.4 <- nlsLM(my_formula4,
                 data = ten.4,
                 start = grd4,
                 control = nls.control(maxiter = 100)
)

ten.4$fit <- predict(model.4)

(phil.4 <- ggplot(data = ten.4, aes(x = Time, y = Tension_4)) +
    geom_point()+
    geom_line(aes(y = fit), size = 0.8, col = "red") +
    ggtitle("Non-linear model")
)


# Rate fitting - Guess 3, calculate 3 (raw data)----

setwd(tk_choose.dir("Choose X"))

### read data in
my_files <- list.files(pattern = "Run")
my_data <- map(my_files, ~ read_excel(.x, skip = 22))
names(my_data) <- my_files

# my_files <- list.files(pattern = "Run")
# my_data <- map(my_files, ~ read_excel(.x, skip = 22) %>%
#                  dplyr::select(Time, Force_One) %>% 
#                  dplyr::filter(Time >= Time[[which(Force_One == max(Force_One))]])
#                                )
# names(my_data) <- my_files


# my_files <- list.files(pattern = "Run")
# my_data <- map_df(my_files, ~ read_excel(.x, skip = 22) %>%
#                  dplyr::mutate(filename = .x) %>%
#                  dplyr::select(filename, Time, Force_One))
# my_data_nest <-
#   my_data %>%
#   nest(data = c(Time, Force_One))


### Run 2: 16 mM Pi........................................................
run2 <- my_data$Run2.xlsx %>%
  filter(Time >= 0.102, Time <=0.2)

run2$Time0 <- run2$Time - run2$Time[[1]]

df2 <- data.frame(t = run2$Time0,
                  y = run2$Force_One)

dygraph(df2)

r2.phase2 <- df2 %>% 
  filter(t <= 0.0088)

r2.p2.linfit <- lm(log(r2.phase2$y) ~ r2.phase2$t)
r2.phase2$lin <- predict(r2.p2.linfit)

lingrd2 <- list(a = 10^r2.p2.linfit$coefficients[[1]],
               b = (-r2.p2.linfit$coefficients[[2]])/(log10(exp(1)))
)

r2.phase2.model <- nlsLM(y ~ (a*exp(-b*t)),
                      data = r2.phase2,
                      start = lingrd2,
                      control = nls.control(maxiter = 100)
)

r2.phase2$fit <- predict(r2.phase2.model)

(r2.phase2.graph <- ggplot(data = r2.phase2, aes(x = t, y = y)) +
    geom_point()+
    geom_line(aes(y = fit), size = 0.8, col = "red") +
    ggtitle("Run 2 Phase 2")
)

r2.phase2.model.summary <- broom::tidy(r2.phase2.model)

grd2 <- list(a = r2.phase2.model.summary$estimate[[1]],
            b = r2.phase2.model.summary$estimate[[2]],
            c = tail(df2$y, n =1),
            d = r2.phase2.model.summary$estimate[[2]]/2,
            e = r2.phase2.model.summary$estimate[[1]],
            g = r2.phase2.model.summary$estimate[[2]]/4
)


my_formula1 <- y ~ (a*exp(-b*t))+ 
  (c*(1.0-exp(-d*t))) + 
  (e*exp(-g*t))


run2.model <- nlsLM(my_formula1,
                    data = df2,
                    start = grd2,
                    control = nls.control(maxiter = 100))

run2.model.tidy <- tidy(run2.model)


run2.phase2 <- nlsLM(my_formula1,
                     data = df2,
                     start = list(a = run2.model.tidy$estimate[[1]],
                                  b = run2.model.tidy$estimate[[2]],
                                  c = 0,
                                  d = run2.model.tidy$estimate[[4]],
                                  e = 0,
                                  g = run2.model.tidy$estimate[[6]]),
                                  control = nls.control(maxiter = 100)
)

run2.phase3 <- nlsLM(my_formula1,
                     data = df2,
                     start = list(a = 0,
                                  b = run2.model.tidy$estimate[[2]],
                                  c = run2.model.tidy$estimate[[3]],
                                  d = run2.model.tidy$estimate[[4]],
                                  e = 0,
                                  g = run2.model.tidy$estimate[[6]]),
                     control = nls.control(maxiter = 100)
)

run2.phase4 <- nlsLM(my_formula1,
                     data = df2,
                     start = list(a = 0,
                                  b = run2.model.tidy$estimate[[2]],
                                  c = 0,
                                  d = run2.model.tidy$estimate[[4]],
                                  e = run2.model.tidy$estimate[[5]],
                                  g = run2.model.tidy$estimate[[6]]),
                     control = nls.control(maxiter = 100)
)

df2$fit <- predict(run2.model)
df2$phase2 <- predict(run2.phase2)
df2$phase3 <- predict(run2.phase3)
df2$phase4 <- predict(run2.phase4)


(run2.graph <- ggplot(data = df2, aes(x = t, y = y)) +
    geom_point()+
    geom_line(aes(y = fit), size = 0.8, col = "red") +
    ggtitle("Run2: 16mM Pi")
)

(fit.breakup <- ggplot(data = df2, aes(x = t, y=y)) +
  geom_line(aes(y=fit), size = 0.8, col = "red") + 
  geom_line(aes(y=phase2), size = 0.6, col = "blue") +
  geom_line(aes(y=phase3), size = 0.6, col = "green") +
  geom_line(aes(y=phase4), size = 0.6, col = "orange") +
    ggtitle("Run2: 16mM Pi")
)



### Run 3: 8mM Pi...............................................................
run3 <- my_data$Run3.xlsx %>% 
  filter(Time >= 0.102, Time <=0.21)

run3$Time0 <- run3$Time - run3$Time[[1]]

df3 <- data.frame(t = run3$Time0,
                  y = run3$Force_One)

dygraph(df3)

r3.phase2 <- df3 %>% 
  filter(t <= 0.0114)

r3.p2.linfit <- lm(log(r3.phase2$y) ~ r3.phase2$t)
r3.phase2$lin <- predict(r3.p2.linfit)

lingrd3 <- list(a = 10^r3.p2.linfit$coefficients[[1]],
                b = (-r3.p2.linfit$coefficients[[2]])/(log10(exp(1)))
)

r3.phase2.model <- nlsLM(y ~ (a*exp(-b*t)),
                         data = r3.phase2,
                         start = lingrd3,
                         control = nls.control(maxiter = 100)
)

r3.phase2$fit <- predict(r3.phase2.model)

(r3.phase2.graph <- ggplot(data = r3.phase2, aes(x = t, y = y)) +
    geom_point()+
    geom_line(aes(y = fit), size = 0.8, col = "red") +
    ggtitle("Run 3, Phase 2")
)

r3.phase2.model.summary <- broom::tidy(r3.phase2.model)

grd3 <- list(a = r3.phase2.model.summary$estimate[[1]],
             b = r3.phase2.model.summary$estimate[[2]],
             c = tail(df3$y, n =1),
             d = r3.phase2.model.summary$estimate[[2]]/2,
             e = r3.phase2.model.summary$estimate[[1]],
             g = r3.phase2.model.summary$estimate[[2]]/2
)


my_formula1 <- y ~ (a*exp(-b*t))+ 
  (c*(1.0-exp(-d*t))) + 
  (e*exp(-g*t))


run3.model <- nlsLM(my_formula1,
                    data = df3,
                    start = grd3,
                    control = nls.control(maxiter = 100)
)

df3$fit <- predict(run3.model)


(run3.graph <- ggplot(data = df3, aes(x = t, y = y)) +
    geom_point()+
    geom_line(aes(y = fit), size = 0.8, col = "red") +
    ggtitle("Run 3: 8mM Pi")
)

run3.model

### Run 4: 4mM Pi................................................................
run4 <- my_data$Run4.xlsx %>% 
  filter(Time >= 0.1023, Time <=0.25)

run4$Time0 <- run4$Time - run4$Time[[1]]

df4 <- data.frame(t = run4$Time0,
                  y = run4$Force_One)

dygraph(df4)

r4.phase2 <- df4 %>% 
  filter(t <= 0.0154)

r4.p2.linfit <- lm(log(r4.phase2$y) ~ r4.phase2$t)
r4.phase2$lin <- predict(r4.p2.linfit)

lingrd4 <- list(a = 10^r4.p2.linfit$coefficients[[1]],
                b = (-r4.p2.linfit$coefficients[[2]])/(log10(exp(1)))
)

r4.phase2.model <- nlsLM(y ~ (a*exp(-b*t)),
                         data = r4.phase2,
                         start = lingrd4,
                         control = nls.control(maxiter = 100)
)

r4.phase2$fit <- predict(r4.phase2.model)

(r4.phase2.graph <- ggplot(data = r4.phase2, aes(x = t, y = y)) +
    geom_point()+
    geom_line(aes(y = fit), size = 0.8, col = "red") +
    ggtitle("Run 4 Phase 2")
)

r4.phase2.model.summary <- broom::tidy(r4.phase2.model)

grd4 <- list(a = r4.phase2.model.summary$estimate[[1]],
             b = r4.phase2.model.summary$estimate[[2]],
             c = tail(df4$y, n =1),
             d = r4.phase2.model.summary$estimate[[2]]/2,
             e = r4.phase2.model.summary$estimate[[1]],
             g = r4.phase2.model.summary$estimate[[2]]/4
)


my_formula1 <- y ~ (a*exp(-b*t))+ 
  (c*(1.0-exp(-d*t))) + 
  (e*exp(-g*t))


run4.model <- nlsLM(my_formula1,
                    data = df4,
                    start = grd4,
                    control = nls.control(maxiter = 100)
)

df4$fit <- predict(run4.model)


(run4.graph <- ggplot(data = df4, aes(x = t, y = y)) +
    geom_point()+
    geom_line(aes(y = fit), size = 0.8, col = "red") +
    ggtitle("Run 4: 4mM Pi")
)

run4.model

### Run 5: 2 mM Pi.............................................................

run5 <- my_data$Run5.xlsx %>% 
  filter(Time >= 0.102, Time <=0.33)

run5$Time0 <- run5$Time - run5$Time[[1]]

df5 <- data.frame(t = run5$Time0,
                  y = run5$Force_One)

dygraph(df5)

r5.phase2 <- df5 %>% 
  filter(t <= 0.029)

r5.p2.linfit <- lm(log(r5.phase2$y) ~ r5.phase2$t)
r5.phase2$lin <- predict(r5.p2.linfit)

lingrd5 <- list(a = 10^r5.p2.linfit$coefficients[[1]],
                b = (-r5.p2.linfit$coefficients[[2]])/(log10(exp(1)))
)

r5.phase2.model <- nlsLM(y ~ (a*exp(-b*t)),
                         data = r5.phase2,
                         start = lingrd5,
                         control = nls.control(maxiter = 100)
)

r5.phase2$fit <- predict(r5.phase2.model)

(r5.phase2.graph <- ggplot(data = r5.phase2, aes(x = t, y = y)) +
    geom_point()+
    geom_line(aes(y = fit), size = 0.8, col = "red") +
    ggtitle("Run 5 Phase 2")
)

r5.phase2.model.summary <- broom::tidy(r5.phase2.model)

grd5 <- list(a = r5.phase2.model.summary$estimate[[1]],
             b = r5.phase2.model.summary$estimate[[2]],
             c = tail(df5$y, n = 1),
             d = r5.phase2.model.summary$estimate[[2]]/2,
             e = r5.phase2.model.summary$estimate[[1]],
             g = r5.phase2.model.summary$estimate[[2]]/4
)


my_formula1 <- y ~ (a*exp(-b*t))+ 
  (c*(1.0-exp(-d*t))) + 
  (e*exp(-g*t))


run5.model <- nlsLM(my_formula1,
                    data = df5,
                    start = grd5,
                    control = nls.control(maxiter = 100)
)

df5$fit <- predict(run5.model)


(run5.graph <- ggplot(data = df5, aes(x = t, y = y)) +
    geom_point()+
    geom_line(aes(y = fit), size = 0.8, col = "red") +
    ggtitle("Run 5: 2mM Pi")
)

run5.model

### Run 6: 0mM Pi................................................................

run6 <- my_data$Run6.xlsx %>% 
  filter(Time >= 0.102, Time <=0.33)

run6$Time0 <- run6$Time - run6$Time[[1]]

df6 <- data.frame(t = run6$Time0,
                  y = run6$Force_One)

dygraph(df6)

r6.phase2 <- df6 %>% 
  filter(t <= 0.0426)

r6.p2.linfit <- lm(log(r6.phase2$y) ~ r6.phase2$t)
r6.phase2$lin <- predict(r6.p2.linfit)

lingrd6 <- list(a = 10^r6.p2.linfit$coefficients[[1]],
                b = (-r6.p2.linfit$coefficients[[2]])/(log10(exp(1)))
)

r6.phase2.model <- nlsLM(y ~ (a*exp(-b*t)),
                         data = r6.phase2,
                         start = lingrd6,
                         control = nls.control(maxiter = 100)
)

r6.phase2$fit <- predict(r6.phase2.model)

(r6.phase2.graph <- ggplot(data = r6.phase2, aes(x = t, y = y)) +
    geom_point()+
    geom_line(aes(y = fit), size = 0.8, col = "red") +
    ggtitle("Run 6 Phase 2")
)

r6.phase2.model.summary <- broom::tidy(r6.phase2.model)

grd6 <- list(a = r6.phase2.model.summary$estimate[[1]],
             b = r6.phase2.model.summary$estimate[[2]],
             c = tail(df6$y, n =1),
             d = r6.phase2.model.summary$estimate[[2]]/2,
             e = r6.phase2.model.summary$estimate[[1]],
             g = r6.phase2.model.summary$estimate[[2]]/4
)


my_formula1 <- y ~ (a*exp(-b*t))+ 
  (c*(1.0-exp(-d*t))) + 
  (e*exp(-g*t))


run6.model <- nlsLM(my_formula1,
                    data = df6,
                    start = grd6,
                    control = nls.control(maxiter = 100)
)

df6$fit <- predict(run6.model)


(run6.graph <- ggplot(data = df6, aes(x = t, y = y)) +
    geom_point()+
    geom_line(aes(y = fit), size = 0.8, col = "red") +
    ggtitle("Run 6: 0mM Pi")
)

run6.model
 
### Saving all .............................................................

p <- ggpubr::ggarrange(run2.graph, run3.graph, run4.graph, run5.graph, run6.graph,
                       ncol = 2,
                       nrow = 3)

run2.model.tidy <- broom::tidy(run2.model)
run3.model.tidy <- broom::tidy(run3.model)
run4.model.tidy <- broom::tidy(run4.model)
run5.model.tidy <- broom::tidy(run5.model)
run6.model.tidy <- broom::tidy(run6.model)

m <- list(run2.model.tidy, 
          run3.model.tidy, 
          run4.model.tidy, 
          run5.model.tidy,
          run6.model.tidy)

names(m) <- c("16mM Pi", 
              "8mM Pi", 
              "4mM Pi", 
              "2mM Pi", 
              "0mM Pi")

# exporting data
ggpubr::ggexport(p, filename = "Woods_Fiberx_Plots.pdf")
writexl::write_xlsx(m,
                    path = 'Woods_Fiberx_Fits.xlsx'
)

