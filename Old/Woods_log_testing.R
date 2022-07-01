## Rate Fittings - Troubleshooting with logs
## Stretch Activation and Fatigue Study 
## Woods Master's Thesis
## Created 4/22/22 by PW
## Last updated: 4/24/22 by PW


library(tcltk)
library(tidyverse)
library(readxl)
library(dygraphs)
library(RcppRoll)
library(minpack.lm)
library(ggpubr)
library(broom)
library(writexl)

#### Random data to test single exp fit --------------------------------------------
x <- c(0.001,
       0.101,
       0.201,
       0.301,
       0.401,
       0.501,
       0.601,
       0.701,
       0.801)
y <- c(9.950124792,
       6.035055754,
       3.660446348,
       2.220172938,
       1.346602957,
       0.81675598,
       0.495387543,
       0.300467733,
       0.182242893)

z <- log10(y)


plot(x,z)
# test <- plot(x,log(y))

test <- data.frame(x = x,
                   y = y)

lm.test <- lm(z ~ x)

summary(lm.test)

grd9 <- list(a = 10^(lm.test$coefficients[[1]]),
             b = (-lm.test$coefficients[[2]])/(log10(exp(1)))
             )


test$fit <- predict(lm.test)

(test.plot <- ggplot(data = test, aes(x = x, y = z)) +
                geom_point() +
                geom_line(aes(y = z), size = 0.8, col = "red") +
                ggtitle("Test")
)

test.model <- nlsLM(y ~ a* exp(-b * x),
                     data = test,
                     start = grd9,
                     control = nls.control(maxiter = 100))
# predicted fit
test$predict <- predict(test.model)

# data w/ fit
(test.plot.predict <- ggplot(data = test, aes(x = x, y = y)) + 
                geom_point() + 
                geom_line(aes(y = predict), size = 0.8, col = "red") +
                ggtitle("Condition 2")
)


#### reading in data -------------------------------------------

setwd(tk_choose.dir("Choose X", caption = "Select directory"))

my_files <- list.files(pattern = "Run") 
my_data <- map(my_files, ~ read_excel(.x, skip = 23))
names(my_data) <- my_files

theme_set(theme_classic())
#### Data filtering ----------------------------------------

cond2 <- my_data$Run2.xlsx %>% 
        select(Time, Force_One) %>% 
        filter(Time >= 0.1016, Time <= 0.19)

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

#### Phase 2: Single exp fit -----------------------------------------

# new variable called log with log values of Force_One
c2.phase2$log <- log10(c2.phase2$Force_One)

plot(c2.phase2$Time0, c2.phase2$log)

#linear fit + prediction 
lm.c2.p2 <- lm(log10(c2.phase2$Force_One) ~ c2.phase2$Time0)
c2.phase2$fit <- predict(lm.c2.p2)

# linear fit of logs values of Force_One w/ prediction line
(plot1 <- ggplot(data = c2.phase2, aes(x = Time0, y = log)) +
        geom_point()+
        geom_line(aes(y = fit), size = 0.8, col = "red") +
        ggtitle("Cond 2, Phase 2 Linear Fit")
)

# Non linear model
grd1 <- list(a = 10^(lm.c2.p2$coefficients[[1]]),
             b = (-lm.c2.p2$coefficients[[2]])/(log10(exp(1))))

df <- data.frame(x = c2.phase2$Time0,
                 y = c2.phase2$Force_One)

mdl <- nlsLM(y ~ a* exp(-b * x),
        data = df,
        start = grd1,
        control = nls.control(maxiter = 100))

c2.phase2$nl <- predict(mdl)

(plot2 <- ggplot(data = c2.phase2, aes(x = Time0, y = Force_One)) +
                geom_point()+
                geom_line(aes(y = nl), size = 0.8, col = "red") +
                ggtitle("Cond 2, Phase 2 Exp Fit")
)


#### single exp fit using chad's data (fiber 18 condition 2)
#### Phase 3: Single exponential fit ----------------------------------

c2.phase3$Time0 <- c2.phase3$Time0 - c2.phase3$Time0[[1]]

c2.phase3$log <- log10(c2.phase3$Force_One)
plot(c2.phase3$Time0, c2.phase3$log)

lm.c2.p3 <- lm(log10(c2.phase3$Force_One) ~ c2.phase3$Time0)
c2.phase3$lin <- predict(lm.c2.p3)

(plot3 <- ggplot(data = c2.phase3, aes(x = Time0, y = log)) +
                geom_point()+
                geom_line(aes(y = lin), size = 0.8, col = "red") +
                ggtitle("Cond 2, Phase 3 Linear Fit")
        )

grd2 <- list(a = 10^(lm.c2.p3$coefficients[[1]]),
            b = (-lm.c2.p3$coefficients[[2]])/(log10(exp(1))))

df2 <- data.frame(x = c2.phase3$Time0,
                  y = c2.phase3$Force_One)

#f <- y ~ a * (1 - exp(-b * x))


# not negative b makes the fit somewhat better??
mdl2 <- nlsLM(y ~ a * (1 - exp(b * x)),
              data = df2,
              start = grd33,
              control = nls.control(maxiter = 100))

c2.phase3$nl <- predict(mdl2)

(plot4 <- ggplot(data = c2.phase3, aes(x = Time0, y = Force_One)) +
                geom_point()+
                geom_line(aes(y = nl), size = 0.8, col = "red") +
                ggtitle("Cond 2, Phase 3 Exp Fit")
)

#### Phase 4: Single exp fit ---------------------------------------

lm.c2.p4 <- lm(log10(c2.phase4$Force_One) ~ c2.phase4$Time0)
c2.phase4$log <- log10(c2.phase4$Force_One)

plot(c2.phase4$Time0, c2.phase4$log)

c2.phase4$lin <- predict(lm.c2.p4)
(plot5 <- ggplot(data = c2.phase4, aes(x = Time0, y = log)) +
                geom_point()+
                geom_line(aes(y = lin), size = 0.8, col = "red") +
                ggtitle("Cond 2, Phase 4 Linear Fit")
)

grd3 <-list(a = 10^(lm.c2.p4$coefficients[[1]]),
            b = (-lm.c2.p4$coefficients[[2]])/(log10(exp(1))))

df3 <- data.frame(x = c2.phase4$Time0,
                  y = c2.phase4$Force_One)

mdl3 <- nlsLM(y ~ a* exp(-b * x),
              data = df3,
              start = grd3,
              control = nls.control(maxiter = 1000))

c2.phase4$nl <- predict(mdl3)

(plot6 <- ggplot(data = c2.phase4, aes(x = Time0, y = Force_One)) +
                geom_point()+
                geom_line(aes(y = nl), size = 0.8, col = "red") +
                ggtitle("Cond 2, Phase 4 Exp Fit")
)

#### Phase 2 + 3 together --------------------------------------

p2.3 <- cond2 %>% 
        filter(Time0 <= 0.0555)

grd4 <- list(a2 = grd2[[1]],
             r2 = grd2[[2]],
             a3 = grd3[[1]],
             r3 = grd3[[2]])

df4 <- data.frame(x = p2.3$Time0,
                  y = p2.3$Force_One)

p2.3.mdl <- nlsLM(y ~ (a2* exp(-r2 * x)) + (a3* exp(-r3 * x)),
                  data = df4,
                  start = grd4,
                  control = nls.control(maxiter = 100)
                  )

p2.3$fit <- predict(p2.3.mdl)

(plot7 <- ggplot(data = p2.3, aes(x = Time0, y = Force_One)) +
                geom_point()+
                geom_line(aes(y = fit), size = 0.8, col = "red") +
                ggtitle("Cond 2: Phase 2 + 3 Exp Fit")
)


