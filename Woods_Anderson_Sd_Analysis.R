library(tidyverse)
library(readxl)
library(lmerTest)
library(multcomp)
library(emmeans)

setwd("C:/Users/pcw00/Dropbox/University of Massachusetts Amherst/Thesis- Stretch Activation/Anderson - Shortening Deactivation/Data")

my_data <- read_excel("SA_SD-Fatigue_Tension_PW_5-7-23.xlsx",
                      sheet = "Included",
                      skip = 5,
                      na = "") %>% 
  filter(SD3_Num == 1)

my_data_IIX <- my_data %>% filter(fiber_type == "IIX")
my_data_IIB <- my_data %>% filter(fiber_type == "IIB")

### MHC IIX -----------------------------

fsd.fit.IIX <- lmer(Fsd ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse),data = my_data_IIX)
(fsd.anova.IIX <- anova(fsd.fit.IIX))
(Fsd.emm.IIX <- emmeans(fsd.fit.IIX, specs = "Exp_Con"))

fsdf0.fit.IIX <- lmer(FsdF0 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse),data = my_data_IIX)
(fsdf0.anova.IIX <- anova(fsdf0.fit.IIX))
(Fsdf0.emm.IIX <- emmeans(fsdf0.fit.IIX, specs = "Exp_Con"))
(fsdf0.hoc.IIX <- summary(glht(fsdf0.fit.IIX,linfct = mcp(Exp_Con = "Tukey"))))

fsdtotal.fit.IIX <- lmer(Fsd_total ~ Exp_Con + (1 + as.factor(Exp_Con) | Mouse), data = my_data_IIX)
(fsdtotal.anova.IIX <- anova(fsdtotal.fit.IIX))
(Fsdtotal.emm.IIX <- emmeans(fsdtotal.fit.IIX, specs = "Exp_Con"))
(fsdtotal.hoc.IIX <- summary(glht(fsdtotal.fit.IIX,linfct = mcp(Exp_Con = "Tukey"))))

t3.fit.IIX <- lmer(t3 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse),data = my_data_IIX)
(t3.anova.IIX <- anova(t3.fit.IIX))
(t3.emm.IIX <- emmeans(t3.fit.IIX, specs = "Exp_Con"))

### MHC IIB --------------------------------
fsd.fit.IIB <- lmer(Fsd ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse),data = my_data_IIB)
(fsd.anova.IIB <- anova(fsd.fit.IIB))
(Fsd.emm.IIB <- emmeans(fsd.fit.IIB, specs = "Exp_Con"))
(fsd.hoc.IIB <- summary(glht(fsd.fit.IIB,linfct = mcp(Exp_Con = "Tukey"))))


fsdf0.fit.IIB <- lmer(FsdF0 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse),data = my_data_IIB)
(fsdf0.anova.IIB <- anova(fsdf0.fit.IIB))
(Fsdf0.emm.IIB <- emmeans(fsdf0.fit.IIB, specs = "Exp_Con"))
(fsdf0.hoc.IIB <- summary(glht(fsdf0.fit.IIB,linfct = mcp(Exp_Con = "Tukey"))))

fsdtotal.fit.IIB <- lmer(Fsd_total ~ Exp_Con + (1 + as.factor(Exp_Con) | Mouse), data = my_data_IIB)
(fsdtotal.anova.IIB <- anova(fsdtotal.fit.IIB))
(Fsdtotal.emm.IIB <- emmeans(fsdtotal.fit.IIB, specs = "Exp_Con"))
(fsdtotal.hoc.IIB <- summary(glht(fsdtotal.fit.IIB,linfct = mcp(Exp_Con = "Tukey"))))


t3.fit.IIB <- lmer(t3 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse),data = my_data_IIB)
(t3.anova.IIB <- anova(t3.fit.IIB))
(t3.emm.IIB <- emmeans(t3.fit.IIB, specs = "Exp_Con"))
(t3.hoc.IIB <- summary(glht(t3.fit.IIB, linfct = mcp(Exp_Con = "Tukey"))))
