library(tidyverse)
library(readxl)
library(lmerTest)

theme_set(theme_classic())

setwd("C:/Users/Phil/Dropbox/Thesis- Stretch Activation/Data/Woods - Master's Thesis/Project/Tension + AaBbCc")

my_data <- read_excel("SA-Fatigue_Tension+Step+Kinetics_PW_10-20-22.xlsx", 
                      sheet = "Included",
                      skip = 5,
                      na="") %>% 
  filter(Exp_Con_Num %in% c(2:6)) %>% 
  filter(fiber_type_num %in% c(1:4)) %>% 
  filter(Ran_Num == 1) %>% 
  filter(P3_num == 1) %>%
  group_by(Exp_Con, fiber_type, fiber_type_num)

fit.f0 <- lmer(Po_Post_Step ~ fiber_type*Exp_Con + (1|Mouse), data = my_data)
anova(fit.f0)

fit.fsa <- lmer()


### MHC I --------------------------------------------------------------------------------

fiberI <- my_data %>% 
  filter(fiber_type == "I")

fiberI.f0.fit <- lmer(Po_Post_Step ~ Exp_Con + (1|Mouse), data = fiberI)
anova(fiberI.f0.fit)

fiberI.fsa.fit <- lmer(Fsa ~ Exp_Con + (1|Mouse), data = fiberI)
anova(fiberI.fsa.fit)

fiberI.ratio.fit <- lmer(FsaF0 ~ Exp_Con + (1|Mouse), data = fiberI)
anova(fiberI.ratio.fit)

### MHC IIA ------------------------------------------------------------------------------

fiberIIA <- my_data %>% 
  filter(fiber_type == "IIA")

fiberIIA.f0.fit <- lmer(Po_Post_Step ~ Exp_Con + (1|Mouse), data = fiberIIA)
anova(fiberIIA.f0.fit)

fiberIIA.fsa.fit <- lmer(Fsa ~ Exp_Con + (1|Mouse), data = fiberIIA)
anova(fiberIIA.fsa.fit)

fiberIIA.ratio.fit <- lmer(FsaF0 ~ Exp_Con + (1|Mouse), data = fiberIIA)
anova(fiberIIA.ratio.fit)

### MHC IIX -----------------------------------------------------------------------------

fiberIIX <- my_data %>% 
  filter(fiber_type == "IIX")

fiberIIX.f0.fit <- lmer(Po_Post_Step ~ Exp_Con + (1|Mouse), data = fiberIIX)
anova(fiberIIX.f0.fit)

fiberIIX.fsa.fit <- lmer(Fsa ~ Exp_Con + (1|Mouse), data = fiberIIX)
anova(fiberIIX.fsa.fit)

fiberIIX.ratio.fit <- lmer(FsaF0 ~ Exp_Con + (1|Mouse), data = fiberIIX)
anova(fiberIIX.ratio.fit)

### MHC IIB ------------------------------------------------------------------------------

fiberIIB <- my_data %>% 
  filter(fiber_type == "IIB")

fiberIIB.f0.fit <- lmer(Po_Post_Step ~ Exp_Con + (1|Mouse), data = fiberIIB)
anova(fiberIIB.f0.fit)

fiberIIB.fsa.fit <- lmer(Fsa ~ Exp_Con + (1|Mouse), data = fiberIIB)
anova(fiberIIB.fsa.fit)

fiberIIB.ratio.fit <- lmer(FsaF0 ~ Exp_Con + (1|Mouse), data = fiberIIB)
anova(fiberIIB.ratio.fit)
