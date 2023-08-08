library(tidyverse)
library(readxl)
library(lmerTest)
library(multcomp)
library(emmeans)

# setwd("C:/Users/Phil/Dropbox/Thesis- Stretch Activation/Data/Woods - Master's Thesis/Project/Tension + AaBbCc")
setwd("C:/Users/pcw00/Dropbox/University of Massachusetts Amherst/Thesis- Stretch Activation/Data/Woods - Master's Thesis/Project/Tension + AaBbCc")


my_data <- read_excel("SA-Fatigue_Tension+Step+Kinetics_PW_5-19-23.xlsx", 
                      sheet = "Manuscript.2",
                      skip = 5,
                      na="") %>% 
  filter(Exp_Con_Num %in% c(2:4)) %>% 
  filter(fiber_type_num %in% c(1:4)) 


### MHC I---------------------

fiberI <- my_data %>% 
  filter(fiber_type == "I")

fiberI.p3 <- fiberI %>% 
  filter(P3_num == 1)

fiberI.f0.fit <- lmer(Po_Post_Step ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberI)
anova(fiberI.f0.fit)
(I_F0_emm <- emmeans(fiberI.f0.fit, specs = "Exp_Con"))
(I_F0_posthoc <- summary(glht(fiberI.f0.fit, 
                                linfct = mcp(Exp_Con = "Tukey"))))

mean(fiberI.p3$Fsa)
sd(fiberI.p3$Fsa) / sqrt(length(fiberI.p3$Fsa))
mean(fiberI.p3$FsaF0)
sd(fiberI.p3$FsaF0) / sqrt(length(fiberI.p3$FsaF0))
mean(fiberI.p3$Fsa_total)
sd(fiberI.p3$Fsa_total) / sqrt(length(fiberI.p3$Fsa_total))


### MHC IIA -------------------------------

fiberIIA <- my_data %>% 
  filter(fiber_type == "IIA")

fiberIIA.p3 <- fiberIIA %>% 
  filter(P3_num == 1)

fiberIIA.f0.fit <- lmer(Po_Post_Step ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIA)
anova(fiberIIA.f0.fit)
(IIA_F0_emm <- emmeans(fiberIIA.f0.fit, specs = "Exp_Con"))
(IIA_F0_posthoc <- summary(glht(fiberIIA.f0.fit, 
                             linfct = mcp(Exp_Con = "Tukey"))))

fiberIIA.fsa.fit <- lmer(Fsa ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIA.p3)
anova(fiberIIA.fsa.fit)
(IIA_Fsa_emm <- emmeans(fiberIIA.fsa.fit, specs = "Exp_Con"))
(IIA_Fsa_posthoc <- summary(glht(fiberIIA.fsa.fit, 
                               linfct = mcp(Exp_Con = "Tukey"))))

fiberIIA.ratio.fit <- lmer(FsaF0 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIA.p3)
anova(fiberIIA.ratio.fit)
(IIA_ratio_emm <- emmeans(fiberIIA.ratio.fit, specs = "Exp_Con"))
(IIA_ratio_posthoc <- summary(glht(fiberIIA.ratio.fit, 
                               linfct = mcp(Exp_Con = "Tukey"))))
fiberIIA.FsaTotal.fit <- lmer(Fsa_total ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIA.p3)
anova(fiberIIA.FsaTotal.fit)
(IIA_FsaTotal_emm <- emmeans(fiberIIA.FsaTotal.fit, specs = "Exp_Con"))
(IIA_FsaTotal_posthoc <- summary(glht(fiberIIA.FsaTotal.fit, 
                                   linfct = mcp(Exp_Con = "Tukey"))))

fiberIIA.a2.fit <- lmer(a2 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIA.p3)
anova(fiberIIA.a2.fit)
(IIA_a2_emm <- emmeans(fiberIIA.a2.fit, specs = "Exp_Con"))
(IIA_a2_posthoc <- summary(glht(fiberIIA.a2.fit, 
                             linfct = mcp(Exp_Con = "Tukey"))))

fiberIIA.a3.fit <- lmer(a3 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIA.p3)
anova(fiberIIA.a3.fit)
(IIA_a3_emm <- emmeans(fiberIIA.a3.fit, specs = "Exp_Con"))
(IIA_a3_posthoc <- summary(glht(fiberIIA.a3.fit, 
                             linfct = mcp(Exp_Con = "Tukey"))))

fiberIIA.a4.fit <- lmer(a4 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIA.p3)
anova(fiberIIA.a4.fit)
(IIA_a4_emm <- emmeans(fiberIIA.a4.fit, specs = "Exp_Con"))
(IIA_a4_posthoc <- summary(glht(fiberIIA.a4.fit,
                             linfct = mcp(Exp_Con = "Tukey"))))

fiberIIA.r2.fit <- lmer(r2 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIA.p3)
anova(fiberIIA.r2.fit)
(IIA_r2_emm <- emmeans(fiberIIA.r2.fit, specs = "Exp_Con"))
(IIA_r2_posthoc <- summary(glht(fiberIIA.r2.fit, 
                             linfct = mcp(Exp_Con = "Tukey"))))

fiberIIA.r3.fit <- lmer(r3 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIA.p3)
anova(fiberIIA.r3.fit)
(IIA_r3_emm <- emmeans(fiberIIA.r3.fit, specs = "Exp_Con"))
(IIA_r3_posthoc <- summary(glht(fiberIIA.r3.fit, 
                             linfct = mcp(Exp_Con = "Tukey"))))

fiberIIA.r3.fit <- lmer(r3 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIA.p3,
                        control = lmerControl(optimizer = "Nelder_Mead"))
anova(fiberIIA.r3.fit)
(IIA_r3_emm <- emmeans(fiberIIA.r3.fit, specs = "Exp_Con"))
(IIA_r3_posthoc <- summary(glht(fiberIIA.r3.fit, 
                                linfct = mcp(Exp_Con = "Tukey"))))

fiberIIA.t3.fit <- lmer(sa.t3 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIA.p3)
anova(fiberIIA.t3.fit)
(IIA_t3_emm <- emmeans(fiberIIA.t3.fit, specs = "Exp_Con"))
(IIA_t3_posthoc <- summary(glht(fiberIIA.t3.fit, 
                                linfct = mcp(Exp_Con = "Tukey"))))


fiberIIA.t3.fit <- lmer(t3 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIA.p3,
                        control = lmerControl(optimizer = "Nelder_Mead"))
anova(fiberIIA.t3.fit)
(IIA_t3_emm <- emmeans(fiberIIA.t3.fit, specs = "Exp_Con"))
(IIA_t3_posthoc <- summary(glht(fiberIIA.t3.fit, 
                                linfct = mcp(Exp_Con = "Tukey"))))



fiberIIA.r4.fit <- lmer(r4 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIA.p3)

fiberIIA.r4.fit <- lmer(r4 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIA.p3,
                        control = lmerControl(optimizer = "Nelder_Mead"))
anova(fiberIIA.r4.fit)
(IIA_r4_emm <- emmeans(fiberIIA.r4.fit, specs = "Exp_Con"))
(IIA_r4_posthoc <- summary(glht(fiberIIA.r4.fit,
                             linfct = mcp(Exp_Con = "Tukey"))))

### MHC IIA: No SA Response---------------------------

fiberIIA.a2.fit <- lmer(a2 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = IIA.data.np3)
anova(fiberIIA.a2.fit)
(IIA_a2_emm <- emmeans(fiberIIA.a2.fit, specs = "Exp_Con"))
(IIA_a2_posthoc <- summary(glht(fiberIIA.a2.fit, 
                                linfct = mcp(Exp_Con = "Tukey"))))

fiberIIA.a3.fit <- lmer(a3 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = IIA.data.np3)
anova(fiberIIA.a3.fit)
(IIA_a3_emm <- emmeans(fiberIIA.a3.fit, specs = "Exp_Con"))
(IIA_a3_posthoc <- summary(glht(fiberIIA.a3.fit, 
                                linfct = mcp(Exp_Con = "Tukey"))))

fiberIIA.a4.fit <- lmer(a4 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = IIA.data.np3)
anova(fiberIIA.a4.fit)
(IIA_a4_emm <- emmeans(fiberIIA.a4.fit, specs = "Exp_Con"))
(IIA_a4_posthoc <- summary(glht(fiberIIA.a4.fit,
                                linfct = mcp(Exp_Con = "Tukey"))))

fiberIIA.r2.fit <- lmer(r2 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = IIA.data.np3)
anova(fiberIIA.r2.fit)
(IIA_r2_emm <- emmeans(fiberIIA.r2.fit, specs = "Exp_Con"))
(IIA_r2_posthoc <- summary(glht(fiberIIA.r2.fit, 
                                linfct = mcp(Exp_Con = "Tukey"))))

fiberIIA.r3.fit <- lmer(r3 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = IIA.data.np3)
anova(fiberIIA.r3.fit)
(IIA_r3_emm <- emmeans(fiberIIA.r3.fit, specs = "Exp_Con"))
(IIA_r3_posthoc <- summary(glht(fiberIIA.r3.fit, 
                                linfct = mcp(Exp_Con = "Tukey"))))

fiberIIA.r4.fit <- lmer(r4 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = IIA.data.np3)
anova(fiberIIA.r4.fit)
(IIA_r4_emm <- emmeans(fiberIIA.r4.fit, specs = "Exp_Con"))
(IIA_r4_posthoc <- summary(glht(fiberIIA.r4.fit,
                                linfct = mcp(Exp_Con = "Tukey"))))

### MHC IIX -----------------------

fiberIIX <- my_data %>% 
  filter(fiber_type == "IIX")
fiberIIX.p3 <- fiberIIX %>% 
  filter(P3_num ==1)

fiberIIX.f0.fit <- lmer(Po_Post_Step ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIX)
anova(fiberIIX.f0.fit)
(IIX_F0_emm <- emmeans(fiberIIX.f0.fit, specs = "Exp_Con"))
(IIX_F0_posthoc <- summary(glht(fiberIIX.f0.fit, 
                               linfct = mcp(Exp_Con = "Tukey"))))

fiberIIX.fsa.fit <- lmer(Fsa ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIX.p3)
anova(fiberIIX.fsa.fit)
(IIX_Fsa_emm <- emmeans(fiberIIX.fsa.fit, specs = "Exp_Con"))
(IIX_Fsa_posthoc <- summary(glht(fiberIIX.fsa.fit, 
                               linfct = mcp(Exp_Con = "Tukey"))))

fiberIIX.ratio.fit <- lmer(FsaF0 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIX.p3)
anova(fiberIIX.ratio.fit)
(IIX_ratio_emm <- emmeans(fiberIIX.ratio.fit, specs = "Exp_Con"))
(IIX_ratio_posthoc <- summary(glht(fiberIIX.ratio.fit, 
                                linfct = mcp(Exp_Con = "Tukey"))))

fiberIIX.FsaTotal.fit <- lmer(Fsa_total ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIX.p3)
anova(fiberIIX.FsaTotal.fit)
(IIX_ratio_emm <- emmeans(fiberIIX.FsaTotal.fit, specs = "Exp_Con"))
(IIX_ratio_posthoc <- summary(glht(fiberIIX.FsaTotal.fit, 
                                   linfct = mcp(Exp_Con = "Tukey"))))

fiberIIX.a2.fit <- lmer(a2 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIX.p3)
anova(fiberIIX.a2.fit)
(IIX_a2_emm <- emmeans(fiberIIX.a2.fit, specs = "Exp_Con"))
# IIX_a2_posthoc <- summary(glht(fiberIIX.a2.fit, 
#                                linfct = mcp(Exp_Con = "Tukey")))

fiberIIX.a3.fit <- lmer(a3 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIX.p3)
anova(fiberIIX.a3.fit)
(IIX_a3_emm <- emmeans(fiberIIX.a3.fit, specs = "Exp_Con"))
# IIX_a3_posthoc <- summary(glht(fiberIIX.a3.fit,
#                                linfct = mcp(Exp_Con = "Tukey")))

fiberIIX.a4.fit <- lmer(a4 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIX.p3)
anova(fiberIIX.a4.fit)
(IIX_a4_emm <- emmeans(fiberIIX.a4.fit, specs = "Exp_Con"))
# IIX_a4_posthoc <- summary(glht(fiberIIX.a4.fit,
#                                linfct = mcp(Exp_Con = "Tukey")))

fiberIIX.r2.fit <- lmer(r2 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIX.p3)
anova(fiberIIX.r2.fit)
(IIX_r2_emm <- emmeans(fiberIIX.r2.fit, specs = "Exp_Con"))
# IIX_r2_posthoc <- summary(glht(fiberIIX.r2.fit, 
#                                linfct = mcp(Exp_Con = "Tukey")))

fiberIIX.r3.fit <- lmer(r3 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIX.p3)
anova(fiberIIX.r3.fit)
(IIX_r3_emm <- emmeans(fiberIIX.r3.fit, specs = "Exp_Con"))
(IIX_r3_posthoc <- summary(glht(fiberIIX.r3.fit, 
                               linfct = mcp(Exp_Con = "Tukey"))))

fiberIIX.t3.fit <- lmer(sa.t3 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIX.p3)
anova(fiberIIX.t3.fit)
(IIX_t3_emm <- emmeans(fiberIIX.t3.fit, specs = "Exp_Con"))
(IIX_t3_posthoc <- summary(glht(fiberIIX.t3.fit, 
                                linfct = mcp(Exp_Con = "Tukey"))))

fiberIIX.r4.fit <- lmer(r4 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIX.p3)
anova(fiberIIX.r4.fit)
(IIX_r4_emm <- emmeans(fiberIIX.r4.fit, specs = "Exp_Con"))
(IIX_r4_posthoc <- summary(glht(fiberIIX.r4.fit,
                               linfct = mcp(Exp_Con = "Tukey"))))

### MHC IIB-----------------------------

fiberIIB <- my_data %>% 
  filter(fiber_type == "IIB")

fiberIIB.f0.fit <- lmer(Po_Post_Step ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIB)
anova(fiberIIB.f0.fit)
(IIB_F0_emm <- emmeans(fiberIIB.f0.fit, specs = "Exp_Con"))
(IIB_F0_posthoc <- summary(glht(fiberIIB.f0.fit, 
                               linfct = mcp(Exp_Con = "Tukey"))))


fiberIIB.Fsa.fit <- lmer(Fsa ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIB)
anova(fiberIIB.Fsa.fit)
(IIB_Fsa_emm <- emmeans(fiberIIB.Fsa.fit, specs = "Exp_Con"))
(IIB_Fsa_posthoc <- summary(glht(fiberIIB.Fsa.fit, 
                               linfct = mcp(Exp_Con = "Tukey"))))

fiberIIB.ratio.fit <- lmer(FsaF0 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIB)
anova(fiberIIB.ratio.fit)
(IIB_ratio_emm <- emmeans(fiberIIB.ratio.fit, specs = "Exp_Con"))
(IIB_ratio_posthoc <- summary(glht(fiberIIB.ratio.fit, 
                                linfct = mcp(Exp_Con = "Tukey"))))

fiberIIB.FsaTotal <- lmer(Fsa_total ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIB)
anova(fiberIIB.FsaTotal)
(IIB_FsaTotal_emm <- emmeans(fiberIIB.FsaTotal, specs = "Exp_Con"))
(IIB_FsaTotal_posthoc <- summary(glht(fiberIIB.FsaTotal, 
                                   linfct = mcp(Exp_Con = "Tukey"))))


fiberIIB.a2.fit <- lmer(a2 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIB)
anova(fiberIIB.a2.fit)
(IIB_a2_emm <- emmeans(fiberIIB.a2.fit, specs = "Exp_Con"))
# IIB_a2_posthoc <- summary(glht(fiberIIB.a2.fit, 
#                                linfct = mcp(Exp_Con = "Tukey")))


fiberIIB.a3.fit <- lmer(a3 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIB)
anova(fiberIIB.a3.fit)
(IIB_a3_emm <- emmeans(fiberIIB.a3.fit, specs = "Exp_Con"))
# IIB_a3_posthoc <- summary(glht(fiberIIB.a3.fit, 
#                                linfct = mcp(Exp_Con = "Tukey")))


fiberIIB.a4.fit <- lmer(a4 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIB)
anova(fiberIIB.a4.fit)
(IIB_a4_emm <- emmeans(fiberIIB.a4.fit, specs = "Exp_Con"))
# IIB_a4_posthoc <- summary(glht(fiberIIB.a4.fit,
#                                linfct = mcp(Exp_Con = "Tukey")))


fiberIIB.r2.fit <- lmer(r2 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIB)
anova(fiberIIB.r2.fit)
(IIB_r2_emm <- emmeans(fiberIIB.r2.fit, specs = "Exp_Con"))
# IIB_r2_posthoc <- summary(glht(fiberIIB.r2.fit, 
#                                linfct = mcp(Exp_Con = "Tukey")))


fiberIIB.r3.fit <- lmer(r3 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIB)
anova(fiberIIB.r3.fit)
(IIB_r3_emm <- emmeans(fiberIIB.r3.fit, specs = "Exp_Con"))
# IIB_r3_posthoc <- summary(glht(fiberIIB.r3.fit, 
#                                linfct = mcp(Exp_Con = "Tukey")))


fiberIIB.t3.fit <- lmer(t3 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIB)
anova(fiberIIB.t3.fit)
(IIB_t3_emm <- emmeans(fiberIIB.t3.fit, specs = "Exp_Con"))
# IIB_r3_posthoc <- summary(glht(fiberIIB.r3.fit, 
#                                linfct = mcp(Exp_Con = "Tukey")))


fiberIIB.r4.fit <- lmer(r4 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIB)
anova(fiberIIB.r4.fit)
(IIB_r4_emm <- emmeans(fiberIIB.r4.fit, specs = "Exp_Con"))
# IIB_r4_posthoc <- summary(glht(fiberIIB.r4.fit,
#                                linfct = mcp(Exp_Con = "Tukey")))


### t3 analysis: Between group differences ----------------------------

fiberI <- my_data %>% filter(fiber_type == "I")
mean(fiberI$sa.t3)
sd(fiberI$sa.t3) / sqrt(length(fiberI$sa.t3))


fiberIIA <- my_data %>% filter(fiber_type == "IIA")
mhcIIA.t3 <- lmer(sa.t3 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIA)
(IIA_t3_emm<- emmeans(mhcIIA.t3, specs = "Exp_Con"))
(IIA_t3_posthoc <- summary(glht(mhcIIA.t3, 
                                linfct = mcp(Exp_Con = "Tukey"))))

fiberIIX <- my_data %>% filter(fiber_type == "IIX")
mhcIIX.t3 <- lmer(sa.t3 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIX)
(IIX_t3_emm<- emmeans(mhcIIX.t3, specs = "Exp_Con"))
anova(mhcIIX.t3)
(IIX_t3_posthoc <- summary(glht(mhcIIX.t3, 
                                linfct = mcp(Exp_Con = "Tukey"))))

fiberIIB <- my_data %>% filter(fiber_type == "IIB")
mhcIIB.t3 <- lmer(sa.t3 ~ Exp_Con + (1 + as.factor(Exp_Con) |Mouse), data = fiberIIB)
(IIB_t3_emm <- emmeans(mhcIIB.t3, specs = "Exp_Con"))
(IIB_t3_posthoc <- summary(glht(mhcIIB.t3, 
                                linfct = mcp(Exp_Con = "Tukey"))))



### t3 analysis: Within group differences -----------------------------------
active.t3 <- my_data %>%
  filter(Exp_Con_Num == 4) %>% 
  mutate(sa.t3 = sa.t3 * 1000)

highfat.t3 <- active.t3 <- my_data %>%
  filter(Exp_Con_Num == 3) %>% 
  mutate(sa.t3 = sa.t3 * 1000)

highfat.t3.lmer <- lmer(sa.t3 ~ fiber_type + (1|Mouse), data =highfat.t3)
anova(highfat.t3.lmer)
(highfat.t3.emm <- emmeans(highfat.t3.lmer, specs = "fiber_type"))
(highfat.t3.posthoc <- summary(glht(highfat.t3.lmer, 
                              linfct = mcp(fiber_type= "Tukey"))))






