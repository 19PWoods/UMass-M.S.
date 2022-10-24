library(tidyverse)
library(readxl)
library(lmerTest)
library(multcomp)
library(emmeans)

setwd("C:/Users/Phil/Dropbox/Thesis- Stretch Activation/Data/Woods - Master's Thesis/Project/Tension + AaBbCc")

my_data <- read_excel("SA-Fatigue_Tension+Step+Kinetics_PW_10-20-22.xlsx", 
                      sheet = "Included",
                      skip = 5,
                      na="") %>% 
  filter(Exp_Con_Num %in% c(2:6)) %>% 
  filter(fiber_type_num %in% c(1:4)) %>% 
  filter(Ran_Num == 1) 

### Within Group Differences (changes within a fiber type) ----------------------------------------------------

### MHC I......

fiberI <- my_data %>% 
  filter(fiber_type == "I")

fiberI.p3 <- fiberI %>% 
  filter(P3_num == 1)

## Repeated measured linear mixed model
fiberI.f0.fit <- lmer(Po_Post_Step ~ Exp_Con + (1|Mouse), data = fiberI)
I_F0_anova <- anova(fiberI.f0.fit)

## Getting estimated marginal means
I_F0_emm <- emmeans(fiberI.f0.fit, specs = "Exp_Con")

## Post-hoc tests (two types)
I_F0_posthoc <- summary(glht(fiberI.f0.fit, 
                             linfct = mcp(Exp_Con = "Tukey")))

# this from example I found online that also used linear model
# I_F0_posthoc <- summary(glht(fiberI.f0.fit, 
#              linfct = mcp(Exp_Con = "Tukey")),
#         test = adjusted(type = "bonferroni"))

# # this from youtube video I found that used emmeans on model (did not show creation of model)
# I_tukey <- contrast(I_F0_emm,
#                     method = "pairwise")
# summary(I_tukey)
# # 
# # I_tukey.2 <- contrast(I_eff, 
# #                     method = "pairwise",
# #                     adjust = "bonferroni")



fiberI.fsa.fit <- lmer(Fsa ~ Exp_Con + (1|Mouse), data = fiberI.p3)
anova(fiberI.fsa.fit)
I_Fsa_emm <- emmeans(fiberI.fsa.fit, specs = "Exp_Con")
I_Fsa_posthoc <- summary(glht(fiberI.fsa.fit, 
                             linfct = mcp(Exp_Con = "Tukey")))

fiberI.ratio.fit <- lmer(FsaF0 ~ Exp_Con + (1|Mouse), data = fiberI.p3)
anova(fiberI.ratio.fit)
I_ratio_emm <- emmeans(fiberI.ratio.fit, specs = "Exp_Con")
I_ratio_posthoc <- summary(glht(fiberI.ratio.fit, 
                              linfct = mcp(Exp_Con = "Tukey")))

fiberI.r2.fit <- lmer(r2 ~ Exp_Con + (1|Mouse), data = fiberI.p3)
anova(fiberI.r2.fit)
I_r2_emm <- emmeans(fiberI.r2.fit, specs = "Exp_Con")
I_r2_posthoc <- summary(glht(fiberI.r2.fit, 
                                linfct = mcp(Exp_Con = "Tukey")))

fiberI.r3.fit <- lmer(r3 ~ Exp_Con + (1|Mouse), data = fiberI.p3)
anova(fiberI.r3.fit)
I_r3_emm <- emmeans(fiberI.r3.fit, specs = "Exp_Con")
I_r3_posthoc <- summary(glht(fiberI.r3.fit, 
                             linfct = mcp(Exp_Con = "Tukey")))

fiberI.r4.fit <- lmer(r4 ~ Exp_Con + (1|Mouse), data = fiberI.p3)
anova(fiberI.r4.fit)
I_r4_emm <- emmeans(fiberI.r4.fit, specs = "Exp_Con")
# I_r4_posthoc <- summary(glht(fiberI.r4.fit, 
#                              linfct = mcp(Exp_Con = "Tukey")))

### MHC IIA .......

fiberIIA <- my_data %>% 
  filter(fiber_type == "IIA")

fiberIIA.p3 <- fiberIIA %>% 
  filter(P3_num == 1)

fiberIIA.f0.fit <- lmer(Po_Post_Step ~ Exp_Con + (1|Mouse), data = fiberIIA)

anova(fiberIIA.f0.fit)
IIA_F0_emm <- emmeans(fiberIIA.f0.fit, specs = "Exp_Con")
IIA_F0_posthoc <- summary(glht(fiberIIA.f0.fit, 
                             linfct = mcp(Exp_Con = "Tukey")))

fiberIIA.fsa.fit <- lmer(Fsa ~ Exp_Con + (1|Mouse), data = fiberIIA.p3)
anova(fiberIIA.fsa.fit)
IIA_Fsa_emm <- emmeans(fiberIIA.fsa.fit, specs = "Exp_Con")
IIA_Fsa_posthoc <- summary(glht(fiberIIA.fsa.fit, 
                               linfct = mcp(Exp_Con = "Tukey")))

fiberIIA.ratio.fit <- lmer(FsaF0 ~ Exp_Con + (1|Mouse), data = fiberIIA.p3)
anova(fiberIIA.ratio.fit)
IIA_ratio_emm <- emmeans(fiberIIA.ratio.fit, specs = "Exp_Con")
IIA_ratio_posthoc <- summary(glht(fiberIIA.ratio.fit, 
                               linfct = mcp(Exp_Con = "Tukey")))

fiberIIA.r2.fit <- lmer(r2 ~ Exp_Con + (1|Mouse), data = fiberIIA.p3)
anova(fiberIIA.r2.fit)
IIA_r2_emm <- emmeans(fiberIIA.r2.fit, specs = "Exp_Con")
IIA_r2_posthoc <- summary(glht(fiberIIA.r2.fit, 
                             linfct = mcp(Exp_Con = "Tukey")))

fiberIIA.r3.fit <- lmer(r3 ~ Exp_Con + (1|Mouse), data = fiberIIA.p3)
anova(fiberIIA.r3.fit)
IIA_r3_emm <- emmeans(fiberIIA.r3.fit, specs = "Exp_Con")
IIA_r3_posthoc <- summary(glht(fiberIIA.r3.fit, 
                             linfct = mcp(Exp_Con = "Tukey")))

fiberIIA.r4.fit <- lmer(r4 ~ Exp_Con + (1|Mouse), data = fiberIIA.p3)
anova(fiberIIA.r4.fit)
IIA_r4_emm <- emmeans(fiberIIA.r4.fit, specs = "Exp_Con")
IIA_r4_posthoc <- summary(glht(fiberIIA.r4.fit,
                             linfct = mcp(Exp_Con = "Tukey")))


### MHC IIX .......

fiberIIX <- my_data %>% 
  filter(fiber_type == "IIX")
fiberIIX.p3 <- fiberIIX %>% 
  filter(P3_num ==1)

fiberIIX.f0.fit <- lmer(Po_Post_Step ~ Exp_Con + (1|Mouse), data = fiberIIX)
anova(fiberIIX.f0.fit)
IIX_F0_emm <- emmeans(fiberIIX.f0.fit, specs = "Exp_Con")
IIX_F0_posthoc <- summary(glht(fiberIIX.f0.fit, 
                               linfct = mcp(Exp_Con = "Tukey")))

fiberIIX.fsa.fit <- lmer(Fsa ~ Exp_Con + (1|Mouse), data = fiberIIX.p3)
anova(fiberIIX.fsa.fit)
IIX_Fsa_emm <- emmeans(fiberIIX.fsa.fit, specs = "Exp_Con")
IIX_Fsa_posthoc <- summary(glht(fiberIIX.fsa.fit, 
                               linfct = mcp(Exp_Con = "Tukey")))

fiberIIX.ratio.fit <- lmer(FsaF0 ~ Exp_Con + (1|Mouse), data = fiberIIX.p3)
anova(fiberIIX.ratio.fit)
IIX_ratio_emm <- emmeans(fiberIIX.ratio.fit, specs = "Exp_Con")
IIX_ratio_posthoc <- summary(glht(fiberIIX.ratio.fit, 
                                linfct = mcp(Exp_Con = "Tukey")))

### MHC IIB .......

fiberIIB <- my_data %>% 
  filter(fiber_type == "IIB")

fiberIIB.f0.fit <- lmer(Po_Post_Step ~ Exp_Con + (1|Mouse), data = fiberIIB)
anova(fiberIIB.f0.fit)
IIB_F0_emm <- emmeans(fiberIIB.f0.fit, specs = "Exp_Con")
IIB_F0_posthoc <- summary(glht(fiberIIB.f0.fit, 
                               linfct = mcp(Exp_Con = "Tukey")))

fiberIIB.Fsa.fit <- lmer(Fsa ~ Exp_Con + (1|Mouse), data = fiberIIB)
anova(fiberIIB.Fsa.fit)
IIB_Fsa_emm <- emmeans(fiberIIB.Fsa.fit, specs = "Exp_Con")
IIB_Fsa_posthoc <- summary(glht(fiberIIB.Fsa.fit, 
                               linfct = mcp(Exp_Con = "Tukey")))

fiberIIB.ratio.fit <- lmer(FsaF0 ~ Exp_Con + (1|Mouse), data = fiberIIB)
anova(fiberIIB.ratio.fit)
IIB_ratio_emm <- emmeans(fiberIIB.ratio.fit, specs = "Exp_Con")
IIB_ratio_posthoc <- summary(glht(fiberIIB.ratio.fit, 
                                linfct = mcp(Exp_Con = "Tukey")))



### Between Group Differences (changes within experimental group) ---------------------------------------------------------------------

## Fatigue pCa 5.2.........................................

fat_pca_5.2 <- my_data %>% 
  filter(Exp_Con_Num == 2)

fat_pca_5.2.p3 <- fat_pca_5.2 %>% 
  filter(P3_num == 1)

f5.2_f0_model <- lmer(Po_Post_Step ~ fiber_type + (1|Mouse), data = fat_pca_5.2)
anova(f5.2_f0_model)
# f5.2_f0_emm <- emmeans(f5.2_f0_model, specs = "fiber_type")
# f5.2_f0_posthoc <- summary(glht(f5.2_f0_model, 
#                                 linfct = mcp(fiber_type = "Tukey")))

f5.2_fsa_model <- lmer(Fsa ~ fiber_type + (1|Mouse), data = fat_pca_5.2.p3)
anova(f5.2_fsa_model)
# f5.2_fsa_emm <- emmeans(f5.2_fsa_model, specs = "fiber_type")
# f5.2_fsa_posthoc <- summary(glht(f5.2_fsa_model,
#                                 linfct = mcp(fiber_type = "Tukey")))

f5.2_ratio_model <- lmer(FsaF0~ fiber_type + (1|Mouse), data = fat_pca_5.2.p3)
anova(f5.2_ratio_model)
f5.2_ratio_emm <- emmeans(f5.2_ratio_model, specs = "fiber_type")
# f5.2_ratio_posthoc <- summary(glht(f5.2_ratio_model,
#                                  linfct = mcp(fiber_type = "Tukey")))


## Fatigue pCa 5.1............................................
fat_pca_5.1 <- my_data %>% 
  filter(Exp_Con_Num == 3)

fat_pca_5.1.p3 <- fat_pca_5.1 %>% 
  filter(P3_num == 1 )

f5.1_f0_model <- lmer(Po_Post_Step ~ fiber_type + (1|Mouse), data = fat_pca_5.1)
anova(f5.1_f0_model)
f5.1_f0_emm <- emmeans(f5.1_f0_model, specs = "fiber_type")
f5.1_f0_posthoc <- summary(glht(f5.1_f0_model,
                                linfct = mcp(fiber_type = "Tukey")))

f5.1_fsa_model <- lmer(Fsa ~ fiber_type + (1|Mouse), data = fat_pca_5.1.p3)
anova(f5.1_fsa_model)
f5.1_fsa_emm <- emmeans(f5.1_fsa_model, specs = "fiber_type")
f5.1_fsa_posthoc <- summary(glht(f5.1_fsa_model,
                                 linfct = mcp(fiber_type = "Tukey")))

f5.1_ratio_model <- lmer(FsaF0~ fiber_type + (1|Mouse), data = fat_pca_5.1.p3)
anova(f5.1_ratio_model)
f5.1_ratio_emm <- emmeans(f5.1_ratio_model, specs = "fiber_type")
f5.1_ratio_posthoc <- summary(glht(f5.1_ratio_model,
                                   linfct = mcp(fiber_type = "Tukey")))

## Fatigue pCa 5.0..........................................
fat_pca_5.0 <- my_data %>% 
  filter(Exp_Con_Num == 4)

fat_pca_5.0.p3 <- fat_pca_5.0 %>% 
  filter(P3_num == 1 )

f5.0_f0_model <- lmer(Po_Post_Step ~ fiber_type + (1|Mouse), data = fat_pca_5.0)
anova(f5.0_f0_model)
f5.0_f0_emm <- emmeans(f5.0_f0_model, specs = "fiber_type")
# f5.0_f0_posthoc <- summary(glht(f5.0_f0_model,
#                                 linfct = mcp(fiber_type = "Tukey")))

f5.0_fsa_model <- lmer(Fsa ~ fiber_type + (1|Mouse), data = fat_pca_5.0.p3)
anova(f5.0_fsa_model)
f5.0_fsa_emm <- emmeans(f5.0_fsa_model, specs = "fiber_type")
# f5.0_fsa_posthoc <- summary(glht(f5.0_fsa_model,
#                                  linfct = mcp(fiber_type = "Tukey")))

f5.0_ratio_model <- lmer(FsaF0~ fiber_type + (1|Mouse), data = fat_pca_5.0.p3)
anova(f5.0_ratio_model)
f5.0_ratio_emm <- emmeans(f5.0_ratio_model, specs = "fiber_type")
f5.0_ratio_posthoc <- summary(glht(f5.0_ratio_model,
                                   linfct = mcp(fiber_type = "Tukey")))


## Fatigue pCa 4.5...........................................
fat_pca_4.5 <- my_data %>% 
  filter(Exp_Con_Num == 5)

fat_pca_4.5.p3 <- fat_pca_4.5 %>% 
  filter(P3_num==1)

f4.5_f0_model <- lmer(Po_Post_Step ~ fiber_type + (1|Mouse), data = fat_pca_4.5)
anova(f4.5_f0_model)
f4.5_f0_emm <- emmeans(f4.5_f0_model, specs = "fiber_type")
# f4.5_f0_posthoc <- summary(glht(f4.5_f0_model,
#                                 linfct = mcp(fiber_type = "Tukey")))

f4.5_fsa_model <- lmer(Fsa ~ fiber_type + (1|Mouse), data = fat_pca_4.5.p3)
anova(f4.5_fsa_model)
f4.5_fsa_emm <- emmeans(f4.5_fsa_model, specs = "fiber_type")
f4.5_fsa_posthoc <- summary(glht(f4.5_fsa_model,
                                 linfct = mcp(fiber_type = "Tukey")))

f4.5_ratio_model <- lmer(FsaF0~ fiber_type + (1|Mouse), data = fat_pca_4.5.p3)
anova(f4.5_ratio_model)
f4.5_ratio_emm <- emmeans(f4.5_ratio_model, specs = "fiber_type")
f4.5_ratio_posthoc <- summary(glht(f4.5_ratio_model,
                                   linfct = mcp(fiber_type = "Tukey")))


## Active............................................
act <- my_data %>% 
  filter(Exp_Con_Num==6)

act_f0_model <- lmer(Po_Post_Step ~ fiber_type + (1|Mouse), data = act)
anova(act_f0_model)
act_f0_emm <- emmeans(act_f0_model, specs = "fiber_type")
# act_f0_posthoc <- summary(glht(act_f0_model,
#                                 linfct = mcp(fiber_type = "Tukey")))

act_fsa_model <- lmer(Fsa ~ fiber_type + (1|Mouse), data = act)
anova(act_fsa_model)
act_fsa_emm <- emmeans(act_fsa_model, specs = "fiber_type")
act_fsa_posthoc <- summary(glht(act_fsa_model,
                                 linfct = mcp(fiber_type = "Tukey")))

act_ratio_model <- lmer(FsaF0~ fiber_type + (1|Mouse), data = act)
anova(act_ratio_model)
act_ratio_emm <- emmeans(act_ratio_model, specs = "fiber_type")
act_ratio_posthoc <- summary(glht(act_ratio_model,
                                   linfct = mcp(fiber_type = "Tukey")))



