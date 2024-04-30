library(tidyverse)

raw_data = readxl::read_excel(file.choose(),
                             sheet = "Manuscript.2",
                             skip = 5,
                             na="") 

my_data = raw_data %>% 
  filter(Exp_Con_Num %in% c(3,4)) %>% 
  filter(fiber_type_num %in% c(1:4)) %>% 
  mutate(iso = ifelse(fiber_type_num == 1,1,2)) %>% 
  filter(iso == 2)

mdl = lm(F0 ~ Exp_Con + Fsa,
   data = my_data)

summary(mdl)

my_data$Exp_Con = relevel(factor(my_data$Exp_Con),
                          "Fat_4.5")
mdl = lm(F0 ~ Exp_Con + Fsa,
         data = my_data)

summary(mdl)
