library(tidyverse)

my_data = readxl::read_excel(file.choose(),
                             sheet = "Manuscript.2",
                             skip = 5,
                             na="") 

my_data = my_data %>% 
  filter(Exp_Con_Num %in% c(2:4)) %>% 
  filter(fiber_type_num %in% c(1:4)) %>% 
  mutate(iso = ifelse(fiber_type_num == 1,1,2)) %>% 
  filter(iso == 2)

mdl = lm(F0 ~ Fsa + Exp_Con,
   data = my_data)

summary(mdl)

my_data$Exp_Con = relevel(factor(my_data$Exp_Con),
                          "Fat_4.5")
mdl = lm(F0 ~ Fsa + Exp_Con,
         data = my_data)

summary(mdl)
