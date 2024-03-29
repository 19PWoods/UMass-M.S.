library(tidyverse)
library(readxl)
library(writexl)

setwd("C:/Users/Phil/Dropbox/Thesis- Stretch Activation/Data/Woods - Master's Thesis/Project/Tension + AaBbCc")

my_data <- read_excel("SA-Fatigue_Tension+Step+Kinetics_PW_10-28-22.xlsx", 
                      sheet = "Included",
                      skip = 5,
                      na="") %>% 
  filter(Exp_Con_Num %in% c(2:6)) %>% 
  filter(Ran_Num == 1) %>% 
  filter(fiber_type_num %in% c(1:4)) %>% 
  select(Filename, Muscle,Exp_Con, fiber_type,Po_Pre_Step, Fsa,FsaF0) 

write_xlsx(my_data, path = "Woods_Fat-Ten.xlsx")


df2 <- read_excel("Woods_pCa50-Active_vs_Fatigue.xlsx") %>% 
  mutate(fat_group = case_when(Perc_Active < 35 ~ 'A',
                               Perc_Active >= 35 & Perc_Active <=60 ~ 'B',
                               Perc_Active > 60 & Perc_Active <= 80 ~ 'C',
                               Perc_Active > 80 ~ 'D'),
         .after = FsaF0)

(gg1 <- ggplot(data = df2, aes(x = fat_group, y = Fsa))+
    geom_point()
  
)