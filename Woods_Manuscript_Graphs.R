library(tidyverse)
library(readxl)
library(ggpattern)
library(patchwork)
theme_set(theme_bw() + theme_classic())

setwd("C:/Users/Phil/Dropbox/Thesis- Stretch Activation/Data/Woods - Master's Thesis/Project/Tension + AaBbCc")

raw_data_f0 <- read_excel("SA-Fatigue_Tension+Step+Kinetics_PW_10-28-22.xlsx", 
                       sheet = "Included",
                       skip = 5,
                       na="") %>% 

  filter(Exp_Con_Num %in% c(3,5,6)) %>% 
  filter(fiber_type_num %in% c(1:4)) %>% 
  group_by(Exp_Con, fiber_type, fiber_type_num)

raw_data_gg <- read_excel("SA-Fatigue_Tension+Step+Kinetics_PW_10-28-22.xlsx", 
                          sheet = "Included",
                          skip = 5,
                          na="") %>% 
  filter(P3_num == 1) %>% 
  filter(Ran_Num == 1) %>% 
  filter(Exp_Con_Num %in% c(3,5,6)) %>% 
  filter(fiber_type_num %in% c(1:4)) %>% 
  group_by(Exp_Con, fiber_type, fiber_type_num)

my_data <- read_excel("Woods_EMM_10-29-22.xlsx",
                      na = "Included",
                      sheet = "EMM") %>% 
  filter(Include == 1) 



# traces_gg <- ggplot(my_data,
#                     aes(x = Time,
#                         ))

(F0 <- my_data %>% 
  filter(Value == "F0") %>% 
  group_by(Exp_Con, fiber_type, fiber_type_num) %>% 
  ggplot(aes(x = Exp_Con,
             y = EMM,
             group = fiber_type_num)) + 
  geom_bar(aes(fill = fiber_type),
           color = "black",
           stat = "identity",
           position = position_dodge(),
           size = 1) +
  geom_point(data = raw_data_f0,
             aes(x = Exp_Con,
                 y = Po_Pre_Step,
                 shape = Exp_Con),
             size = 2,
             position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin=EMM - SE,
                    ymax=EMM + SE),
                width=0.4,
                size = 1.5,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(label = c("14","32","11","19","14","32","11","19","14","32","11","19"), 
                y = 65),
            vjust = 6,
            size = 7,
            position = position_dodge(width = 0.9)) +
    guides(fill=guide_legend(title = "Fiber Types")) +
    guides(shape = "none") +
    ylab(bquote(F[0])) + 
    scale_shape_manual(values = c(16,15,17)) +
    theme(axis.title.y = element_text(size = 30),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 20),
          axis.text.x = element_text(size = 20),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 18),
          legend.key.size = unit(1,"cm"),
          axis.line = element_line(linewidth = 1),
          axis.ticks = element_line(linewidth = 1)) +
    scale_fill_manual(breaks = c("I","IIA","IIX","IIB"),
                      values = c("#FDFEFE" , "#D0D3D4", "#7B7D7D","#424949")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,350)) +
  scale_x_discrete(breaks = c("Active",
                              "Fat_4.5",
                              "Fat_5.1"),
                   labels = c("Active",
                              expression(atop("High Calcium",
                                              paste("Fatigue"))),
                              expression(atop("Low Calcium",
                                              paste("Fatigue")))))
)

(Fsa <- my_data %>% 
    filter(Value == "Fsa") %>% 
    group_by(Exp_Con, fiber_type, fiber_type_num) %>% 
    ggplot(aes(x = Exp_Con,
               y = EMM,
               group = fiber_type_num)) + 
    geom_bar(aes(fill = fiber_type),
             color = "black",
             stat = "identity",
             position = position_dodge(),
             size = 1) +
    geom_point(data = raw_data_gg,
               aes(x = Exp_Con,
                   y = Fsa,
                   shape = Exp_Con),
               size = 2,
               position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin=EMM - SE,
                      ymax=EMM + SE),
                  width=0.4,
                  size = 1.5,
                  position = position_dodge(width = 0.9)) +
    geom_text(aes(label = c("14","32","11","19",
                            "28","11","19",
                            "8","11","19"), 
                  y = 12),
              vjust = 6,
              size = 7,
              position = position_dodge(width = 0.9)) +
    guides(fill=guide_legend(title = "Fiber Types")) +
    guides(shape = "none") +
    ylab(bquote(F[SA])) +
    scale_shape_manual(values = c(16,15,17)) +
    theme(axis.title.y = element_text(size = 30),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 20),
          axis.text.x = element_blank(),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 18),
          legend.key.size = unit(1,"cm"),
          axis.line = element_line(size = 1),
          axis.ticks = element_line(size = 1)) +
    scale_fill_manual(breaks = c("I","IIA","IIX","IIB"),
                      values = c("#FDFEFE" , "#D0D3D4", "#7B7D7D","#424949")) +
    scale_y_continuous(expand = c(0,0), limits = c(0,75)) +
    scale_x_discrete(breaks = c("Active",
                                "Fat_4.5",
                                "Fat_5.1"),
                     labels = c("Active",
                                expression(atop("High Calcium",
                                                paste("Fatigue"))),
                                expression(atop("Low Calcium",
                                                paste("Fatigue")))))
)


(F0_Fsa <- Fsa/F0 + plot_layout(ncol = 1, heights = c(4,4)))


(FsaF0 <- my_data %>% 
    filter(Value == "Ratio") %>% 
    group_by(Exp_Con, fiber_type, fiber_type_num) %>% 
    ggplot(aes(x = Exp_Con,
               y = EMM,
               group = fiber_type_num)) + 
    geom_bar(aes(fill = fiber_type),
             color = "black",
             stat = "identity",
             position = position_dodge(),
             size = 1) +
    geom_point(data = raw_data_gg,
               aes(x = Exp_Con,
                   y = FsaF0),
               size = 2,
               position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin=EMM - SE,
                      ymax=EMM + SE),
                  width=0.4,
                  size = 1.5,
                  position = position_dodge(width = 0.9)) +
    guides(fill=guide_legend(title = "Fiber Types")) +
    ylab(bquote(F[SA])) + 
    theme(axis.title.y = element_text(size = 30),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 20),
          axis.text.x = element_text(size = 20),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 18),
          legend.key.size = unit(1,"cm"),
          axis.line = element_line(size = 1),
          axis.ticks = element_line(size = 1)) +
    scale_fill_manual(breaks = c("I","IIA","IIX","IIB"),
                      values = c("#FDFEFE" , "#D0D3D4", "#7B7D7D","#424949")) +
    scale_y_continuous(expand = c(0,0), limits = c(0,55)) +
    scale_x_discrete(breaks = c("Active",
                                "Fat_4.5",
                                "Fat_5.1"),
                     labels = c("Active",
                                expression(atop("High Calcium",
                                                paste("Fatigue"))),
                                expression(atop("Low Calcium",
                                                paste("Fatigue")))))
)

active <- raw_data_gg %>% 
  mutate(iso = if(fiber_type_num == 1){
    1 
  } else {
    2
  }) %>% 
  filter(iso == 2) %>% 
  filter(Exp_Con == "Active")

fat_4.5 <- raw_data_gg %>% 
  mutate(iso = if(fiber_type_num == 1){
    1 
  } else {
    2
  }) %>% 
  filter(iso == 2) %>% 
  filter(Exp_Con == "Fat_4.5")

fat_5.1 <- raw_data_gg %>% 
  mutate(iso = if(fiber_type_num == 1){
    1 
  } else {
    2
  }) %>% 
  filter(iso == 2) %>% 
  filter(Exp_Con == "Fat_5.1")


active_lm <- lm(active$Fsa ~ active$Po_Pre_Step)
fat_4.5_lm <- lm(fat_4.5$Fsa ~ fat_4.5$Po_Pre_Step)
fat_5.1_lm <- lm(fat_5.1$Fsa ~ fat_5.1$Po_Pre_Step)

active$mdl <- predict(active_lm)
fat_4.5$mdl <- predict(fat_4.5_lm)
fat_5.1$mdl <- predict(fat_5.1_lm)



(FsavsF0_scatter <- raw_data_gg %>% 
  mutate(iso = if(fiber_type_num == 1){
    1 
  } else {
    2
  }) %>% 
  filter(iso == 2) %>% 
  ggplot(aes(x = Po_Pre_Step,
             y = Fsa)) +
  geom_point(aes(shape = Exp_Con),
             size = 2) +
  geom_line(data = active,
            aes(x = Po_Pre_Step,
                y = mdl),
            size = 1,
            linetype = "solid") +
   geom_line(data = fat_4.5,
              aes(x = Po_Pre_Step,
                  y = mdl),
              size = 1,
              linetype = "longdash")+
  geom_line(data = fat_5.1,
            aes(x = Po_Pre_Step,
                y = mdl),
            size = 1,
            linetype = "dotted")+
  guides(shape=guide_legend("Experimental Condition"))+
  ylab(bquote(F[SA])) + 
  xlab(bquote(F[0])) +
  scale_shape_manual(values = c(1,0,2)) +
  scale_x_continuous(limits = c(0,300)) +
  scale_shape_discrete(labels = c("Active", "High Calcium Fatigue", "Low Calcium Fatigue")) +
  theme(axis.title.y = element_text(size = 30),
          axis.title.x = element_text(size = 30),
          axis.text.y = element_text(size = 20),
          axis.text.x = element_text(size = 20),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 18),
          legend.key.size = unit(1,"cm"),
          legend.spacing.y = unit(1, "cm"),
          axis.line = element_line(size = 1),
          axis.ticks = element_line(size = 1))
  
)
  
  
  
  
  
  
  
  
  



































