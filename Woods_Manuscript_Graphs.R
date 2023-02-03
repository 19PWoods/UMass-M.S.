library(tidyverse)
library(readxl)
library(ggpattern)
theme_set(theme_bw() + theme_classic())

setwd("C:/Users/Phil/Dropbox/Thesis- Stretch Activation/Data/Woods - Master's Thesis/Project/Tension + AaBbCc")

raw_data <- read_excel("SA-Fatigue_Tension+Step+Kinetics_PW_10-28-22.xlsx", 
                       sheet = "NEACSM",
                       skip = 5,
                       na="")%>% 
  filter(P3_num == 1) %>% 
  filter(Exp_Con_Num %in% c(3,5,6)) %>% 
  filter(fiber_type_num %in% c(1:4)) %>% 
  group_by(Exp_Con, fiber_type, fiber_type_num)

my_data <- read_excel("Woods_EMM_10-29-22.xlsx",
                      na = "Included",
                      sheet = "EMM") %>% 
  filter(Include == 1)



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
  geom_point(data = raw_data,
             aes(x = Exp_Con,
                 y = Po_Pre_Step),
             size = 2,
             position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin=EMM - SE,
                    ymax=EMM + SE),
                width=0.4,
                size = 1.5,
                position = position_dodge(width = 0.9)) +
    guides(fill=guide_legend(title = "Fiber Types")) +
    ylab("F0") + 
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
    geom_point(data = raw_data,
               aes(x = Exp_Con,
                   y = Fsa),
               size = 2,
               position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin=EMM - SE,
                      ymax=EMM + SE),
                  width=0.4,
                  size = 1.5,
                  position = position_dodge(width = 0.9)) +
    guides(fill=guide_legend(title = "Fiber Types")) +
    ylab("FSA") + 
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
