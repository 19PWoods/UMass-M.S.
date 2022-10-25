## Masters Thesis: Stretch activation and fatigue 
## Tension + Step + Kinetics Analysis
## Philip C. Woods
## Created: 6/27/22
## Last updated: 6/27/22

library(tidyverse)
library(readxl)
library(tcltk)
library(ggpubr)
library(plotly)
library(grid)
theme_set(theme_classic())


### Original loading in of data --------------------------------------------------
setwd("C:/Users/Phil/Dropbox/Thesis- Stretch Activation/Data/Woods - Master's Thesis/Project/Tension + AaBbCc")

my_data <- read_excel("SA-Fatigue_Tension+Step+Kinetics_PW_10-5-22.xlsx", 
                      sheet = "Trimmed",
                      skip = 5,
                      na="")
phil_awesome_data <-
  my_data %>% 
  dplyr::filter(Exp_Con_Num %in% c(2:7)) %>%
  dplyr::filter(fiber_type_num %in% c(1:7)) %>% 
  dplyr::group_by(fiber_type_num, Exp_Con) %>% 
  dplyr::summarize(n = n(),
                   p0_pre_step_avg = mean(Po_Pre_Step, na.rm=T),
                   p0_pre_step_sd = sd(Po_Pre_Step, na.rm=T),
                   p0_pre_step_se = sd(Po_Pre_Step, na.rm=T)/sqrt(n()),
                   fsa_avg = mean(Fsa, na.rm = T),
                   fsa_sd = sd(Fsa, na.rm = T),
                   fsa_se = sd(Fsa, na.rm = T)/sqrt(n()),
                   fsaf0_avg = mean(FsaF0, na.rm = T),
                   fsaf0_sd = sd(FsaF0, na.rm = T),
                   fsaf0_se = sd(FsaF0, na.rm = T)/sqrt(n()),
                   fsatotal_avg = mean(Fsa_total, na.rm = T),
                   fsatotal_sd = sd(Fsa_total, na.rm = T),
                   fsatotal_se = sd(Fsa_total, na.rm = T)/sqrt(n()),
                   a2_avg = mean(a2, na.rm=T),
                   a2_sd = sd(a2, na.rm = T),
                   a2_se = sd(a2, na.rm = T)/sqrt(n()),
                   r2_avg = mean(r2, na.rm = T),
                   r2_sd = sd(r2, na.rm = T),
                   r2_se = sd(r2, na.rm = T)/sqrt(n()),
                   a3_avg = mean(a3, na.rm=T),
                   a3_sd = sd(a3, na.rm = T),
                   a3_se = sd(a3, na.rm = T)/sqrt(n()),
                   r3_avg = mean(r3, na.rm = T),
                   r3_sd = sd(r3, na.rm = T),
                   r3_se = sd(r3, na.rm = T)/sqrt(n()),
                   a4_avg = mean(a4, na.rm=T),
                   a4_sd = sd(a4, na.rm = T),
                   a4_se = sd(a4, na.rm = T)/sqrt(n()),
                   r4_avg = mean(r4, na.rm = T),
                   r4_sd = sd(r4, na.rm = T),
                   r4_se = sd(r4, na.rm = T)/sqrt(n()),
  )


## Graphs - Averages ---------------------------------------------------------------


# Brent example of copy and paste ggplots
# gg1 <-
#   ggplot(phil_awesome_data, 
#        aes(Exp_Con, fsa_avg))+
#   geom_point()+
#   geom_errorbar(aes(ymin=fsa_avg -fsa_se, 
#                     ymax=fsa_avg+fsa_se), 
#                 width=0.25)+
#   facet_wrap(~Muscle)
# 
# 
# ggplotly(gg1)


(f0_gg <- ggplot(data = phil_awesome_data,
                 aes(x = factor(Exp_Con, 
                                levels = c("Fat_5.2", 
                                           "Fat_5.1",
                                           "Fat_5.0",
                                           "Fat_4.5", 
                                           "Active",
                                           "Active_2")),
                     y = p0_pre_step_avg,
                     color = Muscle)) +
   geom_point() +
   
   geom_line(aes(group = Muscle)) +
   
   scale_color_manual(breaks = c("Soleus", "EDL"),
                      values = c("red", "blue")) +
   geom_errorbar(aes(ymin = p0_pre_step_avg - p0_pre_step_se,
                     ymax = p0_pre_step_avg + p0_pre_step_se,
                     width = 0.1)) +
   ylab("F0") +
   xlab("Experimental Conditions")
 
)

(fsa_gg <- ggplot(data = phil_awesome_data,
                  aes(x = factor(Exp_Con, 
                                 levels = c("Fat_5.2", 
                                            "Fat_5.1",
                                            "Fat_5.0",
                                            "Fat_4.5", 
                                            "Active",
                                            "Active_2")),
                      y = fsa_avg,
                      color = Muscle)) +
    geom_point() +
    
    geom_line(aes(group = Muscle)) +
    
    scale_color_manual(breaks = c("Soleus", "EDL"),
                       values = c("red", "blue")) +
    geom_errorbar(aes(ymin = fsa_avg - fsa_se,
                      ymax = fsa_avg + fsa_se,
                      width = 0.1)) +
    ylab("Fsa") +
    xlab("Experimental Conditions")
  
)

(fsaf0_gg <- ggplot(data = phil_awesome_data,
                    aes(x = factor(Exp_Con, 
                                   levels = c("Fat_5.2", 
                                              "Fat_5.1",
                                              "Fat_5.0",
                                              "Fat_4.5", 
                                              "Active",
                                              "Active_2")),
                        y = fsaf0_avg,
                        color = Muscle)) +
    geom_point() +
    
    geom_line(aes(group = Muscle)) +
    
    scale_color_manual(breaks = c("Soleus", "EDL"),
                       values = c("red", "blue")) +
    geom_errorbar(aes(ymin = fsaf0_avg - fsaf0_se,
                      ymax = fsaf0_avg + fsaf0_se,
                      width = 0.1)) +
    ylab("Fsa/F0") +
    xlab("Experimental Conditions")
  
)

(fsatotal_gg <- ggplot(data = phil_awesome_data,
                       aes(x = factor(Exp_Con, 
                                      levels = c("Fat_5.2", 
                                                 "Fat_5.1",
                                                 "Fat_5.0",
                                                 "Fat_4.5", 
                                                 "Active",
                                                 "Active_2")),
                           y = fsatotal_avg,
                           color = Muscle)) +
    geom_point() +
    
    geom_line(aes(group = Muscle)) +
    
    scale_color_manual(breaks = c("Soleus", "EDL"),
                       values = c("red", "blue")) +
    geom_errorbar(aes(ymin = fsatotal_avg - fsatotal_se,
                      ymax = fsatotal_avg + fsatotal_se,
                      width = 0.1)) +
    ylab("Fsa/(Fsa + F0)") +
    xlab("Experimental Conditions")
  
)
(a2_gg <- ggplot(data = phil_awesome_data,
                 aes(x = factor(Exp_Con, 
                                levels = c("Fat_5.2", 
                                           "Fat_5.1",
                                           "Fat_5.0",
                                           "Fat_4.5", 
                                           "Active",
                                           "Active_2")),
                     y = a2_avg,
                     color = Muscle)) +
    geom_point() +
    
    geom_line(aes(group = Muscle)) +
    
    scale_color_manual(breaks = c("Soleus", "EDL"),
                       values = c("red", "blue")) +
    geom_errorbar(aes(ymin = a2_avg - a2_se,
                      ymax = a2_avg + a2_se,
                      width = 0.1)) +
    ylab("a2") +
    xlab("Experimental Conditions")
  
)

(a3_gg <- ggplot(data = phil_awesome_data,
                 aes(x = factor(Exp_Con, 
                                levels = c("Fat_5.2", 
                                           "Fat_5.1",
                                           "Fat_5.0",
                                           "Fat_4.5", 
                                           "Active",
                                           "Active_2")),
                     y = a3_avg,
                     color = Muscle)) +
    geom_point() +
    
    geom_line(aes(group = Muscle)) +
    
    scale_color_manual(breaks = c("Soleus", "EDL"),
                       values = c("red", "blue")) +
    geom_errorbar(aes(ymin = a3_avg - a3_se,
                      ymax = a3_avg + a3_se,
                      width = 0.1)) +
    ylab("a3") +
    xlab("Experimental Conditions")
  
)

(a4_gg <- ggplot(data = phil_awesome_data,
                 aes(x = factor(Exp_Con, 
                                levels = c("Fat_5.2", 
                                           "Fat_5.1",
                                           "Fat_5.0",
                                           "Fat_4.5", 
                                           "Active",
                                           "Active_2")),
                     y = a4_avg,
                     color = Muscle)) +
    geom_point() +
    
    geom_line(aes(group = Muscle)) +
    
    scale_color_manual(breaks = c("Soleus", "EDL"),
                       values = c("red", "blue")) +
    geom_errorbar(aes(ymin = a4_avg - a4_se,
                      ymax = a4_avg + a4_se,
                      width = 0.1)) +
    ylab("a4") +
    xlab("Experimental Conditions")
  
)

(r2_gg <- ggplot(data = phil_awesome_data,
                 aes(x = factor(Exp_Con, 
                                levels = c("Fat_5.2", 
                                           "Fat_5.1",
                                           "Fat_5.0",
                                           "Fat_4.5", 
                                           "Active",
                                           "Active_2")),
                     y = r2_avg,
                     color = Muscle)) +
    geom_point() +
    
    geom_line(aes(group = Muscle)) +
    
    scale_color_manual(breaks = c("Soleus", "EDL"),
                       values = c("red", "blue")) +
    geom_errorbar(aes(ymin = r2_avg - r2_se,
                      ymax = r2_avg + r2_se,
                      width = 0.1)) +
    ylab("r2") +
    xlab("Experimental Conditions")
  
)

(r3_gg <- ggplot(data = phil_awesome_data,
                 aes(x = factor(Exp_Con, 
                                levels = c("Fat_5.2", 
                                           "Fat_5.1",
                                           "Fat_5.0",
                                           "Fat_4.5", 
                                           "Active",
                                           "Active_2")),
                     y = r3_avg,
                     color = Muscle)) +
    geom_point() +
    
    geom_line(aes(group = Muscle)) +
    
    scale_color_manual(breaks = c("Soleus", "EDL"),
                       values = c("red", "blue")) +
    geom_errorbar(aes(ymin = r3_avg - r3_se,
                      ymax = r3_avg + r3_se,
                      width = 0.1)) +
    ylab("r3") +
    xlab("Experimental Conditions")
  
)

(r4_gg <- ggplot(data = phil_awesome_data,
                 aes(x = factor(Exp_Con, 
                                levels = c("Fat_5.2", 
                                           "Fat_5.1",
                                           "Fat_5.0",
                                           "Fat_4.5", 
                                           "Active",
                                           "Active_2")),
                     y = r4_avg,
                     color = Muscle)) +
    geom_point() +
    
    geom_line(aes(group = Muscle)) +
    
    scale_color_manual(breaks = c("Soleus", "EDL"),
                       values = c("red", "blue")) +
    geom_errorbar(aes(ymin = r4_avg - r4_se,
                      ymax = r4_avg + r4_se,
                      width = 0.1)) +
    ylab("r4") +
    xlab("Experimental Conditions")
  
)

plot_all_sa <- ggarrange(f0_gg, 
                         fsa_gg, 
                         fsaf0_gg, 
                         fsatotal_gg,
                         ncol = 2,
                         nrow = 2)

plot_all_amps <- ggarrange(a2_gg,
                           a3_gg,
                           a4_gg,
                           ncol = 1,
                           nrow = 3)

plot_all_rates <- ggarrange(r2_gg,
                            r3_gg,
                            r4_gg,
                            ncol = 1,
                            nrow = 3)
ggexport(list(plot_all_sa,plot_all_amps,plot_all_rates), filename = "Woods_Thesis_Graphs_Averages.pdf")


## Graphs - Individual Fibers -------------------------------------------------

my_data2 <- my_data %>% 
  filter(Exp_Con_Num %in% c(2:7))

(f0_plot_fibers <- ggplot(data = my_data2, 
                          aes(x = factor(Exp_Con,
                                         levels = unique(Exp_Con)),
                              y = Po_Pre_Step,
                              color = factor(Mouse))) + #w/out character, fiber_num is considered num
    geom_point() +
    geom_line(aes(group = Filename)) +
    xlab("Experiment Conditions") +
    ylab("F0") +
    facet_wrap( ~ Muscle) 
)

(fsa_plot_fibers <- ggplot(data = my_data2, 
                           aes(x = factor(Exp_Con,
                                          levels = unique(Exp_Con)),
                               y = Fsa,
                               color = factor(Mouse))) + #w/out character, fiber_num is considered num
    geom_point() +
    geom_line(aes(group = Filename)) +
    xlab("Experiment Conditions") +
    ylab("Fsa") +
    facet_wrap( ~ Muscle)
)

(fsaf0_plot_fibers <- ggplot(data = my_data2, 
                             aes(x = factor(Exp_Con,
                                            levels = unique(Exp_Con)),
                                 y = FsaF0,
                                 color = factor(Mouse))) + #w/out character, fiber_num is considered num
    geom_point() +
    geom_line(aes(group = Filename)) +
    xlab("Experiment Conditions") +
    ylab("Fsa/F0") +
    facet_wrap( ~ Muscle)
)


(fsatotal_plot_fibers <- ggplot(data = my_data2, 
                                aes(x = factor(Exp_Con,
                                               levels = unique(Exp_Con)),
                                    y = Fsa_total,
                                    color = factor(Mouse))) + #w/out character, fiber_num is considered num
    geom_point() +
    geom_line(aes(group = Filename)) +
    xlab("Experiment Conditions") +
    ylab("Fsa/(Fsa + F0)") +
    facet_wrap( ~ Muscle)
)

(a2_plot_fibers <- ggplot(data = my_data2, 
                          aes(x = factor(Exp_Con,
                                         levels = unique(Exp_Con)),
                              y = a2,
                              color = factor(Mouse))) + #w/out character, fiber_num is considered num
    geom_point() +
    geom_line(aes(group = Filename)) +
    xlab("Experiment Conditions") +
    ylab("a2") +
    facet_wrap( ~ Muscle)
)

(a3_plot_fibers <- ggplot(data = my_data2, 
                          aes(x = factor(Exp_Con,
                                         levels = unique(Exp_Con)),
                              y = a3,
                              color = factor(Mouse))) + #w/out character, fiber_num is considered num
    geom_point() +
    geom_line(aes(group = Filename)) +
    xlab("Experiment Conditions") +
    ylab("a3") +
    facet_wrap( ~ Muscle)
)

(a4_plot_fibers <- ggplot(data = my_data2, 
                          aes(x = factor(Exp_Con,
                                         levels = unique(Exp_Con)),
                              y = a4,
                              color = factor(Mouse))) + #w/out character, fiber_num is considered num
    geom_point() +
    geom_line(aes(group = Filename)) +
    xlab("Experiment Conditions") +
    ylab("a4") +
    facet_wrap( ~ Muscle)
)

(r2_plot_fibers <- ggplot(data = my_data2, 
                          aes(x = factor(Exp_Con,
                                         levels = unique(Exp_Con)),
                              y = r2,
                              color = factor(Mouse))) + #w/out character, fiber_num is considered num
    geom_point() +
    geom_line(aes(group = Filename)) +
    xlab("Experiment Conditions") +
    ylab("r2") +
    facet_wrap( ~ Muscle)
)

(r3_plot_fibers <- ggplot(data = my_data2, 
                          aes(x = factor(Exp_Con,
                                         levels = unique(Exp_Con)),
                              y = r3,
                              color = factor(Mouse))) + #w/out character, fiber_num is considered num
    geom_point() +
    geom_line(aes(group = Filename)) +
    xlab("Experiment Conditions") +
    ylab("r3") +
    facet_wrap( ~ Muscle)
)

(r4_plot_fibers <- ggplot(data = my_data2, 
                          aes(x = factor(Exp_Con,
                                         levels = unique(Exp_Con)),
                              y = r4,
                              color = factor(Mouse))) + #w/out character, fiber_num is considered num
    geom_point() +
    geom_line(aes(group = Filename)) +
    xlab("Experiment Conditions") +
    ylab("r4") +
    facet_wrap( ~ Muscle)
)

plot_fibers_sa <- ggarrange(f0_plot_fibers, 
                            fsa_plot_fibers, 
                            fsaf0_plot_fibers,
                            fsatotal_plot_fibers,
                            
                            ncol = 1,
                            nrow = 4)

plot_fibers_amps <- ggarrange(a2_plot_fibers,
                              a3_plot_fibers,
                              a4_plot_fibers,
                              ncol = 1,
                              nrow = 3)

plot_fibers_rates <- ggarrange(r2_plot_fibers,
                               r3_plot_fibers,
                               r4_plot_fibers,
                               ncol = 1,
                               nrow = 3)

ggexport(list(plot_fibers_sa, plot_fibers_amps, plot_fibers_rates), 
         filename = "Woods_Thesis_Graphs_IndividualFibers.pdf")
### Positive Phase 3 Graphs-------------------------------------------------------------------------

my_data2 <- read_excel("SA-Fatigue_Tension+Step+Kinetics_PW_10-2-22.xlsx", 
                       sheet = "Included",
                       skip = 5,
                       na="")
df2 <- my_data2 %>% 
  filter(Exp_Con_Num %in% c(2:6)) %>% 
  filter(fiber_type_num %in% c(1:4,6,7)) %>% 
  group_by(fiber_type, Exp_Con) %>% 
  summarize(phase3 = plyr::count(P3, vars = 1))

(df2_gg <- df2 %>% 
    group_by(fiber_type) %>% 
    filter(phase3$x == "Yes") %>% 
    ggplot(aes(x = Exp_Con, y = phase3$freq, fill = fiber_type)) +
    geom_bar(stat = 'identity',
             position = position_dodge()) +
    geom_text(aes(label = phase3$freq), 
              position = position_dodge(width = 0.9))  
)

ggsave("Woods_Positive_Phase3.jpeg", df2_gg, width = 12, height = 10, units = "in", dpi = 300)


### NEACSM: F0, Fsa, Ratio scatter plots ----------------------------------------------------------------------


setwd("C:/Users/Phil/Dropbox/Thesis- Stretch Activation/Data/Woods - Master's Thesis/Project/Tension + AaBbCc")

my_data <- read_excel("SA-Fatigue_Tension+Step+Kinetics_PW_10-5-22.xlsx", 
                      sheet = "NEACSM",
                      skip = 5,
                      na="")


acsm_data <- my_data %>%
  filter(Exp_Con_Num %in% c(3,5,6)) %>%
  filter(fiber_type_num %in% (1:4)) %>%
  group_by(Exp_Con, fiber_type, fiber_type_num) %>%
  summarize(n = n(),
            f0_avg = mean(Po_Pre_Step, na.rm=T),
            f0_sd = sd(Po_Pre_Step, na.rm=T),
            f0_se = sd(Po_Pre_Step, na.rm=T)/sqrt(n()),
            fsa_avg = mean(Fsa, na.rm = T),
            fsa_sd = sd(Fsa, na.rm = T),
            fsa_se = sd(Fsa, na.rm = T)/sqrt(n()),
            fsaf0_avg = mean(FsaF0, na.rm = T),
            fsaf0_sd = sd(FsaF0, na.rm = T),
            fsaf0_se = sd(FsaF0, na.rm = T)/sqrt(n()),
            fsatotal_avg = mean(Fsa_total, na.rm = T),
            fsatotal_sd = sd(Fsa_total, na.rm = T),
            fsatotal_se = sd(Fsa_total, na.rm = T)/sqrt(n())
  )

## MHC IIX only
(gg1 <- acsm_data %>% 
    filter(fiber_type == "IIX") %>% 
    ggplot(aes(Exp_Con, f0_avg)) +
    geom_point(aes(col = fiber_type),
               size = 2.5) +
    geom_errorbar(aes(ymin=f0_avg-f0_se,
                      ymax=f0_avg+f0_se,
                      col = fiber_type),
                  width=0.1,
                  size = 1) +
    scale_y_continuous(limits = c(0,200)) +
    ylab("Calcium-activated Specific Tension (mN/mm^2)") +
    guides(color=guide_legend(title = "Fiber Types")) +
    theme(axis.title.y = element_text(size = 18),
          axis.title.x = element_blank(),
          axis.text = element_text(size = 15),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 13),
          legend.key.size = unit(1,"cm")) +
    scale_color_manual(breaks = c("IIX"),
                       values = c("Purple"))
)


(gg1.2 <- acsm_data %>% 
    filter(fiber_type == "IIX") %>% 
    ggplot(aes(Exp_Con, fsa_avg)) +
    geom_point(aes(col = fiber_type),
               size = 2.5) +
    geom_errorbar(aes(ymin=fsa_avg-fsa_se,
                      ymax=fsa_avg+fsa_se,
                      col = fiber_type),
                  width=0.1,
                  size=1) +
    ylab("Stretch-activated Specific Tension (mN/mm^2)") +
    xlab("Experimental Condtions") +
    guides(color=guide_legend(title = "Fiber Types")) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = 18),
          axis.text.y = element_text(size = 15),
          axis.text.x = element_blank(),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 13),
          legend.key.size = unit(1,"cm")) +
    scale_color_manual(breaks = c("IIX"),
                       values = c("Purple"))
)

(gg1.3 <- acsm_data %>% 
    filter(fiber_type == "IIX") %>% 
    ggplot(aes(Exp_Con, fsatotal_avg)) +
    geom_point(aes(col = fiber_type),
               size = 2.5) +
    geom_errorbar(aes(ymin=fsatotal_avg-fsatotal_sd,
                      ymax=fsatotal_avg+fsatotal_sd,
                      col = fiber_type),
                  width=0.1,
                  size = 1) +
    ylab("Stretch- to Calcium-activated Specific Tension (%)") +
    xlab("Experimental Condtions") +
    guides(color=guide_legend(title = "Fiber Types")) +
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 15),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 13),
          legend.key.size = unit(1,"cm")) +
    scale_color_manual(breaks = c("IIX"),
                       values = c("Purple"))
)

## MHC IIX & IIB 
(gg2 <- acsm_data %>% 
    filter(fiber_type == "IIX" | fiber_type == "IIB") %>%  
    ggplot(aes(Exp_Con, f0_avg)) +
    geom_point(aes(col = fiber_type),
               size = 2.5) +
    geom_errorbar(aes(ymin=f0_avg-f0_se,
                      ymax=f0_avg+f0_se,
                      col = fiber_type),
                  width=0.1,
                  size = 1) +
    scale_y_continuous(breaks = seq(0,200, by = 50)) +
    ylab("Calcium-activated Specific Tension (mN/mm^2)") +
    xlab("Experimental Conditions") +
    guides(color=guide_legend(title = "Fiber Types")) +
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 15),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 13),
          legend.key.size = unit(1,"cm")) +
    scale_color_manual(breaks = c("IIX", "IIB"),
                       values = c("Purple", "Red"))
)

(gg2.2 <- acsm_data %>% 
    filter(fiber_type == "IIX" | fiber_type == "IIB") %>% 
    ggplot(aes(Exp_Con, fsa_avg)) +
    geom_point(aes(col = fiber_type),
               size=2.5) +
    geom_errorbar(aes(ymin=fsa_avg-fsa_se,
                      ymax=fsa_avg+fsa_se,
                      col = fiber_type),
                  width=0.1,
                  size = 1) +
    ylab("Stretch-activated Specific Tension (mN/mm^2)") +
    xlab("Experimental Condtions") +
    guides(color=guide_legend(title = "Fiber Types")) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = 18),
          axis.text.y = element_text(size = 15),
          axis.text.x = element_blank(),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 13),
          legend.key.size = unit(1,"cm")) +
    scale_color_manual(breaks = c("IIX", "IIB"),
                       values = c("Purple", "Red"))
)

(gg2.3 <- acsm_data %>% 
    filter(fiber_type == "IIX" | fiber_type == "IIB") %>% 
    ggplot(aes(Exp_Con, fsatotal_avg)) +
    geom_point(aes(col = fiber_type),
               size = 2.5) +
    geom_errorbar(aes(ymin=fsatotal_avg-fsatotal_sd,
                      ymax=fsatotal_avg+fsatotal_sd,
                      col = fiber_type),
                  width=0.1,
                  size = 1) +
    ylab("Stretch- to Calcium-actived Specific Tension (%)") +
    xlab("Experimental Condtions") +
    guides(color=guide_legend(title = "Fiber Types")) +
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 15),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 13),
          legend.key.size = unit(1,"cm")) +
    scale_color_manual(breaks = c("IIX", "IIB"),
                       values = c("Purple", "Red"))
)

## MHC IIA
(gg3 <- acsm_data %>% 
    filter(fiber_type == "IIX" | fiber_type == "IIB" | fiber_type == "IIA") %>%
    ggplot(aes(Exp_Con, f0_avg)) +
    geom_point(aes(col = fiber_type),
               size = 2.5) +
    geom_errorbar(aes(ymin=f0_avg-f0_se,
                      ymax=f0_avg+f0_se,
                      col = fiber_type),
                  width=0.1,
                  size=1) +
    scale_y_continuous(breaks = seq(0,200, by = 50)) +
    ylab("Calcium-activated Specific Tension (mN/mm^2)") +
    xlab("Experimental Conditions") +
    guides(color=guide_legend(title = "Fiber Types")) +
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 15),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 13),
          legend.key.size = unit(1,"cm")) +
    scale_color_manual(breaks = c("IIA", "IIX", "IIB"),
                       values = c("Green", "Purple", "Red"))
)

(gg3.2 <- acsm_data %>% 
    filter(fiber_type == "IIX" | fiber_type == "IIB" | fiber_type == "IIA") %>% 
    ggplot(aes(Exp_Con, fsa_avg)) +
    geom_point(aes(col = fiber_type),
               size=2.5) +
    geom_errorbar(aes(ymin=fsa_avg-fsa_se,
                      ymax=fsa_avg+fsa_se,
                      col = fiber_type),
                  width=0.1,
                  size=1) +
    ylab("Stretch-activated Specific Tension (mN/mm^2)") +
    xlab("Experimental Condtions") +
    guides(color=guide_legend(title = "Fiber Types")) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = 18),
          axis.text.y = element_text(size = 15),
          axis.text.x = element_blank(),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 13),
          legend.key.size = unit(1,"cm")) +
    scale_color_manual(breaks = c("IIA", "IIX", "IIB"),
                       values = c("Green", "Purple", "Red"))
)

(gg3.3 <- acsm_data %>% 
    filter(fiber_type == "IIX" | fiber_type == "IIB" | fiber_type == "IIA") %>% 
    ggplot(aes(Exp_Con, fsatotal_avg)) +
    geom_point(aes(col = fiber_type),
               size=2.5) +
    geom_errorbar(aes(ymin=fsatotal_avg-fsatotal_sd,
                      ymax=fsatotal_avg+fsatotal_sd,
                      col = fiber_type),
                  width=0.1,
                  size=1) +
    ylab("Stretch- to Calcium-activated Specific Tension (%)") +
    xlab("Experimental Condtions") +
    guides(color=guide_legend(title = "Fiber Types")) +
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 15),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 13),
          legend.key.size = unit(1,"cm")) +
    scale_color_manual(breaks = c("IIA", "IIX", "IIB"),
                       values = c("Green", "Purple", "Red"))
)

## All MHC 

(gg4 <- acsm_data %>% 
    ggplot(aes(Exp_Con, f0_avg)) +
    geom_point(aes(col = fiber_type),
               size=2.5) +
    geom_errorbar(aes(ymin=f0_avg-f0_se,
                      ymax=f0_avg+f0_se,
                      col = fiber_type),
                  width=0.1,
                  size=1) +
    scale_y_continuous(breaks = seq(0,200, by = 50)) +
    ylab("Calcium-activated Specific Tension (mN/mm^2)") +
    xlab("Experimental Conditions") +
    guides(color=guide_legend(title = "Fiber Types")) +
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 15),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 13),
          legend.key.size = unit(1,"cm")) +
    scale_color_manual(breaks = c("I", "IIA", "IIX", "IIB"),
                       values = c("Orange", "Green", "Purple", "Red"))
)

(gg4.2 <- acsm_data %>% 
    ggplot(aes(Exp_Con, fsa_avg)) +
    geom_point(aes(col = fiber_type),
               size=2.5) +
    geom_errorbar(aes(ymin=fsa_avg-fsa_se,
                      ymax=fsa_avg+fsa_se,
                      col = fiber_type),
                  width=0.1,
                  size=1) +
    ylab("Stretch-activated Specific Tension (mN/mm^2)") +
    xlab("Experimental Condtions") +
    guides(color=guide_legend(title = "Fiber Types")) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = 18),
          axis.text.y = element_text(size = 15),
          axis.text.x = element_blank(),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 13),
          legend.key.size = unit(1,"cm")) +
    scale_color_manual(breaks = c("I", "IIA", "IIX", "IIB"),
                       values = c("Orange", "Green", "Purple", "Red"))
)

(gg4.3 <- acsm_data %>%
    ggplot(aes(Exp_Con, fsatotal_avg)) +
    geom_point(aes(col = fiber_type),
               size=2.5) +
    geom_errorbar(aes(ymin=fsatotal_avg-fsatotal_sd,
                      ymax=fsatotal_avg+fsatotal_sd,
                      col = fiber_type),
                  width=0.1,
                  size=1) +
    ylab(expression(atop("Stretch- to Calcium-Activated",
                         paste("Specific Tension (%)"))))+
    guides(color=guide_legend(title = "Fiber Types")) +
    scale_y_continuous(limits = c(0,33.5)) +
    theme(axis.title = element_text(size = 30),
          axis.text = element_text(size = 25),
          axis.title.x = element_blank(),
          legend.title = element_text(size = 25),
          legend.text = element_text(size = 20),
          legend.key.size = unit(1,"cm")) +
    scale_color_manual(breaks = c("I",
                                  "IIA",
                                  "IIX",
                                  "IIB"),
                       values = c("#E69F00",
                                  "#56B4E9",
                                  "#CC79A7",
                                  "#009E73"))+
    scale_x_discrete(breaks = c("Active",
                                "Fat_4.5",
                                "Fat_5.1"),
                     labels = c("Active",
                                "High Calcium Fatigue",
                                "Low Calcium Fatigue"))
)



## Bar Plots



# grid.newpage()
# p1<- grid.draw(rbind(ggplotGrob(gg1.2), 
#                      ggplotGrob(gg1), 
#                      size = "last"))
# p2 <- grid.draw(rbind(ggplotGrob(gg2.2),
#                       ggplotGrob(gg2), 
#                       size = "last"))
# p3 <- grid.draw(rbind(ggplotGrob(gg3.2),
#                       ggplotGrob(gg3), 
#                       size = "last"))
# p4 <- grid.draw(rbind(ggplotGrob(gg4.2),
#                       ggplotGrob(gg4), 
#                       size = "last"))

ggexport(gg1.1111, filename = "Woods_ACSM_F0_MHCIIX.png",
         width = 100,height = 100, units = "in", dpi = 300)
ggexport(gg1.2, filename = "Woods_ACSM_Fsa_MHCIIX.png")
ggexport(gg1.3, filename = "Woods_ACSM_Ratio_MHCIIX.png")

ggexport(gg2, filename = "Woods_ACSM_F0_MHCIIX+B.png")
ggexport(gg2.2, filename = "Woods_ACSM_Fsa_MHCIIX+B.png")
ggexport(gg2.3, filename = "Woods_ACSM_Ratio_MHCIIX+B.png")

ggexport(gg3, filename = "Woods_ACSM_F0_MHCIIXBA.png")
ggexport(gg3.2, filename = "Woods_ACSM_Fsa_MHCIIXBA.png")
ggexport(gg3.3, filename = "Woods_ACSM_Ratio_MHCIIXBA.png")

ggexport(gg4, filename = "Woods_ACSM_F0_All.png")
ggexport(gg4.2, filename = "Woods_ACSM_Fsa_All.png")
ggexport(gg4.3, filename = "Woods_ACSM_Ratio_All.png")


### NEACSM: F0, Fsa, Ratio bar plots ----------------------------------------------

setwd("C:/Users/Phil/Dropbox/Thesis- Stretch Activation/Data/Woods - Master's Thesis/Project/Tension + AaBbCc")

my_data <- read_excel("SA-Fatigue_Tension+Step+Kinetics_PW_10-5-22.xlsx", 
                      sheet = "NEACSM",
                      skip = 5,
                      na="")


acsm_data <- my_data %>%
  filter(Exp_Con_Num %in% c(3,5,6)) %>%
  filter(fiber_type_num %in% (1:4)) %>%
  group_by(Exp_Con, fiber_type, fiber_type_num) %>%
  summarize(n = n(),
            f0_avg = mean(Po_Pre_Step, na.rm=T),
            f0_sd = sd(Po_Pre_Step, na.rm=T),
            f0_se = sd(Po_Pre_Step, na.rm=T)/sqrt(n()),
            fsa_avg = mean(Fsa, na.rm = T),
            fsa_sd = sd(Fsa, na.rm = T),
            fsa_se = sd(Fsa, na.rm = T)/sqrt(n()),
            fsaf0_avg = mean(FsaF0, na.rm = T),
            fsaf0_sd = sd(FsaF0, na.rm = T),
            fsaf0_se = sd(FsaF0, na.rm = T)/sqrt(n()),
            fsatotal_avg = mean(Fsa_total, na.rm = T),
            fsatotal_sd = sd(Fsa_total, na.rm = T),
            fsatotal_se = sd(Fsa_total, na.rm = T)/sqrt(n())
  )


acsm_data2 <-  my_data %>% 
  filter(Exp_Con_Num %in% c(3,5,6)) %>% 
  filter(fiber_type_num %in% c(1:4)) %>% 
  group_by(Exp_Con, fiber_type)


## MHC IIX only
x <- acsm_data2 %>% 
  filter(fiber_type == "IIX")

(gg1 <- acsm_data %>% 
    filter(fiber_type == "IIX") %>% 
    ggplot(aes(Exp_Con, f0_avg)) +
    geom_bar(aes(fill = fiber_type),
             stat = "identity") +
    geom_point(data = x,
               aes(x = Exp_Con,
                   y = Po_Pre_Step))+
    geom_errorbar(aes(ymin=f0_avg-f0_se,
                      ymax=f0_avg+f0_se),
                  width=0.1,
                  size = 1) +
    scale_y_continuous(limits = c(0,275)) +
    ylab(expression(atop("Calcium-activated",
                         paste("Specific Tension (mN/mm^2)"))))+
    guides(fill=guide_legend(title = "Fiber Types")) + 
    theme(axis.title.y = element_text(size = 30),
          axis.title.x = element_blank(),
          axis.text = element_text(size = 25),
          legend.title = element_text(size = 25),
          legend.text = element_text(size = 20),
          legend.key.size = unit(1,"cm")) +
    scale_fill_manual(breaks = c("IIX"),
                      values = c("#CC79A7")) +
    scale_x_discrete(breaks = c("Active",
                                "Fat_4.5",
                                "Fat_5.1"),
                     labels = c("Active",
                                expression(atop("High Calcium",
                                                paste("Fatigue"))),
                                expression(atop("Low Calcium",
                                                paste("Fatigue")))))
)

(gg1.2 <- acsm_data %>% 
    filter(fiber_type == "IIX") %>% 
    ggplot(aes(Exp_Con, fsa_avg)) +
    geom_bar(aes(fill = fiber_type),
             stat = "identity") +
    geom_point(data = x,
               aes(x = Exp_Con,
                   y = Fsa))+
    geom_errorbar(aes(ymin=fsa_avg-fsa_se,
                      ymax=fsa_avg+fsa_se),
                  width=0.1,
                  size = 1) +
    scale_y_continuous(limits = c(0,70)) +
    ylab(expression(atop("Stretch-activated",
                         paste("Specific Tension (mN/mm^2)"))))+
    guides(fill=guide_legend(title = "Fiber Types")) + 
    theme(axis.title.y = element_text(size = 30),
          axis.title.x = element_blank(),
          axis.text = element_text(size = 25),
          legend.title = element_text(size = 25),
          legend.text = element_text(size = 20),
          legend.key.size = unit(1,"cm")) +
    scale_fill_manual(breaks = c("IIX"),
                      values = c("#CC79A7")) +
    scale_x_discrete(breaks = c("Active",
                                "Fat_4.5",
                                "Fat_5.1"),
                     labels = c("Active",
                                expression(atop("High Calcium",
                                                paste("Fatigue"))),
                                expression(atop("Low Calcium",
                                                paste("Fatigue")))))
)
(gg1.3 <- acsm_data %>% 
    filter(fiber_type == "IIX") %>%
    ggplot(aes(Exp_Con, 
               fsatotal_avg)) +
    geom_bar(aes(fill = fiber_type),
             stat = "identity")+
    geom_point(data = x,
               aes(x = Exp_Con,
                   y = Fsa_total)) +
    geom_errorbar(aes(ymin=fsatotal_avg-fsatotal_se,
                      ymax=fsatotal_avg+fsatotal_se),
                  position = position_dodge(width = 0.9),
                  width=0.15,
                  size = 1.25) +
    scale_y_continuous(limits = c(0,37)) +
    ylab(expression(atop("Stretch-to-Calcium-activated",
                         paste("Specific Tension (%)"))))+
    guides(fill=guide_legend(title = "Fiber Types")) + 
    theme(axis.title.y = element_text(size = 30),
          axis.title.x = element_blank(),
          axis.text = element_text(size = 25),
          legend.title = element_text(size = 25),
          legend.text = element_text(size = 20),
          legend.key.size = unit(1,"cm")) +
    scale_fill_manual(breaks = c("I",
                                 "IIA",
                                 "IIX",
                                 "IIB"),
                      values = c("#E69F00",
                                 "#56B4E9",
                                 "#CC79A7",
                                 "#009E73")) +
    scale_x_discrete(breaks = c("Active",
                                "Fat_4.5",
                                "Fat_5.1"),
                     labels = c("Active",
                                expression(atop("High Calcium",
                                                paste("Fatigue"))),
                                expression(atop("Low Calcium",
                                                paste("Fatigue")))))
)

## MHC IIX,IIB
y <- acsm_data2 %>% 
  filter(fiber_type == "IIX" | fiber_type == "IIB") 

(gg2 <- acsm_data %>% 
    filter(fiber_type == "IIX" | fiber_type == "IIB") %>% 
    ggplot(aes(Exp_Con, 
               f0_avg, 
               group = fiber_type_num)) +
    geom_bar(aes(fill = fiber_type),
             stat = "identity",
             position = position_dodge()) +
    geom_point(data = y,
               aes(x = Exp_Con,
                   y = Po_Pre_Step),
               position = position_dodge(width = 0.9))+
    geom_errorbar(aes(ymin=f0_avg-f0_se,
                      ymax=f0_avg+f0_se),
                  width=0.1,
                  size = 1,
                  position = position_dodge(width = 0.9)) +
    scale_y_continuous(limits = c(0,275)) +
    ylab(expression(atop("Calcium-activated",
                         paste("Specific Tension (mN/mm^2)"))))+
    guides(fill=guide_legend(title = "Fiber Types")) + 
    theme(axis.title.y = element_text(size = 30),
          axis.title.x = element_blank(),
          axis.text = element_text(size = 25),
          legend.title = element_text(size = 25),
          legend.text = element_text(size = 20),
          legend.key.size = unit(1,"cm")) +
    scale_fill_manual(breaks = c("IIX","IIB"),
                      values = c("#CC79A7","#009E73")) +
    scale_x_discrete(breaks = c("Active",
                                "Fat_4.5",
                                "Fat_5.1"),
                     labels = c("Active",
                                expression(atop("High Calcium",
                                                paste("Fatigue"))),
                                expression(atop("Low Calcium",
                                                paste("Fatigue")))))
)

(gg2.2 <- acsm_data %>% 
    filter(fiber_type == "IIX" | fiber_type == "IIB") %>% 
    ggplot(aes(Exp_Con, 
               fsa_avg, 
               group = fiber_type_num)) +
    geom_bar(aes(fill = fiber_type),
             stat = "identity",
             position = position_dodge()) +
    geom_point(data = y,
               aes(x = Exp_Con,
                   y = Fsa),
               position = position_dodge(width = 0.9))+
    geom_errorbar(aes(ymin=fsa_avg-fsa_se,
                      ymax=fsa_avg+fsa_se),
                  width=0.1,
                  size = 1,
                  position = position_dodge(width = 0.9)) +
    scale_y_continuous(limits = c(0,70)) +
    ylab(expression(atop("Stretch-activated",
                         paste("Specific Tension (mN/mm^2)"))))+
    guides(fill=guide_legend(title = "Fiber Types")) + 
    theme(axis.title.y = element_text(size = 30),
          axis.title.x = element_blank(),
          axis.text = element_text(size = 25),
          legend.title = element_text(size = 25),
          legend.text = element_text(size = 20),
          legend.key.size = unit(1,"cm")) +
    scale_fill_manual(breaks = c("IIX","IIB"),
                      values = c("#CC79A7","#009E73")) +
    scale_x_discrete(breaks = c("Active",
                                "Fat_4.5",
                                "Fat_5.1"),
                     labels = c("Active",
                                expression(atop("High Calcium",
                                                paste("Fatigue"))),
                                expression(atop("Low Calcium",
                                                paste("Fatigue")))))
)

(gg2.3 <- acsm_data %>% 
    filter(fiber_type == "IIX" | fiber_type == "IIB") %>% 
    ggplot(aes(Exp_Con, 
               fsatotal_avg, 
               group = fiber_type_num)) +
    geom_bar(aes(fill = fiber_type),
             stat = "identity",
             position = position_dodge()) +
    geom_point(data = y,
               aes(x = Exp_Con,
                   y = Fsa_total),
               position = position_dodge(width = 0.9))+
    geom_errorbar(aes(ymin=fsatotal_avg-fsatotal_se,
                      ymax=fsatotal_avg+fsatotal_se),
                  width=0.1,
                  size = 1,
                  position = position_dodge(width = 0.9)) +
    scale_y_continuous(limits = c(0,37)) +
    ylab(expression(atop("Stretch-to-Calcium-activated",
                         paste("Specific Tension (mN/mm^2)"))))+
    guides(fill=guide_legend(title = "Fiber Types")) + 
    theme(axis.title.y = element_text(size = 30),
          axis.title.x = element_blank(),
          axis.text = element_text(size = 25),
          legend.title = element_text(size = 25),
          legend.text = element_text(size = 20),
          legend.key.size = unit(1,"cm")) +
    scale_fill_manual(breaks = c("IIX","IIB"),
                      values = c("#CC79A7","#009E73")) +
    scale_x_discrete(breaks = c("Active",
                                "Fat_4.5",
                                "Fat_5.1"),
                     labels = c("Active",
                                expression(atop("High Calcium",
                                                paste("Fatigue"))),
                                expression(atop("Low Calcium",
                                                paste("Fatigue")))))
)

## MHC IIA,IIX,IIB
z <- acsm_data2 %>% 
  filter(fiber_type == "IIX" | fiber_type == "IIB" | fiber_type == "IIA") 

(gg3 <- acsm_data %>% 
    filter(fiber_type == "IIX" | fiber_type == "IIB" | fiber_type == "IIA") %>% 
    ggplot(aes(Exp_Con, 
               f0_avg, 
               group = fiber_type_num)) +
    geom_bar(aes(fill = fiber_type),
             stat = "identity",
             position = position_dodge()) +
    geom_point(data = z,
               aes(x = Exp_Con,
                   y = Po_Pre_Step),
               position = position_dodge(width = 0.9))+
    geom_errorbar(aes(ymin=f0_avg-f0_se,
                      ymax=f0_avg+f0_se),
                  width=0.1,
                  size = 1,
                  position = position_dodge(width = 0.9)) +
    scale_y_continuous(limits = c(0,350)) +
    ylab(expression(atop("Calcium-activated",
                         paste("Specific Tension (mN/mm^2)"))))+
    guides(fill=guide_legend(title = "Fiber Types")) + 
    theme(axis.title.y = element_text(size = 30),
          axis.title.x = element_blank(),
          axis.text = element_text(size = 25),
          legend.title = element_text(size = 25),
          legend.text = element_text(size = 20),
          legend.key.size = unit(1,"cm")) +
    scale_fill_manual(breaks = c("IIA","IIX","IIB"),
                      values = c("#56B4E9", "#CC79A7","#009E73")) +
    scale_x_discrete(breaks = c("Active",
                                "Fat_4.5",
                                "Fat_5.1"),
                     labels = c("Active",
                                expression(atop("High Calcium",
                                                paste("Fatigue"))),
                                expression(atop("Low Calcium",
                                                paste("Fatigue")))))
)
(gg3.2 <- acsm_data %>% 
    filter(fiber_type == "IIX" | fiber_type == "IIB" | fiber_type == "IIA") %>% 
    ggplot(aes(Exp_Con, 
               fsa_avg, 
               group = fiber_type_num)) +
    geom_bar(aes(fill = fiber_type),
             stat = "identity",
             position = position_dodge()) +
    geom_point(data = z,
               aes(x = Exp_Con,
                   y = Fsa),
               position = position_dodge(width = 0.9))+
    geom_errorbar(aes(ymin=fsa_avg-fsa_se,
                      ymax=fsa_avg+fsa_se),
                  width=0.1,
                  size = 1,
                  position = position_dodge(width = 0.9)) +
    scale_y_continuous(limits = c(0,75)) +
    ylab(expression(atop("Stretch-activated",
                         paste("Specific Tension (mN/mm^2)"))))+
    guides(fill=guide_legend(title = "Fiber Types")) + 
    theme(axis.title.y = element_text(size = 30),
          axis.title.x = element_blank(),
          axis.text = element_text(size = 25),
          legend.title = element_text(size = 25),
          legend.text = element_text(size = 20),
          legend.key.size = unit(1,"cm")) +
    scale_fill_manual(breaks = c("IIA","IIX","IIB"),
                      values = c("#56B4E9", "#CC79A7","#009E73")) +
    scale_x_discrete(breaks = c("Active",
                                "Fat_4.5",
                                "Fat_5.1"),
                     labels = c("Active",
                                expression(atop("High Calcium",
                                                paste("Fatigue"))),
                                expression(atop("Low Calcium",
                                                paste("Fatigue")))))
)

(gg3.3 <- acsm_data %>% 
    filter(fiber_type == "IIX" | fiber_type == "IIB" | fiber_type == "IIA") %>% 
    ggplot(aes(Exp_Con, 
               fsatotal_avg, 
               group = fiber_type_num)) +
    geom_bar(aes(fill = fiber_type),
             stat = "identity",
             position = position_dodge()) +
    geom_point(data = z,
               aes(x = Exp_Con,
                   y = Fsa_total),
               position = position_dodge(width = 0.9))+
    geom_errorbar(aes(ymin=fsatotal_avg-fsatotal_se,
                      ymax=fsatotal_avg+fsatotal_se),
                  width=0.1,
                  size = 1,
                  position = position_dodge(width = 0.9)) +
    scale_y_continuous(limits = c(0,40)) +
    ylab(expression(atop("Stretch-to-Calcium-activated",
                         paste("Specific Tension (%)"))))+
    guides(fill=guide_legend(title = "Fiber Types")) + 
    theme(axis.title.y = element_text(size = 30),
          axis.title.x = element_blank(),
          axis.text = element_text(size = 25),
          legend.title = element_text(size = 25),
          legend.text = element_text(size = 20),
          legend.key.size = unit(1,"cm")) +
    scale_fill_manual(breaks = c("IIA","IIX","IIB"),
                      values = c("#56B4E9", "#CC79A7","#009E73")) +
    scale_x_discrete(breaks = c("Active",
                                "Fat_4.5",
                                "Fat_5.1"),
                     labels = c("Active",
                                expression(atop("High Calcium",
                                                paste("Fatigue"))),
                                expression(atop("Low Calcium",
                                                paste("Fatigue")))))
)


## All isoforms
(gg4 <- acsm_data %>% 
    ggplot(aes(Exp_Con, 
               f0_avg, 
               group = fiber_type_num)) +
    geom_bar(aes(fill = fiber_type),
             stat = "identity",
             position = position_dodge())+
    geom_point(data = acsm_data2,
               aes(x = Exp_Con,
                   y = Po_Pre_Step),
               position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin=f0_avg-f0_se,
                      ymax=f0_avg+f0_se),
                  position = position_dodge(width = 0.9),
                  width=0.15,
                  size = 1.25) +
    scale_y_continuous(limits = c(0,350)) +
    ylab(expression(atop("Calcium-activated",
                         paste("Specific Tension (mN/mm^2)"))))+
    guides(fill=guide_legend(title = "Fiber Types")) + 
    theme(axis.title.y = element_text(size = 30),
          axis.title.x = element_blank(),
          axis.text = element_text(size = 25),
          legend.title = element_text(size = 25),
          legend.text = element_text(size = 20),
          legend.key.size = unit(1,"cm")) +
    scale_fill_manual(breaks = c("I",
                                 "IIA",
                                 "IIX",
                                 "IIB"),
                      values = c("#E69F00",
                                 "#56B4E9",
                                 "#CC79A7",
                                 "#009E73")) +
    scale_x_discrete(breaks = c("Active",
                                "Fat_4.5",
                                "Fat_5.1"),
                     labels = c("Active",
                                expression(atop("High Calcium",
                                                paste("Fatigue"))),
                                expression(atop("Low Calcium",
                                                paste("Fatigue")))))
)

(gg4.2 <- acsm_data %>% 
    ggplot(aes(Exp_Con, 
               fsa_avg, 
               group = fiber_type_num)) +
    geom_bar(aes(fill = fiber_type),
             stat = "identity",
             position = position_dodge())+
    geom_point(data = acsm_data2,
               aes(x = Exp_Con,
                   y = Fsa),
               position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin=fsa_avg-fsa_se,
                      ymax=fsa_avg+fsa_se),
                  position = position_dodge(width = 0.9),
                  width=0.15,
                  size = 1.25) +
    scale_y_continuous(limits = c(0,75)) +
    ylab(expression(atop("Stretch-activated",
                         paste("Specific Tension (mN/mm^2)"))))+
    guides(fill=guide_legend(title = "Fiber Types")) + 
    theme(axis.title.y = element_text(size = 30),
          axis.title.x = element_blank(),
          axis.text = element_text(size = 25),
          legend.title = element_text(size = 25),
          legend.text = element_text(size = 20),
          legend.key.size = unit(1,"cm")) +
    scale_fill_manual(breaks = c("I",
                                 "IIA",
                                 "IIX",
                                 "IIB"),
                      values = c("#E69F00",
                                 "#56B4E9",
                                 "#CC79A7",
                                 "#009E73")) +
    scale_x_discrete(breaks = c("Active",
                                "Fat_4.5",
                                "Fat_5.1"),
                     labels = c("Active",
                                expression(atop("High Calcium",
                                                paste("Fatigue"))),
                                expression(atop("Low Calcium",
                                                paste("Fatigue")))))
)

(gg4.3 <- acsm_data %>% 
    ggplot(aes(Exp_Con, 
               fsatotal_avg, 
               group = fiber_type_num)) +
    geom_bar(aes(fill = fiber_type),
             stat = "identity",
             position = position_dodge())+
    geom_point(data = acsm_data2,
               aes(x = Exp_Con,
                   y = Fsa_total),
               position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin=fsatotal_avg-fsatotal_se,
                      ymax=fsatotal_avg+fsatotal_se),
                  position = position_dodge(width = 0.9),
                  width=0.15,
                  size = 1.25) +
    scale_y_continuous(limits = c(0,35)) +
    ylab(expression(atop("Stretch-to-Calcium-activated",
                         paste("Specific Tension (%)"))))+
    guides(fill=guide_legend(title = "Fiber Types")) + 
    theme(axis.title.y = element_text(size = 30),
          axis.title.x = element_blank(),
          axis.text = element_text(size = 25),
          legend.title = element_text(size = 25),
          legend.text = element_text(size = 20),
          legend.key.size = unit(1,"cm")) +
    scale_fill_manual(breaks = c("I",
                                 "IIA",
                                 "IIX",
                                 "IIB"),
                      values = c("#E69F00",
                                 "#56B4E9",
                                 "#CC79A7",
                                 "#009E73")) +
    scale_x_discrete(breaks = c("Active",
                                "Fat_4.5",
                                "Fat_5.1"),
                     labels = c("Active",
                                expression(atop("High Calcium",
                                                paste("Fatigue"))),
                                expression(atop("Low Calcium",
                                                paste("Fatigue")))))
)

# ggexport(gg1, filename = "Woods_ACSM_F0_MHCIIX.jpeg")
# ggexport(gg1.2, filename = "Woods_ACSM_Fsa_MHCIIX.jpeg")
# ggexport(gg1.3, filename = "Woods_ACSM_Ratio_MHCIIX.jpeg")
# 
# ggexport(gg2, filename = "Woods_ACSM_F0_MHCIIX+B.jpeg")
# ggexport(gg2.2, filename = "Woods_ACSM_Fsa_MHCIIX+B.jpeg")
# ggexport(gg2.3, filename = "Woods_ACSM_Ratio_MHCIIX+B.jpeg")
# 
# ggexport(gg3, filename = "Woods_ACSM_F0_MHCIIXBA.jpeg")
# ggexport(gg3.2, filename = "Woods_ACSM_Fsa_MHCIIXBA.jpeg")
# ggexport(gg3.3, filename = "Woods_ACSM_Ratio_MHCIIXBA.jpeg")
# 
# ggexport(gg4, filename = "Woods_ACSM_F0_All.jpeg")
# ggexport(gg4.2, filename = "Woods_ACSM_Fsa_All.jpeg")
# ggexport(gg4.3, filename = "Woods_ACSM_Ratio_All.jpeg")

ggsave("Woods_NEACSM_F0_IIX.jpeg", gg1, width = 12, height = 10, units = "in", dpi = 300)
ggsave("Woods_NEACSM_Fsa_IIX.jpeg", gg1.2, width = 12, height = 10, units = "in", dpi = 300)
ggsave("Woods_NEACSM_ratio_IIX.jpeg", gg1.3, width = 12, height = 10, units = "in", dpi = 300)

ggsave("Woods_NEACSM_F0_IIXB.jpeg", gg2, width = 13, height = 10, units = "in", dpi = 300)
ggsave("Woods_NEACSM_Fsa_IIXB.jpeg", gg2.2, width = 13, height = 10, units = "in", dpi = 300)
ggsave("Woods_NEACSM_ratio_IIXB.jpeg", gg2.3, width = 13, height = 10, units = "in", dpi = 300)

ggsave("Woods_NEACSM_F0_3.jpeg", gg3, width = 14, height = 10, units = "in", dpi = 300)
ggsave("Woods_NEACSM_Fsa_3.jpeg", gg3.2, width = 14, height = 10, units = "in", dpi = 300)
ggsave("Woods_NEACSM_ratio_3.jpeg", gg3.3, width = 14, height = 10, units = "in", dpi = 300)

ggsave("Woods_NEACSM_F0_all_.jpeg", gg4, width = 15, height = 10, units = "in", dpi = 300)
ggsave("Woods_NEACSM_Fsa_all_.jpeg", gg4.2, width = 15, height = 10, units = "in", dpi = 300)
ggsave("Woods_NEACSM_ratio_all_.jpeg", gg4.3, width = 15, height = 10, units = "in", dpi = 300)
### NEACSM: M7F10 (IIX) Rate Fits ------------------------

setwd("C:/Users/Phil/Dropbox/Thesis- Stretch Activation/Data/Woods - Master's Thesis/Project/")

df3 <- read_excel("Woods_M7F10_all-fits.xlsx")

(gg5 <- df3 %>% 
    ggplot(aes(x = Time, 
               col = "#CC79A7")) +
    geom_line(aes(y = Active),
              linetype = "solid",
              size = 2) +
    geom_line(aes(y = Fat_high),
              linetype = "longdash",
              size = 2) +
    geom_line(aes(y = Fat_low),
              linetype = "dotted",
              size = 2) +
    ylab("Specific Tension (mN/mm^2)") +
    theme(axis.title = element_text(size = 30),
          axis.text = element_text(size = 25),
          legend.position = "none")
)

ggexport(gg5, filename = "Woods_NEACSM_3fits.jpeg")

### NEACSM: Mouse 7: MHC I, IIA, IIX, IIB Fits on one graph -----------

setwd("C:/Users/Phil/Dropbox/Thesis- Stretch Activation/Data/Woods - Master's Thesis/Project/")

df4 <- read_excel("Woods_NEACSM_high_Fat_fits.xlsx")

df4.1 <- df4 %>% 
  mutate(I_mod = I/0.002710547, .after = I) %>% 
  mutate(IIA_mod = IIA/0.000780136, .after = IIA) %>% 
  mutate(IIX_mod = IIX/0.001272394, .after = IIX) %>% 
  mutate(IIB_mod = IIB/0.001450591, .after = IIB)


(gg6 <- df4.1 %>% 
    ggplot(aes(x = Time)) +
    geom_line(aes(y = I_mod),
              col = "#E69F00",
              size = 1.25) +
    geom_line(aes(y = IIA_mod),
              col = "#56B4E9",
              size = 1.25) +
    geom_line(aes(y = IIX_mod),
              col = "#CC79A7",
              size = 1.25) +
    geom_line(aes(y = IIB_mod),
              col = "#009E73",
              size = 1.25) +
    labs(y = "Specific Tension (mN/mm^2)") +
    theme(axis.title = element_text(size = 30),
          axis.text = element_text(size = 25))
)

# ggexport(gg6, filename = "Woods_NEACSM_High_Fat_Fits.png")

ggsave("Woods_NEACSM_Fits_allisoforms.jpeg",
       gg6, 
       width = 10, 
       height = 10, 
       units = "in", 
       dpi = 300)


### Defense: Positive Phase 3 in MHC I,IIA,IIX,IIB --------
setwd("C:/Users/Phil/Dropbox/Thesis- Stretch Activation/Data/Woods - Master's Thesis/Project/Tension + AaBbCc")


# M4F6
data_I <- read_excel("Woods_P3_MHCiso_10-25-22.xlsx",
                     sheet = "I",
                     na = "") %>% 
  select(Time, High_Fat,Active)

#M8F6
data_IIA <- read_excel("Woods_P3_MHCiso_10-25-22.xlsx",
                       sheet = "IIA",
                       na = "") %>% 
  select(Time, Low_Fat,High_Fat,Active) %>% 
  filter(Time < 0.15)

#M3F18
data_IIX <- read_excel("Woods_P3_MHCiso_10-25-22.xlsx",
                       sheet = "IIX",
                       na = "") %>% 
  select(Time, Low_Fat,High_Fat,Active) %>% 
  filter(Time < 0.05)

#M6F13
data_IIB <- read_excel("Woods_P3_MHCiso_10-25-22.xlsx",
                       sheet = "IIB",
                       na = "") %>% 
  select(Time, Low_Fat,High_Fat,Active) %>% 
  filter(Time < 0.03)



(I_gg <- ggplot(data_I,
               aes(x = Time)) +
  geom_line(aes(y = High_Fat),
            linetype = "longdash") +
  geom_line(aes(y = Active),
            linetype = "solid")+
    ylab("Force (mN)") 
)

(IIA_gg <- ggplot(data_IIA,
                aes(x = Time)) +
    geom_line(aes(y = Low_Fat),
              linetype = "dotted") +
    geom_line(aes(y = High_Fat),
              linetype = "longdash") +
    geom_line(aes(y = Active),
              linetype = "solid")+
    ylab("Force (mN)") 
)

(IIX_gg <- ggplot(data_IIX,
                  aes(x = Time)) +
    geom_line(aes(y = Low_Fat),
              linetype = "dotted") +
    geom_line(aes(y = High_Fat),
              linetype = "longdash") +
    geom_line(aes(y = Active),
              linetype = "solid")+
    ylab("Force (mN)") 
)

(IIB_gg <- ggplot(data_IIB,
                  aes(x = Time)) +
    geom_line(aes(y = Low_Fat),
              linetype = "dotted") +
    geom_line(aes(y = High_Fat),
              linetype = "longdash") +
    geom_line(aes(y = Active),
              linetype = "solid")+
    ylab("Force (mN)") +
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 12),
          legend.position = "none")
)



