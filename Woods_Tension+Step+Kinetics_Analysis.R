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
theme_set(theme_classic())


setwd("C:/Users/Phil/Dropbox/Thesis- Stretch Activation/Data/Woods - Master's Thesis/Project/Tension + AaBbCc")

my_data <- read_excel("SA-Fatigue_Tension+Step+Kinetics_PW_7-12-22.xlsx", 
                      skip = 5,
                      na="")
phil_awesome_data <-
  my_data %>% 
    dplyr::filter(Exp_Con_Num %in% c(2:6)) %>% 
    dplyr::group_by(Muscle, Exp_Con) %>% 
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
                                  levels = c("Fat_1", 
                                             "Fat_2", 
                                             "Fat_4.5", 
                                             "Active",
                                             "Active_2")),
                       y = p0_pre_step_avg,
                       color = Muscle)) +
    geom_point() +
    
    geom_line(aes(group = Muscle)) +
    
    scale_color_manual(breaks = c("soleus", "EDL"),
                       values = c("red", "blue")) +
    geom_errorbar(aes(ymin = p0_pre_step_avg - p0_pre_step_se,
                      ymax = p0_pre_step_avg + p0_pre_step_se,
                      width = 0.1)) +
    ylab("F0") +
   xlab("Experimental Conditions")
  
)

(fsa_gg <- ggplot(data = phil_awesome_data,
                 aes(x = factor(Exp_Con, 
                                levels = c("Fat_1", 
                                           "Fat_2", 
                                           "Fat_4.5", 
                                           "Active",
                                           "Active_2")),
                     y = fsa_avg,
                     color = Muscle)) +
    geom_point() +
    
    geom_line(aes(group = Muscle)) +
    
    scale_color_manual(breaks = c("soleus", "EDL"),
                       values = c("red", "blue")) +
    geom_errorbar(aes(ymin = fsa_avg - fsa_se,
                      ymax = fsa_avg + fsa_se,
                      width = 0.1)) +
    ylab("Fsa") +
    xlab("Experimental Conditions")
  
)

(fsaf0_gg <- ggplot(data = phil_awesome_data,
                  aes(x = factor(Exp_Con, 
                                 levels = c("Fat_1", 
                                            "Fat_2", 
                                            "Fat_4.5", 
                                            "Active",
                                            "Active_2")),
                      y = fsaf0_avg,
                      color = Muscle)) +
    geom_point() +
    
    geom_line(aes(group = Muscle)) +
    
    scale_color_manual(breaks = c("soleus", "EDL"),
                       values = c("red", "blue")) +
    geom_errorbar(aes(ymin = fsaf0_avg - fsaf0_se,
                      ymax = fsaf0_avg + fsaf0_se,
                      width = 0.1)) +
    ylab("Fsa/F0") +
    xlab("Experimental Conditions")
  
)

(fsatotal_gg <- ggplot(data = phil_awesome_data,
                    aes(x = factor(Exp_Con, 
                                   levels = c("Fat_1", 
                                              "Fat_2", 
                                              "Fat_4.5", 
                                              "Active",
                                              "Active_2")),
                        y = fsatotal_avg,
                        color = Muscle)) +
    geom_point() +
    
    geom_line(aes(group = Muscle)) +
    
    scale_color_manual(breaks = c("soleus", "EDL"),
                       values = c("red", "blue")) +
    geom_errorbar(aes(ymin = fsatotal_avg - fsatotal_se,
                      ymax = fsatotal_avg + fsatotal_se,
                      width = 0.1)) +
    ylab("Fsa/(Fsa + F0)") +
    xlab("Experimental Conditions")
  
)
(a2_gg <- ggplot(data = phil_awesome_data,
                 aes(x = factor(Exp_Con, 
                                levels = c("Fat_1", 
                                           "Fat_2", 
                                           "Fat_4.5", 
                                           "Active",
                                           "Active_2")),
                     y = a2_avg,
                     color = Muscle)) +
    geom_point() +
    
    geom_line(aes(group = Muscle)) +
    
    scale_color_manual(breaks = c("soleus", "EDL"),
                       values = c("red", "blue")) +
    geom_errorbar(aes(ymin = a2_avg - a2_se,
                      ymax = a2_avg + a2_se,
                      width = 0.1)) +
    ylab("a2") +
    xlab("Experimental Conditions")
  
)

(a3_gg <- ggplot(data = phil_awesome_data,
                 aes(x = factor(Exp_Con, 
                                levels = c("Fat_1", 
                                           "Fat_2", 
                                           "Fat_4.5", 
                                           "Active",
                                           "Active_2")),
                     y = a3_avg,
                     color = Muscle)) +
    geom_point() +
    
    geom_line(aes(group = Muscle)) +
    
    scale_color_manual(breaks = c("soleus", "EDL"),
                       values = c("red", "blue")) +
    geom_errorbar(aes(ymin = a3_avg - a3_se,
                      ymax = a3_avg + a3_se,
                      width = 0.1)) +
    ylab("a3") +
    xlab("Experimental Conditions")
  
)

(a4_gg <- ggplot(data = phil_awesome_data,
                 aes(x = factor(Exp_Con, 
                                levels = c("Fat_1", 
                                           "Fat_2", 
                                           "Fat_4.5", 
                                           "Active",
                                           "Active_2")),
                     y = a4_avg,
                     color = Muscle)) +
    geom_point() +
    
    geom_line(aes(group = Muscle)) +
    
    scale_color_manual(breaks = c("soleus", "EDL"),
                       values = c("red", "blue")) +
    geom_errorbar(aes(ymin = a4_avg - a4_se,
                      ymax = a4_avg + a4_se,
                      width = 0.1)) +
    ylab("a4") +
    xlab("Experimental Conditions")
  
)

(r2_gg <- ggplot(data = phil_awesome_data,
                       aes(x = factor(Exp_Con, 
                                      levels = c("Fat_1", 
                                                 "Fat_2", 
                                                 "Fat_4.5", 
                                                 "Active",
                                                 "Active_2")),
                           y = r2_avg,
                           color = Muscle)) +
    geom_point() +
    
    geom_line(aes(group = Muscle)) +
    
    scale_color_manual(breaks = c("soleus", "EDL"),
                       values = c("red", "blue")) +
    geom_errorbar(aes(ymin = r2_avg - r2_se,
                      ymax = r2_avg + r2_se,
                      width = 0.1)) +
    ylab("r2") +
    xlab("Experimental Conditions")
  
)

(r3_gg <- ggplot(data = phil_awesome_data,
                 aes(x = factor(Exp_Con, 
                                levels = c("Fat_1", 
                                           "Fat_2", 
                                           "Fat_4.5", 
                                           "Active",
                                           "Active_2")),
                     y = r3_avg,
                     color = Muscle)) +
    geom_point() +
    
    geom_line(aes(group = Muscle)) +
    
    scale_color_manual(breaks = c("soleus", "EDL"),
                       values = c("red", "blue")) +
    geom_errorbar(aes(ymin = r3_avg - r3_se,
                      ymax = r3_avg + r3_se,
                      width = 0.1)) +
    ylab("r3") +
    xlab("Experimental Conditions")
  
)

(r4_gg <- ggplot(data = phil_awesome_data,
                 aes(x = factor(Exp_Con, 
                                levels = c("Fat_1", 
                                           "Fat_2", 
                                           "Fat_4.5", 
                                           "Active",
                                           "Active_2")),
                     y = r4_avg,
                     color = Muscle)) +
    geom_point() +
    
    geom_line(aes(group = Muscle)) +
    
    scale_color_manual(breaks = c("soleus", "EDL"),
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
  filter(Exp_Con_Num %in% c(2:5))

(f0_plot_fibers <- ggplot(data = my_data2, 
                         aes(x = factor(Exp_Con,
                                        levels = unique(Exp_Con)),
                             y = Po_Pre_Step,
                             color = as.character(fiber_num))) + #w/out character, fiber_num is considered num
  geom_point() +
  geom_line(aes(group = fiber_num)) +
    xlab("Experiment Conditions") +
    ylab("F0") +
  facet_wrap( ~ Muscle) 
)

(fsa_plot_fibers <- ggplot(data = my_data2, 
           aes(x = factor(Exp_Con,
                          levels = unique(Exp_Con)),
               y = Fsa,
               color = as.character(fiber_num))) + #w/out character, fiber_num is considered num
    geom_point() +
    geom_line(aes(group = fiber_num)) +
    xlab("Experiment Conditions") +
    ylab("Fsa") +
    facet_wrap( ~ Muscle)
)

(fsaf0_plot_fibers <- ggplot(data = my_data2, 
                           aes(x = factor(Exp_Con,
                                          levels = unique(Exp_Con)),
                               y = FsaF0,
                               color = as.character(fiber_num))) + #w/out character, fiber_num is considered num
    geom_point() +
    geom_line(aes(group = fiber_num)) +
    xlab("Experiment Conditions") +
    ylab("Fsa/F0") +
    facet_wrap( ~ Muscle)
)


(fsatotal_plot_fibers <- ggplot(data = my_data2, 
                             aes(x = factor(Exp_Con,
                                            levels = unique(Exp_Con)),
                                 y = Fsa_total,
                                 color = as.character(fiber_num))) + #w/out character, fiber_num is considered num
    geom_point() +
    geom_line(aes(group = fiber_num)) +
    xlab("Experiment Conditions") +
    ylab("Fsa/(Fsa + F0)") +
    facet_wrap( ~ Muscle)
)

(a2_plot_fibers <- ggplot(data = my_data2, 
                          aes(x = factor(Exp_Con,
                                         levels = unique(Exp_Con)),
                              y = a2,
                              color = as.character(fiber_num))) + #w/out character, fiber_num is considered num
    geom_point() +
    geom_line(aes(group = fiber_num)) +
    xlab("Experiment Conditions") +
    ylab("a2") +
    facet_wrap( ~ Muscle)
)

(a3_plot_fibers <- ggplot(data = my_data2, 
                          aes(x = factor(Exp_Con,
                                         levels = unique(Exp_Con)),
                              y = a3,
                              color = as.character(fiber_num))) + #w/out character, fiber_num is considered num
    geom_point() +
    geom_line(aes(group = fiber_num)) +
    xlab("Experiment Conditions") +
    ylab("a3") +
    facet_wrap( ~ Muscle)
)

(a4_plot_fibers <- ggplot(data = my_data2, 
                          aes(x = factor(Exp_Con,
                                         levels = unique(Exp_Con)),
                              y = a4,
                              color = as.character(fiber_num))) + #w/out character, fiber_num is considered num
    geom_point() +
    geom_line(aes(group = fiber_num)) +
    xlab("Experiment Conditions") +
    ylab("a4") +
    facet_wrap( ~ Muscle)
)

(r2_plot_fibers <- ggplot(data = my_data2, 
                                aes(x = factor(Exp_Con,
                                               levels = unique(Exp_Con)),
                                    y = r2,
                                    color = as.character(fiber_num))) + #w/out character, fiber_num is considered num
    geom_point() +
    geom_line(aes(group = fiber_num)) +
    xlab("Experiment Conditions") +
    ylab("r2") +
    facet_wrap( ~ Muscle)
)

(r3_plot_fibers <- ggplot(data = my_data2, 
                          aes(x = factor(Exp_Con,
                                         levels = unique(Exp_Con)),
                              y = r3,
                              color = as.character(fiber_num))) + #w/out character, fiber_num is considered num
    geom_point() +
    geom_line(aes(group = fiber_num)) +
    xlab("Experiment Conditions") +
    ylab("r3") +
    facet_wrap( ~ Muscle)
)

(r4_plot_fibers <- ggplot(data = my_data2, 
                          aes(x = factor(Exp_Con,
                                         levels = unique(Exp_Con)),
                              y = r4,
                              color = as.character(fiber_num))) + #w/out character, fiber_num is considered num
    geom_point() +
    geom_line(aes(group = fiber_num)) +
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
