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

my_data <- read_excel("SA-Fatigue_Tension+Step+Kinetics_PW_6-27-22.xlsx", 
                      skip = 5,
                      na="")

soleus <- my_data %>% 
  filter(Muscle == "soleus") %>% 
  filter(Exp_Con_Num %in% c(2:5))

EDL <- my_data %>% 
  filter(Muscle == "EDL") %>% 
  filter(Exp_Con_Num %in% c(2:5))


phil_awesome_data <-
  my_data %>% 
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
                     fsa_total_avg = mean(Fsa_total, na.rm = T),
                     fsa_total_sd = sd(Fsa_total, na.rm = T),
                     fsa_total_se = sd(Fsa_total, na.rm = T)/sqrt(n()),
                     r2_avg = mean(r2, na.rm = T),
                     r2_sd = sd(r2, na.rm = T),
                     r2_se = sd(r2, na.rm = T)/sqrt(n()),
                     r3_avg = mean(r3, na.rm = T),
                     r3_sd = sd(r3, na.rm = T),
                     r3_se = sd(r3, na.rm = T)/sqrt(n()),
                     r4_avg = mean(r4, na.rm = T),
                     r4_sd = sd(r4, na.rm = T),
                     r4_se = sd(r4, na.rm = T)/sqrt(n()),
                     )




# get_fancystats_phase3 <- function(,Muscle){
#   df <- my_data %>% 
#     filter(Muscle == Muscle) %>% 
#     
#     
# 
#   f0 <- data.frame(muscle = df$Muscle, mean = mean(df$Po_Pre_Step), sd = sd(df$Po_Pre_Step))
#   fsa <- data.frame(muscle = df$Muscle, mean = mean(df$Fsa), sd = sd(df$Fsa))
#   fsa_f0 <- data.frame(muscle = df$Muscle, mean = mean(df$FsaF0), sd=sd(df$Fsa))
#   fsa_total <- data.frame(muscle = df$Muscle, mean = mean(df$Fsa_total), sd = sd(df$Fsa_total))
# 
#   rbind(f0,fsa,fsa_f0,fsa_total)
# }
# 
# philly <- map(my_data$Exp_Con,get_fancystats_phase3)

## Phase 3 Amplitude - Averages --------------------------------------------------------------------------

## Soleus

f0_soleus <- data.frame(condition = unique(soleus$Exp_Con), 
                        mean = NA,
                        muscle = "Soleus",
                        sd = NA)
for(i in unique(soleus$Exp_Con)){
  temp_frame <- filter(soleus, Exp_Con %in% i)
  x <- mean(temp_frame$Po_Pre_Step,
            na.rm = TRUE) # Change column according to outcome analyzed
  y <- sd(temp_frame$Po_Pre_Step,
          na.rm = TRUE)
  fill_row <- as.numeric(which(f0_soleus$condition %in% i))
  f0_soleus$mean[fill_row] <- x
  f0_soleus$sd[fill_row] <- y
}

fsa_soleus <- data.frame(condition = unique(soleus$Exp_Con), 
                         mean = NA, 
                         muscle = "Soleus",
                         sd = NA)
for(i in unique(soleus$Exp_Con)){
  temp_frame <- filter(soleus, Exp_Con %in% i)
  x <- mean(temp_frame$Fsa,
            na.rm = TRUE) # Change column according to outcome analyzed
  y <- sd(temp_frame$Fsa,
          na.rm = TRUE)
  fill_row <- as.numeric(which(fsa_soleus$condition %in% i))
  fsa_soleus$mean[fill_row] <- x
  fsa_soleus$sd[fill_row] <- y
}

fsaf0_soleus <- data.frame(condition = unique(soleus$Exp_Con), 
                           mean = NA, 
                           muscle = "Soleus",
                           sd = NA)
for(i in unique(soleus$Exp_Con)){
  temp_frame <- filter(soleus, Exp_Con %in% i)
  x <- mean(temp_frame$FsaF0,
            na.rm = TRUE) # Change column according to outcome analyzed
  y <- sd(temp_frame$FsaF0,
          na.rm = TRUE)
  fill_row <- as.numeric(which(fsaf0_soleus$condition %in% i))
  fsaf0_soleus$mean[fill_row] <- x
  fsaf0_soleus$sd[fill_row] <- y
}

fsatotal_soleus <- data.frame(condition = unique(soleus$Exp_Con), 
                              mean = NA, 
                              muscle = "Soleus",
                              sd = NA)
for(i in unique(soleus$Exp_Con)){
  temp_frame <- filter(soleus, Exp_Con %in% i)
  x <- mean(temp_frame$Fsa_total,
            na.rm = TRUE) # Change column according to outcome analyzed
  y <- sd(temp_frame$Fsa_total,
          na.rm = TRUE)
  fill_row <- as.numeric(which(fsatotal_soleus$condition %in% i))
  fsatotal_soleus$mean[fill_row] <- x
  fsatotal_soleus$sd[fill_row] <- y
}

## EDL

f0_EDL <- data.frame(condition = unique(EDL$Exp_Con), 
                     mean = NA, 
                     muscle = "EDL",
                     sd = NA)
for(i in unique(EDL$Exp_Con)){
  temp_frame <- filter(EDL, Exp_Con %in% i)
  x <- mean(temp_frame$Po_Pre_Step) # Change column according to outcome analyzed
  y <- sd(temp_frame$Po_Pre_Step)
  fill_row <- as.numeric(which(f0_EDL$condition %in% i))
  f0_EDL$mean[fill_row] <- x
  f0_EDL$sd[fill_row] <- y
}

fsa_EDL <- data.frame(condition = unique(EDL$Exp_Con), 
                      mean = NA, 
                      muscle = "EDL",
                      sd = NA)
for(i in unique(EDL$Exp_Con)){
  temp_frame <- filter(EDL, Exp_Con %in% i)
  x <- mean(temp_frame$Fsa,
            na.rm = TRUE) # Change column according to outcome analyzed
  y <- sd(temp_frame$Fsa,
          na.rm = TRUE)
  fill_row <- as.numeric(which(fsa_EDL$condition %in% i))
  fsa_EDL$mean[fill_row] <- x
  fsa_EDL$sd[fill_row] <- y
}

fsaf0_EDL <- data.frame(condition = unique(EDL$Exp_Con), 
                        mean = NA, 
                        muscle = "EDL",
                        sd = NA)
for(i in unique(EDL$Exp_Con)){
  temp_frame <- filter(EDL, Exp_Con %in% i)
  x <- mean(temp_frame$FsaF0,
            na.rm = TRUE) # Change column according to outcome analyzed
  y <- sd(temp_frame$FsaF0,
          na.rm = TRUE)
  fill_row <- as.numeric(which(fsaf0_EDL$condition %in% i))
  fsaf0_EDL$mean[fill_row] <- x
  fsaf0_EDL$sd[fill_row] <- y
}

fsatotal_EDL <- data.frame(condition = unique(EDL$Exp_Con),
                           mean = NA, 
                           muscle = "EDL",
                           sd = NA)
for(i in unique(EDL$Exp_Con)){
  temp_frame <- filter(EDL, Exp_Con %in% i)
  x <- mean(temp_frame$Fsa_total,
            na.rm = TRUE) # Change column according to outcome analyzed
  y <- sd(temp_frame$Fsa_total,
          na.rm = TRUE)
  fill_row <- as.numeric(which(fsatotal_EDL$condition %in% i))
  fsatotal_EDL$mean[fill_row] <- x
  fsatotal_EDL$sd[fill_row] <- y
}

## Rate Fittings - Averages ------------------------------------------------------------

## Soleus

r2_soleus <- data.frame(condition = unique(soleus$Exp_Con), 
                        mean = NA, 
                        muscle = "Soleus",
                        sd = NA)
for(i in unique(soleus$Exp_Con)){
  temp_frame <- filter(soleus, Exp_Con %in% i)
  x <- mean(temp_frame$r2,
            na.rm = TRUE) # Change column according to outcome analyzed
  y <- sd(temp_frame$r2,
          na.rm = TRUE)
  fill_row <- as.numeric(which(r2_soleus$condition %in% i))
  r2_soleus$mean[fill_row] <- x
  r2_soleus$sd[fill_row] <- y
}

r3_soleus <- data.frame(condition = unique(soleus$Exp_Con), 
                        mean = NA, 
                        muscle = "Soleus",
                        sd = NA)
for(i in unique(soleus$Exp_Con)){
  temp_frame <- filter(soleus, Exp_Con %in% i)
  x <- mean(temp_frame$r3,
            na.rm = TRUE) # Change column according to outcome analyzed
  y <- sd(temp_frame$r3,
          na.rm = TRUE)
  fill_row <- as.numeric(which(r3_soleus$condition %in% i))
  r3_soleus$mean[fill_row] <- x
  r3_soleus$sd[fill_row] <- y
}

r4_soleus <- data.frame(condition = unique(EDL$Exp_Con), 
                        mean = NA, 
                        muscle = "Soleus",
                        sd = NA)
for(i in unique(soleus$Exp_Con)){
  temp_frame <- filter(soleus, Exp_Con %in% i)
  x <- mean(temp_frame$r4,
            na.rm = TRUE) # Change column according to outcome analyzed
  y <- sd(temp_frame$r4,
          na.rm = TRUE)
  fill_row <- as.numeric(which(r4_soleus$condition %in% i))
  r4_soleus$mean[fill_row] <- x
  r4_soleus$sd[fill_row] <- y
}

## EDL

r2_EDL <- data.frame(condition = unique(EDL$Exp_Con),
                     mean = NA, 
                     muscle = "EDL",
                     sd = NA)
for(i in unique(EDL$Exp_Con)){
  temp_frame <- filter(EDL, Exp_Con %in% i)
  x <- mean(temp_frame$r2,
            na.rm = TRUE) # Change column according to outcome analyzed
  y <- sd(temp_frame$r2,
          na.rm = TRUE)
  fill_row <- as.numeric(which(r2_EDL$condition %in% i))
  r2_EDL$mean[fill_row] <- x
  r2_EDL$sd[fill_row] <- y
}

r3_EDL <- data.frame(condition = unique(EDL$Exp_Con), 
                     muscle = "EDL", 
                     mean = NA, 
                     sd = NA)
for(i in unique(EDL$Exp_Con)){
  temp_frame <- filter(EDL, Exp_Con %in% i)
  x <- mean(temp_frame$r3,
            na.rm = TRUE) # Change column according to outcome analyzed
  y <- sd(temp_frame$r3,
          na.rm = TRUE)
  fill_row <- as.numeric(which(r3_EDL$condition %in% i))
  r3_EDL$mean[fill_row] <- x
  r3_EDL$sd[fill_row] <- y
}

r4_EDL <- data.frame(condition = unique(EDL$Exp_Con), 
                     mean = NA, 
                     muscle = "EDL",
                     sd = NA)
for(i in unique(EDL$Exp_Con)){
  temp_frame <- filter(EDL, Exp_Con %in% i)
  x <- mean(temp_frame$r4,
            na.rm = TRUE) # Change column according to outcome analyzed
  y <- sd(temp_frame$r4,
          na.rm = TRUE)
  fill_row <- as.numeric(which(r4_EDL$condition %in% i))
  r4_EDL$mean[fill_row] <- x
  r4_EDL$sd[fill_row] <- y
}

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






f0 <- rbind(f0_soleus,f0_EDL)

(f0_plot <- ggplot(data = f0,
                   aes(x = factor(f0$condition,
                                  levels = unique(f0$condition)
                                  ),
                       y = mean,
                       color = muscle)) +
    geom_point() +
    
    geom_line(aes(group = muscle)) +
    
    scale_color_manual(breaks = c("Soleus", "EDL"),
                       values = c("red", "blue")) +
    geom_errorbar(aes(ymin = mean - sd,
                      ymax = mean +  sd,
                      width = 0.1)) +
    ylab("F0")
  
)

Fsa <- rbind(fsa_soleus,fsa_EDL)

(Fsa_plot <- ggplot(data = Fsa,
                   aes(x = factor(Fsa$condition,
                                  levels = unique(Fsa$condition)
                                  ),
                       y = mean,
                       color = muscle)) +
    geom_point() +
    
    geom_line(aes(group = muscle)) +
    
    scale_color_manual(breaks = c("Soleus", "EDL"),
                       values = c("red", "blue")) +
    geom_errorbar(aes(ymin = mean - sd,
                      ymax = mean +  sd,
                      width = 0.1)) +
    ylab("Fsa")
  
)

FsaF0 <- rbind(fsaf0_soleus,fsaf0_EDL)

(FsaF0_plot <- ggplot(data = FsaF0,
                    aes(x = factor(FsaF0$condition,
                                   levels = unique(FsaF0$condition)
                                   ),
                        y = mean,
                        color = muscle)) +
    geom_point() +
    
    geom_line(aes(group = muscle)) +
    
    scale_color_manual(breaks = c("Soleus", "EDL"),
                       values = c("red", "blue")) +
    geom_errorbar(aes(ymin = mean - sd,
                      ymax = mean +  sd,
                      width = 0.1)) +
    ylab("Fsa/F0")
  
)


FsaTotal <- rbind(fsatotal_soleus,fsatotal_EDL)

(FsaTotal_plot <- ggplot(data = FsaTotal,
                      aes(x = factor(FsaTotal$condition,
                                     levels = unique(FsaTotal$condition)
                                     ),
                          y = mean,
                          color = muscle)) +
    geom_point() +
    
    geom_line(aes(group = muscle)) +
    
    scale_color_manual(breaks = c("Soleus", "EDL"),
                       values = c("red", "blue")) +
    geom_errorbar(aes(ymin = mean - sd,
                      ymax = mean +  sd,
                      width = 0.1)) +
    ylab("Fsa/(Fsa + F0)")
  
)

r2 <- rbind(r2_soleus,r2_EDL)

(r2_plot <- ggplot(data = r2,
                      aes(x = factor(r2$condition,
                                     levels = unique(r2$condition)
                                     ),
                          y = mean,
                          color = muscle)) +
    geom_point() +
    
    geom_line(aes(group = muscle)) +
    
    scale_color_manual(breaks = c("Soleus", "EDL"),
                       values = c("red", "blue")) +
    geom_errorbar(aes(ymin = mean - sd,
                      ymax = mean +  sd,
                      width = 0.1)) +
    ylab('r2')
  
)


r3 <- rbind(r3_soleus,r3_EDL)

(r3_plot <- ggplot(data = r3,
              aes(x = factor(r3$condition,
                             levels = unique(r3$condition)
                             ),
                  y = mean,
                  color = muscle)) +
    geom_point() +
    
    geom_line(aes(group = muscle)) +
    
    scale_color_manual(breaks = c("Soleus", "EDL"),
                       values = c("red", "blue")) +
    geom_errorbar(aes(ymin = mean - sd,
                      ymax = mean +  sd,
                      width = 0.1)) +
    ylab("r3")
  
)

r4 <- rbind(r4_soleus,r4_EDL)

(r4_plot <- ggplot(data = r4,
              aes(x = factor(r4$condition,
                             levels = unique(r4$condition)
                             ),
                  y = mean,
                  color = muscle)) +
    geom_point() +
    
    geom_line(aes(group = muscle)) +
    
    scale_color_manual(breaks = c("Soleus", "EDL"),
                       values = c("red", "blue")) +
    geom_errorbar(aes(ymin = mean - sd,
                      ymax = mean +  sd,
                      width = 0.1)) +
    ylab("r4")
  
)


plot_all_amp <- ggarrange(f0_plot, Fsa_plot, FsaF0_plot, FsaTotal_plot,
                      ncol = 2,
                      nrow = 2)

plot_all_rates <- ggarrange(r2_plot, r3_plot, r4_plot,
                            ncol = 1,
                            nrow = 3)

ggexport(plot_all_amp, filename = "Woods_Phase3_Graphs.pdf")
ggexport(plot_all_rates, filename = "Woods_Rates_Graph.pdf")

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
           aes(x = factor(Exp_Con,levels = unique(Exp_Con)),
               y = Fsa,
               color = as.character(fiber_num))) + #w/out character, fiber_num is considered num
    geom_point() +
    geom_line(aes(group = fiber_num)) +
    xlab("Experiment Conditions") +
    ylab("Fsa") +
    facet_wrap( ~ Muscle)
)

(fsaf0_plot_fibers <- ggplot(data = my_data2, 
                           aes(x = factor(Exp_Con,levels = unique(Exp_Con)),
                               y = FsaF0,
                               color = as.character(fiber_num))) + #w/out character, fiber_num is considered num
    geom_point() +
    geom_line(aes(group = fiber_num)) +
    xlab("Experiment Conditions") +
    ylab("Fsa/F0") +
    facet_wrap( ~ Muscle)
)


(fsatotal_plot_fibers <- ggplot(data = my_data2, 
                             aes(x = factor(Exp_Con,levels = unique(Exp_Con)),
                                 y = Fsa_total,
                                 color = as.character(fiber_num))) + #w/out character, fiber_num is considered num
    geom_point() +
    geom_line(aes(group = fiber_num)) +
    xlab("Experiment Conditions") +
    ylab("Fsa/(Fsa + F0)") +
    facet_wrap( ~ Muscle)
)

(r2_plot_fibers <- ggplot(data = my_data2, 
                                aes(x = factor(Exp_Con,levels = unique(Exp_Con)),
                                    y = r2,
                                    color = as.character(fiber_num))) + #w/out character, fiber_num is considered num
    geom_point() +
    geom_line(aes(group = fiber_num)) +
    xlab("Experiment Conditions") +
    ylab("r2") +
    facet_wrap( ~ Muscle)
)

(r3_plot_fibers <- ggplot(data = my_data2, 
                          aes(x = factor(Exp_Con,levels = unique(Exp_Con)),
                              y = r3,
                              color = as.character(fiber_num))) + #w/out character, fiber_num is considered num
    geom_point() +
    geom_line(aes(group = fiber_num)) +
    xlab("Experiment Conditions") +
    ylab("r3") +
    facet_wrap( ~ Muscle)
)

(r4_plot_fibers <- ggplot(data = my_data2, 
                          aes(x = factor(Exp_Con,levels = unique(Exp_Con)),
                              y = r4,
                              color = as.character(fiber_num))) + #w/out character, fiber_num is considered num
    geom_point() +
    geom_line(aes(group = fiber_num)) +
    xlab("Experiment Conditions") +
    ylab("r4") +
    facet_wrap( ~ Muscle)
)

plot_fibers_amp <- ggarrange(f0_plot_fibers, fsa_plot_fibers, fsaf0_plot_fibers, fsatotal_plot_fibers,
                          ncol = 1,
                          nrow = 4)

plot_fibers_rates <- ggarrange(r2_plot_fibers, r3_plot_fibers, r4_plot_fibers,
                            ncol = 1,
                            nrow = 3)
ggexport(list(plot_fibers_amp, plot_fibers_rates), filename = "Woods_Thesis_Graphs_IndividualFibers.pdf")
