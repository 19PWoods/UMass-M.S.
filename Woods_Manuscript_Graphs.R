library(tidyverse)
library(readxl)
library(ggpattern)
library(patchwork)
library(ggbreak)
library(ggtext)
library(envalysis)
library(cowplot)
# theme_set(theme_classic())
# theme_set(theme_publish())
theme_set(theme_cowplot())

# setwd("C:/Users/Phil/Dropbox/Thesis- Stretch Activation/Data/Woods - Master's Thesis/Project/Tension + AaBbCc")
setwd("C:/Users/pcw00/Dropbox/University of Massachusetts Amherst/Thesis- Stretch Activation/Data/Woods - Master's Thesis/Project/Tension + AaBbCc")


## Data Read in --------------------------------------------------------------------------
raw_data_f0 <- read_excel("SA+SD-Fatigue_Tension+Step+Kinetics_PW_5-7-23.xlsx", 
                       sheet = "Included",
                       skip = 5,
                       na="") %>% 
  filter(Exp_Con_Num %in% c(2:4)) %>% 
  filter(fiber_type_num %in% c(1:4)) %>% 
  group_by(Exp_Con, fiber_type, fiber_type_num)

raw_data_sa <- raw_data_f0 %>% 
  filter(P3_num == 1) %>% 
  filter(Ran_Num == 1) 

raw_data_sd <- raw_data_f0 %>% 
  filter(SD3_Num == 1)

my_data <- read_excel("Woods_EMM_2-14-23.xlsx",
                      na = "Included",
                      sheet = "EMM") %>% 
  filter(Include == 1) 

## Raw Trace: SA and SD---------------------------------------------------------

setwd("C:/Users/pcw00/Dropbox/University of Massachusetts Amherst/Thesis- Stretch Activation/Data/Woods - Master's Thesis/Project/Mouse 6/Fiber 13/Baseline")

trace <- read_excel("Run5.xlsx",
                    skip = 29) %>% 
  select(Time, Force_One) %>% 
  filter(Time > 0.05 & Time < 2.65)

# dygraphs::dygraph(trace)

(trace_gg <- ggplot(data = trace, aes(x = Time, y = Force_One)) +
    geom_line() +
    scale_x_break(c(0.15,2.55)) 
)

## SA Traces: MHC I,IIA, IIX, IIB -------------------------------------------------------------------------

trace_I <- read_excel("Woods_EMM_2-14-23.xlsx",
                      sheet = "I",
                      na = "Included") %>% 
  select(Time, Low_Fat, Fiber_type,High_Fat,Active)

trace_IIA <- read_excel("Woods_EMM_2-14-23.xlsx",
                      sheet = "IIA",
                      na = "Included") %>% 
  select(Time, Low_Fat, Fiber_type,High_Fat,Active)

trace_IIX <- read_excel("Woods_EMM_2-14-23.xlsx",
                        sheet = "IIX",
                        na = "Included") %>% 
  select(Time, Low_Fat, Fiber_type,High_Fat,Active)

trace_IIB <- read_excel("Woods_EMM_2-14-23.xlsx",
                        sheet = "IIB",
                        na = "Included") %>% 
  select(Time, Low_Fat, Fiber_type,High_Fat,Active)


(I_trace_gg <- trace_I %>% 
      mutate(Active = Active - 0.02) %>%
      filter(Time <0.4) %>% 
      ggplot(aes(x = Time)) +
      geom_line(aes(y = Low_Fat),
                size = 1,
                linetype = "dotted") +
      geom_line(aes(y = High_Fat),
                size = 1,
                linetype = "longdash") +
      geom_line(aes(y = Active),
                size = 1,
                linetype = "solid")+
     geom_segment(x = 0.18, y = 0.0,
                  xend = 0.18, yend = 0.008,
                  arrow = arrow(length = unit(0.04, "npc"),
                                ends = "both")) +
    geom_text(data = tibble(x = 0.235, y = 0.004),
              aes(x = x, y = y, label = "F[SA]"),
              parse = T,
              size = 6) +
      ylab("Force (mN)") +
      xlab("Time (s)") +
      scale_y_continuous(limits = c(0,0.05)) +
      # scale_color_manual(breaks = c("I"),
      #                    values = c("#FDFEFE")) +
      theme(legend.title = element_blank(),
            legend.text = element_blank(),
            axis.title.x = element_blank())
  )
(IIA_trace_gg <- trace_IIA %>%
    filter(Time < 0.2) %>%
    ggplot(aes(x = Time)) +
    geom_line(aes(y = Low_Fat),
              size = 1,
              linetype = "dotted") +
    geom_line(aes(y = High_Fat),
              size = 1,
              linetype = "longdash") +
    geom_line(aes(y = Active),
              size = 1,
              linetype = "solid")+
    geom_segment(x = 0.0215, y = 0,
                 xend = 0.0215, yend = 0.025,
                 arrow = arrow(length = unit(0.04, "npc"),
                               ends = "both")) +
    geom_segment(x = 0.0245, y = 0,
                xend = 0.0245, yend = 0.0345,
                arrow = arrow(length = unit(0.04, "npc"),
                              ends = "both"),
                linetype = "longdash") +
    geom_segment(x = 0.03, y = 0,
                 xend = 0.03, yend = 0.031,
                 arrow = arrow(length = unit(0.04, "npc"),
                               ends = "both"),
                 linetype = "dotted") +
    geom_text(data = tibble(x = 0.055, y = 0.005),
              aes(x = x, y = y, label = "F[SA]"),
              parse = T,
              size = 6) +
    ylab("Force (mN)") +
    xlab("Time (s)") +
    scale_y_continuous(limits = c(0,0.05)) +
    theme(legend.title = element_blank(),
          legend.text = element_blank())
)

(IIX_trace_gg <- trace_IIX %>%
    filter(Time < 0.1) %>%
    ggplot(aes(x = Time)) +
    geom_line(aes(y = Low_Fat),
              size = 1,
              linetype = "dotted") +
    geom_line(aes(y = High_Fat),
              size = 1,
              linetype = "longdash") +
    geom_line(aes(y = Active),
              size = 1,
              linetype = "solid")+
    geom_segment(x = 0.01, y = 0,
                 xend = 0.01, yend = 0.023,
                 arrow = arrow(length = unit(0.04, "npc"),
                               ends = "both")) +
    geom_segment(x = 0.017, y = 0,
                 xend = 0.017, yend = 0.0305,
                 arrow = arrow(length = unit(0.04, "npc"),
                               ends = "both"),
                 linetype = "longdash") +
    geom_segment(x = 0.015, y = 0,
                 xend = 0.015, yend = 0.0235,
                 arrow = arrow(length = unit(0.04, "npc"),
                               ends = "both"),
                 linetype = "dotted") +
    geom_text(data = tibble(x = 0.0315, y = 0.005),
              aes(x = x, y = y, label = "F[SA]"),
              parse = T,
              size = 6) +
    ylab("Force (mN)") +
    xlab("Time (s)") +
    scale_y_continuous(limits = c(0,0.04)) +
    theme(legend.title = element_blank(),
          legend.text = element_blank(),
          axis.title = element_blank())
)
(IIB_trace_gg <- trace_IIB %>%
    filter(Time < 0.075) %>%
    ggplot(aes(x = Time)) +
    geom_line(aes(y = Low_Fat),
              size = 1,
              linetype = "dotted") +
    geom_line(aes(y = High_Fat),
              size = 1,
              linetype = "longdash") +
    geom_line(aes(y = Active),
              size = 1,
              linetype = "solid")+
    geom_segment(x = 0.005, y = 0,
                 xend = 0.005, yend = 0.049,
                 arrow = arrow(length = unit(0.04, "npc"),
                               ends = "both")) +
    geom_segment(x = 0.006, y = 0,
                 xend = 0.006, yend = 0.055,
                 arrow = arrow(length = unit(0.04, "npc"),
                               ends = "both"),
                 linetype = "longdash") +
    geom_segment(x = 0.0075, y = 0,
                 xend = 0.0075, yend = 0.033,
                 arrow = arrow(length = unit(0.04, "npc"),
                               ends = "both"),
                 linetype = "dotted") +
    geom_text(data = tibble(x = 0.017, y = 0.005),
              aes(x = x, y = y, label = "F[SA]"),
              parse = T,
              size = 6) +
    ylab("Force (mN)") +
    xlab("Time (s)") +
    scale_y_continuous(limits = c(0,0.06)) +
    theme(legend.title = element_blank(),
          legend.text = element_blank(),
          axis.title.y = element_blank())
)

(traces_gg <- (I_trace_gg | IIX_trace_gg)  / (IIA_trace_gg | IIB_trace_gg) +
    plot_annotation(tag_levels = list(c('A','C','B', 'D')),
                    title = "Figure 4")
)

ggsave("Woods_Manuscript_scatterplot.pdf",
       traces_gg, width = 7, height = 7, units = "in",  dpi = 300)

## SA Traces: Active Only (All Fiber Types)-----------------------------------

active_data <- read_excel("Woods_EMM_2-14-23.xlsx",
                          sheet = "Active",
                          na = "") %>% 
  filter(Time<0.2)

(fat_gg <- ggplot(active_data,
                     aes(x = Time,
                         y = Active.nm,
                         col = Fiber_type))+
   geom_line(size = 1)+
   guides(col=guide_legend(title = "Fiber Type"))+
   scale_color_manual(breaks = c("I", "IIA","IIX", "IIB"),
                      values = c("#E69F00","#56B4E9", "#CC79A7","#009E73")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,55)) +
    theme(axis.title = element_blank(),
          axis.text  = element_text(size = 20),
          legend.title = element_blank(),
          legend.text = element_blank(),
          legend.key.size = unit(0,"cm"),
          axis.line = element_line(linewidth = 1),
          axis.ticks = element_line(linewidth = 1))
)


ggsave("Woods_Manuscript_ActiveTrace.jpeg",
       fat_gg, width = 11, height = 6, units = "cm",  dpi = 300)




## Fsa, FSD, F0 Graphs ---------------------------------------------------------------

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
             size = .5) +
    geom_point(data = raw_data_f0,
               aes(x = Exp_Con,
                   y = Po_Pre_Step,
                   shape = Exp_Con),
               size = .5,
               position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin=EMM - SE,
                      ymax=EMM + SE),
                  width=0.5,
                  size = .5,
                  position = position_dodge(width = 0.9)) +
    geom_text(data = tibble(x = 1.668, y = 275),
              aes(x = x, y = y, label = "*"),
              size = 5,
              inherit.aes = F) +
    geom_text(data = tibble(x = 1.885, y = 275),
              aes(x = x, y = y, label = "*"),
              size = 5,
              inherit.aes = F)+
    geom_text(data = tibble(x = 2.12, y = 275),
              aes(x = x, y = y, label = "*"),
              size = 5,
              inherit.aes = F)+
    geom_text(data = tibble(x = 2.35, y = 275),
              aes(x = x, y = y, label = "*"),
              size = 5,
              inherit.aes = F)+
    geom_text(data = tibble(x = 2.67, y = 275),
              aes(x = x, y = y, label = "*"),
              size = 5,
              inherit.aes = F)+
    geom_text(data = tibble(x = 2.88, y = 275),
              aes(x = x, y = y, label = "**"),
              size = 5,
              inherit.aes = F)+
    geom_text(data = tibble(x = 3.12, y = 275),
              aes(x = x, y = y, label = "**"),
              size = 5,
              inherit.aes = F)+
    geom_text(data = tibble(x = 3.36, y = 275),
              aes(x = x, y = y, label = "**"),
              size = 5,
              inherit.aes = F)+
    guides(fill=guide_legend(title = "Fiber Types")) +
    guides(shape = "none") +
    ylab(bquote(F[0])) + 
    scale_shape_manual(values = c(16,15,17)) +
    theme(axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "none") +
    scale_fill_manual(breaks = c("I","IIA","IIX","IIB"),
                      values = c("#FDFEFE" , "#D0D3D4", "#7B7D7D","#424949")) +
    scale_y_continuous(expand = c(0,0), limits = c(0,375)) +
    scale_x_discrete(breaks = c("Active",
                                "Fat_4.5",
                                "Fat_5.1"),
                     labels = c("Active",
                                expression(atop("High ",
                                                paste("Fatigue"))),
                                expression(atop("Low",
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
             size = .5) +
    geom_point(data = raw_data_sa,
               aes(x = Exp_Con,
                   y = Fsa,
                   shape = Exp_Con),
               size = .5,
               position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin=EMM - SE,
                      ymax=EMM + SE),
                  width=0.5,
                  size = 0.5,
                  position = position_dodge(width = 0.9)) +
    geom_text(data = tibble(x = 1.7, y = 75),
              aes(x = x, y = y, label = "*"),
              size = 5,
              inherit.aes = F)+
    geom_text(data = tibble(x = 3.33, y = 75),
              aes(x = x, y = y, label = "**"),
              size = 5,
              inherit.aes = F)+
    guides(fill=guide_legend(title = "Fiber Types")) +
    guides(shape = "none") +
    ylab(bquote(F[SA])) +
    scale_shape_manual(values = c(16,15,17)) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          legend.position = "top",
          legend.justification = "center",
          # legend.key.size = unit(.4,'cm'),
          # legend.title = element_text(size = 10),
          # legend.text = element_text(size = 8),
          axis.ticks.x = element_blank()) +
    scale_fill_manual(breaks = c("I","IIA","IIX","IIB"),
                      values = c("#FDFEFE" , "#D0D3D4", "#7B7D7D","#424949")) +
    scale_y_continuous(expand = c(0,0), limits = c(0,80)) +
    scale_x_discrete(breaks = c("Active",
                                "Fat_4.5",
                                "Fat_5.1"),
                     labels = c("Active",
                                expression(atop("High",
                                                paste("Fatigue"))),
                                expression(atop("Low",
                                                paste("Fatigue")))))
)

(Fsd <- my_data %>% 
    filter(Value == "Fsd") %>% 
    group_by(Exp_Con, fiber_type, fiber_type_num) %>% 
    ggplot(aes(x = Exp_Con,
               y = EMM,
               group = fiber_type_num)) + 
    geom_bar(aes(fill = fiber_type),
             color = "black",
             stat = "identity",
             position = position_dodge(),
             size = .5) +
    geom_point(data = raw_data_sd,
               aes(x = Exp_Con,
                   y = Fsd,
                   shape = Exp_Con),
               size = .5,
               position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin=EMM - SE,
                      ymax=EMM + SE),
                  width=0.5,
                  size = 0.5,
                  position = position_dodge(width = 0.9)) +
    # geom_text(data = tibble(x = 1.7, y = 75),
    #           aes(x = x, y = y, label = "*"),
    #           size = 5,
    #           inherit.aes = F)+
    geom_text(data = tibble(x = 3.25, y = 55),
              aes(x = x, y = y, label = "**"),
              size = 5,
              inherit.aes = F)+
    guides(fill=guide_legend(title = "Fiber Types")) +
    guides(shape = "none") +
    ylab(bquote(F[SD])) +
    scale_shape_manual(values = c(16,15,17)) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          legend.position = "none",
          axis.ticks.x = element_blank()) +
    scale_fill_manual(breaks = c("I","IIA","IIX","IIB"),
                      values = c("#FDFEFE" , "#D0D3D4", "#7B7D7D","#424949")) +
    scale_y_continuous(expand = c(0,0), limits = c(0,60)) +
    scale_x_discrete(breaks = c("Active",
                                "Fat_4.5",
                                "Fat_5.1"),
                     labels = c("Active",
                                expression(atop("High",
                                                paste("Fatigue"))),
                                expression(atop("Low",
                                                paste("Fatigue")))))
)

(Fsa.Fsd.F0 <- Fsa/Fsd/F0 + 
    plot_layout(ncol = 1) +
    plot_annotation(tag_levels = 'A',
                    title = "Figure 3"))

# (F0_Fsa_col <- Fsa_col/F0_col + plot_layout(ncol = 1, heights = c(6,6)))

ggsave("Woods_Manuscript_Fsa_F0.pdf",
       Fsa.Fsd.F0, width = 3.5, height = 6, units = "in",  dpi = 3000)

# ggsave("Woods_Manuscript_Fsa_F0.tiff",
#        F0_Fsa, width = 3.5, height = 5, units = "in",  dpi = 300)

# ggsave("Woods_Manuscript_Fsa_F0_col.jpeg",
#        F0_Fsa_col, width = 8, height = 10, units = "in",  dpi = 300)


## Fsa/F0 , FsdF0, Fsa/(Fsa+F0), Fsd/(Fsd+F0) ---------------------------------------------------

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
            size = .5) +
   geom_point(data = raw_data_sa,
              aes(x = Exp_Con,
                  y = FsaF0,
                  shape = Exp_Con),
              size = .5,
              position = position_dodge(width = 0.9)) +
   geom_errorbar(aes(ymin=EMM - SE,
                     ymax=EMM + SE),
                 width=0.5,
                 size = 0.5,
                 position = position_dodge(width = 0.9)) +
   geom_text(data = tibble(x = 1.7, y = 60),
             aes(x = x, y = y, label = "*"),
             size = 5,
             inherit.aes = F)+
   geom_text(data = tibble(x = 2, y = 60),
             aes(x = x, y = y, label = "*"),
             size = 5,
             inherit.aes = F)+
   geom_text(data = tibble(x = 2.3, y = 60),
             aes(x = x, y = y, label = "*"),
             size = 5,
             inherit.aes = F)+
   geom_text(data = tibble(x = 2.7, y = 60),
             aes(x = x, y = y, label = "*"),
             size = 5,
             inherit.aes = F)+
   geom_text(data = tibble(x = 3, y = 60),
             aes(x = x, y = y, label = "**"),
             size = 5,
             inherit.aes = F)+
   geom_text(data = tibble(x = 3.32, y = 60),
             aes(x = x, y = y, label = "**"),
             size = 5,
             inherit.aes = F)+
   guides(fill=guide_legend(title = "Fiber Types")) +
   guides(shape = "none") +
   ylab(bquote(F[SA]/F[0])) +
   scale_shape_manual(values = c(16,15,17)) +
   theme(axis.title.x = element_blank(),
         axis.text.x = element_blank(),
         legend.position = "top",
         legend.key.size = unit(.4,'cm'),
         legend.title = element_text(size = 10),
         legend.text = element_text(size = 8),
         axis.ticks.x = element_blank()) +
   scale_fill_manual(breaks = c("I","IIA","IIX","IIB"),
                     values = c("#FDFEFE" , "#D0D3D4", "#7B7D7D","#424949")) +
   scale_y_continuous(expand = c(0,0), limits = c(0,65)) +
   scale_x_discrete(breaks = c("Active",
                               "Fat_4.5",
                               "Fat_5.1"),
                    labels = c("Active",
                               expression(atop("High",
                                               paste("Fatigue"))),
                               expression(atop("Low",
                                               paste("Fatigue")))))
)

(FsdF0 <- my_data %>% 
    filter(Value == "FsdF0") %>% 
    group_by(Exp_Con, fiber_type, fiber_type_num) %>% 
    ggplot(aes(x = Exp_Con,
               y = EMM,
               group = fiber_type_num)) + 
    geom_bar(aes(fill = fiber_type),
             color = "black",
             stat = "identity",
             position = position_dodge(),
             size = .5) +
    geom_point(data = raw_data_sd,
               aes(x = Exp_Con,
                   y = FsdF0,
                   shape = Exp_Con),
               size = .5,
               position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin=EMM - SE,
                      ymax=EMM + SE),
                  width=0.5,
                  size = 0.5,
                  position = position_dodge(width = 0.9)) +
    geom_text(data = tibble(x = 1.7, y = 60),
              aes(x = x, y = y, label = "*"),
              size = 5,
              inherit.aes = F)+
    # geom_text(data = tibble(x = 2, y = 60),
    #           aes(x = x, y = y, label = "*"),
    #           size = 5,
    #           inherit.aes = F)+
    # geom_text(data = tibble(x = 2.3, y = 60),
    #           aes(x = x, y = y, label = "*"),
    #           size = 5,
    #           inherit.aes = F)+
    # geom_text(data = tibble(x = 2.7, y = 60),
    #           aes(x = x, y = y, label = "*"),
    #           size = 5,
    #           inherit.aes = F)+
    # geom_text(data = tibble(x = 3, y = 60),
    #           aes(x = x, y = y, label = "**"),
    #           size = 5,
    #           inherit.aes = F)+
    # geom_text(data = tibble(x = 3.32, y = 60),
    #           aes(x = x, y = y, label = "**"),
    #           size = 5,
    #           inherit.aes = F)+
    # guides(fill=guide_legend(title = "Fiber Types")) +
    guides(shape = "none") +
    ylab(bquote(F[SD]/F[0])) +
    scale_shape_manual(values = c(16,15,17)) +
    theme(axis.title.x = element_blank(),
          legend.position = "none",
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    scale_fill_manual(breaks = c("I","IIA","IIX","IIB"),
                      values = c("#FDFEFE" , "#D0D3D4", "#7B7D7D","#424949")) +
    scale_y_continuous(expand = c(0,0), limits = c(0,65)) +
    scale_x_discrete(breaks = c("Active",
                                "Fat_4.5",
                                "Fat_5.1"),
                     labels = c("Active",
                                expression(atop("High",
                                                paste("Fatigue"))),
                                expression(atop("Low",
                                                paste("Fatigue")))))
)

(FsaTotal <- my_data %>% 
    filter(Value == "FsaTotal") %>% 
    group_by(Exp_Con, fiber_type, fiber_type_num) %>% 
    ggplot(aes(x = Exp_Con,
               y = EMM,
               group = fiber_type_num)) + 
    geom_bar(aes(fill = fiber_type),
             color = "black",
             stat = "identity",
             position = position_dodge(),
             size = .5) +
    geom_point(data = raw_data_sa,
               aes(x = Exp_Con,
                   y = Fsa_total,
                   shape = Exp_Con),
               size = .5,
               position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin=EMM - SE,
                      ymax=EMM + SE),
                  width=0.5,
                  size = 0.5,
                  position = position_dodge(width = 0.9)) +
    geom_text(data = tibble(x = 1.7, y = 45),
              aes(x = x, y = y, label = "*"),
              size = 5,
              inherit.aes = F)+
    geom_text(data = tibble(x = 2, y = 45),
              aes(x = x, y = y, label = "*"),
              size = 5,
              inherit.aes = F)+
    geom_text(data = tibble(x = 2.3, y = 45),
              aes(x = x, y = y, label = "*"),
              size = 5,
              inherit.aes = F)+
    geom_text(data = tibble(x = 2.7, y = 45),
              aes(x = x, y = y, label = "*"),
              size = 5,
              inherit.aes = F)+
    geom_text(data = tibble(x = 3, y = 45),
              aes(x = x, y = y, label = "**"),
              size = 5,
              inherit.aes = F)+
    geom_text(data = tibble(x = 3.32, y = 45),
              aes(x = x, y = y, label = "**"),
              size = 5,
              inherit.aes = F)+
    guides(fill=guide_legend(title = "Fiber Types")) +
    guides(shape = "none") +
    ylab(bquote(F[SA]/(F[SA] + F[0]))) +
    scale_shape_manual(values = c(16,15,17)) +
    theme(axis.title.x = element_blank(),
          legend.position = "none",
          axis.ticks.x = element_blank()) +
    scale_fill_manual(breaks = c("I","IIA","IIX","IIB"),
                      values = c("#FDFEFE" , "#D0D3D4", "#7B7D7D","#424949")) +
    scale_y_continuous(expand = c(0,0), limits = c(0,50)) +
    scale_x_discrete(breaks = c("Active",
                                "Fat_4.5",
                                "Fat_5.1"),
                     labels = c("Active",
                                expression(atop("High",
                                                paste("Fatigue"))),
                                expression(atop("Low",
                                                paste("Fatigue")))))
)

(Fsa_FsaF0 <- FsaF0/FsaTotal +
    plot_layout(ncol = 1) +
    plot_annotation(tag_levels = 'A',
                    title = "Figure 3"))


ggsave("Woods_Manuscript_FsaTotal.pdf",
       Fsa_FsaF0, width = 3.5, height = 5, units = "in",  dpi = 3000)

## Fsa vs F0 scatterplot ---------------------------------------------------

active <- raw_data_sa %>% 
  mutate(iso = ifelse(fiber_type_num == 1,1,2)) %>% 
  filter(iso == 2) %>% 
  filter(Exp_Con == "Active")

fat_4.5 <- raw_data_sa %>% 
  mutate(iso = ifelse(fiber_type_num == 1,1,2)) %>% 
  filter(iso == 2) %>% 
  filter(Exp_Con == "Fat_4.5")

fat_5.1 <- raw_data_sa %>% 
  mutate(iso = ifelse(fiber_type_num == 1,1,2)) %>% 
  filter(iso == 2) %>% 
  filter(Exp_Con == "Fat_5.1")


active_lm <- lm(active$Fsa ~ active$Po_Pre_Step)
fat_4.5_lm <- lm(fat_4.5$Fsa ~ fat_4.5$Po_Pre_Step)
fat_5.1_lm <- lm(fat_5.1$Fsa ~ fat_5.1$Po_Pre_Step)

active$mdl <- predict(active_lm)
fat_4.5$mdl <- predict(fat_4.5_lm)
fat_5.1$mdl <- predict(fat_5.1_lm)


# lab1 <- c("Active r<sup>(2)</sup> = 0.572",
#           "High Fatigue r<sup>(2)</sup> = 0.699",
#           "Low Fatigue r<sup>(2)</sup> = 0.846")

(FsavF0.scatter <- raw_data_sa %>% 
    filter(fiber_type_num %in% c(2:4)) %>%
    ggplot(aes(x = Po_Pre_Step,
               y = Fsa)) +
    geom_point(aes(shape = Exp_Con)) +
    geom_line(data = active,
              aes(x = Po_Pre_Step,
                  y = mdl),
              linetype = "solid") +
    geom_line(data = fat_4.5,
              aes(x = Po_Pre_Step,
                  y = mdl),
              linetype = "longdash")+
    geom_line(data = fat_5.1,
              aes(x = Po_Pre_Step,
                  y = mdl),
              linetype = "dotted")+
    guides(shape=guide_legend("Experimental Condition"))+
    ylab(bquote(F[SA])) +
    xlab(bquote(F[0])) +
    scale_shape_manual(values = c(1,0,2)) +
    scale_x_continuous(limits = c(0,300)) +
    # scale_color_manual(breaks = c("I", "IIA","IIX", "IIB"),
    #                    values = c("#FDFEFE" , "#D0D3D4", "#7B7D7D","#424949")) +
    scale_y_continuous(limits = c(0,75)) +
    scale_shape_discrete(labels = c("Active r = 0.756",
                                    "High Fatigue r = 0.836",
                                    "Low Fatigue r = 0.920"))  +
    theme(legend.position = c(.15,.8))
)

FsavF0.scatter <- FsavF0.scatter +
  plot_annotation(title = "Figure 5")

ggsave("Woods_Manuscript_Scatter.pdf",
       FsavF0.scatter, width =7, height = 5, units = "in",  dpi = 300)

  

# (FsavsF0_scatter_col <- raw_data_sa %>% 
#     ggplot(aes(x = Po_Pre_Step,
#                y = Fsa)) +
#     geom_point(aes(shape = Exp_Con,
#                    col = fiber_type),
#                size = 3) +
#     geom_line(data = active,
#               aes(x = Po_Pre_Step,
#                   y = mdl),
#               size = 1,
#               linetype = "solid") +
#     geom_line(data = fat_4.5,
#               aes(x = Po_Pre_Step,
#                   y = mdl),
#               size = 1,
#               linetype = "longdash")+
#     geom_line(data = fat_5.1,
#               aes(x = Po_Pre_Step,
#                   y = mdl),
#               size = 1,
#               linetype = "dotdash")+
#     guides(shape=guide_legend("Experimental Condition"))+
#     ylab(bquote(F[SA])) + 
#     xlab(bquote(F[0])) +
#     scale_shape_manual(values = c(1,0,2)) +
#     scale_x_continuous(limits = c(0,300)) +
#     scale_color_manual(breaks = c("I", "IIA","IIX", "IIB"),
#                        values = c("#E69F00","#56B4E9", "#CC79A7","#009E73")) +
#     scale_y_continuous(limits = c(0,75)) +
#     scale_shape_discrete(labels = c("Active", "High Calcium Fatigue", "Low Calcium Fatigue")) +
#     theme(axis.title.y = element_blank(),
#           axis.title.x = element_blank(),
#           axis.text.y = element_text(size = 15),
#           axis.text.x = element_text(size = 15),
#           legend.position = "none",
#           axis.line = element_line(size = 1),
#           axis.ticks = element_line(size = 2))
#   
# )
#   
# ggsave("Woods_Manuscript_Scatter_col.jpeg",
#        FsavsF0_scatter_col, width =8, height = 4, units = "in",  dpi = 300)
  
## Rates -------------------------------------------------
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
            size = .5) +
   geom_point(data = raw_data_sa,
              aes(x = Exp_Con,
                  y = Fsa,
                  shape = Exp_Con),
              size = .5,
              position = position_dodge(width = 0.9)) +
   geom_errorbar(aes(ymin=EMM - SE,
                     ymax=EMM + SE),
                 width=0.5,
                 size = 0.5,
                 position = position_dodge(width = 0.9)) +
   # geom_text(data = tibble(x = 1.7, y = 75),
   #           aes(x = x, y = y, label = "*"),
   #           size = 5,
   #           inherit.aes = F)+
   guides(fill=guide_legend(title = "Fiber Types")) +
   guides(shape = "none") +
   ylab(bquote(F[SA])) +
   scale_shape_manual(values = c(16,15,17)) +
   # theme(axis.title.x = element_blank(),
   #       axis.text.x = element_blank(),
   #       legend.position = "top",
   #       legend.key.size = unit(.5,'cm')) +
   scale_fill_manual(breaks = c("I","IIA","IIX","IIB"),
                     values = c("#FDFEFE" , "#D0D3D4", "#7B7D7D","#424949")) +
   # scale_y_continuous(expand = c(0,0), limits = c(0,80)) +
   scale_x_discrete(breaks = c("Active",
                               "Fat_4.5",
                               "Fat_5.1"),
                    labels = c("Active",
                               expression(atop("High",
                                               paste("Fatigue"))),
                               expression(atop("Low",
                                               paste("Fatigue")))))
)