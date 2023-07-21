library(tidyverse)
library(readxl)
library(ggpattern)
library(patchwork)
library(ggbreak)
library(ggtext)
library(envalysis)
library(cowplot)
# library(interactions)
theme_set(theme_cowplot())


# setwd("C:/Users/Phil/Dropbox/Thesis- Stretch Activation/Data/Woods - Master's Thesis/Project/Tension + AaBbCc")
setwd("C:/Users/pcw00/Dropbox/University of Massachusetts Amherst/Thesis- Stretch Activation/Data/Woods - Master's Thesis/Project/Tension + AaBbCc")

raw_data_f0 <- read_excel("SA-Fatigue_Tension+Step+Kinetics_PW_5-19-23.xlsx", 
                       sheet = "Manuscript",
                       skip = 5,
                       na="") %>% 
  filter(Exp_Con_Num %in% c(2:4)) %>% 
  filter(fiber_type_num %in% c(1:4)) %>% 
  group_by(Exp_Con, fiber_type, fiber_type_num) 

  
my_data <- read_excel("Woods_EMM_7-18-23.xlsx",
                        sheet = "EMM.2")

raw_data <- read_excel("SA-Fatigue_Tension+Step+Kinetics_PW_5-19-23.xlsx", 
                        sheet = "Manuscript.2",
                        skip = 5,
                        na="") %>% 
  filter(Exp_Con_Num %in% c(2:4)) %>% 
  filter(fiber_type_num %in% c(1:4)) %>% 
  group_by(Exp_Con, fiber_type, fiber_type_num)

## Raw Trace: SA and SD---------------------------------------------------------

setwd("C:/Users/pcw00/Dropbox/University of Massachusetts Amherst/Thesis- Stretch Activation/Data/Woods - Master's Thesis/Project/Mouse 6/Fiber 5/Baseline")

sa <- read_excel("Run5.xlsx",
                 skip = 29) %>%
  select(Time, Force_One) %>% 
  mutate(Force_One = Force_One / 0.000868) 

pa <- read_excel("Run1.xlsx",
                 skip = 29) %>% 
  select(Force_One) %>% 
  rename(Force_Two = Force_One) %>% 
  mutate(Force_Two = Force_Two / 0.000868) 

traces <- data.frame(sa$Time,
                     sa$Force_One,
                     pa$Force_Two)

colnames(traces) <- c("Time",
                      "Active",
                      "Passive")

(trace <- traces %>% 
  filter(Time < 0.14) %>% 
  ggplot(aes(x = Time, col = "#EE6677")) +
  geom_line(aes(y = Active)) + 
  geom_line(aes(y = Passive, alpha = 0.5)) +
  labs(x = "Time (s)",
       y = bquote(Tension~(mN/mm^2))) + 
    theme(legend.position = 'none')+
    geom_segment(x = 0.08, y = 1,
                 xend = 0.08, yend = 34,
                 col = "black",
                 arrow = arrow(length = unit(0.04, "npc"),
                               ends = "both")) +
    geom_text(data = tibble(x = 0.09, y = 20),
                            aes(x = x, y = y, label = "F[SA]"),
                            parse = T, col = "black",
                            size = 6)+
    geom_text(data = tibble(x = 0.05, y = 25),
              aes(x = x, y=y, label = "Phase 1"),
              col = "black",
              size = 4) +
    geom_segment(x = 0.05, y=23,
                 xend = 0.06, yend = 18,
                 col = "black",
                 arrow = arrow(length = unit(0.02,"npc")))+
    geom_text(data = tibble(x = 0.085, y = 50),
              aes(x = x, y=y, label = "Phase 2"),
              col = "black",
              size = 4) +
    geom_segment(x = 0.08, y= 48,
                 xend = 0.072, yend = 44,
                 col = "black",
                 arrow = arrow(length = unit(0.02,"npc")))+
    geom_text(data = tibble(x = 0.1, y = 42),
              aes(x = x, y=y, label = "Phase 3"),
              col = "black",
              size = 4) +
    geom_segment(x = 0.09, y=40,
                 xend = 0.081, yend = 37,
                 col = "black",
                 arrow = arrow(length = unit(0.02,"npc")))+
    geom_text(data = tibble(x = 0.13, y = 30),
              aes(x = x, y=y, label = "Phase 4"),
              col = "black",
              size = 4) +
    geom_segment(x = 0.13, y=28,
                 xend = 0.12, yend = 25,
                 col = "black",
                 arrow = arrow(length = unit(0.02,"npc")))
) 

# trace <- read_excel("Run5.xlsx",
#                     skip = 29) %>% 
#   select(Time, Force_One) %>% 
#   filter(Time > 0.05 & Time < 2.6) %>%
#   mutate(Force_One = ifelse(Time >= 2.5,
#                             Force_One + 0.005086365,
#                             Force_One))

# (trace_gg <- ggplot(data = trace, aes(x = Time, y = Force_One)) +
#     geom_line() +
#     scale_x_break(c(0.10,2.55)) +
#     ylab("Force (mN)") +
#     xlab("Time (s)") +
#     geom_segment(x = 0.08, y = 0.0,
#                  xend = 0.08, yend = 0.034,
#                  arrow = arrow(length = unit(0.035, "npc"),
#                                ends = "both")) +
#     geom_segment(x = 2.575, y = 0.0,
#                  xend = 2.575, yend = -0.03,
#                  arrow = arrow(length = unit(0.035, "npc"),
#                                ends = "both")) +
#     geom_text(data = tibble(x = 0.083, y = 0.015),
#               aes(x = x, y = y, label = "F[SA]"),
#               parse = T,
#               size = 6) +
#     geom_text(data = tibble(x = 2.578, y = -0.02),
#               aes(x = x, y = y, label = "F[SD]"),
#               parse = T,
#               size = 6) +
#     theme_bw() +
#     theme(panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank())
# )

trace_gg <- trace +
  plot_annotation(title = "Figure 1")

ggsave("Woods_Manuscript_RawStretchTrace.pdf",
       trace_gg, width = 5, height = 5, units = "in",  dpi = 3000)

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

active_data <- read_excel("Woods_EMM_7-18-23.xlsx",
                          sheet = "Active",
                          na = "") %>% 
  filter(Time<0.2)

(act.trace <- ggplot(active_data,
                    aes(x = Time,
                        y = Active.nm,
                        group = Fiber_type)) +
  geom_line(size =0.4,
            aes(col = Fiber_type)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,60)) +
  geom_segment(x = 0.032, y=1,
                 xend = 0.032, yend = 26.5,
                 arrow = arrow(length = unit(0.02,"npc"),
                               ends = "both"),
               col = "#66ccee",
               size = 0.3) +
  geom_segment(x = 0.018, y=1,
                 xend = 0.018, yend = 25.5,
                 arrow = arrow(length = unit(0.02,"npc"),
                               ends = "both"),
                 col = "#ee6677",
               size = 0.3) + 
    geom_segment(x = 0.01, y=1,
                 xend = 0.01, yend = 28.5,
                 arrow = arrow(length = unit(0.02,"npc"),
                               ends = "both"),
                 col = "#228833",
                 size = 0.3) +
  geom_segment(x = 0.181, y=1,
                 xend = 0.181, yend = 5.5,
                 arrow = arrow(length = unit(0.02,"npc"),
                               ends = "both"),
               col = "#ccbb44",
               size = 0.3) +
    scale_color_manual(breaks = c("I","IIA","IIX","IIB"),
                      values = c("#ccbb44" , "#66ccee", "#ee6677","#228833")) +
    # scale_color_discrete(labels = c(bquote(I~t[3]~=~0.181),
    #                                 bquote(IIA~t[3]~=~0.032),
    #                                 bqoute(IIX~t[3]~=~0.018),
    #                                 bqoute(IIX~t[3]~=~0.001))) +
  guides(color=guide_legend(title = "Fiber Type",
                            byrow = T))+
  scale_x_continuous(n.break = 10)+
  labs(x = "Time (s)",
       y = bquote(Tension~(mN/mm^2))) +
    theme(legend.position = c(.8,.8),
          legend.spacing.y = unit(0.8, 'mm'),
          legend.title = element_text(size = 6),
          legend.text = element_text(size = 6),
          legend.key.size = unit(.5,'cm'),
          axis.text = element_text(size = 8),
          axis.title = element_text(size =10))
)

act.trace <- act.trace +
  plot_annotation(title = "Figure 5")


ggsave("Woods_Manuscript_ActiveTrace.pdf",
       act.trace, width = 9, height = 7, units = "cm",  dpi = 3000)



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
               size = .4,
               position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin=EMM - SE,
                      ymax=EMM + SE),
                  width=0.5,
                  size = .4,
                  position = position_dodge(width = 0.9)) +
    geom_text(data = tibble(x = 1.668, y = 300),
              aes(x = x, y = y, label = "*"),
              size = 4,
              col = 'red',
              inherit.aes = F) +
    geom_text(data = tibble(x = 1.885, y = 300),
              aes(x = x, y = y, label = "*"),
              size = 4,
              col = 'red',
              inherit.aes = F)+
    geom_text(data = tibble(x = 2.12, y = 300),
              aes(x = x, y = y, label = "*"),
              size = 4,
              col = 'red',
              inherit.aes = F)+
    geom_text(data = tibble(x = 2.35, y = 300),
              aes(x = x, y = y, label = "*"),
              size = 4,
              col = 'red',
              inherit.aes = F)+
    geom_text(data = tibble(x = 2.67, y = 300),
              aes(x = x, y = y, label = "*"),
              size = 4,
              col = 'red',
              inherit.aes = F)+
    geom_text(data = tibble(x = 2.88, y = 300),
              aes(x = x, y = y, label = "*^"),
              size = 4,
              col = 'red',
              inherit.aes = F)+
    geom_text(data = tibble(x = 3.12, y = 300),
              aes(x = x, y = y, label = "*^"),
              size = 4,
              col = 'red',
              inherit.aes = F)+
    geom_text(data = tibble(x = 3.36, y = 300),
              aes(x = x, y = y, label = "*^"),
              size = 4,
              col = 'red',
              inherit.aes = F)+
    guides(shape = "none") +
    ylab(bquote(F[0]~(mN/mm^2))) + 
    scale_shape_manual(values = c(21,22,24)) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = 10),
          axis.text = element_text(size = 8),
          axis.ticks.x = element_blank(),
          legend.position = "none") +
    scale_fill_manual(breaks = c("I","IIA","IIX","IIB"),
                      values = c("#ccbb44" , "#66ccee", "#ee6677","#228833")) +
    scale_y_continuous(expand = c(0,0), limits = c(0,375)) +
    scale_x_discrete(breaks = c("Active",
                                "Fat_4.5",
                                "Fat_5.1"),
                     labels = c("Active",
                                expression(atop(textstyle(High ~ Ca^"2+"),
                                                atop(textstyle('Fatigue'),
                                                         NA))),
                                expression(atop(textstyle(Low ~ Ca^"2+"), 
                                                atop(textstyle('Fatigue'),
                                                                        NA)))
                                ))
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
             position = position_dodge(width = 0.9),
             size = .5) +
    geom_point(data = raw_data,
               aes(x = Exp_Con,
                   y = Fsa,
                   shape = ifelse(Fsa > 0, Exp_Con, NA)),
               size = .4,
               alpha = 0.75,
               position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin=EMM - SE,
                      ymax= ifelse(EMM + SE < 5,0,EMM + SE)),
                  width=0.5,
                  size = 0.4,
                  position = position_dodge(width = 0.9)) +
    geom_text(data = tibble(x = 1.88, y = 75),
              aes(x = x, y = y, label = "*"),
              size = 4,
              col = 'red',
              inherit.aes = F)+
    geom_text(data = tibble(x = 3.33, y = 75),
              aes(x = x, y = y, label = "*^"),
              size = 4,
              col = 'red',
              inherit.aes = F)+
    guides(fill=guide_legend(title = "Fiber Types")) +
    guides(shape = "none") +
    ylab(bquote(F[SA]~(mN/mm^2))) +
    scale_shape_manual(values = c(21,22,24)) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 8),
          axis.title.y = element_text(size = 10),
          legend.position = "top",
          legend.justification = "center",
          legend.key.size = unit(.4,'cm'),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 10),
          axis.ticks.x = element_blank()) +
    scale_fill_manual(breaks = c("I","IIA","IIX","IIB"),
                      values = c("#ccbb44" , "#66ccee", "#ee6677","#228833")) +
    scale_y_continuous(expand = c(0,0), limits = c(0,80)) +
    scale_x_discrete(breaks = c("Active",
                                "Fat_4.5",
                                "Fat_5.1"),
                     labels = c("Active",
                                expression(atop(textstyle(High ~ Ca^"2+"),
                                                atop(textstyle('Fatigue'),
                                                     NA))),
                                expression(atop(textstyle(Low ~ Ca^"2+"), 
                                                atop(textstyle('Fatigue'),
                                                     NA)))
                     ))
)

# (Fsd <- my_data %>% 
#     filter(Value == "Fsd") %>% 
#     group_by(Exp_Con, fiber_type, fiber_type_num) %>% 
#     ggplot(aes(x = Exp_Con,
#                y = EMM,
#                group = fiber_type_num)) + 
#     geom_bar(aes(fill = fiber_type),
#              color = "black",
#              stat = "identity",
#              position = position_dodge(),
#              size = .5) +
#     geom_point(data = raw_data,
#                aes(x = Exp_Con,
#                    y = Fsd,
#                    shape = ifelse(Fsd > 0, Exp_Con, NA)),
#                size = .5,
#                position = position_dodge(width = 0.9)) +
#     geom_errorbar(aes(ymin=EMM - SE,
#                       ymax=EMM + SE),
#                   width=0.5,
#                   size = 0.5,
#                   position = position_dodge(width = 0.9)) +
#     # geom_text(data = tibble(x = 1.7, y = 75),
#     #           aes(x = x, y = y, label = "*"),
#     #           size = 5,
#     #           inherit.aes = F)+
#     geom_text(data = tibble(x = 3.35, y = 55),
#               aes(x = x, y = y, label = "**"),
#               size = 5,
#               inherit.aes = F)+
#     guides(fill=guide_legend(title = "Fiber Types")) +
#     guides(shape = "none") +
#     ylab(bquote(F[SD])) +
#     scale_shape_manual(values = c(21,22,24)) +
#     theme(axis.title.x = element_blank(),
#           axis.text.x = element_blank(),
#           legend.position = "none",
#           axis.ticks.x = element_blank()) +
#     scale_fill_manual(breaks = c("I","IIA","IIX","IIB"),
#                       values = c("#FDFEFE" , "#D0D3D4", "#7B7D7D","#424949")) +
#     scale_y_continuous(expand = c(0,0), limits = c(0,80)) +
#     scale_x_discrete(breaks = c("Active",
#                                 "Fat_4.5",
#                                 "Fat_5.1"),
#                      labels = c("Active",
#                                 expression(atop(textstyle("High"), atop(textstyle('Fatigue'),
#                                                                         NA))),
#                                 expression(atop(textstyle("Low"), atop(textstyle('Fatigue'),
#                                                                        NA)))
#                                 ))
# )

# (Fsa.Fsd.F0 <- Fsa/Fsd/F0 + 
#     plot_layout(ncol = 1) +
#     plot_annotation(tag_levels = 'A',
#                     title = "Figure 3"))

(Fsa.F0 <- Fsa/F0 +
    plot_layout(ncol = 1) +
    plot_annotation(tag_levels = 'A',
                    title = "Figure 2"))

# (F0_Fsa_col <- Fsa_col/F0_col + plot_layout(ncol = 1, heights = c(6,6)))

ggsave("Woods_Manuscript_Fsa_F0.pdf",
       Fsa.F0, width = 3.5, height = 6, units = "in",  dpi = 3000)

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
   geom_point(data = raw_data,
              aes(x = Exp_Con,
                  y = FsaF0,
                  shape = ifelse(FsaF0 <1, NA,Exp_Con)),
              size = .4,
              position = position_dodge(width = 0.9)) +
   geom_errorbar(aes(ymin=EMM - SE,
                     ymax= ifelse(EMM + SE <2, 0, EMM + SE)),
                 width=0.5,
                 size = 0.4,
                 position = position_dodge(width = 0.9)) +
   geom_text(data = tibble(x = 1.885, y = 60),
             aes(x = x, y = y, label = "*"),
             size = 4,
             col = 'red',
             inherit.aes = F)+
   geom_text(data = tibble(x = 2.115, y = 60),
             aes(x = x, y = y, label = "*"),
             size = 4,
             col = 'red',
             inherit.aes = F)+
   geom_text(data = tibble(x = 2.34, y = 60),
             aes(x = x, y = y, label = "*"),
             size = 4,
             col = 'red',
             inherit.aes = F)+
   geom_text(data = tibble(x = 2.89, y = 60),
             aes(x = x, y = y, label = "*"),
             size = 4,
             col = 'red',
             inherit.aes = F)+
   geom_text(data = tibble(x = 3.115, y = 60),
             aes(x = x, y = y, label = "*^"),
             size = 4,
             col = 'red',
             inherit.aes = F)+
   geom_text(data = tibble(x = 3.335, y = 60),
             aes(x = x, y = y, label = "*^"),
             size = 4,
             col = 'red',
             inherit.aes = F)+
   guides(fill=guide_legend(title = "Fiber Types"),
          shape = "none") +
   ylab(bquote(F[SA]/F[0]~("%"))) +
   scale_shape_manual(values = c(21,22,24)) +
   theme(axis.title.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.y = element_text(size = 10),
         axis.text.y = element_text(size = 8),
         legend.position = "top",
         legend.key.size = unit(.4,'cm'),
         legend.title = element_text(size = 10),
         legend.text = element_text(size = 8),
         legend.justification = "center",
         axis.ticks.x = element_blank()) +
   scale_fill_manual(breaks = c("I","IIA","IIX","IIB"),
                     values = c("#ccbb44" , "#66ccee", "#ee6677","#228833")) +
   scale_y_continuous(expand = c(0,0), limits = c(0,65)) +
   scale_x_discrete(breaks = c("Active",
                               "Fat_4.5",
                               "Fat_5.1"),
                    labels = c("Active",
                               expression(atop(textstyle(High ~ Ca^"2+"),
                                               atop(textstyle('Fatigue'),
                                                    NA))),
                               expression(atop(textstyle(Low ~ Ca^"2+"), 
                                               atop(textstyle('Fatigue'),
                                                    NA)))
                    ))
)

# (FsdF0 <- my_data %>% 
#     filter(Value == "FsdF0") %>% 
#     group_by(Exp_Con, fiber_type, fiber_type_num) %>% 
#     ggplot(aes(x = Exp_Con,
#                y = EMM,
#                group = fiber_type_num)) + 
#     geom_bar(aes(fill = fiber_type),
#              color = "black",
#              stat = "identity",
#              position = position_dodge(),
#              size = .5) +
#     geom_point(data = raw_data,
#                aes(x = Exp_Con,
#                    y = FsdF0,
#                    shape = ifelse(FsdF0 <1,NA,Exp_Con)),
#                size = .5,
#                position = position_dodge(width = 0.9)) +
#     geom_errorbar(aes(ymin=EMM - SE,
#                       ymax=EMM + SE),
#                   width=0.5,
#                   size = 0.5,
#                   position = position_dodge(width = 0.9)) +
#     geom_text(data = tibble(x = 2.117, y = 55),
#               aes(x = x, y = y, label = "*"),
#               size = 5,
#               inherit.aes = F)+
#     geom_text(data = tibble(x = 2.34, y = 55),
#               aes(x = x, y = y, label = "*"),
#               size = 5,
#               inherit.aes = F)+
#     geom_text(data = tibble(x = 3.12, y = 55),
#               aes(x = x, y = y, label = "*"),
#               size = 5,
#               inherit.aes = F)+
#     guides(shape = "none") +
#     ylab(bquote(F[SD]/F[0])) +
#     scale_shape_manual(values = c(21,22,24)) +
#     theme(axis.title.x = element_blank(),
#           legend.position = "none",
#           axis.ticks.x = element_blank()) +
#     scale_fill_manual(breaks = c("I","IIA","IIX","IIB"),
#                       values = c("#FDFEFE" , "#D0D3D4", "#7B7D7D","#424949")) +
#     scale_y_continuous(expand = c(0,0), limits = c(0,65)) +
#     scale_x_discrete(breaks = c("Active",
#                                 "Fat_4.5",
#                                 "Fat_5.1"),
#                      labels = c("Active",
#                                 expression(atop(textstyle("High"), atop(textstyle('Fatigue'),
#                                                                         NA))),
#                                 expression(atop(textstyle("Low"), atop(textstyle('Fatigue'),
#                                                                        NA)))
#                                 ))
# )

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
    geom_point(data = raw_data,
               aes(x = Exp_Con,
                   y = Fsa_total,
                   shape = ifelse(Fsa_total <1,NA,Exp_Con)),
               size = .4,
               position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin=EMM - SE,
                      ymax= ifelse(EMM + SE <4,NA,EMM + SE)),
                  width=0.5,
                  size = 0.4,
                  position = position_dodge(width = 0.9)) +
    geom_text(data = tibble(x = 1.89, y = 40),
              aes(x = x, y = y, label = "*"),
              size = 4,
              col = 'red',
              inherit.aes = F)+
    geom_text(data = tibble(x = 2.12, y = 40),
              aes(x = x, y = y, label = "*"),
              size = 4,
              col = 'red',
              inherit.aes = F)+
    geom_text(data = tibble(x = 2.35, y = 40),
              aes(x = x, y = y, label = "*"),
              size = 4,
              col = 'red',
              inherit.aes = F)+
    geom_text(data = tibble(x = 2.89, y = 40),
              aes(x = x, y = y, label = "*"),
              size = 4,
              col = 'red',
              inherit.aes = F)+
    geom_text(data = tibble(x = 3.11, y = 40),
              aes(x = x, y = y, label = "*^"),
              size = 4,
              col = 'red',
              inherit.aes = F)+
    geom_text(data = tibble(x = 3.33, y = 40),
              aes(x = x, y = y, label = "*^"),
              size = 4,
              col = 'red',
              inherit.aes = F)+
    guides(fill=guide_legend(title = "Fiber Types"),
           shape = "none") +
    ylab(bquote(F[SA]/(F[SA] + F[0])~('%'))) +
    scale_shape_manual(values = c(21,22,24)) +
    theme(axis.title.x = element_blank(),
          axis.text = element_text(size = 8),
          axis.title.y = element_text(size = 10),
          legend.position = "none",
          axis.ticks.x = element_blank()) +
    scale_fill_manual(breaks = c("I","IIA","IIX","IIB"),
                      values = c("#ccbb44" , "#66ccee", "#ee6677","#228833")) +
    scale_y_continuous(expand = c(0,0), limits = c(0,45)) +
    scale_x_discrete(breaks = c("Active",
                                "Fat_4.5",
                                "Fat_5.1"),
                     labels = c("Active",
                                expression(atop(textstyle(High ~ Ca^"2+"),
                                                atop(textstyle('Fatigue'),
                                                     NA))),
                                expression(atop(textstyle(Low ~ Ca^"2+"), 
                                                atop(textstyle('Fatigue'),
                                                     NA)))
                     ))
)

# (FsdTotal <- my_data %>% 
#     filter(Value == "FsdTotal") %>% 
#     group_by(Exp_Con, fiber_type, fiber_type_num) %>% 
#     ggplot(aes(x = Exp_Con,
#                y = EMM,
#                group = fiber_type_num)) + 
#     geom_bar(aes(fill = fiber_type),
#              color = "black",
#              stat = "identity",
#              position = position_dodge(),
#              size = .5) +
#     geom_point(data = raw_data,
#                aes(x = Exp_Con,
#                    y = Fsd_total,
#                    shape = ifelse(Fsd_total <1, NA,Exp_Con)),
#                size = .5,
#                position = position_dodge(width = 0.9)) +
#     geom_errorbar(aes(ymin=EMM - SE,
#                       ymax=EMM + SE),
#                   width=0.5,
#                   size = 0.5,
#                   position = position_dodge(width = 0.9)) +
#     geom_text(data = tibble(x = 2.125, y = 38),
#               aes(x = x, y = y, label = "*"),
#               size = 5,
#               inherit.aes = F)+
#     geom_text(data = tibble(x = 2.352, y = 38),
#               aes(x = x, y = y, label = "*"),
#               size = 5,
#               inherit.aes = F)+
#     geom_text(data = tibble(x = 3.13, y = 38),
#               aes(x = x, y = y, label = "*"),
#               size = 5,
#               inherit.aes = F)+
#     guides(fill=guide_legend(title = "Fiber Types")) +
#     guides(shape = "none") +
#     ylab(bquote(F[SD]/(F[SD] + F[0]))) +
#     scale_shape_manual(values = c(21,22,24)) +
#     theme(axis.title.x = element_blank(),
#           legend.position = "none",
#           axis.ticks.x = element_blank()) +
#     scale_fill_manual(breaks = c("I","IIA","IIX","IIB"),
#                       values = c("#FDFEFE" , "#D0D3D4", "#7B7D7D","#424949")) +
#     scale_y_continuous(expand = c(0,0), limits = c(0,40)) +
#     scale_x_discrete(breaks = c("Active",
#                                 "Fat_4.5",
#                                 "Fat_5.1"),
#                      labels = c("Active",
#                                 expression(atop(textstyle("High"), atop(textstyle('Fatigue'),
#                                                                         NA))),
#                                 expression(atop(textstyle("Low"), atop(textstyle('Fatigue'),
#                                                                        NA)))
#                                 ))
# )

# (FsaF0.FsdF0.Fsatotal.Fsdtotal <- (FsaF0 | FsaTotal)/ ( FsdF0| FsdTotal) +
#     plot_layout(ncol = 1) +
#     plot_annotation(tag_levels = 'A',
#                     title = "Figure 4"))

(FsaF0.FsaTotal <- FsaF0 / FsaTotal +
  plot_layout(ncol = 1) +
  plot_annotation(tag_levels = "A",
                  title = "Figure 3"))

ggsave("Woods_Manuscript_Fig3.pdf",
       FsaF0.FsaTotal,
       width = 3.5, height = 6, units = "in",  dpi = 5000)

## Fsa vs F0 scatterplot ---------------------------------------------------

scatter_data <- read_excel("SA-Fatigue_Tension+Step+Kinetics_PW_5-19-23.xlsx", 
                           sheet = "Manuscript",
                           skip = 5,
                           na="") %>% 
  filter(Exp_Con_Num %in% c(2:4)) %>% 
  filter(fiber_type_num %in% c(1:4)) %>% 
  group_by(Exp_Con, fiber_type, fiber_type_num)

active <- scatter_data %>% 
  mutate(iso = ifelse(fiber_type_num == 1,1,2)) %>% 
  filter(iso == 2) %>% 
  filter(Exp_Con == "Active")

fat_4.5 <- scatter_data %>% 
  mutate(iso = ifelse(fiber_type_num == 1,1,2)) %>% 
  filter(iso == 2) %>% 
  filter(Exp_Con == "Fat_4.5")

fat_5.1 <- scatter_data %>% 
  mutate(iso = ifelse(fiber_type_num == 1,1,2)) %>% 
  filter(iso == 2) %>% 
  filter(Exp_Con == "Fat_5.1")


active_lm <- lm(active$Fsa ~ active$Po_Pre_Step)
fat_4.5_lm <- lm(fat_4.5$Fsa ~ fat_4.5$Po_Pre_Step)
fat_5.1_lm <- lm(fat_5.1$Fsa ~ fat_5.1$Po_Pre_Step)

active$mdl <- predict(active_lm)
fat_4.5$mdl <- predict(fat_4.5_lm)
fat_5.1$mdl <- predict(fat_5.1_lm)


(act.scatter  <- scatter_data %>% 
    filter(Exp_Con == "Active") %>% 
    ggplot(aes(x = Po_Pre_Step,
               y = Fsa)) +
    geom_point(aes(shape = Exp_Con,
                   fill = fiber_type),
               color = 'black') +
    geom_line(data = active,
              aes(x = Po_Pre_Step,
                  y = mdl),
              linetype = "solid") +
    geom_richtext(data = tibble(x = 300, y = 70),
                  aes(x = x, y = y, 
                      label = paste0("Active")),
                  fill = NA, 
                  label.color = NA,
                  label.padding = grid::unit(rep(0, 4), "pt"))+
    guides(shape= "none")+
    guides(fill = guide_legend(override.aes = list(shape = 22),
                               "Fiber Types")) +
    ylab(bquote(F[SA]~(mN/mm^2))) +
    # xlab(bquote(F[0])) +
    scale_x_continuous(limits = c(0,350)) +
    scale_y_continuous(limits = c(0,75)) +
    scale_shape_manual(values = c(21),
                         labels = c("Active r = 0.756"))  +
    scale_fill_manual(labels = c("I","IIA","IIX","IIB"),
                      values = c("#ccbb44" , "#66ccee", "#ee6677","#228833")) +
    theme(axis.title.x = element_blank(),
          legend.position = c(.17,.9),
          legend.key.size = unit(.4,'cm'),
          axis.text = element_text(size = 8),
          axis.title.y = element_text(size = 10),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8))
)

(fat4.5.scatter  <-scatter_data %>% 
    filter(Exp_Con_Num == 3) %>% 
    filter(fiber_type_num %in% c(2:4)) %>% 
    ggplot(aes(x = Po_Pre_Step,
               y = Fsa)) +
    geom_point(aes(shape = ifelse(Fsa == 0,NA,Exp_Con),
                   fill = fiber_type),
               color = 'black') +
    geom_line(data = fat_4.5,
              aes(x = Po_Pre_Step,
                  y = mdl),
              linetype = "longdash")+
    geom_richtext(data = tibble(x = 300, y = 70),
                  aes(x = x, y = y, 
                      label = paste0("High Ca<sup>2+</sup> <br> Fatigue")),
                  fill = NA, 
                  label.color = NA,
                  label.padding = grid::unit(rep(0, 4), "pt")) +
    guides(shape= "none",
           fill = "none")+
    scale_x_continuous(limits = c(0,350)) +
    scale_y_continuous(limits = c(0,75)) +
    scale_shape_manual(values = c(22),
                       labels = c("High Fatigue r = 0.836"))  +
    scale_fill_manual(labels = c("IIA","IIX","IIB"),
                      values = c("#66ccee", "#ee6677","#228833")) +
    theme(axis.title = element_blank(),
          legend.position = c(.17,.9),
          legend.key.size = unit(.4,'cm'),
          legend.title = element_text(size = 10),
          axis.text = element_text(size = 8),
          legend.text = element_text(size = 8))
)

(fat5.1.scatter  <- scatter_data %>% 
    filter(Exp_Con_Num == 2) %>% 
    filter(fiber_type_num %in% c(2:4)) %>% 
    ggplot(aes(x = Po_Pre_Step,
               y = Fsa)) +
    geom_point(aes(shape = Exp_Con,
                   fill = fiber_type),
               color = 'black') +
    geom_line(data = fat_5.1,
              aes(x = Po_Pre_Step,
                  y = mdl),
              linetype = "dotted")+
    geom_richtext(data = tibble(x = 300, y = 70),
                  aes(x = x, y = y, 
                      label = paste0("Low Ca<sup>2+</sup> <br> Fatigue")),
                  fill = NA, 
                  label.color = NA,
                  label.padding = grid::unit(rep(0, 4), "pt"))+
    guides(shape= "none",
           fill = "none")+
    ylab(bquote(F[SA]~(mN/mm^2))) +
    xlab(bquote(F[0]~(mN/mm^2))) +
    scale_x_continuous(limits = c(0,350)) +
    scale_y_continuous(limits = c(0,75)) +
    scale_shape_manual(values = c(24),
                       labels = c("Low Fatigue r = 0.920"))  +
    scale_fill_manual(labels = c("IIA","IIX","IIB"),
                      values = c("#66ccee", "#ee6677","#228833")) +
    theme(legend.position = c(.17,.9),
          legend.key.size = unit(.4,'cm'),
          legend.title = element_text(size = 10),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 8),
          legend.text = element_text(size = 8))
)


(FsavF0.scatter <- scatter_data %>% 
    ggplot(aes(x = Po_Pre_Step,
               y = Fsa)) +
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
    geom_richtext(data = tibble(x = 100, y = 68),
                  aes(x = x, y = y, 
                      label = paste0("r<sup>2</sup> = 0.846")),
                  fill = NA, 
                  label.color = NA,
                  label.padding = grid::unit(rep(0, 4), "pt"))+
    geom_richtext(data = tibble(x = 300, y = 70),
                  aes(x = x, y = y, 
                      label = paste0("r<sup>2</sup> = 0.699")),
                  fill = NA, 
                  label.color = NA,
                  label.padding = grid::unit(rep(0, 4), "pt"))+
    geom_richtext(data = tibble(x = 300, y = 30),
                  aes(x = x, y = y, 
                      label = paste0("r<sup>2</sup> = 0.572")),
                  fill = NA, 
                  label.color = NA,
                  label.padding = grid::unit(rep(0, 4), "pt"))+
    guides(shape = "none",
           fill = "none")+
    ylab(bquote(F[SA]~(mN/mm^2))) +
    xlab(bquote(F[0]~(mN/mm^2))) +
    scale_x_continuous(limits = c(0,350)) +
    scale_y_continuous(limits = c(0,75)) +
    scale_shape_manual(values = c(21,22,24),
                         labels = c("Active r = 0.756",
                                    "High Fatigue r = 0.836",
                                    "Low Fatigue r = 0.920"))  +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_text(size = 10),
          axis.text = element_text(size = 8),
          legend.position = c(.17,.9),
          legend.key.size = unit(.4,'cm'),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8))
)

(scattergg <- (act.scatter | fat4.5.scatter ) / (fat5.1.scatter|FsavF0.scatter) +
  plot_annotation(title = "Figure 4",
                  tag_levels = 'A')
)

ggsave("Woods_Manuscript_FsavF0_scatter.pdf",
       scattergg, width =7, height = 7, units = "in",  dpi = 3000)

## t3 -------------------------------------------------

(t3 <- my_data %>%
   filter(Value == "t3") %>%
   mutate(EMM = EMM *1000) %>% 
   mutate(SE = SE *1000) %>% 
   group_by(Exp_Con, fiber_type, fiber_type_num) %>%
   ggplot(aes(x = Exp_Con,
              y = EMM,
              group = fiber_type_num)) +
   scale_y_cut(breaks = 60, which = c(1,2), scales = c(1,2)) +
   # scale_y_break(c(0.06,0.1)) +
   geom_bar(aes(fill = fiber_type),
            color = "black",
            stat = "identity",
            position = position_dodge(),
            size = .5) +
   geom_point(data = raw_data,
              aes(x = Exp_Con,
                  y = sa.t3 *1000,
                  shape = Exp_Con),
              size = .5,
              position = position_dodge(width = 0.9)) +
   geom_errorbar(aes(ymin=EMM - SE,
                     ymax=EMM + SE),
                 width=0.5,
                 size = .4,
                 position = position_dodge(width = 0.9)) +
   geom_text(data = tibble(x = 1.89, y = 100),
             aes(x = x, y = y, label = "*"),
             size = 4,
             col = "red",
             inherit.aes = F)+
   geom_text(data = tibble(x = 3.36, y = 100),
             aes(x = x, y = y, label = "*^"),
             size = 4,
             col = "red",
             inherit.aes = F)+
   guides(shape = "none",
          fill = guide_legend(title = "Fiber Types")) +
   ylab(bquote(t[3]~(ms))) +
   scale_shape_manual(values = c(21,22,24)) +
   theme_bw()+
   theme(axis.title.x = element_blank(),
         axis.title.y = element_text(size = 10),
         axis.text = element_text(size = 8),
         axis.ticks.x = element_blank(),
         legend.position = "top",
         legend.key.size = unit(.4,'cm'),
         legend.title = element_text(size = 10),
         legend.text = element_text(size = 8),
         legend.justification = "center",
         panel.border = element_blank(),
         panel.grid = element_blank(),
         axis.line = element_line(colour = "black")) +
   scale_fill_manual(breaks = c("I","IIA","IIX","IIB"),
                     values = c("#ccbb44" , "#66ccee", "#ee6677","#228833")) +
   scale_y_continuous(expand = c(0,0), 
                      limits = c(0,300),
                      n.breaks = 4) +
   scale_x_discrete(breaks = c("Active",
                               "Fat_4.5",
                               "Fat_5.1"),
                    labels = c("Active",
                               expression(atop(textstyle(High ~ Ca^"2+"),
                                               atop(textstyle('Fatigue'),
                                                    NA))),
                               expression(atop(textstyle(Low ~ Ca^"2+"),
                                               atop(textstyle('Fatigue'),
                                                    NA)))
                    ))
)



ggsave("Woods_Manuscript_t3.pdf",
       t3, width =3.5, height = 3, units = "in",  dpi = 3000)

## Interaction Package test ----

data <- raw_data %>% 
  filter(fiber_type_num %in% c(2:4)) %>% 
  filter(Fsa > 0)

mdl <-lm(Fsa ~ Po_Pre_Step * Exp_Con, data = data)
interactions::probe_interaction(mdl, pred = Po_Pre_Step, modx = Exp_Con_Num, plot.points = T)

johnson_neyman(mdl, pred = Po_Pre_Step, modx = Exp_Con)
