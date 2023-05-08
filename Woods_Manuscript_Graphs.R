library(tidyverse)
library(readxl)
library(ggpattern)
library(patchwork)
library(ggsignif)
theme_set(theme_classic())

# setwd("C:/Users/Phil/Dropbox/Thesis- Stretch Activation/Data/Woods - Master's Thesis/Project/Tension + AaBbCc")
setwd("C:/Users/pcw00/Dropbox/University of Massachusetts Amherst/Thesis- Stretch Activation/Data/Woods - Master's Thesis/Project/Tension + AaBbCc")


## Data Read in --------------------------------------------------------------------------
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

## SA Traces -------------------------------------------------------------------------

trace_I <- read_excel("Woods_EMM_10-29-22.xlsx",
                      sheet = "I",
                      na = "Included") %>% 
  select(Time, Low_Fat, Fiber_type,High_Fat,Active)

trace_IIA <- read_excel("Woods_EMM_10-29-22.xlsx",
                      sheet = "IIA",
                      na = "Included") %>% 
  select(Time, Low_Fat, Fiber_type,High_Fat,Active)

trace_IIX <- read_excel("Woods_EMM_10-29-22.xlsx",
                        sheet = "IIX",
                        na = "Included") %>% 
  select(Time, Low_Fat, Fiber_type,High_Fat,Active)

trace_IIB <- read_excel("Woods_EMM_10-29-22.xlsx",
                        sheet = "IIB",
                        na = "Included") %>% 
  select(Time, Low_Fat, Fiber_type,High_Fat,Active)


# scale_color_manual(breaks = c("I", "IIA","IIX", "IIB"),
#                    values = c("#E69F00","#56B4E9", "#CC79A7","#009E73")) 

(I_trace_gg <- trace_I %>% 
    mutate(Active = Active - 0.02) %>% 
    # mutate(High_Fat = High_Fat - 0.02) %>% 
    # mutate(Low_Fat = Low_Fat - 0.02) %>% 
    ggplot(aes(x = Time,
               col = Fiber_type)) +
    geom_line(aes(y = Low_Fat),
              size = 2,
              linetype = "dotted") +
    geom_line(aes(y = High_Fat),
              size = 2,
              linetype = "longdash") +
    geom_line(aes(y = Active),
              size = 2,
              linetype = "solid")+
    ylab("Force (mN)") +
    xlab("Time (s)") +
    scale_y_continuous(limits = c(0,0.05)) +
    scale_color_manual(breaks = c("I"),
                       values = c("#E69F00")) +
    theme(axis.title.x = element_text(size = 30),
          axis.title.y = element_text(size = 30),
          axis.text  = element_text(size = 20),
          legend.title = element_blank(),
          legend.text = element_blank(),
          legend.key.size = unit(0,"cm"),
          axis.line = element_line(linewidth = 1),
          axis.ticks = element_line(linewidth = 1))
)

(IIA_trace_gg <- trace_IIA %>% 
    filter(Time < 0.2) %>% 
    ggplot(aes(x = Time,
               col = Fiber_type)) +
    geom_line(aes(y = Low_Fat),
              size = 2,
              linetype = "dotted") +
    geom_line(aes(y = High_Fat),
              size = 2,
              linetype = "longdash") +
    geom_line(aes(y = Active),
              size = 2,
              linetype = "solid")+
    ylab("Force (mN)") +
    xlab("Time (s)") +
    scale_y_continuous(limits = c(0,0.05)) +
    scale_color_manual(breaks = c("IIA"),
                       values = c("#56B4E9")) +
    theme(axis.title.x = element_text(size = 30),
          axis.title.y = element_text(size = 30),
          axis.text  = element_text(size = 20),
          legend.title = element_blank(),
          legend.text = element_blank(),
          legend.key.size = unit(0,"cm"),
          axis.line = element_line(linewidth = 1),
          axis.ticks = element_line(linewidth = 1))
)

(IIX_trace_gg <- trace_IIX %>% 
    filter(Time < 0.1) %>% 
    ggplot(aes(x = Time,
               col = Fiber_type)) +
    geom_line(aes(y = Low_Fat),
              size = 2,
              linetype = "dotted") +
    geom_line(aes(y = High_Fat),
              size = 2,
              linetype = "longdash") +
    geom_line(aes(y = Active),
              size = 2,
              linetype = "solid")+
    scale_y_continuous(limits =  c(0,0.038)) + 
    scale_color_manual(breaks = c("IIX"),
                       values = c("#CC79A7"))+
    theme(axis.title = element_blank(),
          axis.text  = element_text(size = 20),
          legend.title = element_blank(),
          legend.text = element_blank(),
          legend.key.size = unit(0,"cm"),
          axis.line = element_line(linewidth = 1),
          axis.ticks = element_line(linewidth = 1))
)

(IIB_trace_gg <- trace_IIB %>% 
    filter(Time <0.05) %>% 
    ggplot(aes(x = Time,
               col = Fiber_type)) +
    geom_line(aes(y = Low_Fat),
              size = 2,
              linetype = "dotted") +
    geom_line(aes(y = High_Fat),
              size = 2,
              linetype = "longdash") +
    geom_line(aes(y = Active),
              size = 2,
              linetype = "solid")+
    xlab("Time (s)")+
    scale_color_manual(breaks = c("IIB"),
                       values = c("#009E73"))+
    scale_y_continuous(limits = c(0,0.06)) +
    theme(axis.title.x = element_text(size = 30),
          axis.title.y = element_blank(),
          axis.text  = element_text(size = 20),
          legend.position = "none",
          axis.line = element_line(linewidth = 1),
          axis.ticks = element_line(linewidth = 1))
)


(traces_gg <- (I_trace_gg | IIX_trace_gg)  / (IIA_trace_gg | IIB_trace_gg)
)

ggsave("Woods_Manuscript_scatterplot.jpeg",
       traces_gg, width = 16, height = 12, units = "in",  dpi = 300)

## SA Traces in One Graph -----------------------------------

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




## Fsa & F0 Graphs ---------------------------------------------------------------


# (F0_col <- my_data %>% 
#     filter(Value == "F0") %>% 
#     group_by(Exp_Con, fiber_type, fiber_type_num) %>% 
#     ggplot(aes(x = Exp_Con,
#                y = EMM,
#                group = fiber_type_num)) + 
#     geom_bar(aes(fill = fiber_type),
#              stat = "identity",
#              position = position_dodge(),
#              size = 1) +
#     geom_point(data = raw_data_f0,
#                aes(x = Exp_Con,
#                    y = Po_Pre_Step,
#                    shape = Exp_Con),
#                size = 2,
#                position = position_dodge(width = 0.9)) +
#     geom_errorbar(aes(ymin=EMM - SE,
#                       ymax=EMM + SE),
#                   width=0.4,
#                   size = 1.5,
#                   position = position_dodge(width = 0.9)) +
#     geom_text(aes(label = c("14","14","14","32","32","32","11","11","11","19","19","19"),
#                   y = 15),
#               size = 7,
#               position = position_dodge(width = 0.9)) +
#     guides(fill=guide_legend(title = "Fiber Types")) +
#     guides(shape = "none") +
#     scale_shape_manual(values = c(16,15,17)) +
#     scale_fill_manual(breaks = c("I", "IIA","IIX", "IIB"),
#                        values = c("#E69F00","#56B4E9", "#CC79A7","#009E73")) +
#     theme(axis.title.y = element_blank(),
#           axis.title.x = element_blank(),
#           axis.text.y = element_text(size = 20),
#           axis.text.x = element_blank(),
#           legend.position = "none",
#           axis.line = element_line(linewidth = 1),
#           axis.ticks = element_line(linewidth = 2)) +
#     scale_y_continuous(expand = c(0,0), limits = c(0,375)) +
#     scale_x_discrete(breaks = c("Active",
#                                 "Fat_4.5",
#                                 "Fat_5.1"),
#                      labels = c("Active",
#                                 expression(atop("High Calcium",
#                                                 paste("Fatigue"))),
#                                 expression(atop("Low Calcium",
#                                                 paste("Fatigue")))))
# )

#  (Fsa_col <- my_data %>% 
#     filter(Value == "Fsa") %>% 
#     group_by(Exp_Con, fiber_type, fiber_type_num) %>% 
#     ggplot(aes(x = Exp_Con,
#                y = EMM,
#                group = fiber_type_num)) + 
#     geom_bar(aes(fill = fiber_type),
#              stat = "identity",
#              position = position_dodge(),
#              size = 1) +
#     geom_point(data = raw_data_gg,
#                aes(x = Exp_Con,
#                    y = Fsa,
#                    shape = Exp_Con),
#                size = 2,
#                position = position_dodge(width = 0.9)) +
#     geom_errorbar(aes(ymin=EMM - SE,
#                       ymax=EMM + SE),
#                   width=0.4,
#                   size = 1.5,
#                   position = position_dodge(width = 0.9)) +
#     geom_text(aes(label = c("14","32","28","8","11","11","11","19","19","19"),
#                   y = 3),
#               size = 7,
#               position = position_dodge(width = 0.9)) +
#     guides(fill=guide_legend(title = "Fiber Types")) +
#     guides(shape = "none") +
#     ylab(bquote(F[SA])) +
#     scale_shape_manual(values = c(16,15,17)) +
#     theme(axis.title.y = element_blank(),
#           axis.title.x = element_blank(),
#           axis.text.y = element_text(size = 20),
#           axis.text.x = element_blank(),
#           legend.position = "none",
#           axis.line = element_line(size = 1),
#           axis.ticks = element_line(size = 2)) +
#     scale_fill_manual(breaks = c("I","IIA","IIX","IIB"),
#                       values = c("#E69F00","#56B4E9", "#CC79A7","#009E73")) +
#     scale_y_continuous(expand = c(0,0), limits = c(0,75)) +
#     scale_x_discrete(breaks = c("Active",
#                                 "Fat_4.5",
#                                 "Fat_5.1"),
#                      labels = c("Active",
#                                 expression(atop("High Calcium",
#                                                 paste("Fatigue"))),
#                                 expression(atop("Low Calcium",
#                                                 paste("Fatigue")))))
# )

fat4.5.label <- data.frame(Group = "Fat_4.5",
                           )

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
    geom_text(data = tibble(x = 2.9, y = 275),
              aes(x = x, y = y, label = "*#"),
              size = 5,
              inherit.aes = F)+
    geom_text(data = tibble(x = 3.15, y = 275),
              aes(x = x, y = y, label = "*#"),
              size = 5,
              inherit.aes = F)+
    geom_text(data = tibble(x = 3.4, y = 275),
              aes(x = x, y = y, label = "*#"),
              size = 5,
              inherit.aes = F)+
    guides(fill=guide_legend(title = "Fiber Types")) +
    guides(shape = "none") +
    ylab(bquote(F[0])) + 
    scale_shape_manual(values = c(16,15,17)) +
    theme(axis.title.x = element_blank(),
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
    geom_point(data = raw_data_gg,
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
    geom_text(data = tibble(x = 3.35, y = 75),
              aes(x = x, y = y, label = "*#"),
              size = 5,
              inherit.aes = F)+
    guides(fill=guide_legend(title = "Fiber Types")) +
    guides(shape = "none") +
    ylab(bquote(F[SA])) +
    scale_shape_manual(values = c(16,15,17)) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          legend.position = "top",
          legend.key.size = unit(.5,'cm')) +
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

(F0_Fsa <- Fsa/F0 + 
    plot_layout(ncol = 1) +
    plot_annotation(tag_levels = 'A'))

# (F0_Fsa_col <- Fsa_col/F0_col + plot_layout(ncol = 1, heights = c(6,6)))

ggsave("Woods_Manuscript_Fsa_F0.pdf",
       F0_Fsa, width = 3.5, height = 5, units = "in",  dpi = 300)

# ggsave("Woods_Manuscript_Fsa_F0_col.jpeg",
#        F0_Fsa_col, width = 8, height = 10, units = "in",  dpi = 300)


## Fsa/F0 and Fsa/(Fsa+F0) ---------------------------------------------------

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
                   y = FsaF0,
                   shape = Exp_Con),
               size = 2,
               position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin=EMM - SE,
                      ymax=EMM + SE),
                  width=0.4,
                  size = 1.5,
                  position = position_dodge(width = 0.9)) +
    # geom_text(aes(label = c("14","32","28","8","11","11","11","19","19","19"),
    #               y = 1),
    #           size = 7,
    #           position = position_dodge(width = 0.9)) +
    guides(fill=guide_legend(title = "Fiber Types")) +
   guides(shape = "none") +
   scale_shape_manual(values = c(16,15,17)) +
    ylab(bquote(F[SA]/F[0])) + 
    theme(axis.title.y = element_text(size = 30),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 20),
          axis.text.x = element_text(size = 20),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 16),
          legend.key.size = unit(0.75, 'cm'),
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

(FsaF0_col <- my_data %>% 
    filter(Value == "Ratio") %>% 
    group_by(Exp_Con, fiber_type, fiber_type_num) %>% 
    ggplot(aes(x = Exp_Con,
               y = EMM,
               group = fiber_type_num)) + 
    geom_bar(aes(fill = fiber_type),
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
    geom_text(aes(label = c("14","32","28","8","11","11","11","19","19","19"),
                  y = 1),
              size = 7,
              position = position_dodge(width = 0.9)) +
    guides(fill=guide_legend(title = "Fiber Types")) +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 20),
          axis.text.x = element_blank(),
          legend.title = element_text(size = 25),
          legend.text = element_text(size = 20),
          legend.key.size = unit(1.5,"cm"),
          axis.line = element_line(size = 1),
          axis.ticks = element_line(size = 1)) +
    scale_fill_manual(breaks = c("I","IIA","IIX","IIB"),
                      values = c("#E69F00","#56B4E9", "#CC79A7","#009E73")) +
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


ggsave("Woods_Manuscript_FsaF0.jpeg",
       FsaF0, width =9, height = 9, units = "in",  dpi = 300)

ggsave("Woods_Manuscript_FsaF0_col.jpeg",
       FsaF0_col, width =8, height = 10, units = "in",  dpi = 300)


(Fsa_Total <- my_data %>% 
    filter(Value == "FsaTotal") %>% 
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
                   y = Fsa_total,
                   shape = Exp_Con),
               size = 2,
               position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin=EMM - SE,
                      ymax=EMM + SE),
                  width=0.4,
                  size = 1.5,
                  position = position_dodge(width = 0.9)) +
    # geom_text(aes(label = c("14","32","28","8","11","11","11","19","19","19"),
    #               y = 2),
    #           size = 7,
    #           position = position_dodge(width = 0.9)) +
    # guides(fill=guide_legend(title = "Fiber Types")) +
    ylab(bquote(F[SA]/(F[SA] + F[0]))) + 
    guides(shape = "none") +
    scale_shape_manual(values = c(16,15,17)) +
    theme(axis.title.y = element_text(size = 30),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 20),
          axis.text.x = element_text(size = 20),
          # legend.title = element_text(size = 20),
          # legend.text = element_text(size = 18),
          # legend.key.size = unit(1,"cm"),
          legend.position = "none",
          axis.line = element_line(size = 1),
          axis.ticks = element_line(size = 1)) +
    scale_fill_manual(breaks = c("I","IIA","IIX","IIB"),
                      values = c("#FDFEFE" , "#D0D3D4", "#7B7D7D","#424949")) +
    scale_y_continuous(expand = c(0,0), limits = c(0,36)) +
    scale_x_discrete(breaks = c("Active",
                                "Fat_4.5",
                                "Fat_5.1"),
                     labels = c("Active",
                                expression(atop("High Calcium",
                                                paste("Fatigue"))),
                                expression(atop("Low Calcium",
                                                paste("Fatigue")))))
)


(Fsa_FsaF0 <- FsaF0/Fsa_Total + plot_layout(ncol = 1, heights = c(7,6)))
ggsave("Woods_Manuscript_FsaF0Total.jpeg",
       Fsa_FsaF0, width = 9, height = 10, units = "in",  dpi = 300)

## Fsa vs F0 scatterplot ---------------------------------------------------

active <- raw_data_gg %>% 
  mutate(iso = ifelse(fiber_type_num == 1,1,2)) %>% 
  filter(iso == 2) %>% 
  filter(Exp_Con == "Active")

fat_4.5 <- raw_data_gg %>% 
  mutate(iso = ifelse(fiber_type_num == 1,1,2)) %>% 
  filter(iso == 2) %>% 
  filter(Exp_Con == "Fat_4.5")

fat_5.1 <- raw_data_gg %>% 
  mutate(iso = ifelse(fiber_type_num == 1,1,2)) %>% 
  filter(iso == 2) %>% 
  filter(Exp_Con == "Fat_5.1")


active_lm <- lm(active$Fsa ~ active$Po_Pre_Step)
fat_4.5_lm <- lm(fat_4.5$Fsa ~ fat_4.5$Po_Pre_Step)
fat_5.1_lm <- lm(fat_5.1$Fsa ~ fat_5.1$Po_Pre_Step)

active$mdl <- predict(active_lm)
fat_4.5$mdl <- predict(fat_4.5_lm)
fat_5.1$mdl <- predict(fat_5.1_lm)



(FsavsF0_scatter <- raw_data_gg %>% 
  mutate(iso = ifelse(fiber_type_num == 1,1,2))%>% 
  filter(iso == 2) %>% 
  ggplot(aes(x = Po_Pre_Step,
             y = Fsa)) +
  geom_point(aes(shape = Exp_Con),
             size = 3) +
  geom_line(data = active,
            aes(x = Po_Pre_Step,
                y = mdl),
            size = 3,
            linetype = "solid") +
   geom_line(data = fat_4.5,
              aes(x = Po_Pre_Step,
                  y = mdl),
              size = 3,
              linetype = "longdash")+
  geom_line(data = fat_5.1,
            aes(x = Po_Pre_Step,
                y = mdl),
            size = 3,
            linetype = "dotdash")+
  guides(shape=guide_legend("Experimental Condition"))+
  ylab(bquote(F[SA])) + 
  xlab(bquote(F[0])) +
  scale_shape_manual(values = c(1,0,2)) +
  scale_x_continuous(limits = c(0,300)) +
  scale_y_continuous(limits = c(0,75)) +
  scale_shape_discrete(labels = c("Active", "High Calcium Fatigue", "Low Calcium Fatigue")) +
  theme(axis.title.y = element_text(size = 40),
          axis.title.x = element_text(size = 40),
          axis.text.y = element_text(size = 30),
          axis.text.x = element_text(size = 30),
          legend.title = element_text(size = 25),
          legend.text = element_text(size = 25),
          legend.key.size = unit(1,"cm"),
          axis.line = element_line(size = 2),
          axis.ticks = element_line(size = 2))
  
)
  
ggsave("Woods_Manuscript_Scatter.jpeg",
       FsavsF0_scatter, width =15, height = 10, units = "in",  dpi = 300)
  
  
(FsavsF0_scatter_col <- raw_data_gg %>% 
    ggplot(aes(x = Po_Pre_Step,
               y = Fsa)) +
    geom_point(aes(shape = Exp_Con,
                   col = fiber_type),
               size = 3) +
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
              linetype = "dotdash")+
    guides(shape=guide_legend("Experimental Condition"))+
    ylab(bquote(F[SA])) + 
    xlab(bquote(F[0])) +
    scale_shape_manual(values = c(1,0,2)) +
    scale_x_continuous(limits = c(0,300)) +
    scale_color_manual(breaks = c("I", "IIA","IIX", "IIB"),
                       values = c("#E69F00","#56B4E9", "#CC79A7","#009E73")) +
    scale_y_continuous(limits = c(0,75)) +
    scale_shape_discrete(labels = c("Active", "High Calcium Fatigue", "Low Calcium Fatigue")) +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 15),
          axis.text.x = element_text(size = 15),
          legend.position = "none",
          axis.line = element_line(size = 1),
          axis.ticks = element_line(size = 2))
  
)
  
ggsave("Woods_Manuscript_Scatter_col.jpeg",
       FsavsF0_scatter_col, width =8, height = 4, units = "in",  dpi = 300)
  
