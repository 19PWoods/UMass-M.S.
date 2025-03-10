library(tidyverse)
library(pbkrtest)
library(multcomp)
library(cowplot)
library(emmeans)
library(patchwork)

### Reading Data in ---------------------------
my_data = readxl::read_excel(file.choose(),
                             sheet = "Fsa",
                             na="") 
my_data = my_data %>% 
  filter(FiberTypeNum %in% c(1,2,4)) %>% 
  mutate(ExpCond = as.factor(ExpCond))

my_data_test = my_data %>% 
  group_by(ExpCond, FiberType, FiberTypeNum) %>% 
  summarize(N = n(),
            Tension = mean(F0, na.rm = T),
            Tension_SD = sd(F0)/n(),
            SA = mean(FSA),
            SE = sd(FSA)/n(),
            Ratio = mean(FSAF0) * 100,
            Ratio_SE = sd(FSAF0)/n() *100,
            Total = mean(FsaTotal)*100,
            Total_SE = sd(FsaTotal)/n()*100)

### Statistical Models -------------------------
I = my_data %>% filter(FiberType == "I")
I_F0 = TukeyHSD(aov(I$F0~I$ExpCond))
I_FSA = TukeyHSD(aov(I$FSA~I$ExpCond))
I_Ratio = TukeyHSD(aov(I$FSAF0~I$ExpCond))
I_total = TukeyHSD(aov(I$FsaTotal~I$ExpCond))

# Not sure if I trust this method, it seemingly is giving wonky results
# II_F0 = lmer(F0 ~ ExpCond + FiberType + (1 + ExpCond | Mouse),data = II)
# emmeans(II_F0, list(pairwise ~ ExpCond + FiberType),adjust = "tukey")
II = my_data %>% filter(FiberType == "IIB")

II_F0 = lmer(F0 ~ ExpCond + (1 + as.factor(ExpCond) | Mouse), data = II)
summary(glht(II_F0, linfct = mcp(ExpCond = "Tukey")))
II_FSA = lmer(FSA ~ ExpCond + (1 + as.factor(ExpCond) | Mouse), data = II)
summary(glht(II_FSA, linfct = mcp(ExpCond = "Tukey")))
II_FSAF0 = lmer(FSAF0 ~ ExpCond + (1 + as.factor(ExpCond) | Mouse), data = II)
summary(glht(II_FSAF0, linfct = mcp(ExpCond = "Tukey")))
II_Total = lmer(FsaTotal ~ ExpCond + (1 + as.factor(ExpCond) | Mouse), data = II)
summary(glht(II_Total, linfct = mcp(ExpCond = "Tukey")))

### Graph -----------------------------------------
(F0_gg = my_data_test %>% 
   ggplot(aes(x = ExpCond, group = FiberType)) +
   geom_bar(aes(y = Tension,
                fill = FiberType),
            color = "black",
            stat = "identity",
            position = "dodge",
            size = 0.3) +
   geom_point(data = my_data,
              aes(y = F0),
              size = 0.8,
              position = position_dodge(width =0.9)) +
   geom_errorbar(aes(ymin = Tension - Tension_SD,
                     ymax = Tension + Tension_SD),
                 width = 0.5,
                 position = position_dodge(width = 0.9)) +
   guides(fill=guide_legend(title = "Fiber Types"),
          shape = "none") +
   labs(x = "[Pi]",
        y = bquote(F[0]~(mN/mm^2))) +
   theme_set(theme_cowplot()) +
   scale_y_continuous(expand = c(0,0),
                      limits = c(0,110)) +
   theme(axis.title.x = element_text(size = 12),
         axis.text.x = element_text(size = 12),
         axis.title.y = element_text(size = 12),
         axis.text.y = element_text(size = 12),
         legend.position = "none",
         axis.ticks.x = element_blank()) +
   scale_fill_manual(breaks = c("I","IIA","IIB"),
                     values = c("#ccbb44" , "#66ccee", "#228833")) +
   geom_text(data = tibble(x = 0.71, y = 105),
             aes(x = x, y = y, label = "a"),
             size = 3,
             col = "#ccbb44",
             inherit.aes = F)+
   geom_text(data = tibble(x = 1.71, y = 105),
             aes(x = x, y = y, label = "a"),
             size = 3,
             col = "#ccbb44",
             inherit.aes = F)+
   geom_text(data = tibble(x = 2.71, y = 105),
             aes(x = x, y = y, label = "a"),
             size = 3,
             col = "#ccbb44",
             inherit.aes = F) +
   geom_text(data = tibble(x = 3.71, y = 105),
             aes(x = x, y = y, label = "b"),
             size = 3,
             col = "#ccbb44",
             inherit.aes = F)+
   geom_text(data = tibble(x = 4.71, y = 105),
             aes(x = x, y = y, label = "b"),
             size = 3,
             col = "#ccbb44",
             inherit.aes = F)+
   geom_text(data = tibble(x = 1.01, y = 105),
             aes(x = x, y = y, label = "a"),
             size = 3,
             col = "#66ccee",
             inherit.aes = F)+
   geom_text(data = tibble(x = 2.01, y = 105),
             aes(x = x, y = y, label = "ab"),
             size = 3,
             col = "#66ccee",
             inherit.aes = F)+
   geom_text(data = tibble(x = 3.01, y = 105),
             aes(x = x, y = y, label = "bc"),
             size = 3,
             col = "#66ccee",
             inherit.aes = F)+
   geom_text(data = tibble(x = 4.01, y = 105),
             aes(x = x, y = y, label = "ce"),
             size = 3,
             col = "#66ccee",
             inherit.aes = F)+
   geom_text(data = tibble(x = 5.01, y = 105),
             aes(x = x, y = y, label = "ef"),
             size = 3,
             col = "#66ccee",
             inherit.aes = F)+
   geom_text(data = tibble(x = 1.35, y = 105),
             aes(x = x, y = y, label = "a"),
             size = 3,
             col = "#228833",
             inherit.aes = F)+
   geom_text(data = tibble(x = 2.35, y = 105),
             aes(x = x, y = y, label = "ab"),
             size = 3,
             col = "#228833",
             inherit.aes = F)+
   geom_text(data = tibble(x = 3.35, y = 105),
             aes(x = x, y = y, label = "bc"),
             size = 3,
             col = "#228833",
             inherit.aes = F) +
   geom_text(data = tibble(x = 4.35, y = 105),
             aes(x = x, y = y, label = "ce"),
             size = 3,
             col = "#228833",
             inherit.aes = F)+
   geom_text(data = tibble(x = 5.35, y = 105),
             aes(x = x, y = y, label = "e"),
             size = 3,
             col = "#228833",
             inherit.aes = F)
)

(FSA_gg = my_data_test %>% 
   ggplot(aes(x = ExpCond, group = FiberType)) +
    geom_bar(aes(y = SA,
                 fill = FiberType),
             color = "black",
             stat = "identity",
             position = "dodge",
             size = 0.3) +
    geom_point(data = my_data,
               aes(y = FSA),
               size = 0.8,
               position = position_dodge(width =0.9)) +
    geom_errorbar(aes(ymin = SA - SE,
                      ymax = SA + SE),
                  width = 0.5,
                  position = position_dodge(width = 0.9)) +
   guides(fill=guide_legend(title = "Fiber Types"),
          shape = "none") +
   labs(x = "[Pi]",
        y = bquote(F[SA]~(mN/mm^2))) +
   theme_set(theme_cowplot()) +
   scale_y_continuous(expand = c(0,0),
                      limits = c(0,22)) +
   theme(axis.title.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.y = element_text(size = 12),
         axis.text.y = element_text(size = 12),
         legend.position = "top",
         legend.justification = "center",
         legend.key.size = unit(.4,'cm'),
         legend.title = element_text(size = 10),
         legend.text = element_text(size = 8),
         axis.ticks.x = element_blank()) +
   scale_fill_manual(breaks = c("I","IIA","IIB"),
                     values = c("#ccbb44" , "#66ccee", "#228833")) +
    geom_text(data = tibble(x = 0.71, y = 21),
              aes(x = x, y = y, label = "a"),
              size = 3,
              col = "#ccbb44",
              inherit.aes = F) +
    geom_text(data = tibble(x = 1.71, y = 21),
              aes(x = x, y = y, label = "a"),
              size = 3,
              col = "#ccbb44",
              inherit.aes = F)+
    geom_text(data = tibble(x = 2.71, y = 21),
              aes(x = x, y = y, label = "b"),
              size = 3,
              col = "#ccbb44",
              inherit.aes = F)+
    geom_text(data = tibble(x = 3.71, y = 21),
              aes(x = x, y = y, label = "b"),
              size = 3,
              col = "#ccbb44",
              inherit.aes = F)+
    geom_text(data = tibble(x = 4.71, y = 21),
              aes(x = x, y = y, label = "b"),
              size = 3,
              col = "#ccbb44",
              inherit.aes = F)+
    geom_text(data = tibble(x =1.01, y = 21),
              aes(x = x, y = y, label = "a"),
              size = 3,
              col = "#66ccee",
              inherit.aes = F)+
    geom_text(data = tibble(x =2.01, y = 21),
              aes(x = x, y = y, label = "ab"),
              size = 3,
              col = "#66ccee",
              inherit.aes = F)+
    geom_text(data = tibble(x =3.01, y = 21),
              aes(x = x, y = y, label = "ab"),
              size = 3,
              col = "#66ccee",
              inherit.aes = F)+
    geom_text(data = tibble(x =4.01, y = 21),
              aes(x = x, y = y, label = "ab"),
              size = 3,
              col = "#66ccee",
              inherit.aes = F) +
    geom_text(data = tibble(x =5.01, y = 21),
              aes(x = x, y = y, label = "b"),
              size = 3,
              col = "#66ccee",
              inherit.aes = F) +
    geom_text(data = tibble(x = 1.35, y = 21),
              aes(x = x, y = y, label = "ab"),
              size = 3,
              col = "#228833",
              inherit.aes = F)+
    geom_text(data = tibble(x = 2.35, y = 21),
              aes(x = x, y = y, label = "ab"),
              size = 3,
              col = "#228833",
              inherit.aes = F) +
    geom_text(data = tibble(x = 3.35, y = 21),
              aes(x = x, y = y, label = "ab"),
              size = 3,
              col = "#228833",
              inherit.aes = F) +
    geom_text(data = tibble(x = 4.35, y = 21),
              aes(x = x, y = y, label = "c"),
              size = 3,
              col = "#228833",
              inherit.aes = F) +
    geom_text(data = tibble(x = 5.35, y = 21),
              aes(x = x, y = y, label = "d"),
              size = 3,
              col = "#228833",
              inherit.aes = F)
)

(FSAF0_gg = my_data_test %>% 
    ggplot(aes(x = ExpCond, group = FiberType)) +
    geom_bar(aes(y = Ratio,
                 fill = FiberType),
             color = "black",
             stat = "identity",
             position = "dodge",
             size = 0.3) +
    geom_point(data = my_data,
               aes(y = FSAF0*100),
               size = 0.8,
               position = position_dodge(width =0.9)) +
    geom_errorbar(aes(ymin = Ratio - Ratio_SE,
                      ymax = Ratio + Ratio_SE),
                  width = 0.5,
                  position = position_dodge(width = 0.9)) +
    guides(fill=guide_legend(title = "Fiber Types"),
           shape = "none") +
    labs(x = "[Pi]",
         y = bquote(F[SA]/F[0]~("%"))) +
    theme_set(theme_cowplot()) +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,70)) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.position = "top",
          legend.justification = "center",
          legend.key.size = unit(.4,'cm'),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          axis.ticks.x = element_blank()) +
    scale_fill_manual(breaks = c("I","IIA","IIB"),
                      values = c("#ccbb44" , "#66ccee", "#228833"))+
    geom_text(data = tibble(x = 0.71, y = 62),
              aes(x = x, y = y, label = "a"),
              size = 3,
              col = "#ccbb44",
              inherit.aes = F) +
    geom_text(data = tibble(x = 1.71, y = 62),
              aes(x = x, y = y, label = "a"),
              size = 3,
              col = "#ccbb44",
              inherit.aes = F)+
    geom_text(data = tibble(x = 2.71, y = 62),
              aes(x = x, y = y, label = "ab"),
              size = 3,
              col = "#ccbb44",
              inherit.aes = F)+
    geom_text(data = tibble(x = 3.71, y = 62),
              aes(x = x, y = y, label = "b"),
              size = 3,
              col = "#ccbb44",
              inherit.aes = F)+
    geom_text(data = tibble(x = 4.71, y = 62),
              aes(x = x, y = y, label = "c"),
              size = 3,
              col = "#ccbb44",
              inherit.aes = F) +
    geom_text(data = tibble(x = 1.01, y = 62),
              aes(x = x, y = y, label = "a"),
              size = 3,
              col = "#66ccee",
              inherit.aes = F)+
    geom_text(data = tibble(x = 2.01, y = 62),
              aes(x = x, y = y, label = "b"),
              size = 3,
              col = "#66ccee",
              inherit.aes = F)+
    geom_text(data = tibble(x = 3.01, y = 62),
              aes(x = x, y = y, label = "c"),
              size = 3,
              col = "#66ccee",
              inherit.aes = F)+
    geom_text(data = tibble(x = 4.01, y = 62),
              aes(x = x, y = y, label = "d"),
              size = 3,
              col = "#66ccee",
              inherit.aes = F)+
    geom_text(data = tibble(x = 5.01, y = 62),
              aes(x = x, y = y, label = "e"),
              size = 3,
              col = "#66ccee",
              inherit.aes = F) +
    geom_text(data = tibble(x = 1.35, y = 62),
              aes(x = x, y = y, label = "a"),
              size = 3,
              col = "#228833",
              inherit.aes = F)+
    geom_text(data = tibble(x = 2.35, y = 62),
              aes(x = x, y = y, label = "ab"),
              size = 3,
              col = "#228833",
              inherit.aes = F)+
    geom_text(data = tibble(x = 3.35, y = 62),
              aes(x = x, y = y, label = "ab"),
              size = 3,
              col = "#228833",
              inherit.aes = F)+
    geom_text(data = tibble(x = 4.35, y = 62),
              aes(x = x, y = y, label = "ab"),
              size = 3,
              col = "#228833",
              inherit.aes = F)+
    geom_text(data = tibble(x = 5.35, y = 62),
              aes(x = x, y = y, label = "a"),
              size = 3,
              col = "#228833",
              inherit.aes = F)
)

(Total_gg = my_data_test %>% 
    ggplot(aes(x = ExpCond, group = FiberType)) +
    geom_bar(aes(y = Total,
                 fill = FiberType),
             color = "black",
             stat = "identity",
             position = "dodge",
             size = 0.3) +
    geom_point(data = my_data,
               aes(y = FsaTotal*100),
               size = 0.8,
               position = position_dodge(width =0.9)) +
    geom_errorbar(aes(ymin = Total - Total_SE,
                      ymax = Total + Total_SE),
                  width = 0.5,
                  position = position_dodge(width = 0.9)) +
    guides(fill=guide_legend(title = "Fiber Types"),
           shape = "none") +
    labs(x = "[Pi]",
         y = bquote(F[SA]/(F[SA]+F[0])~("%"))) +
    theme_set(theme_cowplot()) +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,65)) +
    theme(axis.title.x = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.position = "none",
          axis.ticks.x = element_blank()) +
    scale_fill_manual(breaks = c("I","IIA","IIB"),
                      values = c("#ccbb44" , "#66ccee", "#228833"))+
    geom_text(data = tibble(x = 0.71, y = 62),
              aes(x = x, y = y, label = "a"),
              size = 3,
              col = "#ccbb44",
              inherit.aes = F) +
    geom_text(data = tibble(x = 1.71, y = 62),
              aes(x = x, y = y, label = "a"),
              size = 3,
              col = "#ccbb44",
              inherit.aes = F)+
    geom_text(data = tibble(x = 2.71, y = 62),
              aes(x = x, y = y, label = "ab"),
              size = 3,
              col = "#ccbb44",
              inherit.aes = F)+
    geom_text(data = tibble(x = 3.71, y = 62),
              aes(x = x, y = y, label = "b"),
              size = 3,
              col = "#ccbb44",
              inherit.aes = F)+
    geom_text(data = tibble(x = 4.71, y = 62),
              aes(x = x, y = y, label = "c"),
              size = 3,
              col = "#ccbb44",
              inherit.aes = F) +
    geom_text(data = tibble(x = 1.01, y = 62),
              aes(x = x, y = y, label = "a"),
              size = 3,
              col = "#66ccee",
              inherit.aes = F)+
    geom_text(data = tibble(x = 2.01, y = 62),
              aes(x = x, y = y, label = "b"),
              size = 3,
              col = "#66ccee",
              inherit.aes = F)+
    geom_text(data = tibble(x = 3.01, y = 62),
              aes(x = x, y = y, label = "c"),
              size = 3,
              col = "#66ccee",
              inherit.aes = F)+
    geom_text(data = tibble(x = 4.01, y = 62),
              aes(x = x, y = y, label = "d"),
              size = 3,
              col = "#66ccee",
              inherit.aes = F)+
    geom_text(data = tibble(x = 5.01, y = 62),
              aes(x = x, y = y, label = "e"),
              size = 3,
              col = "#66ccee",
              inherit.aes = F) +
    geom_text(data = tibble(x = 1.35, y = 62),
              aes(x = x, y = y, label = "a"),
              size = 3,
              col = "#228833",
              inherit.aes = F)+
    geom_text(data = tibble(x = 2.35, y = 62),
              aes(x = x, y = y, label = "ab"),
              size = 3,
              col = "#228833",
              inherit.aes = F)+
    geom_text(data = tibble(x = 3.35, y = 62),
              aes(x = x, y = y, label = "ab"),
              size = 3,
              col = "#228833",
              inherit.aes = F)+
    geom_text(data = tibble(x = 4.35, y = 62),
              aes(x = x, y = y, label = "ab"),
              size = 3,
              col = "#228833",
              inherit.aes = F)+
    geom_text(data = tibble(x = 5.35, y = 62),
              aes(x = x, y = y, label = "a"),
              size = 3,
              col = "#228833",
              inherit.aes = F)
)


chad <- (FSA_gg | FSAF0_gg) / (F0_gg | Total_gg) +
  plot_annotation(tag_levels = list(c('A', 'C','B', 'D')),
                  title = "Figure 7")

setwd("C:/Users/pcw00/Dropbox/University of Massachusetts Amherst/Thesis- Stretch Activation/Data/Woods - Master's Thesis/Project/Tension + AaBbCc/Manuscript Graphs/tiff images")

ggsave("Chad_F0.jpeg",
       F0_gg, width = 3.5, height = 3, units = "in", dpi = 3000)
ggsave("Chad_Fsa.jpeg",
       FSA_gg, width = 3.5, height = 3, units = "in", dpi = 3000)
ggsave("Chad_FsaF0.jpeg",
       FSAF0_gg, width = 3.5, height = 3, units = "in", dpi = 3000)
ggsave("Chad_total.jpeg",
       Total_gg, width = 3.5, height = 3, units = "in", dpi = 3000)


ggsave("Woods_ChadGraph.jpeg",
       chad, width = 7, height = 7, 
       units = "in", dpi = 3000)


output_folder <- "C:/Users/pcw00/Dropbox/University of Massachusetts Amherst/Thesis- Stretch Activation/Data/Woods - Master's Thesis/Project/Tension + AaBbCc/Manuscript Graphs/"
output_file_fig7 <- paste0(output_folder, "Figure7.pdf") 
ggsave(filename = output_file_fig7,
       plot = chad,
       width = 7, height = 7, units = "in", dpi = 3000)

