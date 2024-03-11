library(tidyverse)
library(pbkrtest)
library(multcomp)
library(cowplot)
library(emmeans)

### Reading Data in ---------------------------
my_data = readxl::read_excel(file.choose(),
                             sheet = "Fsa",
                             na="") 
my_data = my_data %>% 
  filter(FiberTypeNum %in% c(1,2,4)) %>% 
  mutate(ExpCond = as.factor(ExpCond))



my_data_test = my_data %>% 
  group_by(ExpCond, FiberType, FiberTypeNum) %>% 
  summarize(Tension = mean(F0),
            SA = mean(FSA),
            SE = sd(FSA)/n()
            )

### Conducting Model --------------------------------------


sa_mdl = lmer(FSA ~ ExpCond + FiberType + 
                (1 + ExpCond | Mouse),
              data = my_data)

anova(sa_mdl)

sa_results = emmeans(sa_mdl, list(pairwise ~ ExpCond + FiberType),
        adjust = "tukey")

values = as.data.frame(sa_results$`emmeans of ExpCond, FiberType`)
p.value = as.data.frame(sa_results$`pairwise differences of ExpCond, FiberType`)

p.value.fil = p.value %>% filter(p.value < 0.05)


sa_mdl_simple = lmer(FSA ~ FiberType + ExpCond + 
                       (1 + ExpCond | Mouse),
                     data = my_data)

emmeans(sa_mdl_simple, specs = "FiberType")
sa_results_fibertype = emmeans(sa_mdl, list(pairwise ~  FiberType + ExpCond),
                     adjust = "tukey")


### MHC II Model Test ----------------------------
II = my_data %>% 
  filter(FiberType == "IIB")

II.mdl = lmer(FSA ~ ExpCond + (1 + as.factor(ExpCond) | Mouse),
              data = II)

emmeans(II.mdl, specs = "ExpCond")
summary(anova(II.mdl))
summary(glht(II.mdl, linfct = mcp(ExpCond = "Tukey")))

### Graph -----------------------------------------
# (chad_graph = ggplot(data = values,
#                     aes(x = ExpCond, group = FiberType)) +
#   geom_bar(aes(y = emmean,
#                fill = FiberType,
#                color = "black"),
#                stat = "identity",
#                position = position_dodge(),
#                size = 0.3) +
#   geom_point(data = my_data,
#              aes(y = FSA),
#              size = 0.8,
#              position = position_dodge(width = 1)) +
#   geom_errorbar(aes(ymin = emmean - SE,
#                     ymax = emmean + SE),
#                 size = 0.8,
#                 position = position_dodge(width =0.9))
# )


# (chad_graph = ggplot(data = values,
#                      aes(x = ExpCond, group = FiberType)) +
#     geom_bar(aes(y = emmean,
#                  fill = FiberType),
#              color = "black",
#              stat = "identity",
#              position = "dodge",
#              size = 0.3) +
#     geom_point(data = my_data,
#                aes(y = FSA),
#                size = 0.8,
#                position = position_dodge(width =1.8)) +
#     geom_errorbar(aes(ymin = emmean - SE,
#                       ymax = emmean + SE),
#                   size = 0.8,
#                   position = "dodge") +
#    guides(fill=guide_legend(title = "Fiber Types"),
#           shape = "none") +
#    labs(x = "[Pi]",
#         y = bquote(F[SA]~(mN/mm^2))) +
#    theme_set(theme_cowplot()) +
#    theme(axis.title.x = element_blank(),
#          axis.text.x = element_blank(),
#          axis.title.y = element_text(size = 12),
#          axis.text.y = element_text(size = 12),
#          legend.position = "top",
#          legend.key.size = unit(.4,'cm'),
#          legend.title = element_text(size = 10),
#          legend.text = element_text(size = 8),
#          axis.ticks.x = element_blank()) +
#    scale_fill_manual(breaks = c("I","IIA","IIB"),
#                      values = c("#ccbb44" , "#66ccee", "#228833"))
# )

(chad_graph = my_data_test %>% 
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
   geom_text(data = tibble(x = 5, y = 21),
             aes(x = x, y = y, label = "*"),
             size = 5,
             col = "red",
             inherit.aes = F) +
   geom_text(data = tibble(x = 5.32, y = 21),
             aes(x = x, y = y, label = "***"),
             size = 5,
             col = "red",
             inherit.aes = F) +
   guides(fill=guide_legend(title = "Fiber Types"),
          shape = "none") +
   labs(x = "[Pi]",
        y = bquote(F[SA]~(mN/mm^2))) +
   theme_set(theme_cowplot()) +
   scale_y_continuous(expand = c(0,0),
                      limits = c(0,22)) +
   theme(axis.title.x = element_text(size = 12),
         axis.text.x = element_text(size = 12),
         axis.title.y = element_text(size = 12),
         axis.text.y = element_text(size = 12),
         legend.position = "top",
         legend.justification = "center",
         legend.key.size = unit(.4,'cm'),
         legend.title = element_text(size = 10),
         legend.text = element_text(size = 8),
         axis.ticks.x = element_blank()) +
   scale_fill_manual(breaks = c("I","IIA","IIB"),
                     values = c("#ccbb44" , "#66ccee", "#228833"))
)

ggsave("Woods_ChadGraph.pdf",
       chad_graph, widt = 3.5, height = 33, units = "in", dpi = 3000)




