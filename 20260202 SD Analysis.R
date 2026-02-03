library(tidyverse)
library(readxl)
library(openxlsx)




sddata <- read_excel(file.choose(),
                     sheet = "Manuscript",
                     na = "") %>% 
  filter(Exp_Con_Num %in% c(2:4)) %>%
  filter(`Fiber Type Num` %in% c(1:4)) %>%
  # mutate(fiber_type = factor(fiber_type, ##isnt FUCKING WORKING YOU STUPID LANGUAGE!!!!
  #                            levels = c("I", "IIA", "IIX", "IIB"),
  #                            labels = c("I", "IIA", "IIX", "IIB"))) %>%
  group_by(Exp_Con_Num, Exp_Con, `Fiber Type Num`, `Fiber Type`)  


# Function to fit repeated measures linear mixed model
fit_model_rep <- function(df, var_name) {
  formula <- as.formula(paste0("`", var_name, "` ~ Exp_Con + (1 + as.factor(Exp_Con) | Mouse)"))
  model <- lmerTest::lmer(formula, data = df)
  model
}

# Function to perform Tukey post-hoc analysis
posthoc_analysis <- function(model) {
  hoc = summary(multcomp::glht(model, linfct = multcomp::mcp(Exp_Con = "Tukey")))
  out = broom.mixed::tidy(hoc)
  
  out$p.value_formatted <- format(out$p.value, scientific = TRUE, digits = 10)
  return(out)
}


# Model 1: Repeated measures linear mixed model for F0 & F0-FSD (all fiber types)
var_names_1 <- c("F0", "F0-FSD")

model1 <- sddata %>% 
  select(Mouse, Exp_Con_Num, `Fiber Type Num`, all_of(var_names_1)) %>%
  group_by(`Fiber Type Num`, `Fiber Type`) %>%
  nest() %>%
  mutate(
    models = map(data, function(df) {
      map(var_names_1, ~ fit_model_rep(df, .x))
    }),
    model_summary = map(models, ~ map(.x, anova)),
    p_values = map(model_summary, ~ map_chr(.x, ~ format(.x[["Pr(>F)"]][1], scientific = TRUE, digits = 10))), 
    f_values = map(model_summary, ~ map_dbl(.x, ~ .x[["F value"]][1])),
    posthoc = map(models, ~map(.x, posthoc_analysis))
  )

pvals_model1 <- model1 %>%
       select(`Fiber Type Num`, `Fiber Type`, p_values, f_values) %>%
       mutate(var = list(var_names_1)) %>%
       unnest(c(p_values, f_values, var)) %>%
       rename(
             p_value = p_values,
             f_value = f_values,
             variable = var
         )

posthoc_model1 <- model1 %>%
  select(`Fiber Type Num`, `Fiber Type`, posthoc) %>%
  mutate(variable = list(var_names_1)) %>%
  unnest(c(posthoc, variable)) %>%
  unnest(posthoc)


# Model 2: Repeated measures linear mixed model for FSD variables (MHC IIX & IIB only)

var_names_2 <- c("FSD", "FSD:F0", "FSD:Total", "a2", "r2", "a3", "r3", "a4", "r4")

model2 <- sddata %>% 
  filter(`Fiber Type Num` %in% c(3,4)) %>% 
  select(Mouse, Exp_Con_Num, `Fiber Type Num`, all_of(var_names_2)) %>%
  group_by(`Fiber Type Num`, `Fiber Type`) %>%
  nest() %>%
  mutate(
    models = map(data, function(df) {
      map(var_names_2, ~ fit_model_rep(df, .x))
    }),
    model_summary = map(models, ~ map(.x, anova)),
    p_values = map(model_summary, ~ map_chr(.x, ~ format(.x[["Pr(>F)"]][1], scientific = TRUE, digits = 10))), 
    f_values = map(model_summary, ~ map_dbl(.x, ~ .x[["F value"]][1])),
    posthoc = map(models, ~map(.x, posthoc_analysis))
  )


pvals_model2 <- model2 %>%
  select(`Fiber Type Num`, `Fiber Type`, p_values, f_values) %>%
  mutate(var = list(var_names_2)) %>%
  unnest(c(p_values, f_values, var)) %>%
  rename(
    p_value = p_values,
    f_value = f_values,
    variable = var
  )

posthoc_model2 <- model2 %>%
  select(`Fiber Type Num`, `Fiber Type`, posthoc) %>%
  mutate(variable = list(var_names_2)) %>%
  unnest(c(posthoc, variable)) %>%
  unnest(posthoc)


write.xlsx(
  list(
    "Model1_Pvalues"  = pvals_model1,
    "Model1_Posthoc"  = posthoc_model1,
    "Model2_Pvalues"  = pvals_model2,
    "Model2_Posthoc"  = posthoc_model2
  ),
  file = "SDModelResults.xlsx",
  overwrite = TRUE
)

