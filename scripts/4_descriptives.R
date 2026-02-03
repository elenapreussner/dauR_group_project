#########################
##### prerequisites #####
#########################

library(tidyverse)
library(ggplot2)


##################################### 
#### summary statistics #############
#####################################

summary_4groups <- bind_rows(
  full_dataset_main_clean %>% 
    filter(price_sqm < 10000, school_nearby == 1) %>% 
    mutate(group = "school_nearby"),
  full_dataset_main_clean %>% 
    filter(price_sqm < 10000, abitur_nearby == 1) %>% 
    mutate(group = "abitur"),
  full_dataset_main_clean %>% 
    filter(price_sqm < 10000, school_nearby == 1, abitur_nearby == 0) %>% 
    mutate(group = "non_abitur"),
  full_dataset_main_clean %>% 
    filter(price_sqm < 10000, school_nearby == 0) %>% 
    mutate(group = "control")
  ) %>%
  group_by(group) %>%
  summarise(n = n(), across(c(price_sqm, living_area, site_area, rooms_n, 
                              baths_n, age_building, cellar), 
                            list(mean = ~mean(., na.rm = TRUE), 
                                 sd = ~sd(., na.rm = TRUE)), 
                            .names = "{.col}_{.fn}"), .groups = "drop")

summary_4groups_table <- summary_4groups %>%
  mutate(group = factor(group, levels = c("school_nearby", "abitur", 
                                          "non_abitur", "control"))) %>%
  arrange(group) %>%
  select(group, price_sqm_mean, price_sqm_sd, living_area_mean, living_area_sd,
         site_area_mean, site_area_sd, rooms_n_mean, rooms_n_sd, baths_n_mean, 
         baths_n_sd, age_building_mean, age_building_sd, cellar_mean, 
         cellar_sd) %>%
  pivot_longer(cols = -group) %>%
  separate(name, into = c("var", "stat"), sep = "_(?=mean$|sd$)") %>%
  mutate(var = case_when(
    var == "price_sqm" ~ "Price per sqm",
    var == "living_area" ~ "Living Area",
    var == "site_area" ~ "Site Area",
    var == "rooms_n" ~ "Rooms",
    var == "baths_n" ~ "Baths",
    var == "age_building" ~ "Building Age",
    var == "cellar" ~ "Cellar",
    TRUE ~ var
  )) %>%
  pivot_wider(names_from = c(group, stat), values_from = value, 
              names_sep = "_") %>%
  select(var, school_nearby_mean, school_nearby_sd, abitur_mean, abitur_sd,
         non_abitur_mean, non_abitur_sd, control_mean, control_sd)

##################################### 
#### balance check ##################
#####################################

balance_summary <- summary(balance_check)$sum.all %>%
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  filter(Variable != "distance")

vars <- balance_summary$Variable
t_stats <- map_dbl(vars, ~{
  t.test(as.formula(paste(.x, "~ school_nearby")), 
         data = full_dataset_main_clean)$statistic
})

balance_table <- balance_summary %>%
  select(Variable, `Means Treated`, `Means Control`, `Std. Mean Diff.`) %>%
  mutate(t_statistic = t_stats)

##################################### 
#### balance check plot #############
#####################################

balance_plot_data <- bind_rows(
  summary(balance_check)$sum.all %>%
    as.data.frame() %>%
    rownames_to_column("Variable") %>%
    mutate(method = "Before Matching") %>%
    select(Variable, method, std_diff = `Std. Mean Diff.`),
  summary(ps_main)$sum.matched %>%
    as.data.frame() %>%
    rownames_to_column("Variable") %>%
    mutate(method = "After Matching") %>%
    select(Variable, method, std_diff = `Std. Mean Diff.`)
) %>%
  filter(Variable != "distance") %>%
  mutate(std_diff_abs = abs(std_diff))

balance_plot <- ggplot(balance_plot_data, aes(x = std_diff_abs, y = Variable, 
                                              color = method, shape = method)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0.1, linetype = "dashed", color = "black") +
  labs(
    x = "Standardized Mean Difference",  # Option 1
    # x = "Balance (Std. Mean Diff.)",              # Option 2
    # x = "Group Difference (standardized)",         # Option 3
    y = "",
    color = "Method",
    shape = "Method"
  ) +
  scale_color_manual(values = c("Before Matching" = "#F8766D", "After Matching" = "#00BFC4")) +
  scale_shape_manual(values = c("Before Matching" = 16, "After Matching" = 17)) +
  theme_minimal() +
  theme(legend.position = "right")
