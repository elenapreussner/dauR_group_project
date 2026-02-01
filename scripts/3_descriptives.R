library(tidyverse)
library(ggplot2)

#####################################
#### general summary statistics 
#####################################

# define variable groups
housing_vars <- c("living_area", "site_area", "rooms_n", "baths_n", 
                  "age_building", "cellar")
neighborhood_vars <- c("immigrants_percents", "average_age", 
                       "pharmacy", "hospital", "doctors", "park")
price_var <- c("price_sqm")

all_vars <- c(price_var, housing_vars, neighborhood_vars)


#####################################
#### three-way comparison
#####################################

# create three groups
analysis_data_3groups <- full_dataset_main_clean %>%
  filter(price_sqm < 10000) %>%
  mutate(
    non_abitur_nearby = if_else(school_nearby == 1 & abitur_nearby == 0, 1L, 0L),
    group = case_when(
      abitur_nearby == 1 ~ "abitur",
      school_nearby == 1 & abitur_nearby == 0 ~ "non_abitur",
      school_nearby == 0 ~ "control",
      TRUE ~ NA_character_
    ),
    group = factor(group, levels = c("abitur", "non_abitur", "control"))
  )

# summary statistics for 3 groups
summary_3groups <- analysis_data_3groups %>%
  filter(!is.na(group)) %>%
  group_by(group) %>%
  summarise(
    n = n(),
    across(all_of(all_vars), 
           list(mean = ~mean(., na.rm = TRUE),
                sd = ~sd(., na.rm = TRUE)),
           .names = "{.col}_{.fn}"),
    .groups = "drop"
  )

# price table by group
price_table_3groups <- analysis_data_3groups %>%
  filter(!is.na(group)) %>%
  group_by(group) %>%
  summarise(
    n = n(),
    mean = mean(price_sqm, na.rm = TRUE),
    sd = sd(price_sqm, na.rm = TRUE),
    median = median(price_sqm, na.rm = TRUE),
    min = min(price_sqm, na.rm = TRUE),
    max = max(price_sqm, na.rm = TRUE),
    .groups = "drop"
  )

# balance tests - three comparisons
vars <- c("price_sqm", "living_area", "site_area", "rooms_n", "baths_n", 
          "age_building")

# abitur vs. control
balance_tests_abitur_control <- lapply(vars, function(v) {
  test <- t.test(as.formula(paste(v, "~ abitur_nearby")), 
                 data = analysis_data_3groups %>% filter(group %in% c("abitur", "control")))
  data.frame(
    comparison = "Abitur vs. Control",
    variable = v,
    mean_diff = test$estimate[1] - test$estimate[2],
    t_stat = test$statistic,
    p_value = test$p.value
  )
}) %>% bind_rows()

# non-abitur vs. control
balance_tests_non_abitur_control <- lapply(vars, function(v) {
  test <- t.test(as.formula(paste(v, "~ non_abitur_nearby")), 
                 data = analysis_data_3groups %>% filter(group %in% c("non_abitur", "control")))
  data.frame(
    comparison = "Non-Abitur vs. Control",
    variable = v,
    mean_diff = test$estimate[1] - test$estimate[2],
    t_stat = test$statistic,
    p_value = test$p.value
  )
}) %>% bind_rows()

# abitur vs. non-abitur
balance_tests_abitur_non_abitur <- lapply(vars, function(v) {
  data_abitur <- analysis_data_3groups %>% filter(group == "abitur") %>% pull(!!sym(v))
  data_non_abitur <- analysis_data_3groups %>% filter(group == "non_abitur") %>% pull(!!sym(v))
  test <- t.test(data_abitur, data_non_abitur)
  data.frame(
    comparison = "Abitur vs. Non-Abitur",
    variable = v,
    mean_diff = test$estimate[1] - test$estimate[2],
    t_stat = test$statistic,
    p_value = test$p.value
  )
}) %>% bind_rows()

# combine all tests
balance_tests_3groups <- bind_rows(
  balance_tests_abitur_control,
  balance_tests_non_abitur_control,
  balance_tests_abitur_non_abitur
) %>%
  mutate(
    mean_diff = round(mean_diff, 3),
    t_stat = round(t_stat, 3),
    p_value = sprintf("%.3f", p_value)
  ) %>%
  select(comparison, variable, mean_diff, t_stat, p_value)

# plots 
plot_distribution_3groups <- analysis_data_3groups %>%
  filter(!is.na(group)) %>%
  ggplot(aes(x = price_sqm, fill = group)) +
  geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
  facet_wrap(~ group, ncol = 1, scales = "free_y") +  
  scale_fill_brewer(palette = "Accent") +
  labs(
    title = "Distribution of House Prices by Treatment Status",
    x = "Price per sqm (EUR)",
    y = "Number of Houses"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

plot_boxplot_3groups <- analysis_data_3groups %>%
  filter(!is.na(group)) %>%
  ggplot(aes(x = group, y = price_sqm, fill = group)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Accent") +
  labs(
    title = "House Prices by Treatment Group",
    x = "Group",
    y = "Price per sqm (EUR)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#####################################
#### simple comparison
#####################################

# create two groups (school_nearby vs. control)
analysis_data_2groups <- full_dataset_main_clean %>%
  filter(price_sqm < 10000) %>%
  mutate(
    group = if_else(school_nearby == 1, "School nearby", "Control"),
    group = factor(group, levels = c("School nearby", "Control"))
  )

# summary statistics for 2 groups
summary_2groups <- analysis_data_2groups %>%
  group_by(group) %>%
  summarise(
    n = n(),
    across(all_of(all_vars), 
           list(mean = ~mean(., na.rm = TRUE),
                sd = ~sd(., na.rm = TRUE)),
           .names = "{.col}_{.fn}"),
    .groups = "drop"
  )

# price table by group
price_table_2groups <- analysis_data_2groups %>%
  group_by(group) %>%
  summarise(
    n = n(),
    mean = mean(price_sqm, na.rm = TRUE),
    sd = sd(price_sqm, na.rm = TRUE),
    median = median(price_sqm, na.rm = TRUE),
    min = min(price_sqm, na.rm = TRUE),
    max = max(price_sqm, na.rm = TRUE),
    .groups = "drop"
  )

# balance tests - school_nearby vs. control
balance_tests_2groups <- lapply(vars, function(v) {
  test <- t.test(as.formula(paste(v, "~ school_nearby")), 
                 data = full_dataset_main_clean %>% filter(price_sqm < 10000))
  data.frame(
    variable = v,
    mean_treatment = test$estimate[1],
    mean_control = test$estimate[2],
    mean_diff = test$estimate[1] - test$estimate[2],
    t_stat = test$statistic,
    p_value = test$p.value
  )
}) %>% 
  bind_rows() %>%
  mutate(
    mean_treatment = round(mean_treatment, 3),
    mean_control = round(mean_control, 3),
    mean_diff = round(mean_diff, 3),
    t_stat = round(t_stat, 3),
    p_value = sprintf("%.3f", p_value)
  )

# plots 
plot_distribution_2groups <- analysis_data_2groups %>%
  ggplot(aes(x = price_sqm, fill = group)) +
  geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
  facet_wrap(~ group, ncol = 1, scales = "free_y") +  
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Distribution of House Prices (School Nearby vs. Control)",
    x = "Price per sqm (EUR)",
    y = "Number of Houses"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

plot_boxplot_2groups <- analysis_data_2groups %>%
  ggplot(aes(x = group, y = price_sqm, fill = group)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "House Prices: School Nearby vs. Control",
    x = "Group",
    y = "Price per sqm (EUR)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
