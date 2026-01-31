#########################################
#### create objects for final report ####
#########################################

library(modelsummary)
library(kableExtra)


#### main specification

models_main <- list(
  "Baseline" = m1,
  "Model 2"  = m2,
  "Model 3"  = m3,
  "Baseline " = m1_matched,
  "Model 2 "  = m2_matched,
  "Model 3 "  = m3_matched
)

table_main <- modelsummary(
  models_main,
  coef_omit = "^(?!school_nearby$).*",
  coef_map  = c("school_nearby" = "School nearby"),
  
  add_rows = data.frame(
    term = c("Building controls", "Neighborhood controls", "Region fixed effects"),
    Baseline  = c("-", "-", "✔"),
    `Model 2` = c("✔", "-", "✔"),
    `Model 3` = c("✔", "✔", "✔"),
    `Baseline `  = c("-", "-", "✔"),
    `Model 2 `   = c("✔", "-", "✔"),
    `Model 3 `   = c("✔", "✔", "✔"),
    check.names = FALSE
  ),
  
  gof_map = data.frame(
    raw   = c("nobs", "r.squared"),
    clean = c("Observations", "R²"),
    fmt   = c(0, 3)
  ),
  
  title = "Effect of Secondary School Proximity on House Prices",
  stars = TRUE,
  gof_omit = "IC|Log|Adj",
  notes = "Note: Standard errors clustered at the municipality level in parentheses.",
  
  output = "kableExtra"
) |>
  add_header_above(c(" " = 1, "Unmatched sample" = 3, "Matched sample" = 3)) |>
  kable_styling(full_width = FALSE)

table_main



#### heterogeneity model



modelsummary(
  list(
    "Unmatched sample" = model_heterogeneity,
    "Matched sample"   = model_heterogeneity_matched
  ),
  statistic = "({std.error})",
  stars = c('*' = .10, '**' = .05, '***' = .01),
  
  coef_omit = "^(?!(school_nearby$|school_nearby:abitur_nearby$)).*",
  coef_map  = c(
    "school_nearby" = "School nearby",
    "school_nearby:abitur_nearby" = "School nearby × Gymnasium nearby"
  ),
  
  gof_map = data.frame(
    raw   = c("nobs", "r.squared"),
    clean = c("Observations", "R²"),
    fmt   = c(0, 3)
  ),
  
  add_rows = data.frame(
    term = c("Building controls", "Neighborhood controls", "Region fixed effects"),
    `Unmatched sample` = c("✔", "✔", "✔"),
    `Matched sample`   = c("✔", "✔", "✔"),
    check.names = FALSE
  ),
  
  title = "Effect of School with academic track proximity on House Prices",
  gof_omit = "IC|Log|Adj",
  notes = "Note: Standard errors clustered at the municipality level in parentheses."
)



