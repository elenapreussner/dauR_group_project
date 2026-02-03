#########################################
#### create objects for final report ####
#########################################

library(modelsummary)
library(kableExtra)

is_latex <- knitr::is_latex_output()

#### main specification

models_main <- list(
  "Baseline"      = m1,
  "Intermediate"  = m2,
  "Full"          = m3,
  "Baseline"     = m1_matched,
  "Intermediate" = m2_matched,
  "Full"         = m3_matched
)

table_main <- modelsummary(
  models_main,
  coef_omit = "^(?!school_nearby$).*",
  coef_map  = c("school_nearby" = "School nearby"),
  
  add_rows = data.frame(
    term = c("Building controls", "Neighborhood controls", 
             "Region fixed effects"),
    "(1)" = c("-", "-", "✔"),
    "(2)" = c("✔", "-", "✔"),
    "(3)" = c("✔", "✔", "✔"),
    "(4)" = c("-", "-", "✔"),
    "(5)" = c("✔", "-", "✔"),
    "(6)" = c("✔", "✔", "✔"),
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
  notes = 
    "Note: Standard errors clustered at the municipality level in parentheses.",
  
  output = "latex"   
) |>
  add_header_above(
    c(" " = 1, "Unmatched sample" = 3, "Matched sample" = 3)
  ) |>
  kable_styling(
    full_width = FALSE,
    position = "center",
    latex_options = c("hold_position", "booktabs")
  )


table_main

#### heterogeneity model


table_heterogeneity <- modelsummary(
  list(
    "Full (U)" = model_heterogeneity,
    "Full (M)" = model_heterogeneity_matched
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
    term = c("Building controls", "Neighborhood controls", 
             "Region fixed effects"),
    "(1)" = c("✔", "✔", "✔"),
    "(2)" = c("✔", "✔", "✔"),
    check.names = FALSE
  ),
  
  title = "Effect of School with academic track proximity on House Prices",
  gof_omit = "IC|Log|Adj",
  notes = 
    "Note: Standard errors clustered at the municipality level in parentheses.",
  
  output = "latex"  
) |>
  add_header_above(c(" " = 1, "Unmatched sample" = 1, "Matched sample" = 1)) |>
  kable_styling(
    full_width = FALSE,
    position = "center",
    latex_options = c("hold_position", "booktabs")
  )

table_heterogeneity


###############################
#### save for presentation ####
###############################

# main specification

# writeLines(
#   as.character(table_main),
#   file.path("presentation/results_main_specification.tex")
# )
#   
# # heterogeneity model
# 
# 
# writeLines(
#   as.character(table_heterogeneity),
#   file.path("presentation/results_heterogeneity.tex")
# )