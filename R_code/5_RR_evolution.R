# 1. Loading packages
# 2. Data importation
# 3. Parameters
# 4. Data preparation
# 5. Modification of RR effect (m)
# 6. Attribution of RR to each food group
# 7. TTFE
# 8. Calculation of the RR with TTFE
# 9. Combinaison of the RR of each food group by year
# 10. Combine RR of diets by year
# 11. Relative RR of diets to baseline
# 12. Graphs : Relative RR of diets to baseline
# 13. Data exportation

################################################################################################################################
#                                             1. Loading packages                                                              #
################################################################################################################################

pacman::p_load(
    rio, # File import/export
    here, # File path management
    dplyr, # Data manipulation
    tidyr, # Data manipulation
    tidyverse # Data management, ggplot included
)

################################################################################################################################
#                                             2. Data importation                                                              #
################################################################################################################################

# RR by intake (g/day), simulated dose-response relationships
rr_table <- import(here("data_clean", "rr_table_interpolated_sim.csv"))

# Expositions : diets evolution through time
diets_evo <- import(here("results", "diets", "diets_evo.csv"))

################################################################################################################################
#                                             3. Parameters                                                                    #
################################################################################################################################

source(here("R_code", "0_parameters.R"))

################################################################################################################################
#                                             4. Data preparation                                                              #
################################################################################################################################

# Rename RR column
rr_table <- rr_table %>%
  rename("rr" = "rr_interpolated")

################################################################################################################################
#                                             5. Modification of RR effect (m)                                                 #
################################################################################################################################

rr_table <- rr_table %>%
  mutate(rr_a = case_when(
    rr < 1 ~ rr + (1 - rr) * (1 - m),
    rr >= 1 ~ 1 / (m / rr + 1 - m)
  )) %>%
  select("simulation_id", "food_group", "quantity", "rr_a") %>%
  rename("rr" = "rr_a")

################################################################################################################################
#                                             6. Attribution of RR to each food group                                           #
################################################################################################################################

# Rounding intakes to match the RR table
diets_evo <- diets_evo %>%
  mutate(quantity = round(quantity)) %>%
  left_join(rr_table, by = c("food_group", "quantity"), relationship = "many-to-many")

# Calculation of the mean and 95% CI for each year
ic95_rr_fg <- diets_evo %>%
  group_by(food_group, scenario, year, quantity) %>%
  summarise(
    mean_rr = mean(rr, na.rm = TRUE), # Mean of simulated RR
    lower_ci = quantile(rr, 0.025, na.rm = TRUE), # Lower limit of the 95% CI
    upper_ci = quantile(rr, 0.975, na.rm = TRUE) # Upper limit of the 95% CI
  ) %>% 
  ungroup()

################################################################################################################################
#                                             7. TTFE                                                                          #
################################################################################################################################

# TTFE duration
ttfe <- tibble(0:ttfe_time) %>%
  rename("time" = "0:ttfe_time")

# Calculation of the % allocated to the RR over the duration of the TTFE
ttfe <- ttfe %>%
  mutate(ttfe = case_when(
    ttfe_time == 0 ~ 1,
    ttfe_dynamics == "immediate" & ttfe$time == 0 ~ 1,
    ttfe_dynamics == "immediate" & ttfe$time > 0 ~ NA_real_,
    ttfe_dynamics == "linear" ~ time / ttfe_time,
    ttfe_dynamics == "cosine" ~ (1 - cos(pi * (time / ttfe_time)^p_ttfe)) / 2,
    ttfe_dynamics == "sigmoidal" ~ (1 / (1 + exp(-lambda_ttfe * (time / ttfe_time - 1 / 2))) - 1 / (1 + exp(lambda_ttfe / 2))) /
      (1 - 2 / (1 + exp(lambda_ttfe / 2))),
    ttfe_dynamics == "log" ~ log(1 + eta_ttfe * time / ttfe_time) / log(1 + eta_ttfe),
    TRUE ~ NA_real_
  ))

################################################################################################################################
#                                             8. Calculation of the RR with TTFE                                               #
################################################################################################################################

# Calculation of RR values over the duration of the TTFE
# After the end of the TTFE: RR = NA

diets_evo <- diets_evo %>%
  rowwise() %>%
  mutate(year_n = list(seq(from = (year_i - 2 * ttfe_time), to = (year_f + 2 * ttfe_time)))) %>%
  unnest(year_n) %>%
  mutate(rr_n = case_when(
    year_n < year ~ NA_real_,
    year_n >= year & year_n <= year + max(ttfe$time) ~ rr * ttfe$ttfe[match(year_n - year, ttfe$time)],
    year_n > year + max(ttfe$time) ~ NA_real_
  )) %>%
  ungroup()

################################################################################################################################
#                                             9. Combinaison of the RR of each food group by year                              #
################################################################################################################################

# RR of a food group in a year is the mean of the RR of this food group generated with the TTFE for this year
rr_evo_food_combined <- diets_evo %>%
  group_by(scenario, year_n, food_group, simulation_id) %>%
  summarize(
    mean_rr = sum(rr_n, na.rm = TRUE) / sum(ttfe$ttfe[match(pmin(year_n - year, ttfe_time), ttfe$time)], na.rm = TRUE), 
    # pmin compares year_n - year with ttfe_time and chooses the minimum value
    # to avoided considering the whole TTFE duration when year_n - year < ttfe_time
    .groups = "drop"
  ) %>% 
  ungroup()

# Calculation of the mean and 95% CI for each year
ic95_rr_fg_combined <- rr_evo_food_combined %>%
  group_by(food_group, scenario, year_n) %>%
  summarise(
    combined_rr = mean(mean_rr, na.rm = TRUE),
    lower_ci = quantile(mean_rr, 0.025, na.rm = TRUE), # Lower limit of the 95% CI
    upper_ci = quantile(mean_rr, 0.975, na.rm = TRUE) # Upper limit of the 95% CI
  ) %>% 
  ungroup()

################################################################################################################################
#                                             10. Combine RR of diets by year                                                  #
################################################################################################################################

# RR of a complete diet is calculated as the product of the RR of each food for a year

calc_combined_rr <- function(df) {
  df %>%
    group_by(scenario, year_n, simulation_id) %>%
    summarize(combined_rr = prod(mean_rr, na.rm = TRUE)) %>%
    ungroup()
}

rr_evo_diets <- calc_combined_rr(rr_evo_food_combined) %>%
  rename("year" = "year_n")

# Calculation of the mean and 95% CI for each year
ic95_rr_diets <- rr_evo_diets %>%
  group_by(scenario, year) %>%
  summarise(
    mean_rr = mean(combined_rr, na.rm = TRUE),
    lower_ci = quantile(combined_rr, 0.025, na.rm = TRUE), # Lower limit of the 95% CI
    upper_ci = quantile(combined_rr, 0.975, na.rm = TRUE) # Upper limit of the 95% CI
  ) %>% 
  ungroup()

################################################################################################################################
#                                             11. Relative RR of diets to baseline                                             #
################################################################################################################################

# Calculation of the relative RR of diets to baseline
rr_evo_diets <- rr_evo_diets %>%
  group_by(year, simulation_id) %>%
  mutate(relative_rr = combined_rr / combined_rr[scenario == "actuel"]) %>% # Change the baseline if needed !
  ungroup()

# While the implementation of the diets has not started, the relative RR is equal to 1
rr_evo_diets <- rr_evo_diets %>%
  mutate(relative_rr = case_when(
    year == year_i - 2 * ttfe_time ~ 1,
    TRUE ~ relative_rr
  ))

# Calculation of the mean and 95% CI for each year
ic95_rr_diets_relative <- rr_evo_diets %>%
  group_by(scenario, year) %>%
  summarise(
    mean_rr = mean(relative_rr, na.rm = TRUE),
    lower_ci = quantile(relative_rr, 0.025, na.rm = TRUE), # Lower limit of the 95% CI
    upper_ci = quantile(relative_rr, 0.975, na.rm = TRUE) # Upper limit of the 95% CI
  ) %>% 
  ungroup()

################################################################################################################################
#                                             12. Graphs : Relative RR of diets to baseline                                    #
################################################################################################################################

graph_rr_diets_rel <- ggplot(
  ic95_rr_diets_relative %>%
    filter(scenario != "actuel"),
  aes(
    x = year,
    y = mean_rr,
    color = scenario
  )
) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = scenario), alpha = 0.5, linetype = 0) +
  geom_line(linewidth = 0.6, na.rm = TRUE) +
  labs(
    title = "",
    x = "",
    y = "RR",
    color = "Scenario",
    fill = "Scenario"
  ) +
  scale_color_manual(
    values = col_scenario,
    labels = labels_scenario
  ) +
  scale_fill_manual(
    values = col_scenario,
    labels = labels_scenario
  ) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    strip.text = element_text(face = "bold", size = rel(1)),
    legend.position = "bottom"
  ) +
  guides(
    color = guide_legend(title = NULL),
    fill = guide_legend(title = NULL)
  )

plot(graph_rr_diets_rel)

################################################################################################################################
#                                             13. Data exportation                                                             #
################################################################################################################################

# RR associated with each food group intake (no TTFE considered)
export(ic95_rr_fg, here("results", "RR", "IC95_rr_fg_intakes.xlsx"))

# RR associated with each food group intake (with TTFE considered)
export(rr_evo_food_combined, here("results", "RR", "rr_evo_fg.csv"))
export(ic95_rr_fg_combined, here("results", "RR", "IC95_rr_evo_fg.xlsx"))

# RR of diets (absolute and relative to baseline)
export(rr_evo_diets, here("results", "RR", "rr_evo_diets.csv"))
export(ic95_rr_diets, here("results", "RR", "IC95_rr_evo_diets.xlsx"))
export(ic95_rr_diets_relative, here("results", "RR", "IC95_rr_evo_diets_relative.xlsx"))

ggsave(here("results", "RR", "rr_evo_diets_rel.pdf"), graph_rr_diets_rel)
