################################################################################################################################
#                                             1. Loading packages                                                              #
################################################################################################################################

pacman::p_load(
    rio, # File import/export
    here, # File path management
    dplyr, # Data manipulation
    tidyr, # Data manipulation
    tidyverse, # Data management, ggplot included
    flextable, # Creation of tables
    scales # Formatting of numbers, notably for percentages
)

################################################################################################################################
#                                             2. Data importation                                                              #
################################################################################################################################

# RR of each food group for each year and scenarioe, considering the time to full effect
rr_evo_food_combined <- import(here("results", "RR", "rr_evo_fg.csv"))

# Variations of food intakes
diets_var <- import(here("results", "diets", "diets_var.csv"))

# Total deaths for each year and scenario
ic95_total_deaths <- import(here("results", "HIA", "IC95_tot_deaths.xlsx"))

################################################################################################################################
#                                             3. Parameters                                                                    #
################################################################################################################################

source(here("R_code", "0_parameters.R"))

################################################################################################################################
#                                             4 . RR of food group relative to baseline                                        #
################################################################################################################################

rr_fg_relative <- rr_evo_food_combined %>%
  group_by(year_n, food_group, simulation_id) %>%
  mutate(rr_fg_relative = mean_rr / mean_rr[scenario == "actuel"]) %>% # Change the baseline if necessary
  ungroup()

# While the implementation of diets has not started, the relative RR is equal to 1
rr_fg_relative <- rr_fg_relative %>%
  mutate(rr_fg_relative = case_when(
    year_n == year_i - 2 * ttfe_time ~ 1,
    TRUE ~ rr_fg_relative
  ))

# Mean and 95% confidence interval of the relative RR for each food group and scenario
ic95_rr_fg_relative <- rr_fg_relative %>%
  group_by(scenario, year_n, food_group) %>%
  summarise(
    mean_rr_fg = mean(rr_fg_relative, na.rm = TRUE),
    lower_ci = quantile(rr_fg_relative, 0.025, na.rm = TRUE), # Lower limit of the 95% CI
    upper_ci = quantile(rr_fg_relative, 0.975, na.rm = TRUE) # Upper limit of the 95% CI
  ) %>% 
  ungroup()

################################################################################################################################
#                                             5. Contribution to the result of foood group intakes                             #
################################################################################################################################

# Calculation of the contribution to the result of the variation in consumption of each food group compared to the baseline
contrib <- ic95_rr_fg_relative %>%
  mutate(
    delta = -(1 - mean_rr_fg) * 100,
    delta_low = -(1 - lower_ci) * 100,
    delta_upp = -(1 - upper_ci) * 100
  ) %>%
  select(scenario, year_n, food_group, delta, delta_low, delta_upp) %>%
  rename("year" = "year_n")

################################################################################################################################
#                                             6. Heat maps                                                                     #
################################################################################################################################

# Order of the food groups
diets_var$food_group <- factor(diets_var$food_group, levels = order_food_groups)

# Heat map of the variation in food intake to the baseline
hm_var <- ggplot(data = diets_var %>%
               filter(scenario != "actuel",
                      year == 2050),
             aes(x = scenario, y = food_group, fill = sign(var) * log1p(abs(var)))) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  coord_fixed() +
  scale_fill_gradient2(low = "#002414", 
                       high = "#8b0066", 
                       mid = "white", 
                       midpoint = 0,
                       limits = c(min(sign(diets_var$var) * log1p(abs(diets_var$var)), na.rm = TRUE), 
                                  max(sign(diets_var$var) * log1p(abs(diets_var$var)), na.rm = TRUE)),
                       breaks = sign(c(-50, -10, 0, 20, 100, 500)) * log1p(abs(c(-50, -10, 0, 20, 100, 500))),  # Breaks adaptés
                       labels = c("- 50%", "- 10%", "0%", "20%", "100%", "500%")) +  # Étiquettes adaptées) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
  plot.title = element_text(size = rel(2), face = "bold", hjust = 0.5),
  axis.title.x = element_text(size = rel(1.5)),
  axis.title.y = element_text(size = rel(1.5)),
  legend.key.size = unit(1.2, "cm"), 
  legend.position = "right",
  legend.title = element_text(face = "bold", size = rel(1.5))) +
  labs(x = "", y = "", fill = "Intake variation") +
  ggtitle("Intake variation of several food groups\n compared to the baseline scenario") +
  scale_x_discrete(labels = labels_scenario[diets_var$scenario]) +
  scale_y_discrete(labels = labels_food_groups)

plot(hm_var)

# Order of the food groups
contrib$food_group <- factor(contrib$food_group, levels = order_food_groups)

# Heat map of the change in mortality due to the variation in food intake to the baseline
hm_contrib <- ggplot(data = contrib %>%
               filter(scenario != "actuel",
                      year == 2050),
             aes(x = scenario, y = food_group, fill = delta)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  coord_fixed() +
  scale_fill_gradient2(low = "#00388d", high = "#850028", mid = "#ffffff", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
  plot.title = element_text(size = rel(2), face = "bold", hjust = 0.5),
  axis.title.x = element_text(size = rel(1.5)),
  axis.title.y = element_text(size = rel(1.5)), 
  legend.position = "right",
  legend.title = element_text(face = "bold", size = rel(1.5))) +
  labs(x = "", y = "", fill = "Change in\nmortality (%)") +
  ggtitle("Change in mortality due to the intake variation of several\nfood groups compared to the baseline in 2050") +
  scale_x_discrete(labels = labels_scenario) +
  scale_y_discrete(labels = labels_food_groups_delta)

plot(hm_contrib)

################################################################################################################################
#                                             7. Tables                                                                        #
################################################################################################################################

## PLEASE RUN AGAIN PART 5 SO THAT THE ORDER OF FOOD GROUPS IS RESET ##

# Table of the variations and contributions of food intakes to the results for the year 2050
summary_contrib <- contrib %>%
  inner_join(ic95_total_deaths, by = c("scenario", "year")) %>%
  inner_join(diets_var, by = c("scenario", "year", "food_group")) %>%  
  group_by("scenario", "year", "food_group") %>%
  mutate(
    deaths = delta * mean_tot_deaths / 100,
    var = percent(var / 100, accuracy = 0.1),
    delta = percent(delta / 100, accuracy = 0.1)
  ) %>%
  ungroup() %>%
  select(scenario, year, food_group, quantity, var, delta, deaths) %>%
  mutate(
    quantity = round(quantity),
    deaths = round(deaths),
    food_group = factor(food_group,
                             levels = order_food_groups,
                             labels = labels_food_groups[order_food_groups]))

contrib_2050 <- summary_contrib %>%
  filter(
    year == 2050,
    scenario != "actuel"
  ) %>%
  pivot_wider(
    names_from = "scenario",
    values_from = c("quantity", "var", "delta", "deaths")
  ) %>%
  select(-"year") %>%
  select(
    "food_group",
    "quantity_sc1", "var_sc1", "delta_sc1", "deaths_sc1",
    "quantity_sc2", "var_sc2", "delta_sc2", "deaths_sc2",
    "quantity_sc3", "var_sc3", "delta_sc3", "deaths_sc3",
    "quantity_sc4", "var_sc4", "delta_sc4", "deaths_sc4",
  ) %>%
  qflextable() %>% # Création du tableau et ajustement automatique de la largeur des colonnes
  add_header_row(
    top = TRUE, # Ajout d'une ligne d'en-tête
    values = c("Food group", "S1", "", "", "", "S2", "", "", "", "S3", "", "", "", "S4", "", "", "")
  ) %>%
  set_header_labels( # Renommer des colonnes de la 2e ligne d'en-tête
    "food_group" = "",
    "quantity_sc1" = "Intake in 2050 (g/d/pers)",
    "var_sc1" = "Intake variation vs baseline",
    "delta_sc1" = "Change in mortality",
    "deaths_sc1" = "Deaths due to intake variation",
    "quantity_sc2" = "Intake in 2050 (g/d/pers)",
    "var_sc2" = "Intake variation vs baseline",
    "delta_sc2" = "Change in mortality",
    "deaths_sc2" = "Deaths due to intake variation",
    "quantity_sc3" = "Intake in 2050 (g/d/pers)",
    "var_sc3" = "Intake variation vs baseline",
    "delta_sc3" = "Change in mortality",
    "deaths_sc3" = "Deaths due to intake variation",
    "quantity_sc4" = "Intake (Intake in 2050 (g/d/pers)",
    "var_sc4" = "Intake variation vs baseline",
    "delta_sc4" = "Change in mortality",
    "deaths_sc4" = "Deaths due to intake variation"
  ) %>%
  vline(part = "all", j = 5) %>% # Ligne verticale après la colonne 5
  vline(part = "all", j = 9) %>% # Ligne verticale après la colonne 9
  vline(part = "all", j = 13) %>% # Ligne verticale après la colonne 13
  merge_at(i = 1, j = 2:5, part = "header") %>% # Fusion des cellules de la 1ère ligne d'en-tête
  merge_at(i = 1, j = 6:9, part = "header") %>%
  merge_at(i = 1, j = 10:13, part = "header") %>%
  merge_at(i = 1, j = 14:17, part = "header") %>%
  align(align = "center", j = c(2:17), part = "all") %>% # Centrer le contenu des cellules sauf Food group
  bold(i = 1, part = "header") %>% # Mettre en gras la 1ère ligne d'en-tête
  bg(part = "all", bg = "white")
  #bg(., i = ~ food_group %in% c("Whole grains"), j = c("food_group", "delta_sc3", "delta_sc4"), part = "body", bg = "#b95151") %>%
  #bg(., i = ~ food_group %in% c("Fruits", "Nuts", "Fish", "Eggs"), j = c("food_group", "delta_sc3", "delta_sc4"), part = "body", bg =  "#f38585")

plot(contrib_2050)
################################################################################################################################
#                                             8. Data exportation                                                              #
################################################################################################################################

# Change in mortality due to the variation in food intake to the baseline
export(contrib, here("results", "contributions", "FG_contributions.xlsx"))

# Table of the variations and contributions of food intakes to the results for the year 2050
save_as_image(contrib_2050, here("results", "contributions", "contributions_2050.png"))

# Heat maps of variation in food intake and change in mortality in 2050
ggsave(here("results", "contributions", "hm_contrib.pdf"), hm_contrib)
ggsave(here("results", "contributions", "hm_var.pdf"), hm_var)
