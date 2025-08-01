# 1. Loading packages
# 2. Data importation
# 3. Parameters
# 4. Functions for diets evolution
# 5. Diets evolution in scenarios
# 6. Intake variations compared to the baseline diet (%)
# 7. Data exportation

################################################################################################################################
#                                             1. Loading packages                                                              #
################################################################################################################################

pacman::p_load(
    rio, # file import/export
    here, # file path management
    dplyr, # data manipulation
    tidyr, # data manipulation
    tidyverse, # Data management, ggplot included
    flextable # Tables 
)

################################################################################################################################
#                                             2. Data importation                                                              #
################################################################################################################################

# Expositions : SISAE diets in 2050
diets <- import(here("data", "DOUGLAS_diets.xlsx"))

################################################################################################################################
#                                             3. Parameters                                                                    #
################################################################################################################################

source(here("R_code", "0_parameters.R"))

################################################################################################################################
#                                             4. Functions for diets evolution                                                 #
################################################################################################################################

# Linear implementation
calc_food_q_lin <- function(q_i, q_f, year_n, year_i, year_f) {
    (q_f - q_i) / (year_f - year_i) * (year_n - year_f) + q_f
}

# Cosine interpolation implementation
calc_food_q_cos <- function(q_i, q_f, year_n, year_i, year_f, p) {
    q_i + (q_f - q_i) * (1 - cos(pi * ((year_n - year_i) / (year_f - year_i))^p)) / 2
}

# sigmoidal implementation
calc_food_q_sig <- function(q_i, q_f, year_n, year_i, year_f, lambda) {
    (q_i + q_f) / 2 + (q_f - q_i) * (1 / (1 + exp(-lambda * ((year_n - year_i) / (year_f - year_i) - 1 / 2))) - 1 / 2) * (-1 / (2 * (1 / (1 + exp(lambda / 2)) - 1 / 2)))
}

################################################################################################################################
#                                             5. Diets evolution in scenarios                                                  #
################################################################################################################################

# Calculation of intakes of each food group for each year
diets_evo <- diets %>%
    select("food_group", "actuel", "sc1", "sc2", "sc3", "sc4") %>%
    filter(food_group %in% c(
        "red_meat", "processed_meat", "white_meat", "fish", "eggs", "dairy",
        "fruits", "vegetables", "legumes", "nuts", "whole_grains", "reffined_grains",
        "sugar_sweetened_beverages"
    )) %>%
    mutate(q_i = actuel) %>%
    pivot_longer(
        cols = c("actuel", "sc1", "sc2", "sc3", "sc4"),
        names_to = "scenario",
        values_to = "q_f"
    ) %>%
    crossing(year_n = (year_i - 20):(year_f + 20)) %>%
    mutate(quantity = case_when(
        implementation == "immediate" & year_n < year_i ~ q_i,
        implementation == "immediate" & year_n >= year_i ~ q_f,
        implementation == "linear" & year_n < year_i ~ q_i,
        implementation == "linear" & year_n %in% c(year_i:year_f) ~ mapply(calc_food_q_lin, q_i, q_f, year_n, year_i, year_f),
        implementation == "linear" & year_n > year_f ~ q_f,
        implementation == "cosine" & year_n < year_i ~ q_i,
        implementation == "cosine" & year_n %in% c(year_i:year_f) ~ mapply(calc_food_q_cos, q_i, q_f, year_n, year_i, year_f, p),
        implementation == "cosine" & year_n > year_f ~ q_f,
        implementation == "sigmoidal" & year_n < year_i ~ q_i,
        implementation == "sigmoidal" & year_n %in% c(year_i:year_f) ~ mapply(calc_food_q_sig, q_i, q_f, year_n, year_i, year_f, lambda),
        implementation == "sigmoidal" & year_n > year_f ~ q_f
    )) %>%
    select("food_group", "scenario", "year_n", "quantity") %>%
    rename("year" = "year_n")

# Ordering food groups
diets_evo$food_group <- factor(diets_evo$food_group, levels = order_food_groups)

# Graphical representation of the evolution of food consumption on the entire period of time
graph_diets_evo <- ggplot(
  data = diets_evo %>%
    filter(scenario != "actuel"),
  aes(
    x = year,
    y = quantity,
    fill = food_group
  )
) +
  geom_area(colour = "black", linewidth = 0.2, alpha = 0.6) +
  facet_wrap(~scenario,
    ncol = 2,
    labeller = labeller(scenario = labels_scenario)
  ) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    strip.text = element_text(face = "bold", size = rel(0.8)),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(face = "bold", size = 12),
    legend.key.size = unit(0.3, "cm"),
    plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm")
  ) +
  scale_fill_manual(
    values = col_food_groups,
    labels = labels_food_groups
  ) +
  labs(
    title = "",
    x = "",
    y = "Intakes (g/d/pers)",
    fill = "Food type"
  ) +
  guides(fill = guide_legend(
    nrow = 2,
    title.position = "top",
    title.hjust = 0.5
  ))

plot(graph_diets_evo)

# Graphical representation of the evolution of food consumption on the period of dietary change
diets_evo_shift <- diets_evo %>%
  filter(year %in% c(year_i:year_f))


graph_diets_evo_shift <- ggplot(data = diets_evo_shift %>%
  filter(scenario != "actuel"),
  aes(
  x = year,
  y = quantity,
  fill = food_group
)) +
  geom_area(colour = "black", linewidth = 0.2, alpha = 0.6) +
  facet_wrap(~scenario,
    ncol = 2,
    labeller = labeller(scenario = labels_scenario)
  ) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    strip.text = element_text(face = "bold", size = rel(0.8)),
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    legend.title = element_text(face = "bold", size = 10),
    legend.key.size = unit(0.3, "cm"),
    plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm")
  ) +
  scale_fill_manual(
    values = col_food_groups,
    labels = labels_food_groups
  ) +
  labs(
    title = "",
    x = "",
    y = "Intakes (g/d/pers)",
    fill = "Food type"
  ) +
  guides(fill = guide_legend(
    nrow = 2,
    title.position = "top",
    title.hjust = 0.5
  ))

plot(graph_diets_evo_shift)

################################################################################################################################
#                                             6. Intake variations compared to the baseline diet (%)                           #
################################################################################################################################

# Calculation of the variations in consumption of each food compared to the current diet (%)
diets_var <- diets_evo %>%
    group_by(food_group, year) %>%
    mutate(var = (quantity - quantity[scenario == "actuel"]) / quantity[scenario == "actuel"] * 100) %>% 
    ungroup()

# Graphical representation of the variations in food consumption on the entire period of time
graph_diets_var <- ggplot(
  diets_var %>%
    filter(scenario != "actuel"),
  aes(
    x = year,
    y = var,
    color = food_group
  )
) +
  facet_wrap(~scenario,
    labeller = labeller(scenario = labels_scenario)
  ) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  labs(
    title = "",
    x = "",
    y = "Variations of food intake (%)",
    color = "Food group"
  ) +
  scale_color_manual(
    values = col_food_groups,
    labels = labels_food_groups
  ) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    strip.text = element_text(face = "bold", size = rel(1)),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(
    nrow = 3,
    title.position = "top",
    title.hjust = 0.5
  ))

plot(graph_diets_var)

# Table of variations in food consumption

# Reorder food groups for the table
  diets_var_2 <- diets_var %>%
  mutate(food_group = factor(food_group,
                             levels = order_food_groups,
                             labels = labels_food_groups[order_food_groups]))

table_diets_var <- diets_var_2 %>%
  filter(
    year %in% c(2025, 2035, 2050)
  ) %>%
  mutate(quantity = round(quantity, 1), var = round(var, 1)) %>%
  pivot_wider(
    names_from = c("scenario", "year"),
    values_from = c("quantity", "var")
  ) %>%
  select(
    "food_group",
    "quantity_actuel_2025",
    "quantity_sc1_2050", "var_sc1_2035",, "var_sc1_2050",
    "quantity_sc2_2050", "var_sc2_2035", "var_sc2_2050",
    "quantity_sc3_2050", "var_sc3_2035", "var_sc3_2050",
    "quantity_sc4_2050", "var_sc4_2035", "var_sc4_2050"
  ) %>%
  qflextable() %>% # Create the table with qflextable
  add_header_row(
    top = TRUE, # Add a top header row
    values = c("Food group", "Baseline", "S1", "", "", "S2", "", "", "S3", "", "", "S4", "", "")
  ) %>%
  set_header_labels( # Rename the columns of the 2nd header row
    "food_group" = "",
    "quantity_actuel_2025" = "Intake(g/d/pers)",
    "quantity_sc1_2050" = "Intake in 2050 (g/d/pers)",
    "var_sc1_2035" = "Intake variation vs baseline in 2035 (%)",
    "var_sc1_2050" = "Intake variation vs baseline in 2050 (%)",
    "quantity_sc2_2050" = "Intake in 2050 (g/d/pers)",
    "var_sc2_2035" = "Intake variation vs baseline in 2035 (%)",
    "var_sc2_2050" = "Intake variation vs baseline in 2050 (%)",
    "quantity_sc3_2050" = "Intake in 2050 (g/d/pers)",
    "var_sc3_2035" = "Intake variation vs baseline in 2035 (%)",
    "var_sc3_2050" = "Intake variation vs baseline in 2050 (%)",
    "quantity_sc4_2050" = "Intake in 2050 (g/d/pers)",
    "var_sc4_2035" = "Intake variation vs baseline in 2035 (%)",
    "var_sc4_2050" = "Intake variation vs baseline in 2050 (%)"
  ) %>%
  vline(part = "all", j = 2) %>% # Vertical line after the 2nd column
  vline(part = "all", j = 5) %>%
  vline(part = "all", j = 8) %>% 
  vline(part = "all", j = 11) %>%
  merge_at(i = 1, j = 3:5, part = "header") %>% # Merge cells 3 to 5 in the 1st header row
  merge_at(i = 1, j = 6:8, part = "header") %>%
  merge_at(i = 1, j = 9:11, part = "header") %>%
  merge_at(i = 1, j = 12:14, part = "header") %>%
  align(align = "center", j = c(2:14), part = "all") %>% # Center the text in all columns except Food group
  bold(i = 1, part = "header") %>% # Bold the first row of the header
  bg(part = "all", bg = "white") # Set the background color of the table to white

plot(table_diets_var)

################################################################################################################################
#                                             7. Data exportation                                                              #
################################################################################################################################

## Diets implementation
  # Intakes (g/j/pers)
      export(diets_evo, here("results", "diets", "diets_evo.csv"))
      ggsave(here("results", "diets", "diets_evo.pdf"), graph_diets_evo)
      ggsave(here("results", "diets", "diets_evo_shift.pdf"), graph_diets_evo_shift)

  # Variations (%)
      export(diets_var, here("results", "diets", "diets_var.csv"))
      ggsave(here("results", "diets", "diets_var.pdf"), graph_diets_var)

      save_as_image(table_diets_var, here("results", "diets", "table_diets_var.png"))

