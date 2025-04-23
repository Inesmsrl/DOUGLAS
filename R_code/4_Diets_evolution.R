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
    crossing(year_n = (year_i - 2 * ttfe_time):(year_f + 2 * ttfe_time)) %>%
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

# Graphical representation of the evolution of food consumption on the period of dietary change
diets_evo_shift <- diets_evo %>%
  filter(year %in% c(year_i:year_f))


graph_diets_evo_shift <- ggplot(data = diets_evo_shift, aes(
  x = year,
  y = quantity,
  fill = food_group
)) +
  geom_area(colour = "black", linewidth = 0.2, alpha = 0.6) +
  facet_wrap(~scenario,
    ncol = 3,
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

################################################################################################################################
#                                             5. Intake variations compared to the baseline diet (%)                           #
################################################################################################################################

# Calculation of the variations in consumption of each food compared to the current diet (%)
diets_var <- diets_evo %>%
    group_by(food_group, year) %>%
    mutate(var = (quantity - quantity[scenario == "actuel"]) / quantity[scenario == "actuel"] * 100)

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

# Table of variations in food consumption
diets_var$food_group <- factor(diets_var$food_group, levels = order_food_groups)

table_diets_var <- diets_var %>%
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
  mutate(food_group = labels_food_groups[food_group]) %>% # Rename food groups
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

################################################################################################################################
#                                             6. Observed to optimized diets                                                   #
################################################################################################################################

function_diets_evo <- function(observed, optimized) {
  
  diets_evo <- diets %>%
    select("food_group", all_of(c(observed, optimized))) %>%
    filter(food_group %in% c(
      "red_meat", "processed_meat", "white_meat", "fish", "eggs", "dairy",
      "fruits", "vegetables", "legumes", "nuts", "whole_grains", "reffined_grains",
      "sugar_sweetened_beverages"
    )) %>%
    mutate(q_i = .[[observed]]) %>%
    pivot_longer(
      cols = all_of(c(observed, optimized)),
      names_to = "scenario",
      values_to = "q_f"
    ) %>%
    crossing(year_n = (year_i - 2 * ttfe_time):(year_f + 2 * ttfe_time)) %>%
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

  return(diets_evo)
}

diets_evo_meat3 <- function_diets_evo("meat3", "meat3_optim")
diets_evo_meat2 <- function_diets_evo("meat2", "meat2_optim")
diets_evo_meat1 <- function_diets_evo("meat1", "meat1_optim")
diets_evo_pesce <- function_diets_evo("pesce", "pesce_optim")
diets_evo_vege <- function_diets_evo("vege", "vege_optim")
diets_evo_vegan <- function_diets_evo("vegan", "vegan_optim")

plot_diets_evo <- function(observed, optimized) {
  
  diets_evo <- function_diets_evo(observed, optimized)

  diets_evo$food_group <- factor(diets_evo$food_group, levels = order_food_groups)

  graph_diets_evo <- ggplot(
    data = diets_evo %>%
      filter(scenario != observed,
             year %in% c(year_i:year_f)),
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

  return(graph_diets_evo)
}

graph_diets_evo_meat3 <- plot_diets_evo("meat3", "meat3_optim")
graph_diets_evo_meat2 <- plot_diets_evo("meat2", "meat2_optim")
graph_diets_evo_meat1 <- plot_diets_evo("meat1", "meat1_optim")
graph_diets_evo_pesce <- plot_diets_evo("pesce", "pesce_optim")
graph_diets_evo_vege <- plot_diets_evo("vege", "vege_optim")
graph_diets_evo_vegan <- plot_diets_evo("vegan", "vegan_optim")

function_diets_var <- function(observed, optimized) {
  
  diets_var <- function_diets_evo(observed, optimized) %>%
    group_by(food_group, year) %>%
    mutate(var = (quantity - quantity[scenario == observed]) / quantity[scenario == observed] * 100)

  return(diets_var)
}

diets_var_meat3 <- function_diets_var("meat3", "meat3_optim")
diets_var_meat2 <- function_diets_var("meat2", "meat2_optim")
diets_var_meat1 <- function_diets_var("meat1", "meat1_optim")
diets_var_pesce <- function_diets_var("pesce", "pesce_optim")
diets_var_vege <- function_diets_var("vege", "vege_optim")
diets_var_vegan <- function_diets_var("vegan", "vegan_optim")

table_diets_var <- function(observed, optimized) {
  
  diets_var <- function_diets_var(observed, optimized)
  opt <- optimized[1]  # Extract one name of diet
  
  # Construction des noms de colonnes dynamiques
  col_obs_q <- paste0("quantity_", observed, "_2025")
  col_opt_q <- paste0("quantity_", opt, "_2050")
  col_opt_v <- paste0("var_", opt, "_2050")
  
  # Building the table
  table <- diets_var %>%
    filter(
      year %in% c(2025, 2050),
      scenario %in% c(observed, opt)
    ) %>%
    mutate(quantity = round(quantity, 1), var = round(var, 1)) %>%
    pivot_wider(
      names_from = c("scenario", "year"),
      values_from = c("quantity", "var")
    ) %>%
    select(
      "food_group",
      all_of(c(col_obs_q, col_opt_q, col_opt_v))
    ) %>%
    mutate(food_group = labels_food_groups[food_group])
  
  
  # Creating the table with qflextable
  table_ft <- table %>%
    qflextable() %>%
    add_header_row(
      top = TRUE,
      values = c("Food group", "Observed", "Optimized", ""),  # 4 values for header row
      colwidths = c(1, 1, 1, 1) 
    ) %>%
    vline(part = "all", j = 2) %>%
    merge_at(i = 1, j = 3:4, part = "header") %>%
    align(align = "center", j = 2:4, part = "all") %>%
    bold(i = 1, part = "header") %>%
    bg(part = "all", bg = "white")
  
  return(table_ft)
}

table_var_meat3 <- table_diets_var("meat3", "meat3_optim")
table_var_meat2 <- table_diets_var("meat2", "meat2_optim")
table_var_meat1 <- table_diets_var("meat1", "meat1_optim")
table_var_pesce <- table_diets_var("pesce", "pesce_optim")
table_var_vege <- table_diets_var("vege", "vege_optim")
table_var_vegan <- table_diets_var("vegan", "vegan_optim")

################################################################################################################################
#                                             7. Data exportation                                                              #
################################################################################################################################

## Diets implementation
  # Intakes (g/j/pers)
      export(diets_evo, here("results", "TEST", "diets", "diets_evo.csv"))
      ggsave(here("results", "TEST", "diets", "diets_evo.pdf"), graph_diets_evo)
      ggsave(here("results", "TEST", "diets", "diets_evo_shift.pdf"), graph_diets_evo_shift)

  # Variations (%)
      export(diets_var, here("results", "TEST", "diets", "diets_var.csv"))
      ggsave(here("results", "TEST", "diets", "diets_var.pdf"), graph_diets_var)

      save_as_image(table_diets_var, here("results", "TEST", "diets", "table_diets_var.png"))

## Observed to optimized diets
  # Intakes (g/j/pers)
    export(diets_evo_meat3, here("results", "Observed_to_Optimized", "meat3", "diets", "diets_evo_meat3.csv"))
    ggsave(here("results", "Observed_to_Optimized", "meat3", "diets", "diets_evo_meat3.pdf"), graph_diets_evo_meat3)

    export(diets_evo_meat2, here("results", "Observed_to_Optimized", "meat2", "diets", "diets_evo_meat2.csv"))
    ggsave(here("results", "Observed_to_Optimized", "meat2", "diets", "diets_evo_meat2.pdf"), graph_diets_evo_meat2)

    export(diets_evo_meat1, here("results", "Observed_to_Optimized", "meat1", "diets", "diets_evo_meat1.csv"))
    ggsave(here("results", "Observed_to_Optimized", "meat1", "diets", "diets_evo_meat1.pdf"), graph_diets_evo_meat1)

    export(diets_evo_pesce, here("results", "Observed_to_Optimized", "pesce", "diets", "diets_evo_pesce.csv"))
    ggsave(here("results", "Observed_to_Optimized", "pesce", "diets", "diets_evo_pesce.pdf"), graph_diets_evo_pesce)

    export(diets_evo_vege, here("results", "Observed_to_Optimized", "vege", "diets", "diets_evo_vege.csv"))
    ggsave(here("results", "Observed_to_Optimized", "vege", "diets", "diets_evo_vege.pdf"), graph_diets_evo_vege)

    export(diets_evo_vegan, here("results", "Observed_to_Optimized", "vegan", "diets", "diets_evo_vegan.csv"))
    ggsave(here("results", "Observed_to_Optimized", "vegan", "diets", "diets_evo_vegan.pdf"), graph_diets_evo_vegan)

  # Variations (%)
    export(diets_var_meat3, here("results", "Observed_to_Optimized", "meat3", "diets", "diets_var_meat3.csv"))
    save_as_image(table_var_meat3, here("results", "Observed_to_Optimized", "meat3", "diets", "table_var_meat3.png"))
    
    export(diets_var_meat2, here("results", "Observed_to_Optimized", "meat2", "diets", "diets_var_meat2.csv"))
    save_as_image(table_var_meat2, here("results", "Observed_to_Optimized", "meat2", "diets", "table_var_meat2.png"))

    export(diets_var_meat1, here("results", "Observed_to_Optimized", "meat1", "diets", "diets_var_meat1.csv"))
    save_as_image(table_var_meat1, here("results", "Observed_to_Optimized", "meat1", "diets", "table_var_meat1.png"))

    export(diets_var_pesce, here("results", "Observed_to_Optimized", "pesce", "diets", "diets_var_pesce.csv"))
    save_as_image(table_var_pesce, here("results", "Observed_to_Optimized", "pesce", "diets", "table_var_pesce.png"))

    export(diets_var_vege, here("results", "Observed_to_Optimized", "vege", "diets", "diets_var_vege.csv"))
    save_as_image(table_var_vege, here("results", "Observed_to_Optimized", "vege", "diets", "table_var_vege.png"))

    export(diets_var_vegan, here("results", "Observed_to_Optimized", "vegan", "diets", "diets_var_vegan.csv"))
    save_as_image(table_var_vegan, here("results", "Observed_to_Optimized", "vegan", "diets", "table_var_vegan.png"))
