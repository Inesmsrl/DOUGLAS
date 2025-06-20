################################################################################################################################
#                                             1. Loading packages                                                              #
################################################################################################################################

pacman::p_load(
  rio,                 # file import/export
  here,                # file path management
  dplyr,               # data manipulation
  tidyr,               # data manipulation
  tidyverse,           # Data management, ggplot included
  patchwork            # graphs combination
)

################################################################################################################################
#                                             2. Data importation                                                              #
################################################################################################################################

# RR table for each food group, assigned to absolute quantities
  
  # Meta-analyses used by Fadnes et al
    # Central values
    rr_table_mid <- import(here("Fadnes_data", "data_clean", "rr_table_mid.xlsx"))

    # IC95 lower
    rr_table_low <- import(here("Fadnes_data", "data_clean", "rr_table_low.xlsx"))

    # IC95 upper
    rr_table_up <- import(here("Fadnes_data", "data_clean", "rr_table_up.xlsx"))

# Update of some food group risk values with new meta-analyses
# White meat, nuts, whole grains, refined grains, SSBs and eggs
    # Central values
    rr_table_mid_new <- import(here("data", "rr_table_quanti_2.xlsx"), sheet = "Mid")
 
    # IC95 lower
    rr_table_low_new <- import(here("data", "rr_table_quanti_2.xlsx"), sheet = "Lower")

    # IC95 upper
    rr_table_up_new <- import(here("data", "rr_table_quanti_2.xlsx"), sheet = "Upper") 


################################################################################################################################
#                                             3. Parameters                                                                    #
################################################################################################################################

# General parameters
  source(here("R_code", "0_parameters.R"))

# Interpolation method ("linear", "spline")
  interpolation <- "spline"
  
# Number of simulations
  n <- 1000
  
################################################################################################################################
#                                             4. Data preparation                                                              #
################################################################################################################################

# Pivot data frames to long format
  rr_table_low <- rr_table_low %>% 
    pivot_longer(cols = "0":"800",
                 names_to = "quantity",
                 values_to = "rr") %>% 
    mutate(quantity = as.numeric(quantity),
           rr = as.numeric(rr))
  
  rr_table_mid <- rr_table_mid %>% 
    pivot_longer(cols = "0":"800",
                 names_to = "quantity",
                 values_to = "rr") %>% 
    mutate(quantity = as.numeric(quantity),
           rr =as.numeric(rr))
  
  rr_table_up <- rr_table_up %>% 
    pivot_longer(cols = "0":"800",
                 names_to = "quantity",
                 values_to = "rr") %>% 
    mutate(quantity = as.numeric(quantity),
           rr =as.numeric(rr))

# One unique table 
  rr_table <- rr_table_mid %>% 
    left_join(rr_table_low, by = c("food_group", "quantity")) %>%
    left_join(rr_table_up, by = c("food_group", "quantity")) %>%
    rename("rr_mid" = "rr.x",
           "rr_low" = "rr.y",
           "rr_up"="rr") %>%
    mutate(rr_low = ifelse(is.na(rr_low), rr_mid, rr_low),
           rr_up = ifelse(is.na(rr_up), rr_mid, rr_up)) %>%
    drop_na(rr_low, rr_mid, rr_up)

# Pivot data frames to long format
  rr_table_low_new <- rr_table_low_new %>% 
    pivot_longer(cols = "0":"1000",
                 names_to = "quantity",
                 values_to = "rr") %>% 
    mutate(quantity = as.numeric(quantity),
           rr =as.numeric(rr))
  
  rr_table_mid_new <- rr_table_mid_new %>% 
    pivot_longer(cols = "0":"1000",
                 names_to = "quantity",
                 values_to = "rr") %>% 
    mutate(quantity = as.numeric(quantity),
           rr =as.numeric(rr))
  
  rr_table_up_new <- rr_table_up_new %>% 
    pivot_longer(cols = "0":"1000",
                 names_to = "quantity",
                 values_to = "rr") %>% 
    mutate(quantity = as.numeric(quantity),
           rr =as.numeric(rr))

# One unique table 
  rr_table_new <- rr_table_mid_new %>% 
    left_join(rr_table_low_new, by = c("food_group", "quantity")) %>%
    left_join(rr_table_up_new, by = c("food_group", "quantity")) %>%
    rename("rr_mid" = "rr.x",
           "rr_low" = "rr.y",
           "rr_up"="rr") %>%
    mutate(rr_low = ifelse(is.na(rr_low), rr_mid, rr_low),
           rr_up = ifelse(is.na(rr_up), rr_mid, rr_up)) %>%
    drop_na(rr_low, rr_mid, rr_up)

################################################################################################################################
#                                             5. Generation of RR normal distributions                                         #
################################################################################################################################

# set a seed for reproducibility
  set.seed(123)
  
# Function to generate RR normal distributions
  generate_RR_distrib = function(food_group, RR, low, sup, N = n){
    # RR, low and sup are the RR and 95%CI of a specific RR
    # N is the number of random values from the distrib
    
    lRR = log(RR)
    l_low = log(low)
    l_sup = log(sup)
    sd1 = (lRR-l_low)/qnorm(1-0.05/2)
    sd2 = (l_sup-lRR)/qnorm(1-0.05/2)
    sd = mean(c(sd1, sd2))
    distr_RR = exp(rnorm(N, lRR, sd))
    
    # just need to truncat values
    distr_RR[distr_RR<0]=0
    
    return(distr_RR)
  }
  
  rr_table <- rr_table %>% 
    rowwise() %>% 
    mutate(rr_distrib = list(generate_RR_distrib(food_group, rr_mid, rr_low, rr_up)))

rr_table_new <- rr_table_new %>% 
    rowwise() %>% 
    mutate(rr_distrib = list(generate_RR_distrib(food_group, rr_mid, rr_low, rr_up)))
  
################################################################################################################################
#                                             6. Sort the RR distributions in ascending order                                  
################################################################################################################################
  
  rr_table <- rr_table %>% 
    rowwise() %>% 
    mutate(rr_distrib = list(sort(unlist(rr_distrib)))) %>%
    ungroup()

  rr_table_new <- rr_table_new %>% 
    rowwise() %>% 
    mutate(rr_distrib = list(sort(unlist(rr_distrib)))) %>%
    ungroup()

################################################################################################################################
#                                             7. Interpolation                                                                 #
################################################################################################################################
  
# Transform the rr_table to long format
  rr_table_long <- rr_table %>% 
    unnest_wider(rr_distrib, names_sep = "_") %>%  # separate the rr_distrib column into multiple columns
    pivot_longer(
      cols = starts_with("rr_distrib_"), 
      names_to = "simulation_id",  # column name for the simulation ID
      values_to = "simulated_rr"  # column name for the simulated RR values
    )

# Interpolation 
  rr_table_interpolated <- rr_table_long %>%
    group_by(food_group, simulation_id) %>%
    complete(quantity = full_seq(0:800, 1)) %>%
    arrange(quantity) %>%
    mutate(rr_interpolated = case_when(
      interpolation == "linear" ~ if_else(is.na(simulated_rr), approx(quantity, simulated_rr, xout = quantity, method = "linear", rule = 1)$y, simulated_rr),
      interpolation == "spline" ~ if_else(is.na(simulated_rr), spline(quantity, simulated_rr, xout = quantity)$y, simulated_rr)
    )) %>%
    # $y, rr, take the interpolated values in y from the approx function and assign them to rr
    mutate(rr_interpolated = if_else(quantity > max(quantity[!is.na(simulated_rr)]), NA_real_, rr_interpolated)) %>%
    ungroup() %>% 
    select("simulation_id", "food_group", "quantity", "rr_interpolated")
  
  rr_table_interpolated <- rr_table_interpolated %>% 
    group_by(food_group) %>% 
    mutate(simulation_id = sample(unique(simulation_id))[match(simulation_id, unique(simulation_id))]) %>% 
    ungroup()

# Transform the rr_table to long format
  rr_table_long_new <- rr_table_new %>% 
    unnest_wider(rr_distrib, names_sep = "_") %>%  # separate the rr_distrib column into multiple columns
    pivot_longer(
      cols = starts_with("rr_distrib_"), 
      names_to = "simulation_id",  # column name for the simulation ID
      values_to = "simulated_rr"  # column name for the simulated RR values
    )

# Interpolation 
  rr_table_interpolated_new <- rr_table_long_new %>%
    group_by(food_group, simulation_id) %>%
    complete(quantity = full_seq(0:1000, 1)) %>%
    arrange(quantity) %>%
    mutate(rr_interpolated = case_when(
      interpolation == "linear" ~ if_else(is.na(simulated_rr), approx(quantity, simulated_rr, xout = quantity, method = "linear", rule = 1)$y, simulated_rr),
      interpolation == "spline" ~ if_else(is.na(simulated_rr), spline(quantity, simulated_rr, xout = quantity)$y, simulated_rr)
    )) %>%
    # $y, rr, take the interpolated values in y from the approx function and assign them to rr
    mutate(rr_interpolated = if_else(quantity > max(quantity[!is.na(simulated_rr)]), NA_real_, rr_interpolated)) %>%
    ungroup() %>% 
    select("simulation_id", "food_group", "quantity", "rr_interpolated")
  
  rr_table_interpolated_new <- rr_table_interpolated_new %>% 
    group_by(food_group) %>% 
    mutate(simulation_id = sample(unique(simulation_id))[match(simulation_id, unique(simulation_id))]) %>% 
    ungroup()

################################################################################################################################
#                                             8. Graphical representation of the RR distributions                              #
################################################################################################################################

# Dairy
  graph_dr_sim_dairy <- ggplot(rr_table_interpolated %>% 
                                 filter(food_group == "dairy"),
                               aes(x = quantity,
                                   y = rr_interpolated,
                                   group = simulation_id,
                                   color = food_group))+
    scale_color_manual(values = col_food_groups)+
    geom_line(na.rm = TRUE,
              alpha = 0.05)+
    labs(title = "Dairy",
         x = "Intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")


# Eggs
  graph_dr_sim_eggs <- ggplot(rr_table_interpolated %>% 
                                 filter(food_group == "eggs",
                                        quantity %in% 0:70),
                               aes(x = quantity,
                                   y = rr_interpolated,
                                   group = simulation_id,
                                   color = food_group))+
    scale_color_manual(values = col_food_groups)+
    geom_line(na.rm = TRUE,
              alpha = 0.05)+
    labs(title = "Eggs",
         x = "Intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")

# Fish
  graph_dr_sim_fish <- ggplot(rr_table_interpolated %>% 
                                filter(food_group == "fish",
                                       quantity %in% 0:250),
                              aes(x = quantity,
                                  y = rr_interpolated,
                                  group = simulation_id,
                                  color = food_group))+
    scale_color_manual(values = col_food_groups)+
    geom_line(na.rm = TRUE,
              alpha = 0.05)+
    labs(title = "Fish",
         x = "Intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
# Fruits
  graph_dr_sim_fruits <- ggplot(rr_table_interpolated %>% 
                                filter(food_group == "fruits",
                                       quantity %in% 0:600),
                              aes(x = quantity,
                                  y = rr_interpolated,
                                  group = simulation_id,
                                  color = food_group))+
    scale_color_manual(values = col_food_groups)+
    geom_line(na.rm = TRUE,
              alpha = 0.05)+
    labs(title = "Fruits",
         x = "Intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
# Legumes
  graph_dr_sim_legumes <- ggplot(rr_table_interpolated %>% 
                                  filter(food_group == "legumes",
                                         quantity %in% 0:150),
                                aes(x = quantity,
                                    y = rr_interpolated,
                                    group = simulation_id,
                                    color = food_group))+
    scale_color_manual(values = col_food_groups)+
    geom_line(na.rm = TRUE,
              alpha = 0.05)+
    labs(title = "Legumes",
         x = "Intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
# Nuts
  graph_dr_sim_nuts <- ggplot(rr_table_interpolated %>% 
                                   filter(food_group == "nuts",
                                          quantity %in% 0:30),
                                 aes(x = quantity,
                                     y = rr_interpolated,
                                     group = simulation_id,
                                     color = food_group))+
    scale_color_manual(values = col_food_groups)+
    geom_line(na.rm = TRUE,
              alpha = 0.05)+
    labs(title = "Nuts",
         x = "Intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")

  
# Processed meat
  graph_dr_sim_processed_meat <- ggplot(rr_table_interpolated %>% 
                                   filter(food_group == "processed_meat",
                                          quantity %in% 0:200),
                                 aes(x = quantity,
                                     y = rr_interpolated,
                                     group = simulation_id,
                                     color = food_group))+
    scale_color_manual(values = col_food_groups)+
    geom_line(na.rm = TRUE,
              alpha = 0.05)+
    labs(title = "Processed meat",
         x = "Intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
# Red meat
  graph_dr_sim_red_meat <- ggplot(rr_table_interpolated %>% 
                                          filter(food_group == "red_meat",
                                                 quantity %in% 0:200),
                                        aes(x = quantity,
                                            y = rr_interpolated,
                                            group = simulation_id,
                                            color = food_group))+
    scale_color_manual(values = col_food_groups)+
    geom_line(na.rm = TRUE,
              alpha = 0.05)+
    labs(title = "Red meat",
         x = "Intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
  
# Refined grains
  graph_dr_sim_refined_grains <- ggplot(rr_table_interpolated %>% 
                                          filter(food_group == "reffined_grains",
                                                 quantity %in% 0:150),
                                        aes(x = quantity,
                                            y = rr_interpolated,
                                            group = simulation_id,
                                            color = food_group))+
    scale_color_manual(values = col_food_groups)+
    geom_line(na.rm = TRUE,
              alpha = 0.05)+
    labs(title = "Refined grains",
         x = "Intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
# SSB
  graph_dr_sim_ssb <- ggplot(rr_table_interpolated %>% 
                                          filter(food_group == "sugar_sweetened_beverages",
                                                 quantity %in% 0:300),
                                        aes(x = quantity,
                                            y = rr_interpolated,
                                            group = simulation_id,
                                            color = food_group))+
    scale_color_manual(values = col_food_groups)+
    geom_line(na.rm = TRUE,
              alpha = 0.05)+
    labs(title = "SSB",
         x = "Intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
# Vegetables
  graph_dr_sim_vegetables <- ggplot(rr_table_interpolated %>% 
                                          filter(food_group == "vegetables",
                                                 quantity %in% 0:600),
                                        aes(x = quantity,
                                            y = rr_interpolated,
                                            group = simulation_id,
                                            color = food_group))+
    scale_color_manual(values = col_food_groups)+
    geom_line(na.rm = TRUE,
              alpha = 0.05)+
    labs(title = "Vegetables",
         x = "Intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
# White meat
  graph_dr_sim_white_meat <- ggplot(rr_table_interpolated %>% 
                                          filter(food_group == "white_meat",
                                                 quantity %in% 0:100),
                                        aes(x = quantity,
                                            y = rr_interpolated,
                                            group = simulation_id,
                                            color = food_group))+
    scale_color_manual(values = col_food_groups)+
    geom_line(na.rm = TRUE,
              alpha = 0.05)+
    labs(title = "White meat",
         x = "Intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
# Whole grains
  graph_dr_sim_whole_grains <- ggplot(rr_table_interpolated %>% 
                                          filter(food_group == "whole_grains",
                                                 quantity %in% 0:220),
                                        aes(x = quantity,
                                            y = rr_interpolated,
                                            group = simulation_id,
                                            color = food_group))+
    scale_color_manual(values = col_food_groups)+
    geom_line(na.rm = TRUE,
              alpha = 0.05)+
    labs(title = "Whole grains",
         x = "Intake (g/d/pers)",
         y = "RR")+
    theme(legend.position = "none")
  
  
# All DRF in one plot
 list_dr <- list(graph_dr_sim_dairy, graph_dr_sim_eggs, graph_dr_sim_fish, graph_dr_sim_fruits, graph_dr_sim_legumes,
                 graph_dr_sim_nuts, graph_dr_sim_processed_meat, graph_dr_sim_red_meat, graph_dr_sim_refined_grains,
                 graph_dr_sim_ssb, graph_dr_sim_vegetables, graph_dr_sim_white_meat, graph_dr_sim_whole_grains)
 
 common_theme <- theme(
   plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
   axis.title = element_text(size = 8),
   axis.text = element_text(size = 7)
 )
 
 list_dr <- lapply(list_dr, function(p) p + common_theme)
 
 combined_plot <- reduce(list_dr, `+`) + plot_layout(ncol = 4)
 
 print(combined_plot) 
  
################################################################################################################################
#                                             8. Data exportation                                                              #
################################################################################################################################
  
# Table of DRF simulated
  export(rr_table_interpolated, here("Fadnes_data", "data_clean", "rr_table_interpolated_sim.csv"))
  
# Graphs
  ggsave(here("Fadnes_data", "results", "DRF", "drf_dairy.pdf"), plot = graph_dr_sim_dairy)
  ggsave(here("Fadnes_data", "results", "DRF", "drf_eggs.pdf"), plot = graph_dr_sim_eggs)
  ggsave(here("Fadnes_data", "results", "DRF", "drf_fish.pdf"), plot = graph_dr_sim_fish)
  ggsave(here("Fadnes_data", "results", "DRF", "drf_fruits.pdf"), plot = graph_dr_sim_fruits)
  ggsave(here("Fadnes_data", "results", "DRF", "drf_legumes.pdf"), plot = graph_dr_sim_legumes)
  ggsave(here("Fadnes_data", "results", "DRF", "drf_nuts.pdf"), plot = graph_dr_sim_nuts)
  ggsave(here("Fadnes_data", "results", "DRF", "drf_processed_meat.pdf"), plot = graph_dr_sim_processed_meat)
  ggsave(here("Fadnes_data", "results", "DRF", "drf_red_meat.pdf"), plot = graph_dr_sim_red_meat)  
  ggsave(here("Fadnes_data", "results", "DRF", "drf_refined_grains.pdf"), plot = graph_dr_sim_refined_grains)  
  ggsave(here("Fadnes_data", "results", "DRF", "drf_ssb.pdf"), plot = graph_dr_sim_ssb)  
  ggsave(here("Fadnes_data", "results", "DRF", "drf_vegetables.pdf"), plot = graph_dr_sim_vegetables)  
  ggsave(here("Fadnes_data", "results", "DRF", "drf_white_meat.pdf"), plot = graph_dr_sim_white_meat)  
  ggsave(here("Fadnes_data", "results", "DRF", "drf_whole_grains.pdf"), plot = graph_dr_sim_whole_grains)  
  
  ggsave(here("Fadnes_data", "results", "DRF", "drf_all.pdf"), plot = combined_plot)
