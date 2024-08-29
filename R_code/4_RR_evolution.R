################################################################################################################################
#                                             1. Chargement des packages                                                       #
################################################################################################################################

pacman::p_load(
  rio,                 # Importation de fichiers
  here,                # Localisation des fichiers dans le dossier du projet
  dplyr,               # Manipulation des données
  tidyr                # Manipulation des données
)

################################################################################################################################
#                                             2. Importation des données                                                       #
################################################################################################################################

# Implémentation linéaire des régimes
diets_lin <- import(here("data_clean", "diets_evo_lin_scenarios.xlsx"))

# Implémentation par interpolation cosinus des régimes
diets_cos <- import(here("data_clean", "diets_evo_cos_scenarios.xlsx"))

# Implémentation par sigmoïdale des régimes
diets_sig <- import(here("data_clean", "diets_evo_sig_scenarios.xlsx"))

# Table des risques relatifs pour chaque catégorie alimentaire
rr_table <- import(here("data", "rr_table.xlsx"))

################################################################################################################################
#                                             3. Evolution des RR par année et par aliment                                     #
################################################################################################################################

# RR en fonction de l'implémentation linéaire des régimes 
  rr_evo_lin <- diets_lin %>% 
    inner_join(rr_table, by = "food_group") %>% 
    mutate(rr_evo = rr^(quantity/g_day)) %>%
    select(scenario, food_group, year, rr_evo) %>% 
    rename("rr" = "rr_evo")

# RR en foinction de l'implémentation par interpolation cosinus des régimes
  rr_evo_cos <- diets_cos %>% 
    inner_join(rr_table, by = "food_group") %>% 
    mutate(rr_evo = rr^(quantity/g_day)) %>%
    select(scenario, food_group, year, rr_evo) %>% 
    rename("rr" = "rr_evo")

# RR en fonction de l'implémentation sigmoïdale des régimes 
  rr_evo_sig <- diets_sig %>% 
    inner_join(rr_table, by = "food_group") %>% 
    mutate(rr_evo = rr^(quantity/g_day)) %>%
    select(scenario, food_group, year, rr_evo) %>% 
    rename("rr" = "rr_evo")


################################################################################################################################
#                                             4. Combinaison des RR                                                            #
################################################################################################################################

# Pour chaque année et chaque scénario, multiplication des RR des aliments
# On obtient l'évolution RR du régime de chaque scénario par année

calc_combined_rr <- function(df) {
  df %>%
    group_by(scenario, year) %>%
    summarize(combined_rr = prod(rr, na.rm = TRUE)) %>%
    ungroup()
}

# RR en fonction de l'implémentation linéaire des régimes 
  combined_rr_table_lin <- calc_combined_rr(rr_evo_lin)
  
# RR en fonction de l'implémentation par interpolation cosinus des régimes
  combined_rr_table_cos <- calc_combined_rr(rr_evo_cos)
  
# RR en fonction de l'implémentation sigmoïdale des régimes 
  combined_rr_table_sig <- calc_combined_rr(rr_evo_sig)
 
################################################################################################################################
#                                             5. Représentations graphiques                                                    #
################################################################################################################################
  
# Charte graphique
  col_scenario <- c("actuel" = "azure4",
                    "sc0" = "royalblue2",
                    "sc1" = "darkseagreen4",
                    "sc2" = "aquamarine2",
                    "sc3" = "lightpink",
                    "sc4" = "maroon",
                    "sc5" = "royalblue4")
  
# Implémentation linéaire des régimes
  graph_rr_evo_lin <- ggplot(combined_rr_table_lin, aes(x = year,
                                                        y = combined_rr,
                                                        colour = scenario))+
    geom_line(size = 1)+
    scale_color_manual(values = col_scenario)+
    labs(title = "Whole diet RR evolution in each scenario",
         subtitle = "linear implementation of diets from 2019 to 2050",
         x = "",
         y = "Diet RR") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# Implémentation par interpolation cosinus des régimes
  graph_rr_evo_cos <- ggplot(combined_rr_table_cos, aes(x = year,
                                                        y = combined_rr,
                                                        colour = scenario))+
    geom_line(size = 1)+
    scale_color_manual(values = col_scenario)+
    labs(title = "Whole diet RR evolution in each scenario",
         subtitle = "cosine interpolation implementation of diets from 2019 to 2050",
         x = "",
         y = "Diet RR") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
# Implémentation sigmoïdale des régimes
  graph_rr_evo_sig <- ggplot(combined_rr_table_sig, aes(x = year,
                                                        y = combined_rr,
                                                        colour = scenario))+
    geom_line(size = 1)+
    scale_color_manual(values = col_scenario)+
    labs(title = "Whole diet RR evolution in each scenario",
         subtitle = "sigmoidal implementation of diets from 2019 to 2050",
         x = "",
         y = "Diet RR") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
################################################################################################################################
#                                             6. Exportation des données                                                       #
################################################################################################################################

# RR par scénario, par année et pour chaque aliment selon la dose consomée
export(rr_evo_lin, here("data_clean", "RR_evo_lin.xlsx"))
export(rr_evo_cos, here("data_clean", "RR_evo_cos.xlsx"))
export(rr_evo_sig, here("data_clean", "RR_evo_sig.xlsx"))

# RR combinés pour chaque scénario, par année
export(combined_rr_table_lin, here("data_clean", "combined_rr_lin.xlsx"))
export(combined_rr_table_cos, here("data_clean", "combined_rr_cos.xlsx"))
export(combined_rr_table_sig, here("data_clean", "combined_rr_sig.xlsx"))

# Evolution dans le temps des RR combinés dans chaque scénario
ggsave(here("results", "Diets_RR_evo_lin.pdf"), plot = graph_rr_evo_lin)
ggsave(here("results", "Diets_RR_evo_cos.pdf"), plot = graph_rr_evo_cos)
ggsave(here("results", "Diets_RR_evo_sig.pdf"), plot = graph_rr_evo_sig)
