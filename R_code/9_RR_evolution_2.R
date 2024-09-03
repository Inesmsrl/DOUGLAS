################################################################################################################################
#                                             1. Chargement des packages                                                       #
################################################################################################################################

pacman::p_load(
  rio,                 # Importation de fichiers
  here,                # Localisation des fichiers dans le dossier du projet
  dplyr,               # Manipulation des données
  tidyr,               # Manipulation des données
  purrr               # Opérations itératives
)


################################################################################################################################
#                                             2. Importation des données et paramètres                                         #
################################################################################################################################

# Table des risques relatifs pour chaque catégorie alimentaire, attribués à des quantités absolues (Fadnes)
rr_table <- import(here("data", "rr_table_quanti.xlsx"))

# Implémentation linéaire des régimes
diets_lin <- import(here("data_clean", "diets_evo_lin_scenarios.xlsx"))

# Implémentation par interpolation cosinus des régimes
diets_cos <- import(here("data_clean", "diets_evo_cos_scenarios.xlsx"))

# Implémentation par sigmoïdale des régimes
diets_sig <- import(here("data_clean", "diets_evo_sig_scenarios.xlsx"))


################################################################################################################################
#                                             3. Calcul des rr pour chaque quantité consommée                                  #
################################################################################################################################

rr_table_long <- rr_table %>% 
  pivot_longer(cols = `0.0`:`800.0`, names_to = "quantity", values_to = "rr") %>% 
  mutate(quantity = as.numeric(quantity))

rr_table_interpolated <- rr_table_long %>%
  group_by(food_group) %>%
  complete(quantity = full_seq(0:800, 1)) %>%
  arrange(quantity) %>%
  mutate(rr_interpolated = if_else(is.na(rr), approx(quantity, rr, xout = quantity, method = "linear", rule = 1, ties = mean)$y, rr)) %>%
  mutate(rr_interpolated = if_else(quantity > max(quantity[!is.na(rr)]), NA_real_, rr_interpolated)) %>%
  ungroup() %>% 
  select("food_group", "quantity", "rr_interpolated")

rr_table_interpolated_wide <- rr_table_interpolated %>% 
  pivot_wider(names_from = quantity, values_from = rr_interpolated)

################################################################################################################################
#                                             4. Attribution des rr à chaque régime                                            #
################################################################################################################################

# Dynamique d'implémentation linéaire des régimes alimentaires
diets_lin <- diets_lin %>% 
  mutate(quantity = round(quantity))

rr_evo_lin <- diets_lin %>% 
  left_join(rr_table_interpolated, by = c("food_group", "quantity"))

# Dynamique d'implémentation par interpolation cosinus des régimes alimentaires
diets_cos <- diets_cos %>% 
  mutate(quantity = round(quantity))

rr_evo_cos <- diets_cos %>% 
  left_join(rr_table_interpolated, by = c("food_group", "quantity"))

# Dynamique d'implémentation sigmoïdale des régimes alimentaires
diets_sig <- diets_sig %>% 
  mutate(quantity = round(quantity))

rr_evo_sig <- diets_sig %>% 
  left_join(rr_table_interpolated, by = c("food_group", "quantity"))

################################################################################################################################
#                                             4. Combinaison des rr                                                            #
################################################################################################################################

# Pour chaque année et chaque scénario, multiplication des rr des aliments
# On obtient l'évolution rr du régime de chaque scénario par année

calc_combined_rr <- function(df) {
  df %>%
    group_by(scenario, year) %>%
    summarize(combined_rr = prod(rr_interpolated, na.rm = TRUE)) %>%
    ungroup()
}

# rr en fonction de l'implémentation linéaire des régimes 
combined_rr_table_lin <- calc_combined_rr(rr_evo_lin)

# rr en fonction de l'implémentation par interpolation cosinus des régimes
combined_rr_table_cos <- calc_combined_rr(rr_evo_cos)

# rr en fonction de l'implémentation sigmoïdale des régimes 
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

# Table complète des rr pour chaque quantité d'aliment
export(rr_table_interpolated_wide ,here("data_clean", "rr_table_interpolated.xlsx"))

# rr par scénario, par année et pour chaque aliment selon la dose consomée
export(rr_evo_lin, here("data_clean", "rr_evo_lin_2.xlsx"))
export(rr_evo_cos, here("data_clean", "rr_evo_cos_2.xlsx"))
export(rr_evo_sig, here("data_clean", "rr_evo_sig_2.xlsx"))

# RR combinés pour chaque scénario, par année
export(combined_rr_table_lin, here("data_clean", "combined_rr_lin_2.xlsx"))
export(combined_rr_table_cos, here("data_clean", "combined_rr_cos_2.xlsx"))
export(combined_rr_table_sig, here("data_clean", "combined_rr_sig_2.xlsx"))

# Evolution dans le temps des RR combinés dans chaque scénario
ggsave(here("results", "Diets_RR_evo_lin_2.pdf"), plot = graph_rr_evo_lin)
ggsave(here("results", "Diets_RR_evo_cos_2.pdf"), plot = graph_rr_evo_cos)
ggsave(here("results", "Diets_RR_evo_sig_2.pdf"), plot = graph_rr_evo_sig)

