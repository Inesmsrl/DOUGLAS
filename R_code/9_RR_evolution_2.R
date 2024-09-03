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
rr_table <- import(here("data", "RR_table_quanti.xlsx"))

################################################################################################################################
#                                             3. Calcul des RR pour chaque quantité consommée                                  #
################################################################################################################################

rr_table_long <- rr_table %>% 
  pivot_longer(cols = `0.0`:`800.0`, names_to = "quantity", values_to = "RR") %>% 
  mutate(quantity = as.numeric(quantity))

rr_table_interpolated <- rr_table_long %>%
  group_by(food_group) %>%
  complete(quantity = full_seq(0:800, 1)) %>%
  arrange(quantity) %>%
  mutate(RR_interpolated = if_else(is.na(RR), approx(quantity, RR, xout = quantity, method = "linear", rule = 1, ties = mean)$y, RR)) %>%
  mutate(RR_interpolated = if_else(quantity > max(quantity[!is.na(RR)]), NA_real_, RR_interpolated)) %>%
  ungroup() %>% 
  select("food_group", "quantity", "RR_interpolated") %>% 
  pivot_wider(names_from = quantity, values_from = RR_interpolated)
  
################################################################################################################################
#                                             4. Exportation des données                                                       #
################################################################################################################################

export(rr_table_interpolated ,here("data_clean", "rr_table_interpolated.xlsx"))
