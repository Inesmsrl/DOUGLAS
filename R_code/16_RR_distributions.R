################################################################################################################################
#                                             1. Chargement des packages                                                       #
################################################################################################################################

pacman::p_load(
  rio,                 # Importation de fichiers
  here,                # Localisation des fichiers dans le dossier du projet
  dplyr,               # Manipulation des données
  tidyr,               # Manipulation des données
  tidyverse,           # Data management, inclus ggplot
  purrr                # Opérations itératives
)


################################################################################################################################
#                                             2. Importation des données et paramètres                                         #
################################################################################################################################

# Table des risques relatifs pour chaque catégorie alimentaire, attribués à des quantités absolues (Fadnes)
rr_table_mid <- import(here("data", "rr_table_quanti.xlsx"), sheet = "Mid")

# Table des RR, IC95 lower
rr_table_low <- import(here("data", "rr_table_quanti.xlsx"), sheet = "Lower")

# Table des RR, IC95 upper
rr_table_up <- import(here("data", "rr_table_quanti.xlsx"), sheet = "Upper")

################################################################################################################################
#                                             3. Préparation des données                                                       #
################################################################################################################################

# Pivoter les dataframes des RR en format long
  rr_table_low <- rr_table_low %>% 
    pivot_longer(cols = "0":"800",
                 names_to = "quantity",
                 values_to = "rr") %>% 
    mutate(quantity = as.numeric(quantity),
           rr =as.numeric(rr))
  
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

# Un tableau unique
  rr_table <- rr_table_mid %>% 
    left_join(rr_table_low, by = c("food_group", "quantity")) %>% 
    left_join(rr_table_up, by = c("food_group", "quantity")) %>% 
    rename("rr_low" = "rr.x",
           "rr_mid" = "rr.y",
           "rr_up"="rr") %>% 
    mutate(rr_low = ifelse(is.na(rr_low), rr_mid, rr_low),
           rr_up = ifelse(is.na(rr_up), rr_mid, rr_up)) %>% 
    drop_na(rr_low, rr_mid, rr_up)
  
################################################################################################################################
#                                             4. Fonction de génération de distributions normales                              #
################################################################################################################################
  
# Fixer une graine pour garantir la reproductibilité des simulations
  set.seed(123)
  
  generate_RR_distrib = function(food_group, RR, low, sup, N = 1000){
    #RR, low and sup are the RR and 95%CI of a specific risk ration
    #N is the number of random values from the distrib
    
    lRR = log(RR)
    l_low = log(low)
    l_sup = log(sup)
    sd1 = (lRR-l_low)/qnorm(1-0.05/2)
    sd2 = (l_sup-lRR)/qnorm(1-0.05/2)
    sd = mean(c(sd1, sd2))
    distr_RR = exp(rnorm(N, lRR, sd))
    
    # just need to truncat values
    distr_RR[distr_RR<0]=0
    
    #if (food_group %in% c("red_meat", "processed_meat", "sugar_sweetened_beverages"))
    #distr_RR[distr_RR<1]=1
    #if (food_group %in% c("fruits", "vegetables", "legumes", "whole_grains", "nuts"))
    #distr_RR[distr_RR>1]=1
    
    return(distr_RR)
  }

################################################################################################################################
#                                             5. Génération des distributions normales pour chaque RR                          #
################################################################################################################################
  
  rr_table <- rr_table %>% 
    rowwise() %>% 
    mutate(rr_distrib = list(generate_RR_distrib(food_group, rr_mid, rr_low, rr_up)))
  
################################################################################################################################
#                                             6. Tri des listes de RR par odre croissant                                       #
################################################################################################################################
  
  rr_table <- rr_table %>% 
    rowwise() %>% 
    mutate(rr_distrib = list(sort(unlist(rr_distrib)))) %>%
    ungroup()

  
################################################################################################################################
#                                             7. Transformation au format long                                                 #
################################################################################################################################
  
# Transformer les simulations en format long
  rr_table_long <- rr_table %>% 
    unnest_wider(rr_distrib, names_sep = "_") %>%  # Séparer les simulations en colonnes distinctes
    pivot_longer(
      cols = starts_with("rr_distrib_"),  # Sélectionner toutes les colonnes de simulations
      names_to = "simulation_id",  # Nom de la colonne contenant les identifiants de simulation
      values_to = "simulated_rr"  # Nom de la colonne contenant les valeurs simulées
    )

  rr_table_interpolated <- rr_table_long %>%
    group_by(food_group, simulation_id) %>%
    complete(quantity = full_seq(0:800, 1)) %>%
    arrange(quantity) %>%
    mutate(rr_interpolated = if_else(is.na(simulated_rr), approx(quantity, simulated_rr, xout = quantity, method = "linear", rule = 1)$y, simulated_rr)) %>% 
    # $y, rr, récupérer les valeurs interpolées en y de la fonction approx et les attribuer à rr
    mutate(rr_interpolated = if_else(quantity > max(quantity[!is.na(simulated_rr)]), NA_real_, rr_interpolated)) %>%
    ungroup() %>% 
    select("simulation_id", "food_group", "quantity", "rr_interpolated")
  
################################################################################################################################
#                                             8. Exportation des données                                                       #
################################################################################################################################
  
  export(rr_table_interpolated, here("data_clean", "rr_table_interpolated_sim.csv"))
  