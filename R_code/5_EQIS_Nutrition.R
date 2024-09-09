################################################################################################################################
#                                             1. Chargement des packages                                                       #
################################################################################################################################

pacman::p_load(
  rio,                 # Importation/Exportation de fichiers
  here,                # Localisation des fichiers dans le dossier du projet
  dplyr,               # Manipulation des données
  tidyr,               # Manipulation des données
  tidyverse,           # Data management, inclus ggplot
  scico                # Palettes de couleur
)

################################################################################################################################
#                                             2. Importation des données                                                       #
################################################################################################################################

# RR combinés par année pour chaque scénario (implémentation linéaire)
  combined_rr_lin <- import(here("data_clean", "combined_rr_lin_ttfe_lin_20.xlsx"))
  
# RR combinés par année pour chaque scénario (implémentation par interpolation cosinus)
  combined_rr_cos <- import(here("data_clean", "combined_rr_cos.xlsx"))
  
# RR combinés par année pour chaque scénario (implémentation sigmoïdale)
  combined_rr_sig <- import(here("data_clean", "combined_rr_sig.xlsx"))
  
# Données INSEE
  
  # Effectifs de population par age et par année
  population <- import(here("data_clean", "population_clean.xlsx"))
  
  # Décès par age et par année
  deaths <- import(here("data_clean", "deaths_clean.xlsx"))

# Mortality rates par age et par année
  MR <- import(here("data_clean", "MR_table.xlsx"))
  
# Bornes temporelles du modèle
  year_i <- 2020 # Année initiale
  year_f <- 2050 # Année finale

################################################################################################################################
#                                             3. Préparation des données                                                       #
################################################################################################################################
  
# Sélectionner les MR entre les bornes temporelles du modèle et pivoter le dataframe en format long
  MR_2050 <- MR %>% 
    select(age, !!sym(as.character(year_i)) : !!sym(as.character(year_f))) %>%
    pivot_longer(cols = !!sym(as.character(year_i)) : !!sym(as.character(year_f)), 
                 names_to = "year", 
                 values_to = "MR") %>% 
    mutate(year = as.numeric(year))
  
# Sélectionner les effectifs de population de 2019 à 2050 et pivoter le dataframe en format long
  population_2050 <- population %>% 
    select("age", !!sym(as.character(year_i)) : !!sym(as.character(year_f))) %>% 
    pivot_longer(cols = !!sym(as.character(year_i)) : !!sym(as.character(year_f)), 
                 names_to = "year", 
                 values_to = "population") %>% 
    mutate(year = as.numeric(year)) %>% 
    arrange(age)

# Renommer la colonne year dans le tableau des RR
  combined_rr_lin <- combined_rr_lin %>% 
    rename("year" = "year_n")
 
# Charte graphique
  col_scenario <- c("actuel" = "azure4",
                    "sc0" = "royalblue2",
                    "sc1" = "darkseagreen4",
                    "sc2" = "aquamarine2",
                    "sc3" = "lightpink",
                    "sc4" = "maroon",
                    "sc5" = "royalblue4")
  
  
################################################################################################################################
#                                             4. Ajustement des MR                                                             #
################################################################################################################################
  
# Ajustement des MR et standardisation par rapport à l'année initiale du modèle 
  
  # Implémentation linéaire des régimes 
       
        # MR (/age/année), RR(/année/scénario), MR ajustés (/age/année/scénario)
           mr_table_lin <- MR_2050 %>% 
             inner_join(combined_rr_lin, by = "year", relationship = "many-to-many") %>%
             group_by(age, year) %>% 
             mutate(adjusted_mr = MR*combined_rr/combined_rr[scenario == "actuel"]) %>% 
             ungroup()
           
        # Wide dataframe MR ajustés (/age/année/scénario)
           adjusted_mr_lin <- mr_table_lin %>% 
             select("age", "year", "scenario", "adjusted_mr") %>% 
             pivot_wider(names_from = "year", values_from = "adjusted_mr")

  # Implémentation par interpolation cosinus des régimes 
       
      # MR (/age/année), RR(/année/scénario), MR ajustés (/age/année/scénario)
         mr_table_cos <- MR_2050 %>% 
           inner_join(combined_rr_cos, by = "year", relationship = "many-to-many") %>%
           group_by(scenario, age) %>% 
           mutate(adjusted_mr = MR*combined_rr/combined_rr[year == year_i]) %>% 
           ungroup()
         
      # Wide dataframe MR ajustés (/age/année/scénario)
         adjusted_mr_cos <- mr_table_cos %>% 
           select("age", "year", "scenario", "adjusted_mr") %>% 
           pivot_wider(names_from = "year", values_from = "adjusted_mr")
       
    # Implémentation sigmoïdale des régimes 
         
        # MR (/age/année), RR(/année/scénario), MR ajustés (/age/année/scénario)
         mr_table_sig <- MR_2050 %>% 
           inner_join(combined_rr_sig, by = "year", relationship = "many-to-many") %>%
           group_by(scenario, age) %>% 
           mutate(adjusted_mr = MR*combined_rr/combined_rr[year == year_i]) %>% 
           ungroup()
         
        # Wide dataframe MR ajustés (/age/année/scénario)
         adjusted_mr_sig <- mr_table_sig %>% 
           select("age", "year", "scenario", "adjusted_mr") %>% 
           pivot_wider(names_from = "year", values_from = "adjusted_mr")
         
################################################################################################################################
#                                             5. Décès évités (implémentation linéaire des régimes )                           #
################################################################################################################################

# Décès par age et par année dans chaque scénario
   
  # Coller les données de population avec MR, RR et MR ajustés
     population_evo_lin <- mr_table_lin %>%
       left_join(population_2050, by = c("age", "year")) %>%
       mutate(deaths = adjusted_mr*population)
     
  # Wide dataframe décès (/age/année/scénario)
     deaths_evo_lin <- population_evo_lin %>% 
       select("age", "year", "scenario", "deaths") %>% 
       pivot_wider(names_from = "year", values_from = "deaths")
     
   
# Nombre total de décès par année et par scénario

     total_deaths_lin <- deaths_evo_lin %>% 
         group_by(scenario) %>%                                 
         summarise(across(!!sym(as.character(year_i)) : !!sym(as.character(year_f)), sum)) %>%
         rowwise() %>%
         mutate(total_deaths = sum(c_across(!!sym(as.character(year_i)) : !!sym(as.character(year_f)))))    

# Nombre total de décès évités par rapport au scénario actuel
     
     # Extraire les décès par année et totaux du scénario actuel
     total_deaths_actuel_lin <- total_deaths_lin %>% 
       filter(scenario == "actuel")
     
     total_avoided_deaths_lin <- total_deaths_lin %>% 
       filter(scenario %in% c("sc0", "sc1", "sc2", "sc3", "sc4", "sc5")) %>% 
       mutate(across(-"scenario",
                     ~ total_deaths_actuel_lin[[cur_column()]] - .)) %>% 
       rename("avoided_deaths" = "total_deaths")
  
# Nombre de décès évités par age et par année par rapport au scénario actuel
     
    # Filtrer les décès du scénario "Tendanciel"
     deaths_actuel_lin <- deaths_evo_lin %>% 
       filter(scenario == "actuel") %>%
       select(-scenario) %>% 
       rename_with(~ paste0("actuel_", .), -age) # Renommer les colonnes pour éviter les conflits lors de la jointure
     
    # Joindre les données par âge et calculer les différences
     avoided_deaths_lin <- deaths_evo_lin %>%
       filter(scenario %in% c("sc0", "sc1", "sc2", "sc3", "sc4", "sc5")) %>%
       left_join(deaths_actuel_lin, by = "age") %>%
       mutate(across(!!sym(as.character(year_i)) : !!sym(as.character(year_f)), 
                     ~ get(paste0("actuel_", cur_column())) - .)) %>% 
       select("age", "scenario", !!sym(as.character(year_i)) : !!sym(as.character(year_f)))
     
    # Transformer en format long pour les graphs
     avoided_deaths_lin_long <- avoided_deaths_lin %>% 
       pivot_longer(cols = !!sym(as.character(year_i)) : !!sym(as.character(year_f)),
                    names_to = "year",
                    values_to = "avoided_deaths") %>% 
       mutate(year = as.numeric(year))
     
    # Décès évités par age et par scénario en 2035
     avoided_deaths_lin_2035 <- avoided_deaths_lin %>% 
       select(age, scenario, "2035") %>% 
       rename("avoided_deaths" = "2035")
     
    # Décès évités par age et par scénario en 2050
     avoided_deaths_lin_2050 <- avoided_deaths_lin %>% 
       select(age, scenario, "2050") %>% 
       rename("avoided_deaths" = "2050")
     
    
# Nombre total de décès évités dans chaque scénario par rapport au scénario tendanciel

# Extraire les décès par année et totaux du scénario tendanciel
     total_deaths_sc0_lin <- total_deaths_lin %>% 
       filter(scenario == "sc0")
     
     total_avoided_deaths_lin <- total_deaths_lin %>% 
       filter(scenario %in% c("actuel", "sc1", "sc2", "sc3", "sc4", "sc5")) %>% 
       mutate(across(-"scenario",
                     ~ total_deaths_sc0_lin[[cur_column()]] - .)) %>% 
       rename("avoided_deaths" = "total_deaths")
     

# Nombre de décès évités par age et par année par rapport au scénario tendanciel

  # Filtrer les décès du scénario "Tendanciel"
     deaths_sc0_lin <- deaths_evo_lin %>% 
       filter(scenario == "sc0") %>%
       select(-scenario) %>% 
       rename_with(~ paste0("sc0_", .), -age) # Renommer les colonnes pour éviter les conflits lors de la jointure
     
  # Joindre les données par âge et calculer les différences
     avoided_deaths_lin <- deaths_evo_lin %>%
       filter(scenario %in% c("actuel", "sc1", "sc2", "sc3", "sc4", "sc5")) %>%
       left_join(deaths_sc0_lin, by = "age") %>%
       mutate(across(!!sym(as.character(year_i)) : !!sym(as.character(year_f)), 
                     ~ get(paste0("sc0_", cur_column())) - .)) %>% 
       select("age", "scenario", !!sym(as.character(year_i)) : !!sym(as.character(year_f)))
     
  # Transformer en format long pour les graphs
     avoided_deaths_lin_long <- avoided_deaths_lin %>% 
       pivot_longer(cols = starts_with("20"),
                    names_to = "year",
                    values_to = "avoided_deaths") %>% 
       mutate(year = as.numeric(year))
     

################################################################################################################################
#                                             6. Décès évités (implémentation par interpolation cosinus des régimes )          #
################################################################################################################################
     
# Décès par age et par année dans chaque scénario
  
# Coller les données de population avec MR, RR et MR ajustés
    population_evo_cos <- mr_table_cos %>%
       left_join(population_2050, by = c("age", "year")) %>%
       mutate(deaths = adjusted_mr*population)
     
# Wide dataframe décès (/age/année/scénario)
    deaths_evo_cos <- population_evo_cos %>% 
       select("age", "year", "scenario", "deaths") %>% 
       pivot_wider(names_from = "year", values_from = "deaths")
     
# Nombre de décès total par année et sur 2019-2050 par scénario
     
    total_deaths_cos <- deaths_evo_cos %>% 
       group_by(scenario) %>%                                 
       summarise(across("2019":"2050", sum)) %>%              
       rowwise() %>%                                          
       mutate(total_deaths = sum(c_across("2019":"2050")))    
    
# Nombre total de décès évités dans chaque scénario par rapport au scénario tendanciel
    
  # Extraire les décès par année et totaux du scénario tendanciel
    total_deaths_sc0_cos <- total_deaths_cos %>% 
      filter(scenario == "sc0")
    
    total_avoided_deaths_cos <- total_deaths_cos %>% 
      filter(scenario %in% c("sc1", "sc2", "sc3", "sc4", "sc5")) %>% 
      mutate(across(-"scenario",
                    ~ total_deaths_sc0_cos[[cur_column()]] - .)) %>% 
      rename("avoided_deaths" = "total_deaths")

# Nombre de décès évités par age et par année par rapport au scénario tendanciel
    
  # Filtrer les décès du scénario "Tendanciel"
    deaths_sc0_cos <- deaths_evo_cos %>% 
      filter(scenario == "sc0") %>%
      select(-scenario) %>% 
      rename_with(~ paste0("sc0_", .), -age) # Renommer les colonnes pour éviter les conflits lors de la jointure
    
  # Joindre les données par âge et calculer les différences
    avoided_deaths_cos <- deaths_evo_cos %>%
      filter(scenario %in% c("sc1", "sc2", "sc3", "sc4", "sc5")) %>%
      left_join(deaths_sc0_cos, by = "age") %>%
      mutate(across("2019":"2050", 
                    ~ get(paste0("sc0_", cur_column())) - .)) %>% 
      select("age", "scenario", "2019":"2050")
    
  # Transformer en format long pour les graphs
    avoided_deaths_cos_long <- avoided_deaths_cos %>% 
      pivot_longer(cols = starts_with("20"),
                   names_to = "year",
                   values_to = "avoided_deaths") %>% 
      mutate(year = as.numeric(year))
    
# Nombre de décès évités par age en 2035 et 2050 par rapport à l'année 2019 dans chaque scénario
  
  # Décès évités en 2035 répartis par age
    avoided_2035_cos <- deaths_evo_cos %>% 
      mutate(avoided_2035 = `2019` - `2035`) %>% 
      select("age", "scenario", "avoided_2035")
  
  # Nombre total de décès évités en 2035 par rapport à 2019
    total_avoided_2035_cos <- avoided_2035_cos %>% 
      group_by(scenario) %>% 
      summarize(total = sum(avoided_2035))
                                
  # Décès évités en 2050 répartis par age
    avoided_2050_cos <- deaths_evo_cos %>% 
      mutate(avoided_2050 = `2050` - `2019`) %>% 
      select("age", "scenario", "avoided_2050")
    
  # Nombre total de décès évités en 2050 par rapport à 2019
    total_avoided_2050_cos <- avoided_2050_cos %>% 
      group_by(scenario) %>% 
      summarize(total = sum(avoided_2050))
    
################################################################################################################################
#                                             7. Décès évités (implémentation sigmoïdale des régimes )                         #
################################################################################################################################
    
# Décès par age et par année dans chaque scénario
    
  # Coller les données de population avec MR, RR et MR ajustés
    population_evo_sig <- mr_table_sig %>%
      left_join(population_2050, by = c("age", "year")) %>%
      mutate(deaths = adjusted_mr*population)
    
  # Wide dataframe décès (/age/année/scénario)
    deaths_evo_sig <- population_evo_sig %>% 
      select("age", "year", "scenario", "deaths") %>% 
      pivot_wider(names_from = "year", values_from = "deaths")
    
# Nombre de décès total par année et sur 2019-2050 par scénario
    
    total_deaths_sig <- deaths_evo_sig %>% 
      group_by(scenario) %>%                                 
      summarise(across("2019":"2050", sum)) %>%              
      rowwise() %>%                                          
      mutate(total_deaths = sum(c_across("2019":"2050")))    
    
# Nombre total de décès évités dans chaque scénario par rapport au scénario tendanciel
    
    # Extraire les décès par année et totaux du scénario tendanciel
    total_deaths_sc0_sig <- total_deaths_sig %>% 
      filter(scenario == "sc0")
    
    total_avoided_deaths_sig <- total_deaths_sig %>% 
      filter(scenario %in% c("sc1", "sc2", "sc3", "sc4", "sc5")) %>% 
      mutate(across(-"scenario",
                    ~ total_deaths_sc0_sig[[cur_column()]] - .)) %>% 
      rename("avoided_deaths" = "total_deaths")
    
# Nombre de décès évités par age et par année par rapport au scénario tendanciel
    
  # Filtrer les décès du scénario "Tendanciel"
    deaths_sc0_sig <- deaths_evo_sig %>% 
      filter(scenario == "sc0") %>%
      select(-scenario) %>% 
      rename_with(~ paste0("sc0_", .), -age) 
    
  # Joindre les données par âge et calculer les différences
    avoided_deaths_sig <- deaths_evo_sig %>%
      filter(scenario %in% c("sc1", "sc2", "sc3", "sc4", "sc5")) %>%
      left_join(deaths_sc0_sig, by = "age") %>%
      mutate(across("2019":"2050", 
                    ~ get(paste0("sc0_", cur_column())) - .)) %>% 
      select("age", "scenario", "2019":"2050")
    
  # Transformer en format long pour les graphs
    avoided_deaths_sig_long <- avoided_deaths_sig %>% 
      pivot_longer(cols = starts_with("20"),
                   names_to = "year",
                   values_to = "avoided_deaths") %>% 
      mutate(year = as.numeric(year))
    
# Nombre de décès évités par age en 2035 et 2050 par rapport à l'année 2019 dans chaque scénario
    
    # Décès évités en 2035 répartis par age
    avoided_2035_sig <- deaths_evo_sig %>% 
      mutate(avoided_2035 = `2019` - `2035`) %>% 
      select("age", "scenario", "avoided_2035")
    
    # Nombre total de décès évités en 2035 par rapport à 2019
    total_avoided_2035_sig <- avoided_2035_sig %>% 
      group_by(scenario) %>% 
      summarize(total = sum(avoided_2035))
    
     # Décès évités en 2050 répartis par age
    avoided_2050_sig <- deaths_evo_sig %>% 
      mutate(avoided_2050 = `2050` - `2019`) %>% 
      select("age", "scenario", "avoided_2050")
    
    # Nombre total de décès évités en 2050 par rapport à 2019
    total_avoided_2050_sig <- avoided_2050_sig %>% 
      group_by(scenario) %>% 
      summarize(total = sum(avoided_2050))
    
    
################################################################################################################################
#                                             8. Graphiques (implémentarion linéaire des régimes)                              #
################################################################################################################################
  
# Nombre de décès total par scénario
  graph_total_deaths_lin <- ggplot(data = total_deaths_lin, aes(x = scenario, 
                                                            y = total_deaths, 
                                                            fill = scenario))+
      geom_bar(stat = "identity", alpha = 0.8)+
      labs(title =  "Total number of deaths in each scenario",
           subtitle = "Linear implementation of diets from 2019 to 2050",
           x = "Scenario", 
           y = "Number of deaths")+
      scale_fill_manual(values = col_scenario)+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
 
# Nombre total de décès évités par scénario
  graph_total_avoided_deaths_lin <- ggplot(data = total_avoided_deaths_lin, aes(x = scenario, 
                                                                                y = avoided_deaths, 
                                                                                fill = scenario))+
      geom_bar(stat = "identity", alpha = 0.8)+
      labs(title = "Total avoided deaths compared to keeping the current diet",
           subtitle = "Linear implementation of diets from 2020 to 2050, linear ttfe of 20 years",
           x = "",
           y = "number of deaths avoided")+
      scale_fill_manual(values = col_scenario)+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# Nombre de décès évités par âge et par année, dans chaque scénario
  heat_map_avoided_deaths_lin <- ggplot(avoided_deaths_lin_long, aes(x = year,
                                                                     y = age,
                                                                     fill = avoided_deaths)) +
    geom_tile()+
    facet_wrap(~ scenario, scales = "fixed") +
    scale_fill_gradient2(low = "dodgerblue4", mid = "grey80", high = "firebrick", limits = c(0,4500)) +
    labs(title = "Avoided deaths compared to keeping the current diet",
         subtitle = "Linear implementation of diets from 2020 to 2050, linear ttfe of 20 years",
         x = "",
         y = "Age",
         fill = "Avoided deaths") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_text(face = "bold",size = rel(0.8)))
  
# Nombre de décès évités par âge et par scénario en 2035
  graph_avoided_deaths_2035 <- ggplot(avoided_deaths_lin_2035, aes(x = age,
                                                                   y = avoided_deaths))+
    facet_wrap(~ scenario, scales = "fixed")+
    geom_bar(stat = "identity" ,
             fill = "maroon")+
    labs(title = "Avoided deaths in 2035 compared to keeping the current diet",
         subtitle = "linear implementation of diets, linear ttfe of 20 years",
         x = "Age",
         y = "Avoided deaths")
  
# Nombre de décès évités par âge et par scénario en 2050
  graph_avoided_deaths_2050 <- ggplot(avoided_deaths_lin_2050, aes(x = age,
                                                                   y = avoided_deaths))+
    facet_wrap(~ scenario, scales = "fixed")+
    geom_bar(stat = "identity" ,
             fill = "darkseagreen4")+
    labs(title = "Avoided deaths in 2050 compared to keeping the current diet",
         subtitle = "linear implementation of diets, linear ttfe of 20 years",
         x = "Age",
         y = "Avoided deaths")
  
################################################################################################################################
#                                             7. Graphiques (implémentarion par interpolation cosinus des régimes)             #
################################################################################################################################

# Nombre de décès total par scénario
  graph_total_deaths_cos <- ggplot(data = total_deaths_cos, aes(x = scenario, 
                                                                y = total_deaths, 
                                                                fill = scenario))+
    geom_bar(stat = "identity", alpha = 0.8)+
    labs(title =  "Total number of deaths in each scenario",
         subtitle = "Cosine interpolation implementation of diets from 2019 to 2050",
         x = "Scenario", 
         y = "Number of deaths")+
    scale_fill_manual(values = col_scenario)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Nombre total de décès évités par scénario par rapport au tendanciel
  graph_total_avoided_deaths_cos <- ggplot(data = total_avoided_deaths_cos, aes(x = scenario, 
                                                                                y = avoided_deaths, 
                                                                                fill = scenario))+
    geom_bar(stat = "identity",  alpha = 0.8)+
    labs(title = "Total avoided deaths compared to tendanciel pathway",
         subtitle = "Cosine interpolation implementation of diets from 2019 to 2050",
         x = "",
         y = "number of deaths avoided")+
    scale_fill_manual(values = col_scenario)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# Nombre de décès évités par âge et par année, dans chaque scénario
  heat_map_avoided_deaths_cos <- ggplot(avoided_deaths_cos_long, aes(x = year,
                                                                     y = age,
                                                                     fill = avoided_deaths)) +
    geom_tile()+
    facet_wrap(~ scenario, scales = "fixed") +
    scale_fill_gradient2(low = "dodgerblue4", mid = "grey80", high = "firebrick", limits = c(-2000, 2000)) +
    labs(title = "Avoided deaths compared to tendancial pathway",
         subtitle = "Cosine interpolation implementation of diets from 2019 to 2050",
         x = "",
         y = "Age",
         fill = "Avoided deaths") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_text(face = "bold",size = rel(0.8)))
  
# Nombre de décès évités en 2035 par rapport à 2019, par âge dans chaque scénario
  
  graph_avoided_deaths_2035_cos <- ggplot(avoided_2035_cos, aes(x = age,
                                                                y = avoided_2035))+
    facet_wrap(~ scenario, scales = "fixed")+
    geom_bar(stat = "identity" ,
             fill = "maroon")+
    labs(title = "Avoided deaths in 2035 compared to 2019",
         subtitle = "Cosine interpolation implementation of diets from 2019 to 2050",
         x = "Age",
         y = "Avoided deaths")
  
# Nombre de décès évités en 2050 par rapport à 2019, par âge dans chaque scénario
  
  graph_avoided_deaths_2050_cos <- ggplot(avoided_2050_cos, aes(x = age,
                                                                y = avoided_2050))+
    facet_wrap(~ scenario, scales = "fixed")+
    geom_bar(stat = "identity" ,
             fill = "maroon")+
    labs(title = "Avoided deaths in 2050 compared to 2019",
         subtitle = "Cosine interpolation implementation of diets from 2019 to 2050",
         x = "Age",
         y = "Avoided deaths")
  
# Nombre de décès évités en 2035 dans chaque scénario par rapport au tendanciel
  
  graph_avoided_deaths_2035_tend_cos <- ggplot (avoided_deaths_cos, aes(x = age,
                                                                        y = `2035`))+
    facet_wrap(~ scenario, scales = "fixed")+
    geom_bar(stat = "identity" ,
             fill = "cadetblue")+
    labs(title = "Avoided deaths in 2035 in each scenario compared to trend pathway",
         subtitle = "Cosine interpolation implementation of diets from 2019 to 2050",
         x = "Age",
         y = "Avoided deaths")
  
  # Nombre de décès évités en 2035 dans chaque scénario par rapport au tendanciel
  
  graph_avoided_deaths_2050_tend_cos <- ggplot (avoided_deaths_cos, aes(x = age,
                                                                        y = `2050`))+
    facet_wrap(~ scenario, scales = "fixed")+
    geom_bar(stat = "identity" ,
             fill = "cadetblue")+
    labs(title = "Avoided deaths in 2050 in each scenario compared to trend pathway",
         subtitle = "Cosine interpolation implementation of diets from 2019 to 2050",
         x = "Age",
         y = "Avoided deaths")
  
################################################################################################################################
#                                             8. Graphiques (implémentarion sigmoïdale des régimes)                            #
################################################################################################################################
  
  # Nombre de décès total par scénario
  graph_total_deaths_sig <- ggplot(data = total_deaths_sig, aes(x = scenario, 
                                                                y = total_deaths, 
                                                                fill = scenario))+
    geom_bar(stat = "identity", alpha = 0.8)+
    labs(title =  "Total number of deaths in each scenario",
         subtitle = "Sigmoidal implementation of diets from 2019 to 2050",
         x = "Scenario", 
         y = "Number of deaths")+
    scale_fill_manual(values = col_scenario)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Nombre total de décès évités par scénario par rapport au tendanciel
  graph_total_avoided_deaths_sig <- ggplot(data = total_avoided_deaths_sig, aes(x = scenario, 
                                                                                y = avoided_deaths, 
                                                                                fill = scenario))+
    geom_bar(stat = "identity",  alpha = 0.8)+
    labs(title = "Total avoided deaths compared to tendanciel pathway",
         subtitle = "Sigmoidal implementation of diets from 2019 to 2050",
         x = "",
         y = "number of deaths avoided")+
    scale_fill_manual(values = col_scenario)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Nombre de décès évités par âge et par année, dans chaque scénario
  heat_map_avoided_deaths_sig <- ggplot(avoided_deaths_sig_long, aes(x = year,
                                                                     y = age,
                                                                     fill = avoided_deaths)) +
    geom_tile()+
    facet_wrap(~ scenario, scales = "fixed") +
    scale_fill_gradient2(low = "dodgerblue4", mid = "grey80", high = "firebrick", limits = c(-2000, 2000)) +
    labs(title = "Avoided deaths compared to tendancial pathway",
         subtitle = "Sigmoidal implementation of diets from 2019 to 2050",
         x = "",
         y = "Age",
         fill = "Avoided deaths") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_text(face = "bold",size = rel(0.8)))
  
  # Nombre de décès évités en 2035 par rapport à 2019, par âge dans chaque scénario
  
  graph_avoided_deaths_2035_sig <- ggplot(avoided_2035_sig, aes(x = age,
                                                                y = avoided_2035))+
    facet_wrap(~ scenario, scales = "fixed")+
    geom_bar(stat = "identity" ,
             fill = "maroon")+
    labs(title = "Avoided deaths in 2035 compared to 2019",
         subtitle = "Sigmoidal implementation of diets from 2019 to 2050",
         x = "Age",
         y = "Avoided deaths")
  
  # Nombre de décès évités en 2050 par rapport à 2019, par âge dans chaque scénario
  
  graph_avoided_deaths_2050_sig <- ggplot(avoided_2050_sig, aes(x = age,
                                                                y = avoided_2050))+
    facet_wrap(~ scenario, scales = "fixed")+
    geom_bar(stat = "identity" ,
             fill = "maroon")+
    labs(title = "Avoided deaths in 2050 compared to 2019",
         subtitle = "Sigmoidal implementation of diets from 2019 to 2050",
         x = "Age",
         y = "Avoided deaths")
  
  # Nombre de décès évités en 2035 dans chaque scénario par rapport au tendanciel
  
  graph_avoided_deaths_2035_tend_sig <- ggplot (avoided_deaths_sig, aes(x = age,
                                                                        y = `2035`))+
    facet_wrap(~ scenario, scales = "fixed")+
    geom_bar(stat = "identity" ,
             fill = "cadetblue")+
    labs(title = "Avoided deaths in 2035 in each scenario compared to trend pathway",
         subtitle = "Sigmoidal implementation of diets from 2019 to 2050",
         x = "Age",
         y = "Avoided deaths")
  
  # Nombre de décès évités en 2035 dans chaque scénario par rapport au tendanciel
  
  graph_avoided_deaths_2050_tend_sig <- ggplot (avoided_deaths_sig, aes(x = age,
                                                                        y = `2050`))+
    facet_wrap(~ scenario, scales = "fixed")+
    geom_bar(stat = "identity" ,
             fill = "cadetblue")+
    labs(title = "Avoided deaths in 2050 in each scenario compared to trend pathway",
         subtitle = "Sigmoidal implementation of diets from 2019 to 2050",
         x = "Age",
         y = "Avoided deaths")
  
################################################################################################################################
#                                             6. Exportation des données                                                       #
################################################################################################################################
  
# MR ajustés et standardisés sur l'année initiale du modèle, pour chaque scénario, age et année
  
  # Implémentation linéaire des régimes alimentaires
    export(adjusted_mr_lin, here("data_clean", "adjusted_mr_lin.xlsx"))
  
  # Implémentation par interpolation cosinus des régimes alimentaires
    export(adjusted_mr_cos, here("data_clean", "adjusted_mr_cos.xlsx"))
    
  # Implémentation sigmoïdale des régimes alimentaires
    export(adjusted_mr_sig, here("data_clean", "adjusted_mr_sig.xlsx"))
  
# Nombre de décès/age/année
    
  # Implémentation linéaire des régimes alimentaires
    export(deaths_evo_lin, here("data_clean", "deaths_evo_lin.xlsx"))
    
  # Implémentation par interpolation cosinus des régimes alimentaires
    export(deaths_evo_cos, here("data_clean", "deaths_evo_cos.xlsx"))
    
  # Implémentation sigmoïdale des régimes alimentaires
    export(deaths_evo_sig, here("data_clean", "deaths_evo_sig.xlsx"))
    
# Nombre total de décès par scénario
    
  # Implémentation linéaire des régimes alimentaires
    export(total_deaths_lin, here("data_clean", "total_deaths_lin.xlsx"))
    ggsave(here("results", "total_deaths_lin.pdf"), plot = graph_total_deaths_lin)
    
  # Implémentation par interpolation cosinus des régimes alimentaires
    export(total_deaths_cos, here("data_clean", "total_deaths_cos.xlsx"))
    ggsave(here("results", "total_deaths_cos.pdf"), plot = graph_total_deaths_cos)
    
  # Implémentation linéaire des régimes alimentaires
    export(total_deaths_sig, here("data_clean", "total_deaths_sig.xlsx"))
    ggsave(here("results", "total_deaths_sig.pdf"), plot = graph_total_deaths_sig)
    
# Nombre total de décès évités dans chaque scénario
    
  # Implémentation linéaire des régimes alimentaires
    export(total_avoided_deaths_lin, here("data_clean", "total_avoided_deaths_lin.xlsx"))
    ggsave(here("results", "total_avoided_deaths_lin.pdf"), plot = graph_total_avoided_deaths_lin)
    
  # Implémentation par interpolation cosinus des régimes alimentaires
    export(total_avoided_deaths_cos, here("data_clean", "total_avoided_deaths_cos.xlsx"))
    ggsave(here("results", "total_avoided_deaths_cos.pdf"), plot = graph_total_avoided_deaths_cos)
    
  # Implémentation sigmoïdale des régimes alimentaires
    export(total_avoided_deaths_sig, here("data_clean", "total_avoided_deaths_sig.xlsx"))
    ggsave(here("results", "total_avoided_deaths_sig.pdf"), plot = graph_total_avoided_deaths_sig)
    
  
# Nombre de décès évités par age et par année 
    
  # Implémentation linéaire des régimes alimentaires
    export(avoided_deaths_lin, here("data_clean", "avoided_deaths_lin.xlsx"))
    ggsave(here("results", "heat_map_avoided_deaths_lin.pdf"), plot = heat_map_avoided_deaths_lin)
    
  # Implémentation par interpolation cosinus des régimes alimentaires
    export(avoided_deaths_cos, here("data_clean", "avoided_deaths_cos.xlsx"))
    ggsave(here("results", "heat_map_avoided_deaths_cos.pdf"), plot = heat_map_avoided_deaths_cos)
    
  # Implémentation sigmoïdale des régimes alimentaires
    export(avoided_deaths_sig, here("data_clean", "avoided_deaths_sig.xlsx"))
    ggsave(here("results", "heat_map_avoided_deaths_sig.pdf"), plot = heat_map_avoided_deaths_sig)
