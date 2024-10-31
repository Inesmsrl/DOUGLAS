################################################################################################################################
#                                             1. Chargement des packages                                                       #
################################################################################################################################

pacman::p_load(
  rio,                 # Importation de fichiers
  here,                # Localisation des fichiers dans le dossier du projet
  dplyr,               # Manipulation des données
  tidyr,
  tidyverse
)


################################################################################################################################
#                                             2. Importation des données                                                       #
################################################################################################################################

yll <- import(here("results", "IC95_yll.xlsx"))

cost_2040 <- 188000

cost_2050 <- 210000

cost_260 <- 238000

################################################################################################################################
#                                             3. Coûs énconomisés                                                              #
################################################################################################################################

costs <- yll %>% 
  group_by(scenario,)
  mutate(costs = case_when(
    year == 2040 ~ yll * cost_2040,
    year == 2050 ~ yll * cost_2050,
    year == 2060 ~ yll * cost_2060
  ),
  costs_ic_lower = case_when(
    year == 2040 ~ ic_lower * cost_2040,
    year == 2050 ~ ic_lower * cost_2050,
    year == 2060 ~ ic_lower * cost_2060
  ),
  costs_ic_upper = case_when(
    year == 2040 ~ ic_upper * cost_2040,
    year == 2050 ~ ic_upper * cost_2050,
    year == 2060 ~ ic_upper * cost_2060
  ))
  
graph_yll_costs_dates <- ggplot(costs %>% 
                              filter(year %in% c(2040, 2050, 2060),
                                     scenario != "actuel"),
                            aes(x = scenario,
                                y = mean_yll,
                                fill = scenario))+
    geom_bar(stat = "identity",
             position = "dodge",
             alpha = 0.7)+
    geom_errorbar(aes(ymin = lower_ci,
                      ymax = upper_ci),
                  width = 0.2,
                  position = position_dodge(0.9))+
    facet_wrap(~year,
               ncol = 3)+
    scale_y_continuous(labels = scales :: label_comma())+
    scale_fill_manual(values = col_scenario,
                      labels = labels_scenario)+
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "bottom")+
    labs(title = "",
         x = "",
         y = "Costs avoided")+
    guides(fill = guide_legend(title = NULL))
  
  