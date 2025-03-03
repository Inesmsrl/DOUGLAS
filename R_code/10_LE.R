################################################################################################################################
#                                             1. Chargement des packages                                                       #
################################################################################################################################

pacman::p_load(
  rio,                 # Importation de fichiers
  here,                # Localisation des fichiers dans le dossier du projet
  dplyr,               # Manipulation des données
  tidyr,
  tidyverse,
  patchwork            # Combinaison de graphes
)

################################################################################################################################
#                                             2. Importation des données                                                       #
################################################################################################################################

deaths_data <- import(here("results","Main analysis", "data_python.csv"))


################################################################################################################################
#                                             3. Charte graphique                                                              #
################################################################################################################################
col_scenario <- c("actuel" = "azure4",
                  "sc0" = "palevioletred3",
                  "sc1" = "aquamarine3",
                  "sc2" = "lightskyblue3",
                  "sc3" = "#882255",
                  "sc4" = "#DDCC77",
                  "sc5" = "royalblue4")

labels_scenario <- c("actuel" = "Current diet",
                     "sc1" = "Scenario 1",
                     "sc2" = "Scenario 2",
                     "sc3" = "Scenario 3",
                     "sc4" = "Scenario 4")

################################################################################################################################
#                                             3. Espérance de vie conditionnelle                                               #
################################################################################################################################

deaths_data <- deaths_data %>%  
  select("age", "year", "scenario", "simulation_id", "adjusted_mr", "population", "deaths", "avoided_deaths")

calc_conditional_LE <- function(df) {
  df %>%
    arrange(year, age) %>%
    group_by(year, simulation_id, scenario) %>%
    mutate(
      qx = adjusted_mr,                           # Taux de mortalité par âge (déjà dans les données)
      px = 1 - qx,                                # Probabilité de survie entre l'âge x et x+1
      lx = cumprod(lag(px, default = 1)),         # Probabilité de survie jusqu'à l'âge x
      dx = lx * qx,                               # Nombre de décès attendu à chaque âge
      Tx = rev(cumsum(rev(lx)))                   # Somme cumulée des survivants au delà d'un age x = nombre total d'années vécues au delà de l'âge x
    ) %>%
    mutate(
      ex = Tx / lx                                # Espérance de vie conditionnelle à chaque âge = répartition des années vécues au delà de l'âge x par le nombre de survivants à cet âge
    ) 
}

deaths_data <- calc_conditional_LE(deaths_data)

deaths_data <- deaths_data %>% 
  group_by(age, year, scenario, simulation_id) %>% 
  mutate(le = age + ex,
         ylg = avoided_deaths * (le - age)) %>% 
  select(simulation_id, age, year, scenario, deaths, avoided_deaths, le, ylg)

################################################################################################################################
#                                             4. Espérance de vie gagnée                                                       #
################################################################################################################################

le <- deaths_data %>% 
  group_by(simulation_id, year, scenario) %>% 
  summarise(le_year = mean(le)) %>% 
  mutate(leg = (le_year - le_year[scenario == "actuel"])*12)

le <- le %>% 
  group_by(scenario, year) %>% 
  filter(between(leg, quantile(leg, 0.025), quantile(leg, 0.975)))

summary_le <- le %>% 
  group_by(scenario, year) %>% 
  summarise(
    mean_le = mean(leg, na.rm = TRUE),
    lower_ci = quantile(leg, 0.025, na.rm = TRUE),
    upper_ci = quantile(leg, 0.975, na.rm = TRUE)
  )

################################################################################################################################
#                                             5. Figures : Espérance de vie gagnée                                             #
################################################################################################################################

graph_le <- ggplot(summary_le %>% 
                      filter(scenario != "actuel"),
                    aes(x = year,
                        y = mean_le,
                        group = scenario,
                        color = scenario)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = scenario), alpha = 0.5, linetype = 0)+
  geom_line(linewidth = 0.6, na.rm = TRUE)+ 
  labs(
    title = "",
    x = "",
    y = "Life expectancy gained (months)"
  )+
  scale_color_manual(values = col_scenario,
                     labels = labels_scenario)+
  scale_fill_manual(values = col_scenario,
                    labels = labels_scenario)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
        axis.text.y = element_text(size = 7),
        strip.text = element_text(face = "bold",size = rel(1)),
        legend.position = "bottom")+
  guides(color = guide_legend(title = NULL),
         fill = guide_legend(title = NULL))

graph_le_dates <- ggplot(summary_le %>% 
                            filter(year %in% c(2040, 2050, 2060),
                                   scenario != "actuel"),
                          aes(x = scenario,
                              y = mean_le,
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
       y = "LE gained (months)")+
  guides(fill = guide_legend(title = NULL))
 
################################################################################################################################
#                                             6. Années de vie préservées                                                      #
################################################################################################################################

yll <- deaths_data %>% 
  group_by(simulation_id, scenario, year) %>% 
  summarise(yll = sum(ylg))

yll <- yll %>% 
  group_by(scenario, year) %>% 
  filter(between(yll, quantile(yll, 0.025), quantile(yll, 0.975)))

summary_yll <- yll %>% 
  group_by(scenario, year) %>% 
  summarise(
    mean_yll = mean(yll, na.rm = TRUE),
    lower_ci = quantile(yll, 0.025, na.rm = TRUE),
    upper_ci = quantile(yll, 0.975, na.rm = TRUE)
  )

################################################################################################################################
#                                             7. Figures : Années de vie préservées                                                      #
################################################################################################################################

graph_yll <- ggplot(summary_yll %>% 
                                        filter(scenario != "actuel"),
                                      aes(x = year,
                                          y = mean_yll,
                                          group = scenario,
                                          color = scenario)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = scenario), alpha = 0.5, linetype = 0)+
  geom_line(linewidth = 0.6, na.rm = TRUE)+ 
  labs(
    title = "",
    x = "",
    y = "YLL prevented"
  )+
  scale_color_manual(values = col_scenario,
                     labels = labels_scenario)+
  scale_fill_manual(values = col_scenario,
                    labels = labels_scenario)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
        axis.text.y = element_text(size = 7),
        strip.text = element_text(face = "bold",size = rel(1)),
        legend.position = "bottom")+
  guides(color = guide_legend(title = NULL),
         fill = guide_legend(title = NULL))

graph_yll_dates <- ggplot(summary_yll %>% 
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
       y = "YLL preserved")+
  guides(fill = guide_legend(title = NULL))

################################################################################################################################
#                                             6. Figure bénéfices en 2040, 2050, 2060                                          #
################################################################################################################################

# Faire tourner les codes avoir les graphes des décès reportés et des coûts évités

list_graph <- list(graph_avoided_deaths_dates, graph_yll_dates, graph_le_dates, graph_yll_costs_dates)

common_theme <- theme(
  axis.title = element_text(size = 7, face = "bold"),
  strip.text = element_text(size = 6),
  axis.text.y = element_text(size = 6),
  legend.position = "none"
)

list_graph <- lapply(list_graph, function(p) p + common_theme)

common_graph <- reduce(list_graph, `+`) + plot_layout(ncol = 2)

print(common_graph)

################################################################################################################################
#                                             7. Années de vie préservées âge et année spécifique                              #
################################################################################################################################

pop_sp <- population_evo %>% 
  filter(year == 2050,
         age == 20) 

yll_sp <- pop_sp %>% 
  group_by(scenario) %>% 
  filter(between(ylg, quantile(ylg, 0.025), quantile(ylg, 0.975)))

summary_yll_sp <- yll_sp %>% 
  group_by(scenario, year) %>% 
  summarise(
    mean_yll = mean(ylg, na.rm = TRUE),
    lower_ci = quantile(ylg, 0.025, na.rm = TRUE),
    upper_ci = quantile(ylg, 0.975, na.rm = TRUE)
  )


graph_yll_sp <- ggplot(summary_yll_sp %>% 
                            filter(scenario != "actuel"),
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
  scale_y_continuous(labels = scales :: label_comma())+
  scale_fill_manual(values = col_scenario,
                    labels = labels_scenario)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom")+
  labs(title = "",
       x = "",
       y = "YLL preserved")+
  guides(fill = guide_legend(title = NULL))

################################################################################################################################
#                                             8. Espérance de vie gagnée âge et année spécifique                               #
################################################################################################################################

le_sp <- pop_sp %>% 
  group_by(simulation_id, scenario) %>% 
  summarise(le_year = mean(le)) %>% 
  mutate(leg = (le_year - le_year[scenario == "actuel"])*12)

le_sp <- le_sp %>% 
  group_by(scenario) %>% 
  filter(between(leg, quantile(leg, 0.025), quantile(leg, 0.975)))

summary_le_sp <- le_sp %>% 
  group_by(scenario) %>% 
  summarise(
    mean_le = mean(leg, na.rm = TRUE),
    lower_ci = quantile(leg, 0.025, na.rm = TRUE),
    upper_ci = quantile(leg, 0.975, na.rm = TRUE)
  )

graph_le_sp <- ggplot(summary_le_sp %>% 
                           filter(scenario != "actuel"),
                         aes(x = scenario,
                             y = mean_le,
                             fill = scenario))+
  geom_bar(stat = "identity",
           position = "dodge",
           alpha = 0.7)+
  geom_errorbar(aes(ymin = lower_ci,
                    ymax = upper_ci),
                width = 0.2,
                position = position_dodge(0.9))+
  scale_y_continuous(labels = scales :: label_comma())+
  scale_fill_manual(values = col_scenario,
                    labels = labels_scenario)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom")+
  labs(title = "",
       x = "",
       y = "LE gained (months)")+
  guides(fill = guide_legend(title = NULL))

################################################################################################################################
#                                             9. Exportation des données                                                       #
################################################################################################################################

# LE
export(summary_le, here("results", "HIA", "IC95_LE_gained.xlsx"))
ggsave(here("results","HIA", "LE_gained.pdf"), plot = graph_le)
ggsave(here("results","HIA", "LE_gaines_dates.pdf"), plot = graph_le_dates)

# YLL
export(summary_yll, here("results", "HIA", "IC95_yll.xlsx"))
ggsave(here("results", "HIA", "yll_reported.pdf"), plot = graph_yll)
ggsave(here("results", "HIA", "yll_reported_dates.pdf"), plot = graph_yll_dates)


# YLL pour un âge et une année spécifique 
export(summary_yll_sp, here("results", "IC95_yll_sp.xlsx"))
ggsave(here("results", "yll_sp.pdf"), plot = graph_yll_sp)

# LE pour un âge et une année spécifique
export(summary_le_sp, here("results", "IC95_LE_sp.xlsx"))
ggsave(here("results", "LE_sp.pdf"), plot = graph_le_sp)

# Décès évités, YLL, LE, Couts en 2040, 2050, 2060
ggsave(here("results", "HIA_dates.pdf"), plot = common_graph)
