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

population_evo <- import(here("results", "visualization_tool_ic95_sim", "avoided_deaths.csv"))

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

population_evo <- population_evo %>% 
  select("age", "year", "scenario", "simulation_id", "adjusted_mr", "population", "deaths") %>% 
  group_by(year, simulation_id, age) %>% 
  mutate(avoided_deaths = deaths[scenario == "actuel"] - deaths)

calc_conditional_LE <- function(df) {
  df %>%
    arrange(year, age) %>%
    group_by(year, simulation_id, scenario) %>%
    mutate(
      qx = adjusted_mr,                           # Taux de mortalité par âge (déjà dans les données)
      px = 1 - qx,                                   # Probabilité de survie à chaque âge
      lx = cumprod(lag(px, default = 1)),            # Probabilité de survie jusqu'à chaque âge
      dx = lx * qx,                                  # Nombre de décès attendu à chaque âge
      Tx = rev(cumsum(rev(lx)))                      # Somme cumulée des survivants pour espérance de vie
    ) %>%
    mutate(
      ex = Tx / lx                                   # Espérance de vie conditionnelle à chaque âge
    ) 
}

population_evo <- calc_conditional_LE(population_evo)

population_evo <- population_evo %>% 
  group_by(age, year, scenario, simulation_id) %>% 
  mutate(le = age + ex,
         ylg = avoided_deaths * (le - age)) %>% 
  select(simulation_id, age, year, scenario, deaths, avoided_deaths, le, ylg)

################################################################################################################################
#                                             4. Années de vie préservées                                                      #
################################################################################################################################

yll <- population_evo %>% 
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
                scenario %in% c("sc1", "sc2")),
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
#                                             5. Espérance de vie gagnée                                                       #
################################################################################################################################

le <- population_evo %>% 
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

graph_le <- ggplot(summary_le %>% 
                      filter(scenario %in% c("sc1", "sc2")),
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
                                   scenario %in% c("sc1", "sc2")),
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
#                                             6. Figure 2040, 2050, 2060                                                       #
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
#                                             6. Exportation des données                                                       #
################################################################################################################################

# YLL
export(summary_yll, here("results", "IC95_yll.xlsx"))
ggsave(here("results", "yll_reported.pdf"), plot = graph_yll)
ggsave(here("results", "yll_reported_dates.pdf"), plot = graph_yll_dates)

# LE
export(summary_le, here("results", "IC95_LE_gained.xlsx"))
ggsave(here("results", "LE_gained.pdf"), plot = graph_le)
ggsave(here("results", "LE_gaines_dates.pdf"), plot = graph_le_dates)

# Décès évités, YLL, LE, Couts en 2040, 2050, 2060
ggsave(here("results", "HIA_dates.pdf"), plot = common_graph)

