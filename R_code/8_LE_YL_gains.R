# 1. Loading packages
# 2. Data importation
# 3. Parameters
# 4. Conditional life expectancy
# 5. Gain in Life Expectancy
# 6. Graphs : LE gained
# 7. Years of life gained
# 8. Graphs : YLG
# 9. Graph : Health benefits at specific dates
# 10. YLG for a specific age and year
# 11. LEG for a specific age and year

################################################################################################################################
#                                             1. Loading packages                                                              #
################################################################################################################################

pacman::p_load(
  rio,                 # File import/export
  here,                # File path management
  dplyr,               # Data manipulation
  tidyr,               # Data manipulation
  tidyverse,           # Data management, ggplot included
  patchwork,            # Graphs combination
  scales              # For the y-axis labels
)

################################################################################################################################
#                                             2. Data importation                                                              #
################################################################################################################################

deaths_data <- import(here("Python_code", "data_python.csv"))

################################################################################################################################
#                                             3. Parameters                                                                    #
################################################################################################################################

source(here("R_code", "0_parameters.R"))

################################################################################################################################
#                                             4. Conditional life expectancy                                                   #
################################################################################################################################

deaths_data <- deaths_data %>%  
  select("age", "year", "scenario", "simulation_id", "adjusted_mr", "population", "deaths", "avoided_deaths")

calc_conditional_LE <- function(df) {
  df %>%
    arrange(year, age) %>%
    group_by(year, simulation_id, scenario) %>%
    mutate(
      qx = adjusted_mr,                           # MR by age (already in the dataset)
      px = 1 - qx,                                # Survival probability between age x and x+1
      lx = cumprod(lag(px, default = 1)),         # Survival probability to age x
      dx = lx * qx,                               # Number of deaths expected at age x
      Tx = rev(cumsum(rev(lx)))                   # Cumulated sum of survivors beyond age x = total number of years lived beyond age x
    ) %>%
    mutate(
      ex = Tx / lx                                # Conditional life expectancy at age x = distribution of years lived beyond age x by the number of survivors at that age
    ) %>% 
    ungroup()
}

deaths_data <- calc_conditional_LE(deaths_data)

# Calculate the age at death at each age by scenario and the years of life gained (YLG)
deaths_data <- deaths_data %>% 
  group_by(age, year, scenario, simulation_id) %>% 
  mutate(le = age + ex,
         ylg = avoided_deaths * (le - age)) %>% 
  select(simulation_id, age, year, scenario, deaths, avoided_deaths, le, ylg) %>% 
  ungroup()

################################################################################################################################
#                                             5. Gain in Life Expectancy                                                       #
################################################################################################################################

# Calculate the life expectancy gained (LEG) by scenario and year
le <- deaths_data %>% 
  group_by(simulation_id, year, scenario) %>% 
  summarise(le_year = mean(le)) %>% # Mean of age at death of each age
  mutate(leg = (le_year - le_year[scenario == "actuel"]) * 12) %>% # Life expectancy gained in months
  ungroup()

# Delete the 5% most extreme values
le_cut <- le %>% 
  group_by(scenario, year) %>% 
  filter(between(leg, quantile(leg, 0.025), quantile(leg, 0.975))) %>% 
  ungroup()

# Calculate the mean and 95% CI of life expectancy gained by scenario and year
ic95_le <- le_cut %>% 
  group_by(scenario, year) %>% 
  summarise(
    mean_le = mean(leg, na.rm = TRUE),
    lower_ci = quantile(leg, 0.025, na.rm = TRUE), # Lower limit of the 95% CI
    upper_ci = quantile(leg, 0.975, na.rm = TRUE)  # Upper limit of the 95% CI
  ) %>% 
  ungroup()

################################################################################################################################
#                                             6. Graphs : LE gained                                                            #
################################################################################################################################

# Whole time period
graph_le <- ggplot(ic95_le %>% 
                      filter(scenario != "actuel"), #Change the baseline scenario if needed
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

plot(graph_le)

# At specific dates
graph_le_dates <- ggplot(ic95_le %>% 
                            filter(year %in% c(2040, 2050, 2060), # Change the dates if needed
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
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(values = col_scenario,
                    labels = labels_scenario)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom")+
  labs(title = "",
       x = "",
       y = "LE gained (months)")+
  guides(fill = guide_legend(title = NULL))

plot(graph_le_dates)

################################################################################################################################
#                                             7. Years of life gained                                                          #
################################################################################################################################

# Calculate the years of life gained (YLG) by scenario and year
yll <- deaths_data %>% 
  group_by(simulation_id, scenario, year) %>% 
  summarise(yll = sum(ylg)) %>%  # Sum of YLG at each age
  ungroup()

# Delete the 5% most extreme values
yll_cut <- yll %>% 
  group_by(scenario, year) %>% 
  filter(between(yll, quantile(yll, 0.025), quantile(yll, 0.975))) %>% 
  ungroup()

# Mean and 95% CI of YLG by scenario and year
ic95_yll <- yll_cut %>% 
  group_by(scenario, year) %>% 
  summarise(
    mean_yll = mean(yll, na.rm = TRUE),
    lower_ci = quantile(yll, 0.025, na.rm = TRUE), # Lower limit of the 95% CI
    upper_ci = quantile(yll, 0.975, na.rm = TRUE) # Upper limit of the 95% CI
  ) %>% 
  ungroup()

################################################################################################################################
#                                             8. Graphs : YLG                                                                  #
################################################################################################################################

# Whole time period
graph_yll <- ggplot(ic95_yll %>% 
                                        filter(scenario != "actuel"), #Change the baseline scenario if needed
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
  scale_y_continuous(labels = label_comma()) +
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

plot(graph_yll)

# At specific dates
graph_yll_dates <- ggplot(ic95_yll %>% 
         filter(year %in% c(2040, 2050, 2060), # Change the dates if needed
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
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(values = col_scenario,
                     labels = labels_scenario)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom")+
  labs(title = "",
       x = "",
       y = "Prevented YLL")+
  guides(fill = guide_legend(title = NULL))

plot(graph_yll_dates)

################################################################################################################################
#                                             9. Graph : Health benefits at specific dates                                     #
################################################################################################################################

# Run the appropriate code to get the prevented deaths and avoided costs graphs in memory
source(here("R_code", "7_prevented_deaths.R"))
source(here("R_code", "9_Costs_saved.R"))

# List of the graphs you want to combine in one single figure
list_graph <- list(graph_tot_av_deaths_dates, graph_yll_dates, graph_le_dates, graph_yll_costs_dates)

# Theme for the common graph
common_theme <- theme(
  axis.title = element_text(size = 7, face = "bold"),
  strip.text = element_text(size = 6),
  axis.text.y = element_text(size = 6),
  legend.position = "none"
)

# Apply the common theme to each graph
list_graph <- lapply(list_graph, function(p) p + common_theme)

# Combine the graphs into one single figure
common_graph <- reduce(list_graph, `+`) + plot_layout(ncol = 2)

print(common_graph)

################################################################################################################################
#                                             10. YLG for a specific age and year                                              #
################################################################################################################################

# Select the data for a specific age and year
pop_sp <- deaths_data %>% 
  filter(year == 2050,
         age == 18) 

# Delete the 5% most extreme values
yll_sp <- pop_sp %>% 
  group_by(scenario) %>% 
  filter(between(ylg, quantile(ylg, 0.025), quantile(ylg, 0.975))) %>% 
  ungroup()

# Mean and 95% CI of YLG
ic95_yll_sp <- yll_sp %>% 
  group_by(scenario, year) %>% 
  summarise(
    mean_yll = mean(ylg, na.rm = TRUE),
    lower_ci = quantile(ylg, 0.025, na.rm = TRUE),
    upper_ci = quantile(ylg, 0.975, na.rm = TRUE)
  ) %>% 
  ungroup()

# Graph
graph_yll_sp <- ggplot(ic95_yll_sp %>% 
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
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(values = col_scenario,
                    labels = labels_scenario)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom")+
  labs(title = "",
       x = "",
       y = "YLL preserved")+
  guides(fill = guide_legend(title = NULL))

plot(graph_yll_sp)
################################################################################################################################
#                                             11. LEG for a specific age and year                                              #
################################################################################################################################

# Select the data for a specific age and year
le_sp <- pop_sp %>% 
  group_by(simulation_id, scenario) %>% 
  summarise(le_year = mean(le)) %>% 
  mutate(leg = (le_year - le_year[scenario == "actuel"])) %>% 
  ungroup()

# Delete the 5% most extreme values
le_sp <- le_sp %>% 
  group_by(scenario) %>% 
  filter(between(leg, quantile(leg, 0.025), quantile(leg, 0.975))) %>% 
  ungroup()

# Mean and 95% CI of LEG
ic95_le_sp <- le_sp %>% 
  group_by(scenario) %>% 
  summarise(
    mean_le = mean(leg, na.rm = TRUE),
    lower_ci = quantile(leg, 0.025, na.rm = TRUE),
    upper_ci = quantile(leg, 0.975, na.rm = TRUE)
  ) %>% 
  ungroup()

# Graph
graph_le_sp <- ggplot(ic95_le_sp %>% 
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
  scale_y_continuous(labels = label_comma()) +  
  scale_fill_manual(values = col_scenario,
                    labels = labels_scenario)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom")+
  labs(title = "",
       x = "",
       y = "LE gained (years)")+
  guides(fill = guide_legend(title = NULL))

plot(graph_le_sp)
################################################################################################################################
#                                             12. Data exportation                                                             #
################################################################################################################################

export(deaths_data, here("results", "HIA", "deaths_data.csv"))

# LE
export(le, here("results", "HIA", "le.csv"))
export(ic95_le, here("results", "HIA", "IC95_LE.xlsx"))
ggsave(here("results", "HIA", "LE_gained.pdf"), plot = graph_le)
ggsave(here("results", "HIA", "LE_gaines_dates.pdf"), plot = graph_le_dates)

# YLL
export(yll, here("results", "HIA", "yll.csv"))
export(ic95_yll, here("results", "HIA", "IC95_yll.xlsx"))
ggsave(here("results", "HIA", "yll_prevented.pdf"), plot = graph_yll)
ggsave(here("results", "HIA", "yll_prevented_dates.pdf"), plot = graph_yll_dates)

# YLL for a specific age and year 
export(ic95_yll_sp_yll_sp, here("results", "HIA", "IC95_yll_18_2050.xlsx"))
ggsave(here("results", "yll_18_2050.pdf"), plot = graph_yll_sp)

# LE for a specific age and year
export(ic95_le_sp, here("results", "HIA", "IC95_le_18_2050.xlsx"))
ggsave(here("results", "HIA", "LE_18_2050.pdf"), plot = graph_le_sp)

# One figure with graphs of prevented deaths, YLL, LE and avoided costs
ggsave(here("results","HIA", "HIA_dates.pdf"), plot = common_graph)
