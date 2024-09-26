# Fichiers de code R

1. Population_data_management : Mise en forme des données de population de l'INSEE de sorte à ce que les fichiers d'effectifs et de décès aient les mêmes variables et les mêmes catégories d'âge

2. Mortality rates : calcul des taux de mortalité projetés par âge et par année, à partir des données de l'INSEE

3. Diet_changes : Niveaux de consommation de chaque aliment, pour chaque année, dans chaque scénario, à partir des niveau de consommation actuels, jusqu'aux niveaux de consommation décrits dans les scénarios en 2050. Plusieurs dynamiques d'implémentation des régimes sont testées : linéaire, par interpolation cosinus et sigmoïdale.

4. RR_evolution : Calcul de la valeur du risque relatif (RR) associé au niveau de consommation de chaque aliment, chaque année, dans chaque scénario. Ce calcul est fait à partir des relations dose-réponse log-linéaires issues des méta-analyses citées par Fadnes et al., en  2022 et 2024 (relations du type RR = x pour une consommation de y g/j/pers). Les RR de chaque aliment, sont multipliés entre eux pour chaque année, de sorte à obtenir la valeur du RR de chaque régime entier pour chaque année et chaque scénario

5. EQIS_Nutrition : Evaluation de l'impact sanitaire de chaque régime, sur toute la période d'étude du modèle. Cela est fait par ajustement des taux de mortalités avec les risques relatifs des régimes. On calcule alors un nombre de décès évités (total, par âge et par année), par rapport à un scénario de référence (actuel ou tendanciel).

6. Life_expectancy : À DÉVELOPPER. Calcul des années de vie gagnées/perdues.

7. Time to full effect : Exploration de différentes dynamiques de time to full effect

8. EQIS_Specific_date : Evaluation d'impact sanitaire de chaque régime, une année spécifique, en particulier les années 2035 et 2050.

9. RR_evolution_2 : Calcul de la valeur du risque relatif (RR) associé au niveau de consommation de chaque aliment, chaque année, dans chaque scénario. Ce calcul est fait à partir des valeurs du tableau des RR de Fadnes et al., 2022 et 2024. On prend donc ici en compte des relations dose-réponse non monotones. Interpolation linéaire entre les bornes renseignées, de sorte à reproduire une courbe dose-réponse (avec IC95), pour chaque g d'aliment consommé.

10. RR_evolution_TTFE : Les valeurs de RR de RR_evolution_2 sont modifiées pour prendre en compte un time to full effect.

11. Visualization : Outil de visualisation des sorties du modèle DOUGLAS, permettant de changer rapidement les paramètres du modèle (bornes temporelles, borne inférieur de l'âge de la population d'intérêt, dynamique d'implémentation des régimes, dynamique de time to full effect,...)

