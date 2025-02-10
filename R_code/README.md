# Fichiers de code R

1. Population : Mise en forme des données de population de l'INSEE de sorte à ce que les fichiers d'effectifs et de décès aient les mêmes variables et les mêmes catégories d'âge

2. Mortality rates : calcul des taux de mortalité projetés par âge et par année, à partir des données de l'INSEE

3. RR_distributions : Reproduction des courbes dose-réponse pour chaque aliment à partir des tableaux de valeurs de RR issues des méta-analyses citées par Fadnes et al., en  2022 et 2024. Simulation d'un certain nombre de courbes, reproduisant les intervalles de confiance des courbes dose-réponse.

4. DOUGLAS_prevented_deaths : Health Impact Assessment (HIA) menant au calcul des décès évités par le changement de régime d'une population dans un scénario par rapport à un scénario de référence (ici conserver le régime actuel)
   - Implémentation des régimes
   - Mise en relation avec les RR correspondants
   - Calcul des RR avec le time to full effect
   - Ajustement des taux de mortalité
   - Calcul des décès évités

5. DOUGLAS_LE : Utilise les résultats de DOUGLAS_prevented_deaths pour calculer le nombre d'années de vie préservées et l'espérance de vie conditionnelle gagnée par les changements de régime alimentaire

6. DOUGLAS_YLL_Costs : Utilise les résultats de DOUGLAS_LE pour calculer les coûts intangibles de santé associés aux années de vie préservées par les changements de régimes

7. DOUGLAS_Sensitivity_Analysis : Représentation graphique des résultats de l'HIA sur les décès évités en 2040, 2050 et 2060 selon différentes hypothèses sur les paramètres du modèle (implémentation des régimes, réduction de la valeur des RR, time to full effect)

