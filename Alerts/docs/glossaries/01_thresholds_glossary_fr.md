# Glossaire — 01_thresholds_synthesis.rds (Français)

- zone_sante_notification : Nom de la zone de santé utilisée pour les notifications.
- Province : Province contenant la zone de santé.
- Population : Population projetée de la zone de santé (DHIS2 2024 projetée à l'année en cours).
- expected_weekly_deaths_cmr : Nombre attendu de décès communautaires par semaine calculé à partir du taux de mortalité brut (CMR).
- death_threshold_lower_A : Seuil inférieur de décès — Approche A (mortalité de base ; 0.9 × décès attendus dans l'implémentation).
- death_threshold_upper_A : Seuil supérieur de décès — Approche A (1.1 × décès attendus).
- alert_case_threshold_lower_B : Seuil inférieur d'alertes cas — Approche B (benchmark historique Beni, redimensionné à la population de la ZS).
- alert_case_threshold_upper_B : Seuil supérieur d'alertes cas — Approche B.
- alert_death_threshold_lower_B : Seuil inférieur d'alertes décès — Approche B.
- alert_death_threshold_upper_B : Seuil supérieur d'alertes décès — Approche B.
- threshold_time_key : Identifiant de la fenêtre de seuil (date d'ancrage ou début de fenêtre utilisée pour la synthèse).
- week_start : Date de début de la fenêtre de seuil (7 jours).
- recent_case_window_start : Date de début de la fenêtre récente de cas utilisée pour les estimations dérivées des cas.
- recent_case_window_end : Date de fin de cette fenêtre récente.
- recent_case_window_days : Durée de la fenêtre en jours.
- recent_case_anchor_date : Date d'ancrage pour la fenêtre récente.
- alert_case_threshold_C : Seuil central d'alertes cas — Approche C (attentes dérivées des cas) ; égal à beta_c multiplié par estimated_true_cases_recent.
- alert_case_threshold_lower_C : Seuil inférieur d'alertes cas — Approche C ; égal à beta_c_low multiplié par estimated_true_cases_recent.
- alert_case_threshold_upper_C : Seuil supérieur d'alertes cas — Approche C ; égal à beta_c_high multiplié par estimated_true_cases_recent.
- alert_death_threshold_C : Seuil central d'alertes décès — Approche C ; égal à beta_d multiplié par expected_deaths.
- alert_death_threshold_lower_C : Seuil inférieur d'alertes décès — Approche C ; égal à beta_d_low multiplié par expected_deaths.
- alert_death_threshold_upper_C : Seuil supérieur d'alertes décès — Approche C ; égal à beta_d_high multiplié par expected_deaths.
- Alert_case_lower : Borne inférieure synthétisée pour les alertes cas (moyenne des bornes B et C dans le code implémenté).
- Alert_case_upper : Borne supérieure synthétisée pour les alertes cas.
- Alert_death_lower : Borne inférieure synthétisée pour les alertes décès (moyenne des bornes A, B et C).
- Alert_death_upper : Borne supérieure synthétisée pour les alertes décès.
- Alert_case_threshold : Seuil central final pour les alertes cas (point médian entre Alert_case_lower et Alert_case_upper).
- Alert_death_threshold : Seuil central final pour les alertes décès (point médian entre Alert_death_lower et Alert_death_upper).

# Glossaire — Paramètres intermédiaires (01_intermediate_parameters.rds, Français)

Les colonnes suivantes apparaissent dans la composante dérivée des cas de `01_intermediate_parameters.rds` (`case_derived_params`) et dans la table des multiplicateurs.

- estimated_true_cases_recent : Nombre estimé de cas incidents réels dans la fenêtre récente de 7 jours, calculé en divisant les cas confirmés détectés par le taux de détection combiné.
- estimated_true_cases_global : Nombre estimé de cas incidents réels sur toute la période d'analyse, calculé en divisant les cas confirmés cumulés détectés par le taux de détection combiné.
- beta_c : Multiplicateur de décalage de Poisson pour les alertes de cas (vivants) ; exp(intercept) du GLM ajustant les alertes validées vivantes sur les cas réels estimés.
- beta_c_low : Borne inférieure de confiance à 95 % pour beta_c (corrigée de la dispersion ou Poisson exact).
- beta_c_high : Borne supérieure de confiance à 95 % pour beta_c.
- beta_d : Multiplicateur de décalage de Poisson pour les alertes de décès ; exp(intercept) du GLM ajustant les alertes validées décès sur les décès attendus.
- beta_d_low : Borne inférieure de confiance à 95 % pour beta_d.
- beta_d_high : Borne supérieure de confiance à 95 % pour beta_d.
- expected_deaths : Décès attendus dans la fenêtre récente, calculés comme estimated_true_cases_recent multiplié par le CFR (direct).
- expected_secondary : Cas secondaires attendus, calculés comme estimated_true_cases_recent multiplié par contacts_per_case_used et SAR. Puisque SAR = Rt / contacts_per_case_used, cela équivaut à estimated_true_cases_recent multiplié par Rt.
- detection_rate_adj : Taux de détection combiné ; moyenne arithmétique des estimations de détection valides par liens épidémiologiques et par rétro-calcul du CFR (0 < taux <= 1).
- under_detection_rate : Un moins detection_rate_adj.
- detection_rate_source_names : Quelles méthodes de détection ont contribué ("backcalc", "epilink", ou "backcalc,epilink").
- detection_rate_combined_status : Comment le taux combiné a été dérivé ("averaged", "single_source", ou "no_valid_detection_rate").
- cfr_used : CFR utilisé pour les calculs de décès attendus ; spécifique à la ZS lorsque >= 5 cas confirmés, sinon CFR groupé (toutes ZS).
- cfr_backcalc_used : CFR rétro-calculé (décès éligibles / (confirmés + décès éligibles non confirmés)) ; utilisé dans la méthode de détection par rétro-calcul.
- r_used : Taux de croissance exponentiel utilisé dans le terme d'ajustement du délai ; estimation spécifique à la ZS ou repli groupé.
- sar : Taux d'attaque secondaire, calculé comme Rt / contacts_per_case_used ; par défaut 0,1 lorsque manquant.
- contacts_per_case_used : Contacts par cas confirmé utilisés dans la composante dérivée des cas ; spécifique à la ZS lorsque la ZS a au moins 5 cas confirmés dans la fenêtre et un ratio positif, sinon le ratio groupé global estimé à partir des données nettoyées de suivi des contacts.
- pooled_beta_c : Multiplicateur d'alertes de cas groupé (toutes ZS), calculé comme le total des alertes validées vivantes / total des cas réels estimés ; substitué lorsque beta_c spécifique à la ZS est manquant.
- pooled_beta_d : Multiplicateur d'alertes de décès groupé (toutes ZS), calculé comme le total des alertes validées décès / total des décès attendus ; substitué lorsque beta_d spécifique à la ZS est manquant.
- baseline_alive_alerts : Comptage d'alertes de cas de référence Beni mis à l'échelle de la population de la ZS (Approche B). Antérieurement utilisé comme plancher non nul dans les seuils de cas de l'Approche C ; désormais conservé comme valeur de référence uniquement (n'est plus ajouté dans les formules de l'Approche C).
- baseline_death_alerts : Comptage d'alertes de décès de référence Beni mis à l'échelle de la population de la ZS (Approche B). Antérieurement utilisé comme plancher non nul dans les seuils de décès de l'Approche C ; désormais conservé comme valeur de référence uniquement (n'est plus ajouté dans les formules de l'Approche C).

# Glossaire — Contacts par cas (01_intermediate_parameters.rds, français)

L'élément `contacts_per_case` de `01_intermediate_parameters.rds` contient les estimations par ZS issues des données nettoyées de suivi des contacts :

- n_contacts : Contacts enrôlés dans la fenêtre d'analyse.
- n_followed_contacts : Contacts enrôlés comptés comme suivis (au moins une visite de suivi enregistrée, pas antérieure à 30 jours avant l'enrôlement).
- n_confirmed_cases : Cas confirmés avec date d'apparition des symptômes dans la fenêtre d'analyse.
- contacts_per_case_hz : Ratio spécifique à la ZS (n_followed_contacts / n_confirmed_cases).
- contacts_per_case_pooled : Ratio groupé global toutes ZS (total des contacts suivis / total des cas confirmés).
- contacts_per_case_used : Ratio effectivement utilisé ; spécifique à la ZS lorsque la ZS a au moins 5 cas confirmés et un ratio positif, sinon le ratio groupé.
- source : Indique si le ratio utilisé provient de la ZS (« hz ») ou de l'estimation groupée (« pooled »).
