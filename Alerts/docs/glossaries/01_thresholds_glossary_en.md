# Glossary — 01_thresholds_synthesis.rds (English)

- zone_sante_notification: Health zone name used for notifications.
- Province: Province containing the health zone.
- Population: Projected population for the health zone (DHIS2 2024 projected to current year).
- expected_weekly_deaths_cmr: Expected number of community deaths per week computed from the crude mortality rate (CMR).
- death_threshold_lower_A: Lower death threshold from Approach A (baseline mortality; 0.9 × expected deaths in implemented code).
- death_threshold_upper_A: Upper death threshold from Approach A (baseline mortality; 1.1 × expected deaths).
- alert_case_threshold_lower_B: Lower case-alert benchmark from Approach B (Beni historical rates scaled to HZ population).
- alert_case_threshold_upper_B: Upper case-alert benchmark from Approach B.
- alert_death_threshold_lower_B: Lower death-alert benchmark from Approach B.
- alert_death_threshold_upper_B: Upper death-alert benchmark from Approach B.
- threshold_time_key: Identifier for the threshold window (anchor date or window start used by the synthesis).
- week_start: The start date of the threshold time window (7 days).
- recent_case_window_start: Start date of the recent case window used for case-derived estimates.
- recent_case_window_end: End date of the recent case window used for case-derived estimates.
- recent_case_window_days: Length in days of the recent window.
- recent_case_anchor_date: Anchor date for the recent-case window.
- alert_case_threshold_C: Central (point) case-alert threshold from Approach C (case-derived expectation); equals beta_c multiplied by estimated_true_cases_recent.
- alert_case_threshold_lower_C: Lower case-alert threshold from Approach C; equals beta_c_low multiplied by estimated_true_cases_recent.
- alert_case_threshold_upper_C: Upper case-alert threshold from Approach C; equals beta_c_high multiplied by estimated_true_cases_recent.
- alert_death_threshold_C: Central death-alert threshold from Approach C; equals beta_d multiplied by expected_deaths.
- alert_death_threshold_lower_C: Lower death-alert threshold from Approach C; equals beta_d_low multiplied by expected_deaths.
- alert_death_threshold_upper_C: Upper death-alert threshold from Approach C; equals beta_d_high multiplied by expected_deaths.
- Alert_case_lower: Final synthesis lower bound for case alerts (average of B and C lower inputs per implemented code).
- Alert_case_upper: Final synthesis upper bound for case alerts.
- Alert_death_lower: Final synthesis lower bound for death alerts (average of A, B and C lower inputs).
- Alert_death_upper: Final synthesis upper bound for death alerts.
- Alert_case_threshold: Final central (point) case-alert threshold (midpoint of Alert_case_lower and Alert_case_upper).
- Alert_death_threshold: Final central (point) death-alert threshold (midpoint of Alert_death_lower and Alert_death_upper).

# Glossary — Nowcast columns (05_nowcast_by_zone.rds and derived tables)

Counts with a `_nowcast` suffix are right-truncation-corrected estimates of
the true number of confirmed/positive cases: an EpiNow2 nowcast is fitted per
eligible health zone (shared onset -> lab-confirmation delay), and sparse
zones fall back to an empirical delay correction. `_low` / `_high` give the
90% credible interval of the correction.

- nowcast_median / nowcast_lower_90 / nowcast_upper_90: Posterior nowcast of
  confirmed cases by onset date (median and 90% credible interval).
- method: Nowcast method per zone ("epinow2" or "empirical_delay_correction").
- status: Fit status per zone ("fit_ok", "fit_error", "below_min_cases",
  "no_recent_cases" or "no_series").
- n_recent_confirmed_nowcast (+_low/_high): Nowcast-corrected confirmed cases
  in the recent-case window.
- recent_count_source: "nowcast" when the window overlaps the nowcast tail,
  otherwise "observed".
- growth_n_cases_nowcast: Nowcast-corrected case total used for the
  exponential growth fit of a rolling window.
- growth_count_source: "nowcast" when the growth window was corrected,
  otherwise "observed".
- n_detected_nowcast (+_low/_high): Nowcast-corrected cumulative detected
  cases used in the back-calculation detection method.
- detection_rate_cfr_backcalc_nowcast (+_low/_high): Detection rate computed
  with the nowcast-corrected detected-case numerator.
- n_confirmed_nowcast (+_low/_high): Nowcast-corrected confirmed cases in the
  CFR back-calculation table.
- n_confirmed_cases_nowcast (+_low/_high) / contacts_per_case_nowcast:
  Nowcast-corrected confirmed-case denominator and the resulting
  contacts-per-case ratio.
- cumulative_confirmed_cases_nowcast: Nowcast-corrected cumulative confirmed
  cases in the combined detection table.
- confirmed_cases_week_nowcast (+_low/_high): Nowcast-corrected confirmed
  cases per HZ-week used as exposure in the alert-multiplier models.
- case_count_source: "nowcast" when the week-level exposure was corrected,
  otherwise "observed".

# Glossary — Intermediate parameters (01_intermediate_parameters.rds, English)

The following columns appear in the case-derived component of `01_intermediate_parameters.rds` (`case_derived_params`) and the multiplier table.

- estimated_true_cases_recent: Estimated true incident cases in the recent 7-day window, computed as detected confirmed cases divided by the combined detection rate.
- estimated_true_cases_global: Estimated true incident cases across the entire analysis period, computed as cumulative detected cases divided by the combined detection rate.
- beta_c: Poisson offset multiplier for case (alive) alerts; exp(intercept) from the case-alert GLM fitting validated alive alerts against estimated true cases.
- beta_c_low: Lower 95% confidence bound for beta_c (dispersion-corrected or exact Poisson).
- beta_c_high: Upper 95% confidence bound for beta_c.
- beta_d: Poisson offset multiplier for death alerts; exp(intercept) from the death-alert GLM fitting validated dead alerts against expected deaths.
- beta_d_low: Lower 95% confidence bound for beta_d.
- beta_d_high: Upper 95% confidence bound for beta_d.
- expected_deaths: Expected deaths in the recent window, computed as estimated_true_cases_recent multiplied by the (direct) CFR.
- expected_secondary: Expected secondary cases, computed as estimated_true_cases_recent multiplied by contacts_per_case_used and SAR. Since SAR = Rt / contacts_per_case_used, this equals estimated_true_cases_recent multiplied by Rt.
- detection_rate_adj: Combined detection rate; arithmetic mean of valid epi-link and back-CFR detection estimates (0 < rate <= 1).
- under_detection_rate: One minus detection_rate_adj.
- detection_rate_source_names: Which detection methods contributed ("backcalc", "epilink", or "backcalc,epilink").
- detection_rate_combined_status: How the combined rate was derived ("averaged", "single_source", or "no_valid_detection_rate").
- cfr_used: CFR used for expected-death calculations; HZ-specific when >= 5 confirmed cases, otherwise the pooled (all-HZ) CFR.
- cfr_backcalc_used: Back-calculated CFR (eligible deaths / (confirmed + eligible unconfirmed deaths)); used in the back-calculation detection method.
- r_used: Exponential growth rate used in the delay-adjustment term; HZ-specific estimate or pooled fallback.
- sar: Secondary attack rate, computed as Rt / contacts_per_case_used; defaults to 0.1 when missing.
- contacts_per_case_used: Contacts per confirmed case used in the case-derived component; HZ-specific when the HZ has at least 5 confirmed cases in the window and a positive ratio, otherwise the overall pooled ratio estimated from the cleaned contact follow-up data.
- pooled_beta_c: Pooled (all-HZ) case-alert multiplier, computed as total validated alive alerts / total estimated true cases; substituted when HZ-specific beta_c is missing.
- pooled_beta_d: Pooled (all-HZ) death-alert multiplier, computed as total validated dead alerts / total expected deaths; substituted when HZ-specific beta_d is missing.
- baseline_alive_alerts: Beni benchmark case-alert count scaled to HZ population (Approach B). Previously used as a non-zero floor in Approach C case thresholds; now retained as a reference value only (no longer added to Approach C formulas).
- baseline_death_alerts: Beni benchmark death-alert count scaled to HZ population (Approach B). Previously used as a non-zero floor in Approach C death thresholds; now retained as a reference value only (no longer added to Approach C formulas).

# Glossary — Contacts per case (01_intermediate_parameters.rds, English)

The `contacts_per_case` element of `01_intermediate_parameters.rds` holds the per-HZ estimates from the cleaned contact follow-up data:

- n_contacts: Contacts enrolled within the analysis window.
- n_followed_contacts: Enrolled contacts counted as followed (any follow-up visit recorded, not earlier than 30 days before enrollment).
- n_confirmed_cases: Confirmed cases with symptom onset within the analysis window.
- contacts_per_case_hz: HZ-specific ratio (n_followed_contacts / n_confirmed_cases).
- contacts_per_case_pooled: Overall pooled ratio across all HZs (total followed contacts / total confirmed cases).
- contacts_per_case_used: Ratio actually used; HZ-specific when the HZ has at least 5 confirmed cases and a positive ratio, otherwise the pooled ratio.
- source: Whether the used ratio comes from the HZ ("hz") or the pooled estimate ("pooled").
