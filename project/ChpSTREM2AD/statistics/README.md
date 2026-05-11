# ChpSTREM2AD Statistics README

## 2026-04-14 Update

- `run_all.R` creates a dedicated result folder for every execution:
  `project/ChpSTREM2AD/result/YYYYMMDD_HHMMSS/`
- Each version folder contains `data_clean/`, `summary/`, `tables/`, `figures/`, and `report/`.
- The integrated Markdown report is saved to:
  `result/YYYYMMDD_HHMMSS/report/ChpSTREM2AD_analysis_report.md`
- Report language is controlled by `statistics/local.json`, with Chinese as the default.
- Shared defaults are loaded from `common/config/defaults.json`, and shared translation strings are loaded from `common/config/locale.json`.
- Project-level `local.json` can override common settings without editing R scripts.
- Overall and group-wise descriptive statistics are exported into `summary/` and written into the report.
- Formal SEM now runs one cognition definition:
  `MMSE + MoCA`.
- Candidate cognition-model comparison is exported separately for:
  `MMSE + MoCA`, `mPACC + NegaADAS13`, `MMSE + MoCA + mPACC`, and `mPACC`.
- SEM uses latent-variable `lavaan` models.
- SEM main models now default to `indicator_standardization = FALSE`;
  standardization is retained only for sensitivity analysis.
- SEM diagnostics export:
  `sem_model_fit.csv`, `sem_factor_loadings.csv`, `sem_modification_indices_top.csv`,
  `sem_standardization_sensitivity.csv`, `sem_indirect_difference_tests.csv`,
  `sem_multigroup_model_fit.csv`, `sem_multigroup_group_estimates.csv`,
  `sem_multigroup_path_tests.csv`, `sem_parallel_mediation_summary.csv`,
  `sem_parallel_model_fit.csv`, `sem_parallel_multigroup_model_fit.csv`,
  `sem_parallel_multigroup_group_estimates.csv`, `sem_parallel_multigroup_tests.csv`,
  `sem_moderated_mediation_summary.csv`, and `sem_mechanism_interpretation.csv`.
- Reviewer-oriented extended models export:
  `advanced_biomarker_adjusted_*.csv`, `advanced_interaction_*.csv`,
  and `advanced_nonlinear_*.csv`.
- `result/LATEST.txt` records the newest run id and path.

## Entry Files

- Main configuration: `project_config.R`
- Project display and language config: `local.json`
- Full pipeline runner: `run_all.R`

## Formal SEM Cognition Model

1. `Cog_MMSE_MOCA`
   - `MMSE`
   - `MOCA`

## Candidate Cognition Models For Fit Comparison

1. `Cog_MMSE_MOCA`
   - `MMSE`
   - `MOCA`
2. `Cog_mPACC_NegaADAS13`
   - `mPACCdigit`
   - `mPACCtrailsB`
   - `NegaADAS13`
3. `Cog_MMSE_MOCA_mPACC`
   - `MMSE`
   - `MOCA`
   - `mPACCdigit`
   - `mPACCtrailsB`
4. `Cog_mPACC`
   - `mPACCdigit`
   - `mPACCtrailsB`

## Main Output Files

- `data/clean/ChpSTREM2AD_selected_dataset.csv`
- `data/clean/ChpSTREM2AD_analysis_dataset.csv`
- `result/summary/sample_flow_summary.csv`
- `result/summary/sample_platform_summary.csv`
- `result/summary/visit_alignment_summary.csv`
- `result/summary/sample_missingness_summary.csv`
- `result/summary/analysis_n_summary.csv`
- `result/summary/normality_screening.csv`
- `result/summary/group_comparisons_*.csv`
- `result/summary/chp_strem2_*.csv`
- `result/summary/tau_*.csv`
- `result/summary/ptau_*.csv`
- `result/summary/advanced_*.csv`
- `result/summary/sem_*.csv`
- `result/summary/sem_multigroup_*.csv`
- `result/summary/sem_parallel_*.csv`
- `result/summary/sem_moderated_mediation_summary.csv`
- `result/summary/cognition_model_selection_fit.csv`
- `result/report/Cognition_model_selection.md`
- `result/report/ChpSTREM2AD_analysis_report.md`
- `result/report/SEM_stage_difference_figures.md`
- `result/report/Figures_5_to_10_talking_points.md`
- `result/figures/sem_*.png`
- `result/figures/Figure_7_single_mediator_stage_indirect_forest.png`
- `result/figures/Figure_8_single_mediator_moderated_mediation_heatmap.png`
- `result/figures/Figure_9_parallel_mediator_stage_heatmap.png`
- `result/figures/Figure_10_parallel_mediator_moderated_mediation_heatmap.png`

## Biomarker Association Modules

- `02c_sample_flow_missingness_alignment.R`
  - sample-flow summary, ADNI phase/site/visit/T1 sequence provenance, visit-alignment interval summary,
    variable missingness table, and analysis-level sample-size/FIML boundary summary

- `05_chp_strem2_analysis.R`
  - adjusted linear models and partial correlations for `ChP/ICV` and `sTREM2`
- `06_tau_analysis.R`
  - adjusted linear models and partial correlations for `Tau` with `ChP/ICV` and `sTREM2`
- `06_ptau_analysis.R`
  - adjusted linear models and partial correlations for `P-Tau` with `ChP/ICV` and `sTREM2`
- `06b_advanced_biomarker_models.R`
  - reviewer-oriented models including `Tau/Aβ` and `P-Tau/Aβ` adjustment, interaction, and nonlinearity
- `06c_abeta_truncation_sensitivity.R`
  - sensitivity analysis for ADNI CSF `Aβ` ceiling truncation at `1700 pg/mL`
  - exports full-sample vs ceiling-excluded regression and partial-correlation comparisons
- `06d_robustness_extensions.R`
  - FDR adjustment manifest, matched-model sensitivity, alternative `ChP` definitions,
    structure-specificity models, and collinearity diagnostics
- `06e_overlap_weighting_sensitivity.R`
  - generalized overlap-weighting sensitivity analysis for diagnosis imbalance
  - exports balance diagnostics and weighted-versus-primary model comparisons
- `06f_diagnosis_biologic_sensitivity.R`
  - diagnosis-hierarchical models for `ChP/ICV` with `sTREM2`, `Tau`, `P-Tau`, and `Aβ`
  - biologically defined sensitivity analyses using stable amyloid-status subsets derived from `ABETA_convert`
  - exports diagnosis-adjusted comparisons, biologic subset counts, and biologic subset models
- `06g_phase_site_protocol_sensitivity.R`
  - sensitivity analysis additionally adjusting the core `ChP/ICV` models for ADNI phase, grouped site,
    and grouped T1 MRI protocol family
  - exports phase/site/protocol-adjusted estimates, metadata counts, and provenance summaries

## SEM Extension Modules

- `07_sem_mediation.R`
  - single-mediator latent SEM
  - stage-specific configural multigroup SEM and formal indirect-effect difference tests
  - parallel-mediator latent SEM with `sTREM2 + P-Tau + Aβ` and `sTREM2 + Tau + Aβ`
  - moderated-mediation summary export for both single-mediator and parallel-mediator models
- `07b_serial_sem_followup.R`
  - serial mediation follow-up for `PTAU/TAU -> sTREM2 -> ChP/ICV -> cognition`
  - suppression-coverage quantification and serial multigroup tests
- `07c_measurement_invariance.R`
  - measurement invariance checks for the formal cognition latent variable across `CN/MCI/AD`
- `08c_generate_sem_difference_figures.R`
  - stage-specific forest plot and moderated-mediation heatmaps for teaching and supervisor presentation

## Additional Robustness Outputs

- `result/summary/diagnosis_hierarchical_models.csv`
  - full-sample primary-adjusted, diagnosis-adjusted, and within-group estimates
- `result/summary/diagnosis_hierarchical_model_comparison.csv`
  - model comparison statistics and adjusted R-squared changes after adding diagnosis
- `result/summary/biologic_sensitivity_models.csv`
  - subset-specific models in stable amyloid-negative and stable amyloid-positive groups
- `result/summary/biologic_sensitivity_counts.csv`
  - sample counts for biologically defined sensitivity-analysis subsets
- `result/summary/sample_source_summary.csv`
  - source metadata summary after merging ADNI platform/visit fields from `Data_all.csv`
- `result/summary/phase_site_protocol_sensitivity.csv`
  - core `ChP/ICV` association models before and after adjustment for ADNI phase, grouped site,
    and grouped T1 MRI protocol family
- `result/summary/phase_site_protocol_counts.csv`
  - counts for ADNI phase, grouped site, and grouped T1 MRI protocol families used in the sensitivity analysis
- `result/summary/phase_site_protocol_summary.csv`
  - metadata merge audit and retained sample counts for phase/site/protocol sensitivity analyses
- `result/summary/stage_pairwise_key_variables.csv`
  - key stage-pairwise comparisons for age, sex, ChP/ICV, sTREM2, Aβ, Tau, and P-Tau
- `result/tables/stage_pairwise_key_variables_three_line.png`
  - supplementary three-line table for the above pairwise stage comparisons
- `result/figures/stage_group_distribution_panel.png`
  - faceted violin/boxplot panel visualizing key variable distributions across CN, MCI, and dementia
