# Output file locations

All paths are relative to the repository root. In figure filenames, `{panel}`
stands for `a`, `b`, `c`, or `d`.

## Manuscript and supporting files

| File | Location |
|---|---|
| Main manuscript | `paper/Doc100426.tex` |
| Online Resource 1 | `paper/ESM_1.tex` |
| Online Resource 2 | `paper/ESM_2.tex` |
| Original online appendix | `paper/Online_Appendix.tex` |
| Bibliography | `paper/ref.bib` |
| Document class | `paper/sn-jnl.cls` |
| Bibliography style | `paper/sn-apacite.bst` |

## Figures

| Figure | Location |
|---|---|
| 1 — UBN poverty maps | `paper/Fig1.pdf` |
| 2 — Mobile-phone mobility routes | `paper/Fig2.png` |
| 3 — Spatial cross-validation folds | `paper/Fig3.png` |
| 4 — Out-of-sample RMSE and signed error | `paper/Fig4{panel}_rmse.png` and `paper/Fig4{panel}_error.png` |
| 5 — Conditional prediction error | `paper/Fig5{panel}.png` |
| 6 — Calendar prediction performance | `paper/Fig6.png` |
| 7 — Calendar conditional prediction error | `paper/Fig7.png` |
| 8 — Mobility correlations | `paper/Fig8.png` |
| 9 — Mobility scree plot | `paper/Fig9.png` |
| 10 — SHAP variable importance | `paper/Fig10{panel}.png` |
| Original online appendix, Figure 1 | `paper/Figures/cor_mean.png` |

## Tables

| Table | Location |
|---|---|
| Main Table 1 — Data scenarios | Within `paper/Doc100426.tex`, label `tab:data` |
| Main Table 2 — Covariate designs | Within `paper/Doc100426.tex`, label `tab:designs_summary` |
| Main Table 3 — Publication calendar | Within `paper/Doc100426.tex`, label `tab:calendar` |
| Main Table 4 — Repeated spatial cross-validation | Within `paper/Doc100426.tex`, label `tab:repeated-spatial-cv` |
| Online Resource 1, Table S1 — Variable dictionary | Within `paper/ESM_1.tex`, label `tab:variable-complete` |
| Online Resource 2, Table S1 — Generalization gap | Within `paper/ESM_2.tex`, label `tab:xgboost_generalization_gap` |
| Online Resource 2, Table S2 — Hyperparameter tuning | Within `paper/ESM_2.tex`, label `tab:hyperparameter` |
| Original online appendix, Table 1 | `paper/Tables/table_variables.tex` |

## Analysis files

| Files | Location |
|---|---|
| R scripts and helper functions | `new code/` |
| Scenario estimates, predictions, and metrics | `output_tidymodels/{scenario}/` |
| Generated main-analysis figures | `output_tidymodels/figures/` |
| Generated variable dictionary | `output_tidymodels/tables/table_variables.tex` |
| Calendar estimates and tables | `output_tidymodels/calendar/` |
| Calendar figures | `output_tidymodels/calendar/figures/` |
| Repeated spatial-validation results | `output_tidymodels/cv_partitioned/` |

`{scenario}` is `baseline`, `baseline_mobile_mean`,
`baseline_mobile_pca_mean`, or `mobile_only_mean`.
