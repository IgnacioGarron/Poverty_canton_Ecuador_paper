# Replication package for “New Data for Old Deprivations: Can Mobile Phone Mobility Data Help Map Poverty in Ecuador?”

Gustavo Canavire-Bacarreza, Ignacio Garrón, Pablo A. Astudillo-Estévez,
Galo Egas Guayaquil, Edison Choque-Sanchez, and Veronica Coronel Loaiza.

Prepared: November 5, 2026. Main manuscript: `Doc100426.tex`, with its cited
Online Resources `ESM_1.tex` and `ESM_2.tex`.

This repository contains the existing R analysis code, manuscript sources, and
instructions for reproducing the analysis. It uses the existing estimation and plotting scripts; no separate runner is required for every figure or table.


## Data availability

Some data sources used in this project are not publicly available and therefore cannot be shared publicly. However, all codes are freely available to ensure the reproducibility of the analysis. In addition, the underlying data can be made available to referees for review purposes, provided that they agree not to share, distribute, or otherwise make the data publicly accessible, in accordance with the applicable data confidentiality restrictions.

For the request procedure, public source information, and exact local input
filenames, see [DATA_ACCESS.md](DATA_ACCESS.md).

## Repository contents

```text
replicaciont/
├── README.md
├── DATA_ACCESS.md               Data sources, access, and input inventory
├── OUTPUT_GUIDE.md              Figure/table-to-code correspondence
├── environment/
│   └── session_info_reference.txt
├── new code/                   Existing R scripts, helpers, and translations
│   ├── R/
│   ├── models/
│   ├── scenarios/
│   └── cv_partitioned/
├── input/                      Local authorized inputs; excluded from Git
├── output_tidymodels/           Local generated results; excluded from Git
└── paper/
    ├── Doc100426.tex
    ├── ESM_1.tex                Online Resource 1: variable dictionary
    ├── ESM_2.tex                Online Resource 2: validation and tuning
    ├── Online_Appendix.tex
    ├── ref.bib
    ├── sn-jnl.cls
    ├── sn-apacite.bst
    ├── Fig*.png / Fig1.pdf     23 referenced main-manuscript assets
    ├── Figures/cor_mean.png    Original online appendix figure
    └── Tables/table_variables.tex
```


## Software and computational requirements

The existing baseline run recorded R 4.5.1 on Apple ARM64, macOS Tahoe 26.3.1.
Its full package record is retained in
[environment/session_info_reference.txt](environment/session_info_reference.txt).
This is a record of a prior run, not a new verification or a dependency lockfile.

Packages versions are `tidymodels 1.5.0`, `tune 2.1.0`,
`recipes 1.3.3`, `parsnip 1.6.0`, `spatialsample 0.6.1`, `sf 1.0-21`,
`glmnet 4.1-10`, `xgboost 3.2.1.1`, and `mixOmics 6.34.0`.
The setup script installs missing dependencies, including `mixOmics` through
Bioconductor. Quantile XGBoost requires XGBoost >= 2.0.0.
`knitr` and `kableExtra` are additionally required for the online dictionary.

Running the scripts can be computational demanding depending on the characteristics of the computer. Nested spatial cross-validation is specially computationally intensive. Runtime and peak memory have not been benchmarked for this replication package. Building the paper additionally requires pdfLaTeX and BibTeX.

## Running the analysis

The commands below use a POSIX shell (macOS/Linux). On Windows, set the same
environment variables in your shell and invoke `Rscript` with the same script names.
Start at the repository root and enter the code directory. Run all subsequent
analysis commands from **`replicaciont/new code/`**.

### 1. Install dependencies and obtain inputs

```sh
cd "new code"
Rscript setup_dependencies.R
Rscript -e 'install.packages(c("knitr", "kableExtra"), repos = "https://cloud.r-project.org")'
Rscript download_Cantones_ArcGIS_2012.R
```

The final command downloads the public canton geometry into `../input/`.

### 2. Check inputs and estimate the four paper scenarios

```sh
Rscript run_all.R
```


| Script scenario | Information set |
|---|---|
| `baseline` | 947 administrative, socioeconomic, and environmental indicators |
| `baseline_mobile_mean` | Benchmark plus 38 mean mobility indicators |
| `baseline_mobile_pca_mean` | Benchmark plus the mobility component estimated within training folds |
| `mobile_only_mean` | 38 mean mobility indicators |

The sample contains 218 mainland cantons; the three Galápagos cantons are
excluded. Five outer and five inner spatial folds are used. Bayesian tuning
starts with 15 evaluations (increased when required by parameter dimension),
allows 25 additional iterations, and stops after 10 without improvement.
14 combinations are estimated per scenario.

Each scenario writes to `../output_tidymodels/<scenario>/`.

Saved fitted models in `output_tidymodels/<scenario>/models/`, including the
corresponding calendar and repeated-validation model folders, are omitted from
the repository because their files are too large to upload to Git. Regenerate
them locally by rerunning the relevant estimation scripts with the required
data before running steps that read fitted models, such as SHAP variable
importance and calendar construction. To regenerate saved calendar models,
run `CALENDAR_OVERWRITE=true Rscript main_calendar.R` from `new code/`.

For one scenario, use its existing entry point, for example
`Rscript scenarios/baseline/main.R`. Running `run_all.R` without the scenario
filter also estimates sum-mobility variants outside these manuscripts.

### 3. Generate the available figures and dictionary

```sh
Rscript cv_map.R
Rscript box_plot.R
Rscript distributional_analysis.R
Rscript variable_importance_XGboost.R
Rscript table_varables.R
```

Results go to `../output_tidymodels/figures/`
and `../output_tidymodels/tables/`; they do not automatically update `paper/`.

### 4. Estimate the publication-calendar exercise (Table 3 and Figures 6–7)

After completing the main scenarios and `variable_importance_XGboost.R`, obtain
the additional `calendario.xlsx`.

Then run, still from `new code/`:

```sh
Rscript main_calendar.R
Rscript distributional_calendar.R
```

Results are in located in `../output_tidymodels/calendar/`.


### 5. Repeat spatial validation for Table 4 (Appendix C)

```sh
script cv_partitioned/run.R
```

Results are saved in `../output_tidymodels/cv_partitioned/`.


