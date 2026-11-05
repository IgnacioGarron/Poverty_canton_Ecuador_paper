# Repeated spatial cross-validation

This analysis checks whether the estimated incremental value of mobility data
depends on a particular spatial partition. It estimates only the leading Full
XGBoost specification for three aligned scenarios:

1. 947 administrative and environmental predictors;
2. the same predictors plus the mobility factor estimated inside each fold;
3. the same predictors plus the 38 raw mobility indicators.

Within every repetition, all three scenarios use the same outer assessment
cantons. Preprocessing, the mobility PCA, and Bayesian hyperparameter tuning are
re-estimated using only the corresponding outer training sample.

## Full run

Run from the project root or from `new code/`:

```sh
REPEATED_CV_REPEATS=10 \
PRACTICAL_EQUIVALENCE_MARGIN_PP=0.50 \
Rscript "new code/cv_partitioned/run.R"
```

If the current directory is `new code/`, use:

```sh
REPEATED_CV_REPEATS=10 \
PRACTICAL_EQUIVALENCE_MARGIN_PP=0.50 \
Rscript "cv_partitioned/run.R"
```

The `0.50`-percentage-point practical margin is a provisional default. It
should be justified substantively and changed before the final run if another
threshold is more appropriate. Do not select the margin by looking at the
repeated-CV results.

The run is resumable. Existing complete OOF prediction files are reused unless
`REPEATED_CV_OVERWRITE=true` is set.

## Lightweight validation

This checks the three datasets, their alignment, spatial-fold construction, and
workflow definitions without fitting any model:

```sh
REPEATED_CV_VALIDATE_ONLY=true \
REPEATED_CV_OUTPUT_ROOT=/tmp/poverty-cv-partitioned-check \
Rscript "new code/cv_partitioned/run.R"
```

## Main options

| Variable | Default | Purpose |
|---|---:|---|
| `REPEATED_CV_REPEATS` | 10 | Number of alternative spatial partitions |
| `REPEATED_CV_BASE_SEED` | 123 | Seed used for the first partition |
| `REPEATED_CV_SEED_STRIDE` | 10000 | Seed increment between repetitions |
| `PRACTICAL_EQUIVALENCE_MARGIN_PP` | 0.50 | Practical RMSE margin in percentage points |
| `REPEATED_CV_OVERWRITE` | false | Refit repetitions with saved OOF predictions |
| `REPEATED_CV_VALIDATE_ONLY` | false | Validate inputs and workflows without fitting |
| `REPEATED_CV_OUTPUT_ROOT` | `output_tidymodels/cv_partitioned` | Optional output override |
| `SPATIAL_OUTER_V` | 5 | Outer spatial folds |
| `SPATIAL_INNER_V` | 5 | Inner spatial folds used for tuning |
| `BAYES_INITIAL` | 15 | Initial Bayesian-tuning evaluations |
| `BAYES_ITER` | 25 | Additional Bayesian-tuning iterations |
| `XGB_THREADS` | 1 | XGBoost threads per fit |

## Outputs

The default output directory is `output_tidymodels/cv_partitioned/`:

- `repeats/`: OOF predictions and tuning histories for every scenario and
  spatial partition;
- `results/paired_canton_losses.csv`: paired squared- and absolute-loss
  differences for every canton and repetition;
- `results/paired_repeat_metrics.csv`: paired RMSE, MSE, and MAE differences by
  repetition;
- `results/paired_fold_metrics.csv`: the same comparison by repetition and
  outer fold;
- `results/paired_summary.csv`: across-partition summary and practical-margin
  assessment;
- `figures/paired_delta_rmse.pdf`: distribution of paired RMSE differences;
- `tables/repeated_spatial_cv.tex`: appendix results table;
- `tables/repeated_spatial_cv_appendix.tex`: ready-to-input appendix subsection.

After the full run, include the generated appendix with:

```latex
\input{output_tidymodels/cv_partitioned/tables/repeated_spatial_cv_appendix.tex}
```

The empirical 2.5th--97.5th percentile interval across repetitions measures
sensitivity to alternative spatial partitions. It is not a conventional
population-sampling confidence interval because all repetitions reuse the same
218 cantons.
