#!/usr/bin/env Rscript

# Distributional OOS performance under staggered data availability.
#
# The script compares population-weighted LOESS curves for the full-design
# XGBoost model with and without the mobility factor. The calendar currently
# contains Q1, Q2, and Q4 information sets. Because no predictors are released
# in Q3, the cumulative Q2 information set is displayed as Q3 and recorded as
# such in the exported plotting data.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
})

script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0L) {
    file_value <- sub("^--file=", "", file_arg[[1L]])
    file_value <- gsub("~\\+~", " ", file_value)
    return(normalizePath(file_value, mustWork = TRUE))
  }

  candidate <- file.path(getwd(), "new code", "distributional_calendar.R")
  if (file.exists(candidate)) {
    return(normalizePath(candidate, mustWork = TRUE))
  }

  candidate <- file.path(getwd(), "distributional_calendar.R")
  if (file.exists(candidate)) {
    return(normalizePath(candidate, mustWork = TRUE))
  }

  stop("Could not locate distributional_calendar.R.", call. = FALSE)
}

repo_root <- dirname(dirname(script_path()))
calendar_root <- file.path(repo_root, "output_tidymodels", "calendar")
population_path <- file.path(
  repo_root,
  "input", "Data", "INEC", "population_canton_2022.csv"
)
figure_dir <- file.path(calendar_root, "figures")

if (!file.exists(population_path)) {
  stop("Population file not found: ", population_path, call. = FALSE)
}

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

comparison_lookup <- tibble::tribble(
  ~comparison_dir, ~comparison,
  "without_pca", "Administrative data",
  "with_pca",    "Administrative data + mobility factor"
)

prediction_path <- function(comparison_dir, quarter) {
  file.path(
    calendar_root,
    comparison_dir,
    quarter,
    "predictions",
    "oof__xgboost__full.csv"
  )
}

has_complete_quarter <- function(quarter) {
  all(file.exists(vapply(
    comparison_lookup$comparison_dir,
    prediction_path,
    FUN.VALUE = character(1L),
    quarter = quarter
  )))
}

# Use genuine Q3 outputs if they are generated in the future. For the current
# calendar, Q3 has no new releases, so its cumulative information set is Q2.
q3_source <- if (has_complete_quarter("Q3")) {
  "Q3"
} else if (has_complete_quarter("Q2")) {
  warning(
    "No complete Q3 prediction set was found; using the cumulative Q2 ",
    "information set for the Q3 series.",
    call. = FALSE
  )
  "Q2"
} else {
  stop("Neither complete Q3 nor Q2 calendar predictions were found.", call. = FALSE)
}

quarter_lookup <- tibble::tribble(
  ~display_quarter, ~source_quarter,
  "Q1",            "Q1",
  "Q3",            q3_source,
  "Q4",            "Q4"
)

required_quarters <- unique(quarter_lookup$source_quarter)
missing_quarters <- required_quarters[!vapply(
  required_quarters,
  has_complete_quarter,
  FUN.VALUE = logical(1L)
)]
if (length(missing_quarters) > 0L) {
  stop(
    "Missing calendar prediction files for: ",
    paste(missing_quarters, collapse = ", "),
    call. = FALSE
  )
}

standardize_canton_code <- function(x) {
  x <- trimws(as.character(x))
  numeric_code <- suppressWarnings(as.integer(x))
  if (anyNA(numeric_code)) {
    stop("Canton codes must be numeric.", call. = FALSE)
  }
  sprintf("%04d", numeric_code)
}

population <- read_csv(population_path, show_col_types = FALSE) %>%
  transmute(
    CodCanton = standardize_canton_code(CodCanton),
    population_2022 = as.numeric(population_2022)
  )

if (anyDuplicated(population$CodCanton)) {
  stop("Population data contain duplicate canton codes.", call. = FALSE)
}
if (any(!is.finite(population$population_2022) | population$population_2022 <= 0)) {
  stop("Population weights must be finite and strictly positive.", call. = FALSE)
}

read_oof <- function(display_quarter, source_quarter, comparison_dir, comparison) {
  path <- prediction_path(comparison_dir, source_quarter)
  oof <- read_csv(path, show_col_types = FALSE)
  required_columns <- c("CodCanton", "Canton", "fold", "truth", "estimate")
  missing_columns <- setdiff(required_columns, names(oof))

  if (length(missing_columns) > 0L) {
    stop(
      "Missing columns in ", path, ": ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  oof <- oof %>%
    transmute(
      CodCanton = standardize_canton_code(CodCanton),
      Canton,
      fold,
      truth = as.numeric(truth),
      estimate = as.numeric(estimate)
    )

  if (anyDuplicated(oof$CodCanton)) {
    stop("OOF predictions contain duplicate cantons: ", path, call. = FALSE)
  }
  if (any(!is.finite(oof$truth) | !is.finite(oof$estimate))) {
    stop("OOF predictions contain non-finite values: ", path, call. = FALSE)
  }

  oof %>%
    left_join(population, by = "CodCanton") %>%
    mutate(
      display_quarter = display_quarter,
      source_quarter = source_quarter,
      comparison = comparison,
      truth_pp = 100 * truth,
      error_pp = 100 * (estimate - truth)
    ) %>%
    relocate(display_quarter, source_quarter, comparison)
}

calendar_oof <- quarter_lookup %>%
  tidyr::crossing(comparison_lookup) %>%
  rowwise() %>%
  mutate(data = list(read_oof(
    display_quarter,
    source_quarter,
    comparison_dir,
    comparison
  ))) %>%
  ungroup() %>%
  select(data) %>%
  tidyr::unnest(data)

if (anyNA(calendar_oof$population_2022)) {
  missing_codes <- unique(calendar_oof$CodCanton[is.na(calendar_oof$population_2022)])
  stop(
    "Population weights are missing for canton codes: ",
    paste(missing_codes, collapse = ", "),
    call. = FALSE
  )
}

reference_cantons <- sort(unique(calendar_oof$CodCanton))
group_checks <- calendar_oof %>%
  group_by(display_quarter, comparison) %>%
  summarise(
    n = n(),
    n_cantons = n_distinct(CodCanton),
    same_cantons = identical(sort(unique(CodCanton)), reference_cantons),
    .groups = "drop"
  )
if (any(group_checks$n != group_checks$n_cantons) || any(!group_checks$same_cantons)) {
  stop("Calendar panels do not contain the same one-row-per-canton OOF sample.", call. = FALSE)
}

truth_checks <- calendar_oof %>%
  group_by(display_quarter, CodCanton) %>%
  summarise(n_truth = n_distinct(truth), .groups = "drop")
if (any(truth_checks$n_truth != 1L)) {
  stop("Truth values differ between mobility comparisons.", call. = FALSE)
}

calendar_oof <- calendar_oof %>%
  mutate(
    display_quarter = factor(display_quarter, levels = c("Q1", "Q3", "Q4")),
    comparison = factor(
      comparison,
      levels = c("Administrative data", "Administrative data + mobility factor")
    )
  )

data_output <- file.path(figure_dir, "distributional_calendar_loess.csv")
write_csv(calendar_oof, data_output)

q3_caption <- if (q3_source == "Q2") {
  "Note: Q3 uses the cumulative Q2 information set because the calendar contains no Q3 releases."
} else {
  NULL
}

calendar_plot <- ggplot(
  calendar_oof,
  aes(
    x = truth_pp,
    y = error_pp,
    colour = display_quarter,
    linetype = comparison,
    weight = population_2022
  )
) +
  geom_hline(yintercept = 0, colour = "grey40", linetype = "dashed", linewidth = 0.45) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    span = 0.75,
    se = FALSE,
    linewidth = 0.9
  ) +
  scale_colour_manual(values = c(
    "Q1" = "#D55E00",
    "Q3" = "#0072B2",
    "Q4" = "#009E73"
  )) +
  scale_linetype_manual(values = c(
    "Administrative data" = "solid",
    "Administrative data + mobility factor" = "dashed"
  )) +
  scale_x_continuous(breaks = seq(0, 100, by = 20)) +
  scale_y_continuous(breaks = seq(-50, 50, by = 10)) +
  coord_cartesian(xlim = c(0, 100), ylim = c(-50, 50)) +
  labs(
    x = "True 2022 poverty rate (percentage points)",
    y = "OOS bias (pp)",
    colour = "Information set",
    linetype = "Specification",
    caption = ""
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.caption = element_text(hjust = 0, size = 9)
  ) +
  guides(
    colour = guide_legend(order = 1, nrow = 1),
    linetype = guide_legend(order = 2, nrow = 1)
  )

png_output <- file.path(figure_dir, "distributional_calendar_loess.png")
pdf_output <- file.path(figure_dir, "distributional_calendar_loess.pdf")

ggsave(png_output, calendar_plot, width = 8.5, height = 4, dpi = 300)
ggsave(pdf_output, calendar_plot, width = 8.5, height = 4, device = cairo_pdf)

message("Saved plotting data: ", data_output)
message("Saved PNG figure: ", png_output)
message("Saved PDF figure: ", pdf_output)
