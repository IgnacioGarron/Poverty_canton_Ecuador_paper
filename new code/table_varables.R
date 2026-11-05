###############################################################
## Variable provenance table for the paper appendix
##
## Creates a landscape LaTeX longtable of the areas, sources, and
## indicator names covering the 947 administrative and environmental
## covariates plus the 38 mobility indicators used in the baseline +
## mobile-mean analysis.
###############################################################

required_packages <- c("readxl", "dplyr", "knitr", "kableExtra")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]

if (length(missing_packages) > 0L) {
  stop(
    "Install the following packages before running this script: ",
    paste(missing_packages, collapse = ", ")
  )
}

find_project_root <- function(start_directory = getwd()) {
  current_directory <- normalizePath(start_directory, winslash = "/", mustWork = TRUE)
  dictionary_relative_path <- file.path(
    "input", "Data", "Covariables_Actualizadas_2010_enviar",
    "diccionario_covariables_all.xlsx"
  )

  repeat {
    if (file.exists(file.path(current_directory, dictionary_relative_path))) {
      return(current_directory)
    }

    parent_directory <- dirname(current_directory)
    if (identical(parent_directory, current_directory)) {
      stop("Could not locate the project root from: ", start_directory)
    }
    current_directory <- parent_directory
  }
}

project_root <- find_project_root()

dictionary_path <- file.path(
  project_root, "input", "Data", "Covariables_Actualizadas_2010_enviar",
  "diccionario_covariables_all.xlsx"
)
administrative_map_path <- file.path(
  project_root, "output_tidymodels", "baseline", "variable_name_map.csv"
)
mobility_map_path <- file.path(
  project_root, "output_tidymodels", "mobile_only_mean", "variable_name_map.csv"
)
english_names_path <- file.path(
  project_root, "input", "Data", "Covariables_Actualizadas_2010_enviar",
  "top_var_english.xlsx"
)
description_translations_path <- file.path(
  project_root, "new code", "indicator_descriptions_en.tsv"
)
output_directory <- file.path(project_root, "output_tidymodels", "tables")
output_path <- file.path(output_directory, "table_variables.tex")

input_paths <- c(
  dictionary_path,
  administrative_map_path,
  mobility_map_path,
  english_names_path,
  description_translations_path
)
missing_inputs <- input_paths[!file.exists(input_paths)]
if (length(missing_inputs) > 0L) {
  stop("Missing required input file(s):\n", paste(missing_inputs, collapse = "\n"))
}

dictionary_raw <- readxl::read_excel(dictionary_path)
required_dictionary_columns <- c(
  "Area", "operacionestadistica", "Fuente", "Indicador", "Descripcion"
)
missing_dictionary_columns <- setdiff(required_dictionary_columns, names(dictionary_raw))

if (length(missing_dictionary_columns) > 0L) {
  stop(
    "The variable dictionary is missing required column(s): ",
    paste(missing_dictionary_columns, collapse = ", ")
  )
}

dictionary <- dictionary_raw |>
  dplyr::transmute(
    Area = as.character(.data$Area),
    Operation = as.character(.data$operacionestadistica),
    Source = as.character(.data$Fuente),
    Indicator = as.character(.data$Indicador),
    Description = trimws(as.character(.data$Descripcion))
  )

if (anyDuplicated(dictionary$Indicator)) {
  duplicate_indicators <- unique(dictionary$Indicator[duplicated(dictionary$Indicator)])
  stop(
    "The dictionary contains duplicated indicator names: ",
    paste(duplicate_indicators, collapse = ", ")
  )
}

# These nine variables are part of the 947-variable baseline but are created
# in data_processing_2022.R and are therefore absent from the Excel dictionary.
environmental_dictionary <- dplyr::tibble(
  Area = rep("Climate change and deforestation", 9L),
  Operation = c(
    rep("Climate indicators", 3L),
    rep("Deforestation indicators", 6L)
  ),
  Source = c(
    rep("Clima_Ecuador_canton.dta", 3L),
    rep("Defo_Ecuador_canton.dta", 6L)
  ),
  Indicator = c(
    "sd_precipitacion", "max_temperature", "min_temperature",
    "arealoss", "areagain", "areatreecover", "gas_net",
    "gas_emissions", "area"
  )
)

dictionary_complete <- dplyr::bind_rows(
  dictionary,
  environmental_dictionary |>
    dplyr::filter(!.data$Indicator %in% dictionary$Indicator)
)

read_variable_map <- function(path, expected_rows, map_label) {
  variable_map <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  required_map_columns <- c("position", "original_name")
  missing_map_columns <- setdiff(required_map_columns, names(variable_map))
  if (length(missing_map_columns) > 0L) {
    stop(
      map_label, " map is missing required column(s): ",
      paste(missing_map_columns, collapse = ", ")
    )
  }

  if (nrow(variable_map) != expected_rows) {
    stop(
      map_label, " map contains ", nrow(variable_map),
      " rows; expected ", expected_rows, "."
    )
  }

  if (anyDuplicated(variable_map$original_name)) {
    stop(map_label, " map contains duplicated variable names.")
  }

  variable_map |>
    dplyr::transmute(
      Position = as.integer(.data$position),
      Indicator = as.character(.data$original_name)
    ) |>
    dplyr::arrange(.data$Position)
}

administrative_map <- read_variable_map(
  administrative_map_path,
  expected_rows = 947L,
  map_label = "Administrative"
)
mobility_map <- read_variable_map(
  mobility_map_path,
  expected_rows = 38L,
  map_label = "Mobility"
)

english_names_raw <- readxl::read_excel(english_names_path, sheet = "Variables")
required_english_columns <- c("English Name", "Original Variable Name")
missing_english_columns <- setdiff(required_english_columns, names(english_names_raw))
if (length(missing_english_columns) > 0L) {
  stop(
    "The English-name file is missing required column(s): ",
    paste(missing_english_columns, collapse = ", ")
  )
}

english_names <- english_names_raw |>
  dplyr::transmute(
    Indicator = as.character(.data[["Original Variable Name"]]),
    EnglishName = as.character(.data[["English Name"]])
  ) |>
  dplyr::filter(
    !is.na(.data$Indicator),
    !is.na(.data$EnglishName),
    trimws(.data$Indicator) != "",
    trimws(.data$EnglishName) != ""
  ) |>
  dplyr::distinct(.data$Indicator, .keep_all = TRUE)

description_translations <- utils::read.delim(
  description_translations_path,
  sep = "\t",
  quote = "",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
) |>
  dplyr::transmute(
    Description = trimws(as.character(.data$Description_ES)),
    DescriptionEnglish = trimws(as.character(.data$Indicator_name_EN))
  )

if (
  anyNA(description_translations) ||
    any(trimws(as.matrix(description_translations)) == "") ||
    anyDuplicated(description_translations$Description)
) {
  stop("The English description mapping contains missing, blank, or duplicated entries.")
}

curated_environmental_names <- c(
  "sd_precipitacion" = "Rainfall variability (standard deviation)",
  "max_temperature" = "Maximum temperature",
  "min_temperature" = "Minimum temperature",
  "arealoss" = "Forest-area loss",
  "areagain" = "Forest-area gain",
  "areatreecover" = "Tree-cover area",
  "gas_net" = "Net greenhouse-gas emissions",
  "gas_emissions" = "Greenhouse-gas emissions",
  "area" = "Land area"
)

mobility_base_names <- c(
  "viajes" = "Total trips",
  "viajeros" = "Unique travelers",
  "monday" = "Monday trips",
  "tuesday" = "Tuesday trips",
  "wednesday" = "Wednesday trips",
  "thursday" = "Thursday trips",
  "friday" = "Friday trips",
  "saturday" = "Saturday trips",
  "sunday" = "Sunday trips",
  "weekday_all" = "Weekday trips, all periods",
  "weekday_morning" = "Weekday morning trips",
  "weekday_afternoon" = "Weekday afternoon trips",
  "weekday_evening" = "Weekday evening trips",
  "weekday_night" = "Weekday night trips",
  "weekend_all" = "Weekend trips, all periods",
  "weekend_morning" = "Weekend morning trips",
  "weekend_afternoon" = "Weekend afternoon trips",
  "weekend_evening" = "Weekend evening trips",
  "weekend_night" = "Weekend night trips"
)

make_mobility_name <- function(indicator) {
  base_name <- sub("\\.[xy]$", "", indicator)
  flow_scope <- ifelse(
    grepl("\\.x$", indicator),
    "internal canton flows",
    "national flows"
  )
  readable_base <- unname(mobility_base_names[base_name])
  if (is.na(readable_base)) {
    stop("Missing readable name for mobility indicator: ", indicator)
  }
  paste0(readable_base, " (", flow_scope, ")")
}

administrative_table <- administrative_map |>
  dplyr::left_join(dictionary_complete, by = "Indicator")

unmatched_administrative <- administrative_table |>
  dplyr::filter(
    is.na(.data$Area) |
      is.na(.data$Operation) |
      is.na(.data$Source)
  )

if (nrow(unmatched_administrative) > 0L) {
  stop(
    "Missing provenance for administrative/environmental indicator(s): ",
    paste(unmatched_administrative$Indicator, collapse = ", ")
  )
}

area_translations <- c(
  "Acceso a educación" = "Access to education",
  "Inclusión financiera" = "Financial inclusion",
  "Laboral" = "Labor",
  "Salud" = "Health",
  "Sector Real" = "Real sector",
  "Seguridad" = "Security",
  "Socioeconómicas" = "Socioeconomic indicators"
)

operation_translations <- c(
  "Cuentas Nacionales" = "National Accounts",
  "Denuncias de delitos de mayor incidencia" = "Reports of high-incidence crimes",
  "Estadística de Armas Ilícitas" = "Illicit Weapons Statistics",
  "Estadística de Información Ambiental Económica en Gobiernos AutónomosDescentralizados Municipales" =
    "Environmental Economic Information Statistics for Municipal Decentralized Autonomous Governments",
  "Homicidios Intencionales" = "Intentional Homicides",
  "Número de cuentas activas" = "Number of Active Accounts",
  "Puntos de atención financiera" = "Financial Service Access Points",
  "Registro Estadístico de Camas Hospitalarias" = "Hospital Beds Statistical Register",
  "Registro Estadístico de Defunciones Generales" = "General Deaths Statistical Register",
  "Registro Estadístico de Egresos Hospitalarios" = "Hospital Discharges Statistical Register",
  "Registro administrativo histórico (Inicio y fin) del Ministerio de Educación" =
    "Historical Administrative Register of the Ministry of Education (Start and End Dates)",
  "Registro de Empleo en la Seguridad Social" = "Social Security Employment Register",
  "Registros Estadísticos de Nacidos Vivos" = "Live Births Statistical Register"
)

source_translations <- c(
  "BCE" = "Central Bank of Ecuador (BCE)",
  "Banco Central del Ecuador (BCE)" = "Central Bank of Ecuador (BCE)",
  "INEC" = "National Institute of Statistics and Census (INEC)",
  "INEC/AME/BDE" = paste(
    "National Institute of Statistics and Census (INEC) /",
    "Association of Ecuadorian Municipalities (AME) /",
    "Development Bank of Ecuador (BDE)"
  ),
  "Ministerio de Educación" = "Ministry of Education",
  "Ministerio del Interior" = "Ministry of Interior",
  "Ministerio del Interior/INEC" = paste(
    "Ministry of Interior /",
    "National Institute of Statistics and Census (INEC)"
  ),
  "Servicio de Rentas Internas (SRI)" = "Internal Revenue Service (SRI)"
)

translate_exact <- function(values, translations, field_name, passthrough = character()) {
  translated <- unname(translations[values])
  keep_original <- values %in% passthrough
  translated[keep_original] <- values[keep_original]

  missing_values <- unique(values[is.na(translated)])
  if (length(missing_values) > 0L) {
    stop(
      "Missing English translation for ", field_name, " value(s): ",
      paste(missing_values, collapse = ", ")
    )
  }

  translated
}

translate_operation <- function(values) {
  translated <- rep(NA_character_, length(values))
  sri_form <- grepl("^Saiku SRI, formulario [0-9]+$", values)
  translated[sri_form] <- sub(
    "^Saiku SRI, formulario ",
    "SRI Saiku, Form ",
    values[sri_form]
  )
  translated[!sri_form] <- translate_exact(
    values[!sri_form],
    operation_translations,
    field_name = "operation",
    passthrough = c("Climate indicators", "Deforestation indicators")
  )
  translated
}

administrative_table <- administrative_table |>
  dplyr::mutate(
    Area = translate_exact(
      .data$Area,
      area_translations,
      field_name = "area",
      passthrough = "Climate change and deforestation"
    ),
    Operation = translate_operation(.data$Operation),
    Source = translate_exact(
      .data$Source,
      source_translations,
      field_name = "source",
      passthrough = c("Clima_Ecuador_canton.dta", "Defo_Ecuador_canton.dta")
    )
  )

# The .x variables originate in roads_canton_mean.csv (internal canton flows),
# while the .y variables originate in roads_canton_national25_mean.csv
# (national flows), as defined by the left_join in the processing script.
mobility_table <- mobility_map |>
  dplyr::mutate(
    Area = "Mobile mobility data",
    Operation = dplyr::case_when(
      grepl("\\.x$", .data$Indicator) ~ "Internal canton flows (mean)",
      grepl("\\.y$", .data$Indicator) ~ "National flows (mean)",
      TRUE ~ NA_character_
    ),
    Source = "Telefónica"
  )

if (anyNA(mobility_table$Operation)) {
  stop(
    "Could not classify mobility indicator(s): ",
    paste(mobility_table$Indicator[is.na(mobility_table$Operation)], collapse = ", ")
  )
}

variable_provenance <- dplyr::bind_rows(
  administrative_table,
  mobility_table |>
    dplyr::mutate(Position = .data$Position + 947L)
) |>
  dplyr::arrange(.data$Position) |>
  dplyr::select(
    "Area",
    "Operation",
    "Source",
    "Indicator",
    "Description"
  )

if (nrow(variable_provenance) != 985L) {
  stop(
    "The variable-level provenance data contain ",
    nrow(variable_provenance), " rows; expected 985."
  )
}

provenance_required_fields <- variable_provenance |>
  dplyr::select("Area", "Operation", "Source", "Indicator")
if (
  anyNA(provenance_required_fields) ||
    any(trimws(as.matrix(provenance_required_fields)) == "")
) {
  stop("The variable-level provenance data contain missing or blank required fields.")
}

curated_mobility_names <- rep(NA_character_, nrow(variable_provenance))
is_mobility_indicator <- grepl("\\.[xy]$", variable_provenance$Indicator)
curated_mobility_names[is_mobility_indicator] <- vapply(
  variable_provenance$Indicator[is_mobility_indicator],
  make_mobility_name,
  FUN.VALUE = character(1)
)

table_variables <- variable_provenance |>
  dplyr::left_join(english_names, by = "Indicator") |>
  dplyr::left_join(description_translations, by = "Description") |>
  dplyr::mutate(
    CuratedEnvironmentalName = unname(curated_environmental_names[.data$Indicator]),
    CuratedMobilityName = curated_mobility_names,
    ReadableName = dplyr::coalesce(
      .data$EnglishName,
      .data$CuratedEnvironmentalName,
      .data$CuratedMobilityName,
      .data$DescriptionEnglish
    ),
    IndicatorName = paste0(.data$ReadableName, " [", .data$Indicator, "]")
  ) |>
  dplyr::select(
    "Area",
    "Source",
    `Indicator name` = "IndicatorName"
  )

if (nrow(table_variables) != 985L || anyDuplicated(variable_provenance$Indicator)) {
  stop("The final table must contain one unique row for each of the 985 predictors.")
}

if (anyNA(table_variables[["Indicator name"]])) {
  missing_names <- variable_provenance$Indicator[
    is.na(table_variables[["Indicator name"]])
  ]
  stop("Missing readable English name for indicator(s): ", paste(missing_names, collapse = ", "))
}

dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

break_marker <- "KABLEINDICATORBREAKPOINT"
if (any(grepl(break_marker, as.matrix(table_variables), fixed = TRUE))) {
  stop("A table value contains the reserved LaTeX break marker.")
}

insert_break_markers <- function(value, chunk_length = 18L) {
  characters <- strsplit(value, split = "", fixed = TRUE)[[1L]]
  if (length(characters) <= chunk_length) {
    return(value)
  }

  chunk_id <- ceiling(seq_along(characters) / chunk_length)
  paste(
    vapply(split(characters, chunk_id), paste0, collapse = "", FUN.VALUE = character(1)),
    collapse = break_marker
  )
}

latex_table_variables <- table_variables
latex_table_variables$Source <- vapply(
  latex_table_variables$Source,
  insert_break_markers,
  chunk_length = 16L,
  FUN.VALUE = character(1)
)
latex_table_variables[["Indicator name"]] <- vapply(
  latex_table_variables[["Indicator name"]],
  insert_break_markers,
  chunk_length = 28L,
  FUN.VALUE = character(1)
)
latex_table <- knitr::kable(
  latex_table_variables,
  format = "latex",
  booktabs = TRUE,
  longtable = TRUE,
  escape = TRUE,
  align = c(
    ">{\\raggedright\\arraybackslash}p{3.4cm}",
    ">{\\raggedright\\arraybackslash}p{5.6cm}",
    ">{\\raggedright\\arraybackslash}p{12.4cm}"
  ),
  caption = paste(
    "Areas, sources, and indicators for the 947 administrative and environmental",
    "covariates and 38 mobile mobility indicators used in the analysis."
  ),
  label = "variable-provenance",
  linesep = ""
) |>
  kableExtra::kable_styling(
    latex_options = "repeat_header",
    full_width = FALSE,
    font_size = 7,
    position = "center"
  )

latex_output <- as.character(latex_table)
latex_output <- paste(
  strsplit(latex_output, break_marker, fixed = TRUE)[[1L]],
  collapse = "\\allowbreak{}"
)
latex_output <- paste(
  "% Requires \\usepackage{pdflscape,longtable,booktabs,array}",
  "\\begin{landscape}",
  latex_output,
  "\\end{landscape}",
  sep = "\n"
)
writeLines(latex_output, output_path, useBytes = TRUE)

message(
  "Created ", output_path, " with ", nrow(table_variables),
  " variable rows (", nrow(administrative_table), " administrative/environmental + ",
  nrow(mobility_table), " mobility)."
)
