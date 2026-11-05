#################################################################
##        Consolidar datos 2022/2023 sin preescalado          ##
#################################################################

# Esta versión reproduce el procesamiento baseline original, pero devuelve las
# covariables en su escala original. La normalización se estima posteriormente
# dentro de cada fold mediante la recipe del pipeline tidymodels.

preparar_data_consolidada <- function() {

  #################################################################
  ##                         1. Datos                            ##
  #################################################################

  # Información INEC
  data_inec_2022 <- read_excel(
    "../input/Data/Covariables_Actualizadas_2010_enviar/covariables/covariables_2022.xlsx"
  )
  data_inec_2023 <- read_excel(
    "../input/Data/Covariables_Actualizadas_2010_enviar/covariables/covariables_2023.xlsx"
  )

  # Información ambiental
  clima_canton <- read_stata("../input/Data/ENVIRONMENTAL/Clima_Ecuador_canton.dta")
  defo_canton <- read_stata(
    "../input/Data/ENVIRONMENTAL/Defo_Ecuador_canton.dta",
    encoding = "latin1"
  )

  clima_canton <- clima_canton |>
    mutate(CodCanton = DPA_CANTON) |>
    group_by(year, CodCanton) |>
    mutate(
      sd_precipitacion = sd(precipitacion, na.rm = TRUE),
      max_temperature = max(temp_max),
      min_temperature = min(temp_min)
    ) |>
    summarise_if(is.numeric, mean, na.rm = TRUE) |>
    filter(year == 2023 | year == 2022) |>
    select(year, CodCanton, sd_precipitacion, max_temperature, min_temperature)

  defo_canton_2021 <- defo_canton |>
    mutate(CodCanton = DPA_CANTON) |>
    filter(year == 2021) |>
    select(
      year, CodCanton, arealoss, areagain, areatreecover,
      gas_net, gas_emissions, area
    )

  defo_canton_2010 <- defo_canton |>
    mutate(CodCanton = DPA_CANTON) |>
    filter(year == 2010) |>
    select(
      year, CodCanton, arealoss, areagain, areatreecover,
      gas_net, gas_emissions, area
    )

  #################################################################
  ##                         2. Merges                           ##
  #################################################################

  names(data_inec_2022)[names(data_inec_2022) == "CodCanton...2"] <- "CodCanton"

  data1 <- left_join(
    data_inec_2022,
    clima_canton[clima_canton$year == 2022, -1],
    by = "CodCanton",
    keep = FALSE
  )
  data2 <- left_join(
    data_inec_2023,
    clima_canton[clima_canton$year == 2023, -1],
    by = "CodCanton",
    keep = FALSE
  )

  # Se mantienen los niveles de deforestación de 2021 en 2022 y 2023.
  data1 <- left_join(data1, defo_canton_2021[, -1], by = "CodCanton", keep = FALSE)
  data2 <- left_join(data2, defo_canton_2021[, -1], by = "CodCanton", keep = FALSE)

  # Misma imputación del procesamiento original para los tres cantones con NA.
  environmental_variables <- c(
    "sd_precipitacion", "max_temperature", "min_temperature", "arealoss",
    "areagain", "areatreecover", "gas_net", "gas_emissions", "area"
  )
  data1 <- data1 |>
    mutate(across(
      all_of(environmental_variables),
      ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)
    ))
  data2 <- data2 |>
    mutate(across(
      all_of(environmental_variables),
      ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)
    ))

  data1 <- data1[, -1:-6]
  data2 <- data2[, -1:-4]

  #################################################################
  ##                 3. Diccionario y variables                  ##
  #################################################################

  diccionario <- read_excel(
    "../input/Data/Covariables_Actualizadas_2010_enviar/diccionario_covariables_all.xlsx"
  )
  diccionario <- diccionario[, 1:9]

  add_diccionario_environ <- data.frame(matrix(nrow = 9, ncol = 9))
  names(add_diccionario_environ) <- colnames(diccionario)
  add_diccionario_environ$Area <- rep(
    "Sector cambio climático y deforestación",
    9
  )
  add_diccionario_environ$operacionestadistica <- c(
    rep("Cambio climático", 3),
    rep("Deforestación", 6)
  )
  add_diccionario_environ$Indicador <- environmental_variables
  add_diccionario_environ$en_2022 <- rep(TRUE, 9)
  add_diccionario_environ$en_2023 <- rep(TRUE, 9)
  add_diccionario_environ$en_2024 <- rep(TRUE, 9)
  add_diccionario_environ$en_2010 <- c(rep(FALSE, 3), rep(TRUE, 6))

  diccionario <- rbind(diccionario, add_diccionario_environ)

  variables_in <- intersect(
    diccionario$Indicador[diccionario$en_2022],
    diccionario$Indicador[diccionario$en_2023]
  )

  X1 <- data1[, colnames(data1) %in% variables_in]
  X2 <- data2[, colnames(data2) %in% variables_in]

  # Se conserva el mismo criterio de missingness del procesamiento original.
  p <- 0.001
  missing_1 <- as.matrix(apply(apply(X1, 2, is.na), 2, sum))
  missing_2 <- as.matrix(apply(apply(X2, 2, is.na), 2, sum))

  X1 <- X1[, c(missing_1 < 221 * p)]
  X2 <- X2[, c(missing_2 < 221 * p)]

  variables_in_2 <- intersect(colnames(X1), colnames(X2))

  # El escalado original convertía en NA las columnas constantes y terminaba
  # retirándolas. Se conserva esa selección de forma explícita, sin escalar.
  has_variation <- function(x) {
    values <- unique(x[!is.na(x)])
    length(values) > 1L
  }
  varies_2022 <- vapply(X1[, variables_in_2, drop = FALSE], has_variation, logical(1))
  varies_2023 <- vapply(X2[, variables_in_2, drop = FALSE], has_variation, logical(1))
  variables_in_2 <- variables_in_2[varies_2022 & varies_2023]

  X1 <- X1[, names(X1) %in% variables_in_2]
  X2 <- X2[, names(X2) %in% variables_in_2]

  categoria_pri <- diccionario$Area[match(variables_in_2, diccionario$Indicador)]
  categoria_sec <- diccionario$operacionestadistica[
    match(variables_in_2, diccionario$Indicador)
  ]

  #################################################################
  ##                   4. Output sin escalar                     ##
  #################################################################

  list(
    X1 = X1,
    X2 = X2,
    categoria_pri = categoria_pri,
    categoria_sec = categoria_sec,
    Y_2022_censo = data_inec_2022$nbicenso2022,
    Y_2023_mef = data_inec_2023$NBIMEF,
    Y_2023_enemdu = data_inec_2023$NBIENEMDU,
    CodCanton = data_inec_2023$CodCanton,
    Canton = data_inec_2023$Canton
  )
}
