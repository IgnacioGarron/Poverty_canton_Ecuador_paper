new_code_root <- normalizePath(getwd(), mustWork = TRUE)
repo_root <- normalizePath(file.path(new_code_root, ".."), mustWork = TRUE)

if (!requireNamespace("sf", quietly = TRUE)) {
  stop(
    "Falta el paquete sf. Ejecute: ",
    "Rscript 'setup_dependencies.R'"
  )
}

# Feature Service público: Cantones del Ecuador, atribuido a INEC (2012).
service_layer <- paste0(
  "https://services5.arcgis.com/qefe3CGaSKnteGG1/arcgis/rest/services/",
  "Cantones_del_Ecuador/FeatureServer/0"
)
where_clause <- utils::URLencode("DPA_PROVIN <> '90'", reserved = TRUE)
query_url <- paste0(
  service_layer,
  "/query?where=", where_clause,
  "&outFields=*",
  "&returnGeometry=true",
  "&outSR=4326",
  "&resultRecordCount=2000",
  "&f=geojson"
)

output_dir <- file.path(
  repo_root, "input", "Data", "Mapa", "Cantones_ArcGIS_2012"
)
output_file <- file.path(output_dir, "cantones_ecuador_2012.gpkg")
temp_geojson <- tempfile("cantones_ecuador_2012_", fileext = ".geojson")
on.exit(unlink(temp_geojson), add = TRUE)

message("Descargando cantones desde ArcGIS...")
utils::download.file(query_url, temp_geojson, mode = "wb", quiet = FALSE)

ecu_cantones <- sf::st_read(temp_geojson, quiet = TRUE)
required_fields <- c("DPA_CANTON", "DPA_DESCAN", "DPA_PROVIN", "DPA_DESPRO")
missing_fields <- setdiff(required_fields, names(ecu_cantones))
if (length(missing_fields)) {
  stop(
    "La capa descargada no contiene las variables esperadas: ",
    paste(missing_fields, collapse = ", ")
  )
}

standardize_code <- function(x) {
  x <- trimws(as.character(x))
  numeric_code <- grepl("^[0-9]+$", x)
  if (!all(numeric_code)) {
    stop("Se encontraron códigos cantonales no numéricos.")
  }
  sprintf("%04d", as.integer(x))
}

ecu_cantones$DPA_CANTON <- standardize_code(ecu_cantones$DPA_CANTON)
ecu_cantones$DPA_PROVIN <- trimws(as.character(ecu_cantones$DPA_PROVIN))
ecu_cantones <- ecu_cantones[ecu_cantones$DPA_PROVIN != "90", ]

# La capa de 2012 codifica La Concordia como 0808 (Esmeraldas). Los datos del
# modelo usan la codificación posterior 2302 (Santo Domingo de los Tsáchilas).
# Se conservan los atributos originales antes de armonizar la llave del merge.
ecu_cantones$DPA_CANTON_SOURCE <- ecu_cantones$DPA_CANTON
ecu_cantones$DPA_PROVIN_SOURCE <- ecu_cantones$DPA_PROVIN
ecu_cantones$DPA_DESPRO_SOURCE <- ecu_cantones$DPA_DESPRO
la_concordia <-
  ecu_cantones$DPA_CANTON == "0808" &
  toupper(trimws(ecu_cantones$DPA_DESCAN)) == "LA CONCORDIA"
if (sum(la_concordia) != 1L) {
  stop("Se esperaba exactamente una geometría 0808 para La Concordia.")
}
ecu_cantones$DPA_CANTON[la_concordia] <- "2302"
ecu_cantones$DPA_PROVIN[la_concordia] <- "23"
ecu_cantones$DPA_DESPRO[la_concordia] <- "SANTO DOMINGO DE LOS TSACHILAS"

if (nrow(ecu_cantones) != 221L) {
  stop(
    "Se esperaban 221 cantones después de excluir DPA_PROVIN = 90; ",
    "se descargaron ", nrow(ecu_cantones), "."
  )
}
if (anyDuplicated(ecu_cantones$DPA_CANTON)) {
  stop("La capa descargada contiene códigos cantonales duplicados.")
}

galapagos_codes <- c("2001", "2002", "2003")
missing_galapagos <- setdiff(galapagos_codes, ecu_cantones$DPA_CANTON)
if (length(missing_galapagos)) {
  stop(
    "Faltan cantones de Galápagos: ",
    paste(missing_galapagos, collapse = ", ")
  )
}
if (any(sf::st_is_empty(ecu_cantones))) {
  stop("La capa descargada contiene geometrías vacías.")
}

ecu_cantones <- sf::st_make_valid(ecu_cantones)
if (is.na(sf::st_crs(ecu_cantones))) {
  stop("La capa descargada no tiene un sistema de referencia espacial.")
}
ecu_cantones <- sf::st_transform(ecu_cantones, 4326)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
sf::st_write(
  ecu_cantones,
  output_file,
  layer = "cantones_ecuador_2012",
  delete_dsn = TRUE,
  quiet = TRUE
)

message(
  "Descarga verificada: 221 cantones, incluidos 2001, 2002 y 2003; ",
  "La Concordia armonizada de 0808 a 2302."
)
message("GeoPackage guardado en: ", output_file)
