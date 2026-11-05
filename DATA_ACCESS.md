# Data access and input inventory

## Availability statement

Some data sources used in this project are not publicly available and therefore cannot be shared publicly. However, all codes are freely available to ensure the reproducibility of the analysis. In addition, the underlying data can be made available to referees for review purposes, provided that they agree not to share, distribute, or otherwise make the data publicly accessible, in accordance with the applicable data confidentiality restrictions.

## Sources and how to obtain information

| Component | Information and access route |
|---|---|
| Census UBN outcome and Figure 1 | Consult [INEC census results](https://www.censoecuador.gob.ec/resultados-censo/) for the “Pobreza por Necesidades Básicas Insatisfechas” tabulations and historical census results. The [September 2025 NBI report](https://www.ecuadorencifras.gob.ec/documentos/web-inec/Censo2022/Documento_NBI_24.09.25.pdf) documents the measure. Figure 1 compares 2010 and 2022; the fitted outcome is 2022. Request the exact study compilation for numerical replication. |
| Official covariates and dictionary | Request the study's INEC compilation and metadata through the authors for referee review. For general source inquiries, use [INEC's information request page](https://www.ecuadorencifras.gob.ec/requerimientos-de-informacion/). The online appendix identifies the institution for each predictor; a public information request is not an assurance of access to the restricted compilation. |
| Climate and deforestation | Request the exact prepared `Clima_Ecuador_canton.dta` and `Defo_Ecuador_canton.dta` files. [INEC's information request page](https://www.ecuadorencifras.gob.ec/requerimientos-de-informacion/)|
| Canton-level mobility covariates | The model uses 38 indicators: 19 internal and 19 national flow measures, with a June 2024 reference date. These are restricted aggregates based on micro-level data. This data has confidentiality restrictions.|
| Canton boundaries | Run `Rscript download_Cantones_ArcGIS_2012.R` from `new code/`. Its configured source is the [Cantones del Ecuador ArcGIS service](https://services5.arcgis.com/qefe3CGaSKnteGG1/arcgis/rest/services/Cantones_del_Ecuador/FeatureServer/0), attributed in the script to INEC (2012). The estimation stage then excludes Galápagos `2001`, `2002`, and `2003`. |
| Population weights | The distributional-figure script cites INEC Censo 2022, `01_2022_CPV_Estructura_poblacional`, table 1.1, available through the [census results portal](https://www.censoecuador.gob.ec/resultados-censo/). It records this [CSV archive URL](https://www.censoecuador.gob.ec/wp-content/uploads/2024/04/01_2022_CPV_Estructura_poblacional_CSV.zip).|

The benchmark contains 947 indicators: education (426), health (265), real
sector activity (86), financial inclusion (61), socioeconomic conditions (52),
security (46), climate/deforestation (9), and labor (2). Census variables are
not used as predictors.

## Figure 2: unavailable mobile-phone microdata

Figure 2 is `paper/Fig2.png` (original asset name: `movilidad_map.png`). It depicts road-segment optimal
routes by province for June 2024. According to the manuscript, network records
were anonymized, mapped to hexagonal zones, expanded using INEC population
projections, and converted into origin–destination flows and least-cost road
routes. The source geodatabase comprised 24 provincial layers and one
interprovincial layer in EPSG:4326; the map colors the provincial layers.

The underlying mobile-phone microdata are not available. Neither these records
nor the original route-layer geodatabase and map-producing procedure are
included here. 

## Exact local data files used in the study

Paths below are relative to the repository root:

| Required path under `input/` | Purpose |
|---|---|
| `Data/Covariables_Actualizadas_2010_enviar/covariables/covariables_2022.xlsx` | All scenarios; includes census outcome `nbicenso2022` |
| `Data/Covariables_Actualizadas_2010_enviar/diccionario_covariables_all.xlsx` | Predictor selection and variable provenance |
| `Data/ENVIRONMENTAL/Clima_Ecuador_canton.dta` | Climate covariates |
| `Data/ENVIRONMENTAL/Defo_Ecuador_canton.dta` | Deforestation covariates; processor uses 2021 levels in 2022 and 2023 |
| `Data/MOBILE/roads_canton_mean.csv` | Mean internal traffic intensities |
| `Data/MOBILE/roads_canton_national25_mean.csv` | Mean national traffic intensities |
| `Data/MOBILE/roads_canton_sum.csv` | Read by mobility processors, including mean specifications |
| `Data/MOBILE/roads_canton_national25_sum.csv` | Read by mobility processors, including mean specifications |
| `Data/Mapa/Cantones_ArcGIS_2012/cantones_ecuador_2012.gpkg` | Data to create maps |
| `Data/INEC/population_canton_2022.csv` | Distributional plots, canton-level weights; fields `CodCanton`, `population_2022` |
| `Data/Covariables_Actualizadas_2010_enviar/top_var_english.xlsx` | SHAP labels and variable dictionary; sheet `Variables`, fields `Original Variable Name`, `English Name` |
| `Data/Covariables_Actualizadas_2010_enviar/calendario.xlsx` | Calendar exercise; sheet `Sheet1`, fields `variable`, `I`, `II`, `III`, `IV`. Each variable have a unique release quarter; the current code requires no new Q3 releases. |





