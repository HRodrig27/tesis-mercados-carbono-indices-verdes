# ============================================================
# 03_MODELO_DCC_GARCH.R
# DCC-GARCH: mercados de carbono e índices verdes
# Tesis de Maestría en Finanzas - Universidad Externado de Colombia
# ============================================================

source("01_preparacion_datos.R")

library(rugarch)
library(rmgarch)
library(dplyr)
library(tidyr)
library(openxlsx)

# 1. GARCH(1,1) univariados -----------------------------------
uspec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "std"
)

multispec_all <- multispec(replicate(ncol(rets_xts_pct), uspec))

# 2. Especificaciones DCC -------------------------------------
dcc_spec_mvn <- dccspec(
  uspec = multispec_all,
  dccOrder = c(1, 1),
  distribution = "mvnorm"
)

dcc_spec_mvt <- dccspec(
  uspec = multispec_all,
  dccOrder = c(1, 1),
  distribution = "mvt"
)

# 3. Estimación -----------------------------------------------
fit_mvn <- dccfit(
  dcc_spec_mvn,
  data = rets_xts_pct,
  fit.control = list(eval.se = TRUE)
)

fit_mvt <- dccfit(
  dcc_spec_mvt,
  data = rets_xts_pct,
  fit.control = list(eval.se = TRUE)
)

# 4. Comparación DCC-MVN vs DCC-MVT ---------------------------
ic_tbl <- data.frame(
  Modelo = c("DCC_MVN", "DCC_MVT"),
  LogLik = c(likelihood(fit_mvn), likelihood(fit_mvt)),
  AIC = c(infocriteria(fit_mvn)[1], infocriteria(fit_mvt)[1]),
  BIC = c(infocriteria(fit_mvn)[2], infocriteria(fit_mvt)[2])
)

best_fit <- if (ic_tbl$AIC[2] < ic_tbl$AIC[1]) fit_mvt else fit_mvn
best_name <- if (identical(best_fit, fit_mvt)) "DCC_MVT" else "DCC_MVN"

print(ic_tbl)
cat("\nModelo seleccionado:", best_name, "\n")

# 5. Parámetros conjuntos -------------------------------------
joint_parameters <- best_fit@mfit$matcoef[
  grepl("\\[Joint\\]", rownames(best_fit@mfit$matcoef)),
  ,
  drop = FALSE
]

dcc_convergence <- best_fit@mfit$convergence

print(joint_parameters)
cat("Código de convergencia:", dcc_convergence, "\n")

# 6. Persistencia GARCH univariada ----------------------------
coeficientes <- coef(best_fit)
series_modelo <- colnames(rets_xts_pct)

persistencia_garch <- do.call(
  rbind,
  lapply(series_modelo, function(serie) {
    nombre_alpha <- paste0("[", serie, "].alpha1")
    nombre_beta <- paste0("[", serie, "].beta1")

    alpha <- unname(coeficientes[nombre_alpha])
    beta <- unname(coeficientes[nombre_beta])

    data.frame(
      Serie = serie,
      alpha1 = alpha,
      beta1 = beta,
      alpha_beta = alpha + beta,
      Condicion_estabilidad = ifelse(alpha + beta < 1, "Cumple", "No cumple")
    )
  })
)

# 7. Correlaciones condicionales dinámicas --------------------
R_array <- rcor(best_fit)
dates <- index(rets_xts_pct)
Tobs <- dim(R_array)[3]
series_names <- colnames(rets_xts_pct)

get_dcc_pair <- function(name_i, name_j) {
  i <- which(series_names == name_i)
  j <- which(series_names == name_j)
  rho <- sapply(seq_len(Tobs), function(t) R_array[i, j, t])

  data.frame(
    FECHA = dates,
    rho = rho,
    Serie1 = name_i,
    Serie2 = name_j
  )
}

# 8. 28 pares carbono-verde -----------------------------------
pairs_list <- list()
k <- 1

for (c_name in carbon_cols) {
  for (g_name in green_cols) {
    pairs_list[[k]] <- get_dcc_pair(c_name, g_name)
    k <- k + 1
  }
}

dcc_pairs_df <- bind_rows(pairs_list)

# 9. Estadísticos del periodo completo ------------------------
dcc_summary <- dcc_pairs_df %>%
  group_by(Serie1, Serie2) %>%
  summarise(
    Corr_mean = mean(rho, na.rm = TRUE),
    Corr_min = min(rho, na.rm = TRUE),
    Corr_max = max(rho, na.rm = TRUE),
    P10 = quantile(rho, 0.10, na.rm = TRUE),
    P90 = quantile(rho, 0.90, na.rm = TRUE),
    Share_pos = mean(rho > 0, na.rm = TRUE),
    .groups = "drop"
  )

dcc_market_mean <- dcc_summary %>%
  group_by(Serie1) %>%
  summarise(
    Corr_promedio = mean(Corr_mean, na.rm = TRUE),
    .groups = "drop"
  )

# 10. Subperiodos ---------------------------------------------
subperiods <- data.frame(
  Periodo = c("2018-2019", "COVID-2020", "2021-2022", "2023-2024"),
  ini = as.Date(c("2018-01-03", "2020-03-01", "2021-01-01", "2023-01-01")),
  fin = as.Date(c("2019-12-31", "2020-12-31", "2022-12-31", "2024-05-31"))
)

dcc_sub <- dcc_pairs_df %>%
  crossing(subperiods) %>%
  filter(FECHA >= ini, FECHA <= fin) %>%
  group_by(Serie1, Serie2, Periodo) %>%
  summarise(Corr_mean = mean(rho, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Periodo, values_from = Corr_mean)

# 11. Exportación ---------------------------------------------
dir.create("output", showWarnings = FALSE)

wb <- createWorkbook()
addWorksheet(wb, "ADF")
writeData(wb, "ADF", adf_tbl)
addWorksheet(wb, "IC_modelos")
writeData(wb, "IC_modelos", ic_tbl)
addWorksheet(wb, "Parametros_DCC")
writeData(wb, "Parametros_DCC", as.data.frame(joint_parameters), rowNames = TRUE)
addWorksheet(wb, "Persistencia_GARCH")
writeData(wb, "Persistencia_GARCH", persistencia_garch)
addWorksheet(wb, "DCC_resumen")
writeData(wb, "DCC_resumen", dcc_summary)
addWorksheet(wb, "DCC_promedio_mercado")
writeData(wb, "DCC_promedio_mercado", dcc_market_mean)
addWorksheet(wb, "DCC_subperiodos")
writeData(wb, "DCC_subperiodos", dcc_sub)

saveWorkbook(
  wb,
  "output/Resultados_DCC_GARCH.xlsx",
  overwrite = TRUE
)

cat("\nArchivo generado: output/Resultados_DCC_GARCH.xlsx\n")
