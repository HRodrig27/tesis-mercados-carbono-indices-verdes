# ============================================================
# 01_PREPARACION_DATOS.R
# Mercados de carbono e índices verdes
# Tesis de Maestría en Finanzas - Universidad Externado de Colombia
# ============================================================

library(readxl)
library(dplyr)
library(xts)
library(tseries)

# El archivo de Bloomberg no se distribuye en este repositorio.
file_path <- "data/Precios_indices_consolidados_ajustadovf.xlsx"

green_cols <- c(
  "ECO Index", "GRNBIO Index", "GRNREG Index", "GRNGEO Index",
  "GRNFUEL Index", "GRNSOLAR Index", "GRNWIND Index"
)

carbon_cols <- c(
  "EEXXT3PA Index",   # EU ETS
  "CNNCGDEP Index",   # Guangdong
  "CNNCHBEP Index",   # Hubei
  "CNNCSZEP Index"    # Shenzhen
)

all_cols <- c(green_cols, carbon_cols)

df <- read_excel(file_path)

stopifnot("FECHA" %in% names(df))
stopifnot(all(all_cols %in% names(df)))

df <- df %>%
  mutate(FECHA = as.Date(FECHA)) %>%
  arrange(FECHA) %>%
  select(FECHA, all_of(all_cols))

prices_xts <- xts(df[, all_cols], order.by = df$FECHA)

# Retornos logarítmicos diarios
rets_xts <- diff(log(prices_xts))
rets_xts <- na.omit(rets_xts)

# Retornos expresados en porcentaje para DCC-GARCH
rets_xts_pct <- rets_xts * 100

# Objetos específicos para EGARCH-X
green_ret  <- rets_xts[, green_cols]
carbon_ret <- rets_xts[, carbon_cols]

# Fechas comunes entre todas las series
common_dates <- index(na.omit(merge(green_ret, carbon_ret)))
green_ret  <- green_ret[common_dates]
carbon_ret <- carbon_ret[common_dates]

# Prueba ADF sobre retornos porcentuales
adf_tbl <- data.frame(
  Serie = colnames(rets_xts_pct),
  ADF = sapply(colnames(rets_xts_pct), function(x) {
    as.numeric(adf.test(rets_xts_pct[, x])$statistic)
  }),
  p_value = sapply(colnames(rets_xts_pct), function(x) {
    adf.test(rets_xts_pct[, x])$p.value
  })
)

print(adf_tbl)
