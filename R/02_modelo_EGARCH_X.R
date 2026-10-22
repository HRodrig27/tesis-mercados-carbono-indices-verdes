# ============================================================
# 02_MODELO_EGARCH_X.R
# EGARCH-X(1,1) para siete índices verdes
# Tesis de Maestría en Finanzas - Universidad Externado de Colombia
# ============================================================

source("01_preparacion_datos.R")

library(rugarch)
library(dplyr)
library(tibble)
library(tidyr)
library(writexl)
library(FinTS)
library(tseries)

# 1. Especificación EGARCH-X ----------------------------------
# Los retornos de EU ETS, Guangdong, Hubei y Shenzhen se
# incorporan contemporáneamente como variables exógenas
# en la ecuación de varianza.

make_spec <- function(Xmat,
                      arma = c(0, 0),
                      dist = "std",
                      garchOrder = c(1, 1)) {
  ugarchspec(
    variance.model = list(
      model = "eGARCH",
      garchOrder = garchOrder,
      external.regressors = Xmat
    ),
    mean.model = list(
      armaOrder = arma,
      include.mean = TRUE
    ),
    distribution.model = dist
  )
}

fit_one <- function(y, Xmat) {
  spec <- make_spec(
    Xmat = Xmat,
    arma = c(0, 0),
    dist = "std",
    garchOrder = c(1, 1)
  )

  tryCatch(
    ugarchfit(spec = spec, data = y, solver = "hybrid"),
    error = function(e) e
  )
}

# 2. Estimación de los siete índices --------------------------
fits <- list()
failures <- list()

for (gi in green_cols) {
  tmp <- na.omit(merge(green_ret[, gi], carbon_ret))
  y_clean <- as.numeric(tmp[, 1])
  X_clean <- as.matrix(tmp[, -1])

  cat("Estimando:", gi, "| N =", length(y_clean), "\n")

  fit <- fit_one(y_clean, X_clean)

  if (inherits(fit, "error")) {
    failures[[gi]] <- fit$message
    next
  }

  if (!is.null(fit@fit$convergence) && fit@fit$convergence != 0) {
    failures[[gi]] <- paste("No converge. convergence =", fit@fit$convergence)
  }

  fits[[gi]] <- fit
}

# 3. Extracción de coeficientes -------------------------------
extract_results <- function(fit_obj, index_name) {
  m <- fit_obj@fit$matcoef
  df_coef <- as.data.frame(m)
  colnames(df_coef)[1:4] <- c("estimate", "se", "tvalue", "pvalue")

  coef_out <- df_coef %>%
    rownames_to_column("param") %>%
    mutate(index = index_name) %>%
    select(index, param, estimate, se, tvalue, pvalue)

  ic <- infocriteria(fit_obj)

  diag_out <- tibble(
    index = index_name,
    AIC = as.numeric(ic["Akaike", ]),
    BIC = as.numeric(ic["Bayes", ]),
    logLik = likelihood(fit_obj),
    nobs = fit_obj@fit$nobs,
    convergence = fit_obj@fit$convergence
  )

  list(coef = coef_out, diag = diag_out)
}

coef_list <- list()
diag_list <- list()

for (gi in names(fits)) {
  res <- extract_results(fits[[gi]], gi)
  coef_list[[gi]] <- res$coef
  diag_list[[gi]] <- res$diag
}

coef_table <- bind_rows(coef_list)
diag_table <- bind_rows(diag_list)

# 4. Coeficientes de mercados de carbono ---------------------
# vxreg1 = EU ETS
# vxreg2 = Guangdong
# vxreg3 = Hubei
# vxreg4 = Shenzhen

vx_map <- tibble(
  param = c("vxreg1", "vxreg2", "vxreg3", "vxreg4"),
  carbon = c("EU ETS", "Guangdong", "Hubei", "Shenzhen")
)

carbon_effects_long <- coef_table %>%
  inner_join(vx_map, by = "param") %>%
  select(index, carbon, estimate, pvalue)

impacto_carbono <- carbon_effects_long %>%
  pivot_wider(
    id_cols = index,
    names_from = carbon,
    values_from = c(estimate, pvalue),
    names_glue = "{carbon}_{.value}"
  )

# 5. Ajuste Benjamini-Hochberg -------------------------------
# Los 28 contrastes (4 mercados x 7 índices) se tratan como
# una misma familia de pruebas.

bh_results <- carbon_effects_long %>%
  mutate(
    p_BH = p.adjust(pvalue, method = "BH"),
    significativo_5_original = pvalue < 0.05,
    significativo_5_BH = p_BH < 0.05
  ) %>%
  arrange(pvalue)

bh_table <- bh_results %>%
  select(index, carbon, estimate, pvalue, p_BH) %>%
  pivot_wider(
    id_cols = index,
    names_from = carbon,
    values_from = c(estimate, pvalue, p_BH),
    names_glue = "{carbon}_{.value}"
  )

# 6. Diagnósticos de residuos estandarizados -----------------
diagnosticos_residuos <- function(fit_obj,
                                  index_name,
                                  lags_lb = 20,
                                  lags_arch = 12) {
  z <- residuals(fit_obj, standardize = TRUE)
  z <- as.numeric(z)
  z <- z[is.finite(z)]

  lb_z <- Box.test(z, lag = lags_lb, type = "Ljung-Box")
  lb_z2 <- Box.test(z^2, lag = lags_lb, type = "Ljung-Box")
  arch <- FinTS::ArchTest(z, lags = lags_arch)
  jb <- tseries::jarque.bera.test(z)

  tibble(
    index = index_name,
    LB_z_lag = lags_lb,
    LB_z_stat = as.numeric(lb_z$statistic),
    LB_z_pval = as.numeric(lb_z$p.value),
    LB_z2_lag = lags_lb,
    LB_z2_stat = as.numeric(lb_z2$statistic),
    LB_z2_pval = as.numeric(lb_z2$p.value),
    ARCH_lags = lags_arch,
    ARCH_stat = as.numeric(arch$statistic),
    ARCH_pval = as.numeric(arch$p.value),
    JB_stat = as.numeric(jb$statistic),
    JB_pval = as.numeric(jb$p.value)
  )
}

resid_tests <- lapply(names(fits), function(gi) {
  diagnosticos_residuos(fits[[gi]], gi, lags_lb = 20, lags_arch = 12)
})

pruebas_residuos <- bind_rows(resid_tests)

# 7. Modelos fallidos -----------------------------------------
modelos_fallidos <- tibble(
  index = names(failures),
  motivo = unlist(failures)
)

# 8. Exportación ----------------------------------------------
dir.create("output", showWarnings = FALSE)

write_xlsx(
  list(
    "EGARCHX_Coeficientes" = coef_table,
    "EGARCHX_Diagnosticos" = diag_table,
    "Carbono_Volatilidad" = impacto_carbono,
    "BH_28_Contrastes" = bh_results,
    "BH_Tabla" = bh_table,
    "Pruebas_Residuos" = pruebas_residuos,
    "Modelos_Fallidos" = modelos_fallidos
  ),
  path = "output/Resultados_EGARCH_X.xlsx"
)

cat("\nArchivo generado: output/Resultados_EGARCH_X.xlsx\n")
