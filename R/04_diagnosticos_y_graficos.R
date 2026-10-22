# ============================================================
# 04_DIAGNOSTICOS_Y_GRAFICOS.R
# Diagnósticos y figuras complementarias
# Tesis de Maestría en Finanzas - Universidad Externado de Colombia
# ============================================================

# Requiere ejecutar previamente:
# 1) R/01_preparacion_datos.R
# 2) R/02_modelo_EGARCH_X.R
# 3) R/03_modelo_DCC_GARCH.R

library(FinTS)
library(dplyr)
library(ggplot2)
library(reshape2)
library(openxlsx)

dir.create("output", showWarnings = FALSE)
dir.create("output/figuras_dcc", showWarnings = FALSE, recursive = TRUE)

# 1. Diagnósticos de los 11 GARCH univariados ----------------
res_mat <- residuals(best_fit)
sig_mat <- sigma(best_fit)
z_mat <- as.matrix(res_mat / sig_mat)

diagnosticos_garch_dcc <- do.call(
  rbind,
  lapply(seq_len(ncol(z_mat)), function(i) {
    z <- as.numeric(z_mat[, i])
    z <- z[is.finite(z)]

    lb_z <- Box.test(z, lag = 20, type = "Ljung-Box")
    lb_z2 <- Box.test(z^2, lag = 20, type = "Ljung-Box")
    arch <- FinTS::ArchTest(z, lags = 12)

    data.frame(
      Serie = colnames(z_mat)[i],
      N = length(z),
      LB_z_lag = 20,
      LB_z_pvalor = as.numeric(lb_z$p.value),
      LB_z2_lag = 20,
      LB_z2_pvalor = as.numeric(lb_z2$p.value),
      ARCH_LM_lag = 12,
      ARCH_LM_pvalor = as.numeric(arch$p.value)
    )
  })
)

diagnosticos_garch_dcc <- diagnosticos_garch_dcc %>%
  mutate(
    Diagnostico = ifelse(
      LB_z_pvalor > 0.05 & LB_z2_pvalor > 0.05 & ARCH_LM_pvalor > 0.05,
      "Sin evidencia de problemas remanentes",
      "Revisar especificación"
    )
  )

write.xlsx(
  diagnosticos_garch_dcc,
  "output/Diagnosticos_GARCH_DCC.xlsx",
  overwrite = TRUE
)

# 2. Gráficos de correlaciones dinámicas ----------------------
safe_name <- function(x) {
  x <- gsub(" Index", "", x)
  gsub("[^A-Za-z0-9_\\-]", "_", x)
}

pairs_unique <- unique(dcc_pairs_df[, c("Serie1", "Serie2")])

for (row in seq_len(nrow(pairs_unique))) {
  s1 <- pairs_unique$Serie1[row]
  s2 <- pairs_unique$Serie2[row]

  df_pair <- subset(dcc_pairs_df, Serie1 == s1 & Serie2 == s2)

  p <- ggplot(df_pair, aes(x = FECHA, y = rho)) +
    geom_line() +
    labs(
      title = paste0("DCC: ", s1, " vs ", s2),
      x = "Fecha",
      y = expression(rho[t])
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(
      "output/figuras_dcc",
      paste0("DCC_", safe_name(s1), "_", safe_name(s2), ".png")
    ),
    plot = p,
    width = 11,
    height = 3,
    dpi = 300
  )
}

# 3. Heatmaps DCC por subperiodos -----------------------------
plot_heatmap_cg <- function(R_avg, titulo) {
  R_cg <- R_avg[carbon_cols, green_cols, drop = FALSE]

  df_heat <- reshape2::melt(
    R_cg,
    varnames = c("Carbono", "Verde"),
    value.name = "rho"
  )

  df_heat$label <- sprintf("%.3f", df_heat$rho)

  ggplot(df_heat, aes(x = Verde, y = Carbono, fill = rho)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = label), size = 4, color = "black") +
    scale_fill_gradient2(
      low = "#B2182B",
      mid = "#F7F7F7",
      high = "#2166AC",
      midpoint = 0,
      name = expression(rho)
    ) +
    labs(title = titulo, x = "", y = "") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 13),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
}

R_array <- rcor(best_fit)
dates <- index(rets_xts_pct)
all_names <- colnames(rets_xts_pct)

for (k in seq_len(nrow(subperiods))) {
  idx <- which(dates >= subperiods$ini[k] & dates <= subperiods$fin[k])

  if (length(idx) < 10) {
    warning("Subperiodo con pocas observaciones: ", subperiods$Periodo[k])
    next
  }

  R_avg <- apply(
    R_array[, , idx, drop = FALSE],
    c(1, 2),
    mean,
    na.rm = TRUE
  )

  dimnames(R_avg) <- list(all_names, all_names)

  titulo <- paste0(
    "Correlación Promedio DCC - (Carbono Vs Verdes) - ",
    best_name,
    " - ",
    subperiods$Periodo[k]
  )

  p <- plot_heatmap_cg(R_avg, titulo)

  ggsave(
    filename = file.path(
      "output/figuras_dcc",
      paste0(
        "Heatmap_",
        gsub("[^A-Za-z0-9]", "_", subperiods$Periodo[k]),
        ".png"
      )
    ),
    plot = p,
    width = 13,
    height = 6.5,
    dpi = 300,
    bg = "white"
  )
}

cat("\nDiagnósticos y figuras generados en output/.\n")
