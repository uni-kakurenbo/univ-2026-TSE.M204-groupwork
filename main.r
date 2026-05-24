# config.R はバッチから呼ばれた場合（変数設定済み）はスキップする
if (!base::exists("TARGET_START_YEAR")) base::source("config.R")
base::source("preprocess.R")

# 出力ディレクトリの設定
out_dir <- base::file.path("dist", base::paste0(TARGET_START_YEAR, "-", TARGET_END_YEAR))
base::dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

df_analysis <- preprocess_worldbank_data(
  unemp_file = "./data/unemployment.csv",
  gdp_file   = "./data/gdp-growth.csv",
  infl_file  = "./data/inflation.csv",
  start_year = TARGET_START_YEAR,
  end_year   = TARGET_END_YEAR
)

# メタ情報のラベルを生成するヘルパー関数
make_meta_label <- function(n, p_value = NULL, r2 = NULL, adj_r2 = NULL) {
  lines <- base::c(
    base::paste0("Source: ", DATA_SOURCE),
    base::paste0("Years: ", TARGET_START_YEAR, if (TARGET_START_YEAR != TARGET_END_YEAR) base::paste0("-", TARGET_END_YEAR) else ""),
    base::paste0("N = ", n)
  )
  if (!base::is.null(p_value)) {
    lines <- base::c(lines, base::paste0("p = ", base::formatC(p_value, format = "e", digits = 2)))
  }
  if (!base::is.null(r2)) {
    lines <- base::c(lines, base::paste0("R\u00b2 = ", base::round(r2, 3)))
  }
  if (!base::is.null(adj_r2)) {
    lines <- base::c(lines, base::paste0("Adj. R\u00b2 = ", base::round(adj_r2, 3)))
  }
  base::paste(lines, collapse = " | ")
}

# ============================================================
# GDP成長率 vs 失業率
# ============================================================
n_gdp <- base::sum(stats::complete.cases(df_analysis[, c("GDP_Growth", "Unemployment")]))

cor_gdp <- stats::cor.test(
  x = df_analysis$GDP_Growth,
  y = df_analysis$Unemployment,
  use = "complete.obs",
  conf.level = CONFIDENCE_LEVEL
)
base::print(
  base::paste(
    "【GDP成長率 vs 失業率】相関係数 (r) =", base::round(cor_gdp$estimate, 3),
    " / p値 =", base::signif(cor_gdp$p.value, 3)
  )
)

# 単回帰分析: GDP_Growth -> Unemployment
slr_gdp <- stats::lm(
  formula = Unemployment ~ GDP_Growth,
  data = df_analysis,
  na.action = stats::na.exclude
)
slr_gdp_summary <- base::summary(slr_gdp)
base::print("【単回帰分析】 Unemployment ~ GDP_Growth")
base::print(slr_gdp_summary)

# 散布図（相関）
meta_gdp_scatter <- make_meta_label(
  n = n_gdp,
  p_value = cor_gdp$p.value
)
plot_gdp <- ggplot2::ggplot(
  data = df_analysis,
  mapping = ggplot2::aes(x = GDP_Growth, y = Unemployment)
) +
  ggplot2::geom_point(
    color = COLOR_GDP_POINT,
    alpha = PLOT_POINT_ALPHA,
    size  = PLOT_POINT_SIZE
  ) +
  ggplot2::geom_smooth(
    method = "lm",
    color  = COLOR_GDP_LINE,
    fill   = COLOR_GDP_FILL,
    level  = CONFIDENCE_LEVEL
  ) +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title    = "GDP Growth vs Unemployment Rate",
    subtitle = base::paste0("Pearson Correlation: ", base::round(cor_gdp$estimate, 3), "\n", meta_gdp_scatter),
    x        = "GDP Growth (annual %)",
    y        = "Unemployment (% of labor force)"
  ) +
  ggplot2::theme(
    plot.title       = ggplot2::element_text(face = "bold", color = COLOR_GDP_LINE, size = 14),
    plot.subtitle    = ggplot2::element_text(color = "gray50", size = 10),
    panel.grid.minor = ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = base::file.path(out_dir, "scatter_gdp_unemployment.png"),
  plot     = plot_gdp,
  width    = IMAGE_WIDTH,
  height   = IMAGE_HEIGHT,
  dpi      = IMAGE_DPI
)

# 単回帰分析(GDP): 予測値と残差
df_analysis$Predicted_Unemp_GDP <- stats::predict(slr_gdp)
df_analysis$Residuals_GDP <- stats::residuals(slr_gdp)

meta_gdp_reg <- make_meta_label(
  n       = n_gdp,
  p_value = stats::pf(slr_gdp_summary$fstatistic[1], slr_gdp_summary$fstatistic[2], slr_gdp_summary$fstatistic[3], lower.tail = FALSE),
  r2      = slr_gdp_summary$r.squared,
  adj_r2  = slr_gdp_summary$adj.r.squared
)

plot_slr_gdp_ap <- ggplot2::ggplot(
  data = df_analysis,
  mapping = ggplot2::aes(x = Predicted_Unemp_GDP, y = Unemployment)
) +
  ggplot2::geom_point(color = COLOR_GDP_POINT, alpha = PLOT_POINT_ALPHA, size = PLOT_POINT_SIZE) +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "gray30", linetype = "dashed") +
  ggplot2::geom_smooth(method = "lm", color = COLOR_GDP_LINE, fill = COLOR_GDP_FILL, level = CONFIDENCE_LEVEL) +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title = "GDP Regression: Predicted vs Actual",
    subtitle = meta_gdp_reg,
    x = "Predicted Unemployment Rate (%)",
    y = "Actual Unemployment Rate (%)"
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", color = COLOR_GDP_LINE, size = 14),
    plot.subtitle = ggplot2::element_text(color = "gray50", size = 10)
  )

ggplot2::ggsave(
  filename = base::file.path(out_dir, "slr_gdp_actual_vs_predicted.png"),
  plot     = plot_slr_gdp_ap,
  width    = IMAGE_WIDTH,
  height   = IMAGE_HEIGHT,
  dpi      = IMAGE_DPI
)

plot_slr_gdp_res <- ggplot2::ggplot(
  data = df_analysis,
  mapping = ggplot2::aes(x = Predicted_Unemp_GDP, y = Residuals_GDP)
) +
  ggplot2::geom_point(color = COLOR_GDP_POINT, alpha = PLOT_POINT_ALPHA, size = PLOT_POINT_SIZE) +
  ggplot2::geom_hline(yintercept = 0, color = COLOR_GDP_LINE, linetype = "dashed") +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title = "GDP Regression: Residuals vs Fitted",
    subtitle = meta_gdp_reg,
    x = "Fitted values (Predicted Unemployment)",
    y = "Residuals"
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", color = COLOR_GDP_LINE, size = 14),
    plot.subtitle = ggplot2::element_text(color = "gray50", size = 10)
  )

ggplot2::ggsave(
  filename = base::file.path(out_dir, "slr_gdp_residuals_vs_fitted.png"),
  plot     = plot_slr_gdp_res,
  width    = IMAGE_WIDTH,
  height   = IMAGE_HEIGHT,
  dpi      = IMAGE_DPI
)


# ============================================================
# インフレ率 vs 失業率
# ============================================================
n_infl <- base::sum(stats::complete.cases(df_analysis[, c("Inflation", "Unemployment")]))

cor_infl <- stats::cor.test(
  x = df_analysis$Inflation,
  y = df_analysis$Unemployment,
  use = "complete.obs",
  conf.level = CONFIDENCE_LEVEL
)
base::print(base::paste("【インフレ率 vs 失業率】相関係数 (r) =", base::round(cor_infl$estimate, 3), " / p値 =", base::signif(cor_infl$p.value, 3)))

# 単回帰分析: Inflation -> Unemployment
slr_infl <- stats::lm(
  formula = Unemployment ~ Inflation,
  data = df_analysis,
  na.action = stats::na.exclude
)
slr_infl_summary <- base::summary(slr_infl)
base::print("【単回帰分析】 Unemployment ~ Inflation")
base::print(slr_infl_summary)

# 散布図（相関）
meta_infl_scatter <- make_meta_label(
  n = n_infl,
  p_value = cor_infl$p.value
)
plot_infl <- ggplot2::ggplot(
  data = df_analysis,
  mapping = ggplot2::aes(x = Inflation, y = Unemployment)
) +
  ggplot2::geom_point(
    color = COLOR_INFL_POINT,
    alpha = PLOT_POINT_ALPHA,
    size  = PLOT_POINT_SIZE
  ) +
  ggplot2::geom_smooth(
    method = "lm",
    color  = COLOR_INFL_LINE,
    fill   = COLOR_INFL_FILL,
    level  = CONFIDENCE_LEVEL
  ) +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title    = "Inflation vs Unemployment Rate",
    subtitle = base::paste0("Pearson Correlation: ", base::round(cor_infl$estimate, 3), "\n", meta_infl_scatter),
    x        = "Inflation, consumer prices (annual %)",
    y        = "Unemployment (% of labor force)"
  ) +
  ggplot2::theme(
    plot.title       = ggplot2::element_text(face = "bold", color = COLOR_INFL_LINE, size = 14),
    plot.subtitle    = ggplot2::element_text(color = "gray50", size = 10),
    panel.grid.minor = ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = base::file.path(out_dir, "scatter_inflation_unemployment.png"),
  plot     = plot_infl,
  width    = IMAGE_WIDTH,
  height   = IMAGE_HEIGHT,
  dpi      = IMAGE_DPI
)

# 単回帰分析(Inflation): 予測値と残差
df_analysis$Predicted_Unemp_Infl <- stats::predict(slr_infl)
df_analysis$Residuals_Infl <- stats::residuals(slr_infl)

meta_infl_reg <- make_meta_label(
  n       = n_infl,
  p_value = stats::pf(slr_infl_summary$fstatistic[1], slr_infl_summary$fstatistic[2], slr_infl_summary$fstatistic[3], lower.tail = FALSE),
  r2      = slr_infl_summary$r.squared,
  adj_r2  = slr_infl_summary$adj.r.squared
)

plot_slr_infl_ap <- ggplot2::ggplot(
  data = df_analysis,
  mapping = ggplot2::aes(x = Predicted_Unemp_Infl, y = Unemployment)
) +
  ggplot2::geom_point(color = COLOR_INFL_POINT, alpha = PLOT_POINT_ALPHA, size = PLOT_POINT_SIZE) +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "gray30", linetype = "dashed") +
  ggplot2::geom_smooth(method = "lm", color = COLOR_INFL_LINE, fill = COLOR_INFL_FILL, level = CONFIDENCE_LEVEL) +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title = "Inflation Regression: Predicted vs Actual",
    subtitle = meta_infl_reg,
    x = "Predicted Unemployment Rate (%)",
    y = "Actual Unemployment Rate (%)"
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", color = COLOR_INFL_LINE, size = 14),
    plot.subtitle = ggplot2::element_text(color = "gray50", size = 10)
  )

ggplot2::ggsave(
  filename = base::file.path(out_dir, "slr_infl_actual_vs_predicted.png"),
  plot     = plot_slr_infl_ap,
  width    = IMAGE_WIDTH,
  height   = IMAGE_HEIGHT,
  dpi      = IMAGE_DPI
)

plot_slr_infl_res <- ggplot2::ggplot(
  data = df_analysis,
  mapping = ggplot2::aes(x = Predicted_Unemp_Infl, y = Residuals_Infl)
) +
  ggplot2::geom_point(color = COLOR_INFL_POINT, alpha = PLOT_POINT_ALPHA, size = PLOT_POINT_SIZE) +
  ggplot2::geom_hline(yintercept = 0, color = COLOR_INFL_LINE, linetype = "dashed") +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title = "Inflation Regression: Residuals vs Fitted",
    subtitle = meta_infl_reg,
    x = "Fitted values (Predicted Unemployment)",
    y = "Residuals"
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", color = COLOR_INFL_LINE, size = 14),
    plot.subtitle = ggplot2::element_text(color = "gray50", size = 10)
  )

ggplot2::ggsave(
  filename = base::file.path(out_dir, "slr_infl_residuals_vs_fitted.png"),
  plot     = plot_slr_infl_res,
  width    = IMAGE_WIDTH,
  height   = IMAGE_HEIGHT,
  dpi      = IMAGE_DPI
)


# ============================================================
# 重回帰分析
# ============================================================
n_mlr <- base::sum(stats::complete.cases(df_analysis[, c("GDP_Growth", "Inflation", "Unemployment")]))

mlr_model <- stats::lm(
  formula = Unemployment ~ GDP_Growth + Inflation,
  data = df_analysis,
  na.action = stats::na.exclude
)
mlr_summary <- base::summary(mlr_model)

base::print("【重回帰分析】 Unemployment ~ GDP_Growth + Inflation")
base::print(mlr_summary)

# 予測値と残差をデータフレームに追加
df_analysis$Predicted_Unemployment <- stats::predict(mlr_model)
df_analysis$Residuals <- stats::residuals(mlr_model)

meta_mlr <- make_meta_label(
  n       = n_mlr,
  p_value = stats::pf(mlr_summary$fstatistic[1], mlr_summary$fstatistic[2], mlr_summary$fstatistic[3], lower.tail = FALSE),
  r2      = mlr_summary$r.squared,
  adj_r2  = mlr_summary$adj.r.squared
)

# 予測値 vs 実測値 のプロット
plot_mlr <- ggplot2::ggplot(
  data = df_analysis,
  mapping = ggplot2::aes(x = Predicted_Unemployment, y = Unemployment)
) +
  ggplot2::geom_point(
    color = COLOR_MLR_POINT,
    alpha = PLOT_POINT_ALPHA,
    size  = PLOT_POINT_SIZE
  ) +
  ggplot2::geom_abline(
    intercept = 0,
    slope = 1,
    color = "gray30",
    linetype = "dashed"
  ) +
  ggplot2::geom_smooth(
    method = "lm",
    color  = COLOR_MLR_LINE,
    fill   = COLOR_MLR_FILL,
    level  = CONFIDENCE_LEVEL
  ) +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title    = "Multiple Regression: Predicted vs Actual",
    subtitle = meta_mlr,
    x        = "Predicted Unemployment Rate (%)",
    y        = "Actual Unemployment Rate (%)"
  ) +
  ggplot2::theme(
    plot.title       = ggplot2::element_text(face = "bold", color = COLOR_MLR_LINE, size = 14),
    plot.subtitle    = ggplot2::element_text(color = "gray50", size = 10),
    panel.grid.minor = ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = base::file.path(out_dir, "mlr_actual_vs_predicted.png"),
  plot     = plot_mlr,
  width    = IMAGE_WIDTH,
  height   = IMAGE_HEIGHT,
  dpi      = IMAGE_DPI
)

# 残差プロット
plot_residuals <- ggplot2::ggplot(
  data = df_analysis,
  mapping = ggplot2::aes(x = Predicted_Unemployment, y = Residuals)
) +
  ggplot2::geom_point(
    color = COLOR_MLR_POINT,
    alpha = PLOT_POINT_ALPHA,
    size  = PLOT_POINT_SIZE
  ) +
  ggplot2::geom_hline(
    yintercept = 0,
    color = COLOR_MLR_LINE,
    linetype = "dashed"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title    = "Multiple Regression: Residuals vs Fitted",
    subtitle = meta_mlr,
    x        = "Fitted values (Predicted Unemployment)",
    y        = "Residuals"
  ) +
  ggplot2::theme(
    plot.title       = ggplot2::element_text(face = "bold", color = COLOR_MLR_LINE, size = 14),
    plot.subtitle    = ggplot2::element_text(color = "gray50", size = 10),
    panel.grid.minor = ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = base::file.path(out_dir, "mlr_residuals_vs_fitted.png"),
  plot     = plot_residuals,
  width    = IMAGE_WIDTH,
  height   = IMAGE_HEIGHT,
  dpi      = IMAGE_DPI
)
