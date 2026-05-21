base::source("config.R")
base::source("preprocess.R")

df_analysis <- preprocess_worldbank_data(
  unemp_file = "./data/unemployment.csv",
  gdp_file   = "./data/gdp-growth.csv",
  infl_file  = "./data/inflation.csv",
  start_year = TARGET_START_YEAR,
  end_year   = TARGET_END_YEAR
)

cor_gdp <- stats::cor.test(
  x = df_analysis$GDP_Growth,
  y = df_analysis$Unemployment,
  use = "complete.obs",
  conf.level = CONFIDENCE_LEVEL
)
base::print(base::paste("【GDP成長率 vs 失業率】相関係数 (r) =", base::round(cor_gdp$estimate, 3), " / p値 =", base::signif(cor_gdp$p.value, 3)))

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
    subtitle = base::paste0("Pearson Correlation: ", base::round(cor_gdp$estimate, 3), " (", TARGET_START_YEAR, "-", TARGET_END_YEAR, ")"),
    x        = "GDP Growth (annual %)",
    y        = "Unemployment (% of labor force)"
  ) +
  ggplot2::theme(
    plot.title       = ggplot2::element_text(face = "bold", color = COLOR_GDP_LINE, size = 14),
    plot.subtitle    = ggplot2::element_text(color = "gray50"),
    panel.grid.minor = ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = "scatter_gdp_unemployment.png",
  plot     = plot_gdp,
  width    = IMAGE_WIDTH,
  height   = IMAGE_HEIGHT,
  dpi      = IMAGE_DPI
)


cor_infl <- stats::cor.test(
  x = df_analysis$Inflation,
  y = df_analysis$Unemployment,
  use = "complete.obs",
  conf.level = CONFIDENCE_LEVEL
)
base::print(base::paste("【インフレ率 vs 失業率】相関係数 (r) =", base::round(cor_infl$estimate, 3), " / p値 =", base::signif(cor_infl$p.value, 3)))

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
    subtitle = base::paste0("Pearson Correlation: ", base::round(cor_infl$estimate, 3), " (", TARGET_START_YEAR, "-", TARGET_END_YEAR, ")"),
    x        = "Inflation, consumer prices (annual %)",
    y        = "Unemployment (% of labor force)"
  ) +
  ggplot2::theme(
    plot.title       = ggplot2::element_text(face = "bold", color = COLOR_INFL_LINE, size = 14),
    plot.subtitle    = ggplot2::element_text(color = "gray50"),
    panel.grid.minor = ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = "scatter_inflation_unemployment.png",
  plot     = plot_infl,
  width    = IMAGE_WIDTH,
  height   = IMAGE_HEIGHT,
  dpi      = IMAGE_DPI
)
