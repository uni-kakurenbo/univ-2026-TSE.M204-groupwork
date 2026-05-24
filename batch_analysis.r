base::source("config.R")      # 色・サイズ等の定数を読み込む
base::source("preprocess.R")

# config.R の一部を上書き（バッチ用設定）
DATA_SOURCE      <- "World Bank Open Data"
CONFIDENCE_LEVEL <- 0.95

year_ranges <- base::list(
  c(1991, 2000),  # 1990年代
  c(2001, 2010),  # 2000年代
  c(2011, 2019),  # 2010年代（コロナ前）
  c(2020, 2023),  # コロナ禍以降
  c(1991, 2019),  # コロナ前の長期
  c(1991, 2023)   # 全期間
)

DATA_SOURCE      <- "World Bank Open Data"
CONFIDENCE_LEVEL <- 0.95

results <- base::list()

for (yr in year_ranges) {
  TARGET_START_YEAR <- yr[1]
  TARGET_END_YEAR   <- yr[2]
  label <- base::paste0(TARGET_START_YEAR, "-", TARGET_END_YEAR)

  base::cat("\n=== Processing:", label, "===\n")

  df_check <- tryCatch(
    preprocess_worldbank_data(
      unemp_file = "./data/unemployment.csv",
      gdp_file   = "./data/gdp-growth.csv",
      infl_file  = "./data/inflation.csv",
      start_year = TARGET_START_YEAR,
      end_year   = TARGET_END_YEAR
    ),
    error = function(e) NULL
  )

  n_check <- if (!base::is.null(df_check)) {
    base::sum(stats::complete.cases(df_check[, c("GDP_Growth", "Inflation", "Unemployment")]))
  } else 0

  if (base::is.null(df_check) || n_check < 5) {
    base::cat("SKIP:", label, "(insufficient data: N =", n_check, ")\n")
    next
  }

  # main.r を source してプロット生成（TARGET_START_YEAR / TARGET_END_YEAR は上で設定済み）
  base::source("main.r", local = FALSE)

  base::cat("PLOTS saved to:", out_dir, "\n")

  # 統計サマリーを収集
  n <- base::sum(stats::complete.cases(df_analysis[, c("GDP_Growth", "Inflation", "Unemployment")]))

  cor_gdp  <- tryCatch(stats::cor.test(df_analysis$GDP_Growth, df_analysis$Unemployment, use = "complete.obs"), error = function(e) NULL)
  cor_infl <- tryCatch(stats::cor.test(df_analysis$Inflation,  df_analysis$Unemployment, use = "complete.obs"), error = function(e) NULL)

  slr_gdp  <- tryCatch(stats::lm(Unemployment ~ GDP_Growth, data = df_analysis, na.action = stats::na.exclude), error = function(e) NULL)
  slr_infl <- tryCatch(stats::lm(Unemployment ~ Inflation,  data = df_analysis, na.action = stats::na.exclude), error = function(e) NULL)
  mlr      <- tryCatch(stats::lm(Unemployment ~ GDP_Growth + Inflation, data = df_analysis, na.action = stats::na.exclude), error = function(e) NULL)

  get_f_pval <- function(sm) {
    if (base::is.null(sm) || base::is.null(sm$fstatistic)) return(NA)
    stats::pf(sm$fstatistic[1], sm$fstatistic[2], sm$fstatistic[3], lower.tail = FALSE)
  }

  sm_gdp  <- if (!base::is.null(slr_gdp))  base::summary(slr_gdp)  else NULL
  sm_infl <- if (!base::is.null(slr_infl)) base::summary(slr_infl) else NULL
  sm_mlr  <- if (!base::is.null(mlr))      base::summary(mlr)      else NULL

  results[[label]] <- base::list(
    label           = label,
    n               = n,
    cor_gdp_r       = if (!base::is.null(cor_gdp))  base::round(cor_gdp$estimate,  4) else NA,
    cor_gdp_p       = if (!base::is.null(cor_gdp))  cor_gdp$p.value                   else NA,
    cor_infl_r      = if (!base::is.null(cor_infl)) base::round(cor_infl$estimate, 4) else NA,
    cor_infl_p      = if (!base::is.null(cor_infl)) cor_infl$p.value                  else NA,
    slr_gdp_r2      = if (!base::is.null(sm_gdp))  base::round(sm_gdp$r.squared,     4) else NA,
    slr_gdp_adj_r2  = if (!base::is.null(sm_gdp))  base::round(sm_gdp$adj.r.squared,  4) else NA,
    slr_gdp_p       = get_f_pval(sm_gdp),
    slr_infl_r2     = if (!base::is.null(sm_infl)) base::round(sm_infl$r.squared,    4) else NA,
    slr_infl_adj_r2 = if (!base::is.null(sm_infl)) base::round(sm_infl$adj.r.squared, 4) else NA,
    slr_infl_p      = get_f_pval(sm_infl),
    mlr_r2          = if (!base::is.null(sm_mlr)) base::round(sm_mlr$r.squared,      4) else NA,
    mlr_adj_r2      = if (!base::is.null(sm_mlr)) base::round(sm_mlr$adj.r.squared,   4) else NA,
    mlr_p           = get_f_pval(sm_mlr),
    mlr_coef_gdp_b  = if (!base::is.null(sm_mlr)) base::round(sm_mlr$coefficients["GDP_Growth", "Estimate"], 4) else NA,
    mlr_coef_gdp_p  = if (!base::is.null(sm_mlr)) sm_mlr$coefficients["GDP_Growth", "Pr(>|t|)"]               else NA,
    mlr_coef_infl_b = if (!base::is.null(sm_mlr)) base::round(sm_mlr$coefficients["Inflation",  "Estimate"], 4) else NA,
    mlr_coef_infl_p = if (!base::is.null(sm_mlr)) sm_mlr$coefficients["Inflation",  "Pr(>|t|)"]               else NA
  )

  base::cat("STATS: N =", n,
    "| MLR Adj.R2 =", base::round(results[[label]]$mlr_adj_r2, 3),
    "| MLR p =", base::formatC(results[[label]]$mlr_p, format = "e", digits = 2), "\n")
}

# 結果をテキストファイルに保存
output_lines <- base::c()
for (label in base::names(results)) {
  r <- results[[label]]
  sig <- function(p) if (!base::is.na(p) && p < 0.05) "*" else ""
  output_lines <- base::c(output_lines,
    base::paste0("PERIOD=",         r$label),
    base::paste0("N=",              r$n),
    base::paste0("COR_GDP_R=",      r$cor_gdp_r,      sig(r$cor_gdp_p)),
    base::paste0("COR_GDP_P=",      base::formatC(r$cor_gdp_p,  format = "e", digits = 2)),
    base::paste0("COR_INFL_R=",     r$cor_infl_r,     sig(r$cor_infl_p)),
    base::paste0("COR_INFL_P=",     base::formatC(r$cor_infl_p, format = "e", digits = 2)),
    base::paste0("SLR_GDP_R2=",     r$slr_gdp_r2,     " ADJ=", r$slr_gdp_adj_r2,  sig(r$slr_gdp_p)),
    base::paste0("SLR_INFL_R2=",    r$slr_infl_r2,    " ADJ=", r$slr_infl_adj_r2, sig(r$slr_infl_p)),
    base::paste0("MLR_R2=",         r$mlr_r2,         " ADJ=", r$mlr_adj_r2,       sig(r$mlr_p)),
    base::paste0("MLR_P=",          base::formatC(r$mlr_p, format = "e", digits = 2)),
    base::paste0("MLR_COEF_GDP=",   r$mlr_coef_gdp_b,  sig(r$mlr_coef_gdp_p)),
    base::paste0("MLR_COEF_INFL=",  r$mlr_coef_infl_b, sig(r$mlr_coef_infl_p)),
    "---"
  )
}

base::writeLines(output_lines, "batch_results.txt")
base::cat("\nAll done. Results saved to batch_results.txt\n")
