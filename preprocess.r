preprocess_worldbank_data <- function(unemp_file, gdp_file, infl_file, start_year, end_year) {
  clean_wb_data <- function(file_path, value_name) {
    df <- readr::read_csv(file_path, show_col_types = FALSE)

    df_long <- tidyr::pivot_longer(
      data = df,
      cols = tidyselect::matches("^[0-9]{4}$"),
      names_to = "Year",
      values_to = value_name,
      values_drop_na = TRUE
    )

    df_long <- dplyr::mutate(df_long, Year = base::as.numeric(Year))
    df_long <- dplyr::select(
      df_long,
      `Country Code`,
      Year,
      tidyselect::all_of(value_name)
    )

    base::return(df_long)
  }

  unemp_data <- clean_wb_data(unemp_file, "Unemployment")
  gdp_data <- clean_wb_data(gdp_file, "GDP_Growth")
  infl_data <- clean_wb_data(infl_file, "Inflation")

  merged_data <- dplyr::inner_join(
    unemp_data,
    gdp_data,
    by = base::c("Country Code", "Year")
  )
  merged_data <- dplyr::inner_join(
    merged_data,
    infl_data,
    by = base::c("Country Code", "Year")
  )

  merged_data <- dplyr::filter(
    merged_data,
    Year >= start_year & Year <= end_year
  )

  remove_outliers <- function(data, col_name) {
    q1 <- stats::quantile(data[[col_name]], 0.25, na.rm = TRUE)
    q3 <- stats::quantile(data[[col_name]], 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower_bound <- q1 - 1.5 * iqr
    upper_bound <- q3 + 1.5 * iqr

    data <- data[base::is.na(data[[col_name]]) | (data[[col_name]] >= lower_bound & data[[col_name]] <= upper_bound), ]
    base::return(data)
  }

  merged_data <- remove_outliers(merged_data, "Unemployment")
  merged_data <- remove_outliers(merged_data, "GDP_Growth")
  merged_data <- remove_outliers(merged_data, "Inflation")

  base::return(merged_data)
}
