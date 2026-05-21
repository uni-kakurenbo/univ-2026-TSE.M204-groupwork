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
  gdp_data   <- clean_wb_data(gdp_file, "GDP_Growth")
  infl_data  <- clean_wb_data(infl_file, "Inflation")

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

  base::return(merged_data)
}
