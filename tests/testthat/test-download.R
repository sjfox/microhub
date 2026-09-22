test_that("download format helpers default to CSV and recognize Parquet", {
  expect_equal(normalize_download_format(NULL), "csv")
  expect_equal(normalize_download_format("csv"), "csv")
  expect_equal(normalize_download_format("CSV"), "csv")
  expect_equal(normalize_download_format("parquet"), "parquet")
  expect_equal(normalize_download_format("PARQUET"), "parquet")
  expect_equal(normalize_download_format("xlsx"), "csv")

  expect_equal(download_file_extension("csv"), "csv")
  expect_equal(download_file_extension("parquet"), "parquet")
})

test_that("download model filenames are safe and unique", {
  file_names <- download_model_file_names(
    models = c("Regular Baseline", "A/B", "A B", "!!!", NA),
    reference_date_label = "2026-09-08",
    format = "csv"
  )

  expect_equal(
    file_names,
    c(
      "Regular_Baseline_2026-09-08.csv",
      "A_B_2026-09-08.csv",
      "A_B_1_2026-09-08.csv",
      "model_2026-09-08.csv",
      "model_1_2026-09-08.csv"
    )
  )
})

test_that("write_forecast_export writes CSV files", {
  forecast <- tibble(
    model = c("A", "B"),
    reference_date = c("2026-09-08", "2026-09-08"),
    horizon = c(1, 2),
    target_end_date = c("2026-09-15", "2026-09-22"),
    target_group = c("All", "All"),
    output_type = c("quantile", "quantile"),
    output_type_id = c("0.5", "0.5"),
    value = c(10, 20)
  )
  path <- withr::local_tempfile(fileext = ".csv")

  write_forecast_export(forecast, path, "csv")

  written <- readr::read_csv(path, show_col_types = FALSE)
  expect_equal(names(written), names(forecast))
  expect_equal(nrow(written), nrow(forecast))
  expect_equal(written$model, forecast$model)
  expect_equal(as.character(written$reference_date), forecast$reference_date)
  expect_equal(written$value, forecast$value)
})

test_that("write_forecast_export writes Parquet files", {
  skip_if_not_installed("arrow")

  forecast <- tibble(
    model = c("A", "B"),
    reference_date = c("2026-09-08", "2026-09-08"),
    horizon = c(1, 2),
    target_end_date = c("2026-09-15", "2026-09-22"),
    target_group = c("All", "All"),
    output_type = c("quantile", "quantile"),
    output_type_id = c("0.5", "0.5"),
    value = c(10, 20)
  )
  path <- withr::local_tempfile(fileext = ".parquet")

  write_forecast_export(forecast, path, "parquet")

  written <- arrow::read_parquet(path)
  expect_equal(written, forecast)
})

test_that("write_forecast_exports_by_model creates one selected file per model", {
  skip_if(Sys.which("zip") == "", "zip command is not available")

  forecast <- tibble(
    model = c("Regular Baseline", "Regular Baseline", "Copycat"),
    reference_date = c("2026-09-08", "2026-09-08", "2026-09-08"),
    horizon = c(1, 2, 1),
    target_end_date = c("2026-09-15", "2026-09-22", "2026-09-15"),
    target_group = c("All", "All", "All"),
    output_type = c("quantile", "quantile", "quantile"),
    output_type_id = c("0.5", "0.5", "0.5"),
    value = c(10, 20, 30)
  )
  zip_path <- withr::local_tempfile(fileext = ".zip")
  unzip_dir <- withr::local_tempdir()

  write_forecast_exports_by_model(
    df = forecast,
    output_zip = zip_path,
    format = "csv",
    reference_date_label = "2026-09-08"
  )

  utils::unzip(zip_path, exdir = unzip_dir)
  extracted <- sort(list.files(unzip_dir))
  expect_equal(
    extracted,
    c("Copycat_2026-09-08.csv", "Regular_Baseline_2026-09-08.csv")
  )

  copycat <- readr::read_csv(
    file.path(unzip_dir, "Copycat_2026-09-08.csv"),
    show_col_types = FALSE
  )
  expect_equal(copycat$model, "Copycat")
  expect_equal(copycat$value, 30)
})

test_that("write_forecast_exports_by_model can package Parquet files", {
  skip_if(Sys.which("zip") == "", "zip command is not available")
  skip_if_not_installed("arrow")

  forecast <- tibble(
    model = c("Regular Baseline", "Copycat"),
    reference_date = c("2026-09-08", "2026-09-08"),
    horizon = c(1, 1),
    target_end_date = c("2026-09-15", "2026-09-15"),
    target_group = c("All", "All"),
    output_type = c("quantile", "quantile"),
    output_type_id = c("0.5", "0.5"),
    value = c(10, 30)
  )
  zip_path <- withr::local_tempfile(fileext = ".zip")
  unzip_dir <- withr::local_tempdir()

  write_forecast_exports_by_model(
    df = forecast,
    output_zip = zip_path,
    format = "parquet",
    reference_date_label = "2026-09-08"
  )

  utils::unzip(zip_path, exdir = unzip_dir)
  extracted <- sort(list.files(unzip_dir))
  expect_equal(
    extracted,
    c("Copycat_2026-09-08.parquet", "Regular_Baseline_2026-09-08.parquet")
  )

  copycat <- arrow::read_parquet(
    file.path(unzip_dir, "Copycat_2026-09-08.parquet")
  )
  expect_equal(copycat$model, "Copycat")
  expect_equal(copycat$value, 30)
})
