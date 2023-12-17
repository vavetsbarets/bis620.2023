## code to prepare `DATASET` dataset goes here

accel = readRDS(file.path("..", "data-raw", "accel.rds"))
usethis::use_data(accel, overwrite = TRUE)

library(arrow)
library(tibble)

studies <- read_parquet(file.path("..", "data-raw", "studies.parquet")) |> as_tibble()
usethis::use_data(studies, overwrite = TRUE)

conditions <- read_parquet(file.path("..", "data-raw", "conditions.parquet")) |> as_tibble()
usethis::use_data(conditions, overwrite = TRUE)

countries <- read_parquet(file.path("..", "data-raw", "countries.parquet")) |> as_tibble()
usethis::use_data(countries, overwrite = TRUE)

calculated_values <- read_parquet(file.path("..", "data-raw", "calculated_values.parquet")) |> as_tibble()
usethis::use_data(calculated_values, overwrite = TRUE)

reported_events <- read_parquet(file.path("..", "data-raw", "reported_events.parquet")) |> as_tibble()
usethis::use_data(reported_events, overwrite = TRUE)
