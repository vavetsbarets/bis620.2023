## code to prepare `DATASET` dataset goes here

accel = readRDS("accel.rds")
usethis::use_data(accel, overwrite = TRUE)

studies = read_parquet('studies.parquet')
usethis::use_data(studies)

calculated_values = read_parquet('calculated_values.parquet')
usethis::use_data(studies)

conditions = read_parquet('conditions.parquet')
usethis::use_data(conditions)

countries = read_parquet('countries.parquet')
usethis::use_data(countries)

reported_events = read_parquet('reported_events')
usethis::use_data(reported_events)
