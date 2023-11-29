library(dplyr)
library(duckdb)
library(dplyr)
library(DBI)
library(DT)
library(ggplot2)
library(tidyr)
library(purrr)
library(dbplyr)
library(arrow)


# #### PARSING THE DATA ####

studies <- read_parquet(file.path("..", "data", "studies.parquet")) |> as_tibble()
conditions <- read_parquet(file.path("..", "data", "conditions.parquet")) |> as_tibble()
countries <- read_parquet(file.path("..", "data", "countries.parquet")) |> as_tibble()
calculated_values <- read_parquet(file.path("..", "data", "calculated_values.parquet")) |> as_tibble()
reported_events <- read_parquet(file.path("..", "data", "reported_events.parquet")) |> as_tibble()



# reading additional data for WORLD MAP FEATURE:
  # world data from package maps, to build a world map diagram!!!
# it shows countries and corresponding geographical coordinates
world_map <- map_data("world")
# removing Antarctica for better visualization
world_map <- subset(world_map, region!="Antarctica")



##### FUNCTIONS

#### FUNCTIONS MADE IN CLASS ####

#' @title Query keywords from a database table.
#' @description build a SQL query from input keywords and filter the input dataset accordingly;
#' returning void and filtering the duckDB input table
#' @param d duckDB table, input dataset
#' nxd where n is the number of the studies and d is the number of the features
#' @param kwds character, the keywords to look for
#' @param column the column to look for the keywords in
#' @param ignore_case logical, TRUE if the case has to be ignored when searching for a keyword
#' (default TRUE)
#' @param match_all logical, TRUE if we should match ANY of the keywords (union),
#' FALSE if we should match ALL of the keywords
query_kwds <- function(d, kwds, column, ignore_case = TRUE, match_all = FALSE) {
  kwds = kwds[kwds != ""]
  kwds = paste0("%", kwds, "%") |>
    gsub("'", "''", x = _)
  if (ignore_case) {
    like <- " ilike "
  } else{
    like <- " like "
  }
  query = paste(
    paste0(column, like, "'", kwds, "'"),
    collapse = ifelse(match_all, " AND ", " OR ")
  )
  filter(d, sql(query))
}

#'@title Gets the concurrent trials for each date
#'@description Counts the concurrent trials for each date in the dataset then used to plot the concurrent plot;
#'it returns a tibble `all_date` with a `date` column and a `count` of the number of concurrent trials at that date.
#'@param d tibble/table/dataframe, table containing the studies and the starting and ending dates;
#'nx2 where n is the number of filtered studies and 2 are the columns corresponding to the start and end date
get_concurrent_trials = function(d) {

  # Get all of the unique dates.
  all_dates = d |>
    pivot_longer(cols = everything()) |>
    select(-name) |>
    distinct() |>
    arrange(value) |>
    na.omit() |>
    rename(date = value)

  within_date = function(date, starts, ends) {
    date >= starts & date <= ends
  }

  # Get the number of concurrent trials at each of the unique dates.
  all_dates$count =
    map_dbl(
      all_dates$date,
      ~ .x |>
        within_date(d$start_date, d$completion_date) |>
        sum(na.rm = TRUE)
    )

  return(all_dates)
}

#### FUNCTIONS FOR PROBLEMS 1-3 ####

# PROBLEM 1: NEW PLOT_PHASE_HISTOGRAM_NEW FUNCTION
#'@title Create a histogram of the phases of the input studies
#'@description ggplot of an histogram of the phases of the studies in the input dataset,
#' in the app the dataset is filtered by the query;
#' returning void and displaying a ggplot plot
#' NB the function always returns the plot with the x-axis uniform wrt the previously performed query on the dataset
#'@param x table/tibble/data.frame, input dataset
#'nxd where n is the number of filtered studies and d is the number of features, one column
#'@param phases_all character, it is a vector storing all the possible unique values of "phase"
plot_phase_histogram_new = function(x, phases_all) {
  x$phase[is.na(x$phase)] = "NA"
  x = x |>
    select(phase) |>
    group_by(phase) |>
    summarize(n = n())

  # UPDATING THE FUNCTION, PROBLEM 1
  # adding phases which present in the database, but not in x
  # since they are not present in x, the count for them is 0
  zero_phases <- phases_all[!(phases_all %in% x$phase)]
  new_phases <- c(x$phase, zero_phases)
  new_counts <- c(x$n, rep(0, length(zero_phases)))
  # new tibble
  x_new <- tibble(phase = new_phases, n = new_counts)

  # plotting with tibble with new phases
  ggplot(x_new, aes(x = phase, y = n)) +
    geom_col() +
    theme_bw() +
    xlab("Phase") +
    ylab("Number of trials") +
    labs(title = 'Distrbution of trials by phases') +
    theme(axis.title.x = element_text(size=16),
          axis.text.x  = element_text(angle = 345, size=13),
          axis.title.y = element_text(size=16),
          axis.text.y  = element_text(size=13),
          plot.title = element_text(size = 20))
}

#'@title Supporting function for problem 1
#'@description Extracts a vector which has all the unique values of `phase` in the input
#'(unfiltered) table `studies`;
#'it was build instead of directly listing all the unique possible values of "phase" so that if the
#'dataset is updated, the has not no be changed;
#'it returns a character vector `phase_all`
#'@param studies duckDB table, input "studies" table
get_all_phases <- function(studies){

  # getting unique phases of the whole table in the tibble format
  phases_all <- studies |>
    select(phase) |>
    distinct() |>
    collect()

  # replace NA onto "NA"
  phases_all$phase[is.na(phases_all$phase)] = "NA"

  # returing it as a vector
  return(unlist(phases_all))

}



# PROBLEM 2: PLOT CONDITIONS HISTOGRAM
#'@title Create a histogram of the mot represented conditions related to the different studies
#'@description ggplot of an histogram of the conditions related to the studies in the input dataset,
#' in the app the dataset is filtered by the query, but the query acts on the table `studies` and the
#' conditions are stored in the table `conditions`, hence we left join them to filter `conditions` table according
#' to the query too
#' NB the histogram is plotted showing 10 most frequent conditions in descending order.
#' The histogram plots less than 10 conditions if there are less than 10 conditions in total after left join
#'@param studies_tibble table/tibble/data.frame, input `studies` table filtered by the query
#'nxd where n is the number of filtered studies and d is the number of features
#'@param conditions_duckdb duckDB table, input `conditions` table
#'which is defined at the very beginning of this file. It is
#'enough to give as an input `conditions` table with the columns `nct_id` and `names` only
plot_conditions <- function(studies_tibble, conditions_duckdb){

  # converting conditions_duckdb to tibble, so that we can join it with studies_tibble
  conditions_tibble <- conditions_duckdb |>
    select(nct_id, name) |>
    collect()

  # joining conditions and studies and plotting at most top 10 the most frequent conditions
  left_join(studies_tibble, conditions_tibble, by = 'nct_id') |>
    select(nct_id, name) |>
    group_by(name) |>
    summarize(n = n()) |>
    arrange(desc(n)) |>
    head(10) |>
    ggplot(aes(x = reorder(name, -n), y = n)) +
    geom_col() +
    xlab("Conditions") +
    ylab("Number of trials") +
    theme_bw() +
    labs(title = 'Most frequent conditions studied') +
    theme(axis.title.x = element_text(size=16),
          axis.text.x  = element_text(angle = 348, size=12),
          axis.title.y = element_text(size=16),
          axis.text.y  = element_text(size=13),
          plot.title = element_text(size = 20))
}


# Note PROBLEM 3: we don't need any function to be defined here. We just
  # implemented the dropdown in the UI part of the app file


##### PROBLEM 4: NEW FEATURES #####

#### ADVERSE EVENTS FEATURE
#'@title Create a stacked barplot of the most frequent adverse events
#'@description ggplot of a stacked barplot of the most frequent adverse events
#' related to the studies in the input dataset, stacked on the grade (serious and nonserious) of the event;
#' in the app the dataset is filtered by the query, but the query acts on the table `studies` and the
#' adverse events are stored in the table `reported_events`, hence we left join them to filter accordingly
#' to the query the table `reported_events` too
#' returns void and display a ggplot
#' NB the histogram is plotted always showing the 10 most frequent adverse events
#' Note, some studies have no adverse events. Thus,
#' for such studies nothing be shown on the plot, but the number of such studies with no adverse events
#' will be shown in the title of the plot
#'@param studies table/tibble/data.frame, input `studies` table filtered by the query
#'nxd where n is the number of filtered studies and d is the number of features
#'@param reported_events table/tibble/data.frame, input `reported_events` table
#'nxd where n is the number of filtered studies and d is the number of features, it is
#'enough to give as an input `reported_events` with the columns `nct_id`, `organ_system` and `event_type`
plot_adverse_events <- function(studies, reported_events){

  # right join since we want to match all the selected `studies` with the multiple rows
  # corresponding to a study in reported_events: we keep all observations in `studies` matched and
  # unmatched (there should be no unmatched study, but even if so, i.e. there is a
  # study with no adverse events disclosed, the extra line added does not affect
  # the count) and all the observation in `reported_events` that matches `studies` nct_id;
  # we use right_join to be able to set `copy=TRUE`, so that the join does not give an error
  # and we get a faster render than collecting `reported_events`
  reported_events_with_missing <- right_join(reported_events, studies, by = 'nct_id', copy = TRUE)

  # now we should remove observations with missing data.
    # In other words, we remove studies where there are no adverse events
    # as no adverse events were joined to the trial/study
  reported_events <- reported_events_with_missing |>
    filter(!is.na(event_type)) |>
    filter(!is.na(organ_system))

  # now we can calculate for how many studies there are no adverse events
  studies_all_count <- reported_events_with_missing |>
    select(nct_id) |> distinct() |> summarise(n()) |> collect() |> as.numeric()
  studies_no_missing_count <- reported_events |>
    select(nct_id) |> distinct() |> summarise(n()) |> collect() |>  as.numeric()
  # number of studies with no adverse events
  studies_no_events_count <- studies_all_count - studies_no_missing_count

  # now preparing data further for the stacked barplot plotting
  x <- reported_events |>
    select(organ_system,event_type) |>
    group_by(organ_system, event_type) |>
    summarise(n = n())

  xx <- reported_events |>
    select(organ_system) |>
    group_by(organ_system) |>
    summarise(n = n()) |>
    head(10) |>
    ungroup() |>
    select(organ_system)


  x <- right_join(x, xx, by = "organ_system")

  x$event_type[x$event_type == "other"] <- "nonserious"

  # now plotting, but first creating plot title which shows the number of studies with
    # no adverse events
  title_adverse_event <- paste("Most frequent adverse events.\nTrials with no adverse events:",
                               studies_no_events_count)
  ggplot(x, aes(fill=event_type, y=n, x=organ_system)) +
    geom_bar(position="stack", stat="identity") +
    coord_flip() +
    labs(fill = "Grade") +
    xlab("") +
    ylab("Counts") +
    labs(title = title_adverse_event) +
    theme(axis.title.x = element_text(size=16),
          axis.text.x  = element_text(size=12),
          axis.title.y = element_text(size=16),
          axis.text.y  = element_text(size=12),
          plot.title = element_text(size = 15),
          legend.title=element_text(size=12),
          legend.text=element_text(size=12))

}


#### WORLD MAP FEATURE
#'@title Create a world map heatmap which shows the number of trials per country
#'@description heatmap on the world map, which shows the distribution of the studies
#' from the input dataset across the world;
#' in the app the dataset is filtered by the query, but the query acts on the table `studies`.
#' However, there is no information about the countries, in
#' which the sites of trials are located. The information about countries are stored in `countries` table,
#' hence we left join this table to add this information and filter accordingly
#' Also we need `world_map` dataset from the maps package to build the actual world map.
#' Note, for some studies the information about country is missing in the `countries` table. Thus,
#' such observations will not be shown on the map, but the number of such observations with missing data
#' will be shown in the title of the plot
#'@param studies_filt table/tibble/data.frame, input `studies` table filtered by the query
#'nxd where n is the number of filtered studies and d is the number of features
#'@param countries_duckdb duckDB table, input `countries` table
#'which is defined at the very beginning of this file. It is
#'enough to give as an input `countries` table with the columns `nct_id` and `name` only
#'@param world_map data.frame, input `world_map` data.frame, which is defined at
#'the very beginning of this file. This data.frame is needed to build a world map using geom_map
plot_world_map <- function(studies_filt, countries_duckdb, world_map){

  # creating and collecting tibble, so that it can be joined with the studies_filt
  countries_tibble <- countries_duckdb |>
    select(nct_id, name) |>
    collect()

  # joining get_studies and countries
  countries_studies <- left_join(studies_filt, countries_tibble, by = 'nct_id')

  # collecting number of observations where the country information is missing
  data_missing_count <- countries_studies |>
    filter(is.na(name)) |>
    summarize(n()) |>
    as.numeric()

  # getting rid of na values for country column and returning number of observations for each country
    # such table will be used to plot a heatmap
  countries_counts <- countries_studies |>
    filter(!is.na(name)) |>
    group_by(name) |>
    summarise(n = n())

  # renaming some country names in ctgov data to the name in map data, so that
    # country name in ctgov data and map data are aligned,
    # so that the world map heatplot is build in a valid way, and no countries are missed
  countries_counts$name[countries_counts$name == "United States"] <- "USA"
  countries_counts$name[countries_counts$name == "Antigua and Barbuda"] <- "Antigua"
  countries_counts$name[countries_counts$name == "Brunei Darussalam"] <- "Brunei"
  countries_counts$name[countries_counts$name == "Congo"] <- "Republic of Congo"
  countries_counts$name[countries_counts$name == "Congo, The Democratic Republic of the"] <- "Democratic Republic of the Congo"
  countries_counts$name[grep('Ivoire', countries_counts$name)] <- "Ivory Coast"
  countries_counts$name[countries_counts$name == "Czechia"] <- "Czech Republic"
  countries_counts$name[countries_counts$name == "Federated States of Micronesia"] <- "Micronesia"
  # I am gonna mark all former Yugoslavian territories as Serbia,
  # which was a part of Yugoslavia
  # because in the world map there is no Yugoslavia anymore
  countries_counts$name[countries_counts$name == "Former Serbia and Montenegro"] <- "Serbia"
  countries_counts$name[countries_counts$name == "Former Yugoslavia"] <- "Serbia"
  # I'll mark Gibraltar as UK, as there is no Gibraltar in map data,
  # and it's considered as British overseas territory
  countries_counts$name[countries_counts$name == "Gibraltar"] <- "UK"
  countries_counts$name[countries_counts$name == "South Georgia and the South Sandwich Islands"] <- "South Georgia"
  countries_counts$name[countries_counts$name == "Holy See (Vatican City State)"] <- "Vatican"
  # Since no Hong Kong, Macau in world map,
  # I have to rename it as China so that these trials appear on the map
  countries_counts$name[countries_counts$name == "Hong Kong"] <- "China"
  countries_counts$name[countries_counts$name == "Macau"] <- "China"
  countries_counts$name[countries_counts$name == "Iran, Islamic Republic of" ] <- "Iran"
  countries_counts$name[countries_counts$name == "Korea, Democratic People's Republic of"] <- "North Korea"
  countries_counts$name[countries_counts$name == "Korea, Republic of" ] <- "South Korea"
  countries_counts$name[countries_counts$name == "Lao People's Democratic Republic"] <- "Laos"
  countries_counts$name[countries_counts$name == "Libyan Arab Jamahiriya"] <- "Libya"
  countries_counts$name[countries_counts$name == "Macedonia, The Former Yugoslav Republic of"] <- "North Macedonia"
  countries_counts$name[countries_counts$name == "Moldova, Republic of"] <- "Moldova"
  countries_counts$name[countries_counts$name == "Netherlands Antilles"] <- "Netherlands"
  countries_counts$name[countries_counts$name == "Macedonia" ] <- "North Macedonia"
  countries_counts$name[countries_counts$name == "Palestinian Territories, Occupied"] <- "Palestine"
  countries_counts$name[countries_counts$name == "Palestinian Territory, occupied"] <- "Palestine"
  countries_counts$name[grep('union', countries_counts$name)] <- "Reunion"
  countries_counts$name[countries_counts$name == "Russian Federation" ] <- "Russia"
  countries_counts$name[countries_counts$name == "Saint Kitts and Nevis"] <- "Saint Kitts"
  countries_counts$name[countries_counts$name == "Saint Vincent and the Grenadines"] <- "Saint Vincent"
  countries_counts$name[countries_counts$name == "Syrian Arab Republic"] <- "Syria"
  countries_counts$name[countries_counts$name == "The Democratic Republic of the Congo"] <- "Democratic Republic of the Congo"
  countries_counts$name[countries_counts$name == "Trinidad and Tobago"] <- "Tobago"
  countries_counts$name[countries_counts$name == "United Kingdom" ] <- "UK"
  countries_counts$name[countries_counts$name == "United States Minor Outlying Islands"] <- "USA"
  countries_counts$name[countries_counts$name == "Virgin Islands (U.S.)"] <- "Virgin Islands"

  # ggplot FUNCTION WHICH PLOTS THE WORLD MAP
  # https://stackoverflow.com/questions/61838700/query-on-how-to-make-world-heat-map-using-ggplot-in-r
  # first creating the title which will also show the number of observations with missing
    # information about country (these observations are not plotted by heatmap)
  # first the world map is plotted then the heatmap assigns a color to each country based on the count
    # in the countries_counts table. If some country is absent, it stays white in the final plot
  title_world_map <- paste('World map. Trials with missing data:', data_missing_count)
  ggplot(countries_counts) +
    geom_map(
      dat = world_map, map = world_map, aes(map_id = region),
      fill = "white", color = "#7f7f7f", size = 0.25
    ) +
    geom_map(map = world_map, aes(map_id = name, fill = log(n, 10)), linewidth = 0.25) +
    scale_fill_gradient(low = "#ffff00", high = "#ff0000", name = "Number of Trials,\n log10") +
    expand_limits(x = world_map$long, y = world_map$lat) +
    labs(title = title_world_map) +
    theme(plot.title = element_text(size = 15),
          legend.title=element_text(size=12),
          legend.text=element_text(size=12))
}


# SCATTERPLOT
#'@title Create scatterplot which shows how detailed/precise the trial is
#'@description scatterplot, which shows the distribution of the trial enrollment (logarithm, base 10),
#' and the number of total outcomes (sum of primary, secondary and other outomes)
#' used to measure the results of the trial. The data is also clustered
#' by the sponsor type;
#' in the app the dataset is filtered by the query, but the query acts on the table `studies`.
#' This table also contains information about enrollment and sposor_type in the columns `enrollment` and
#' `source_class` respectively
#' However, there is no information about the number of outcomes specified by each trial. This information
#' is stored in `calculated_tables` table,
#' hence we left join this table to add this information and filter accordingly.
#' Note, for some studies the information about enrollment or sponsor type is missing. Thus,
#' such observations will not be shown on the scatterplot, but the number of such observations with missing data
#' will be shown in the title of the plot
#'@param studies_filt table/tibble/data.frame, input `studies` table filtered by the query
#'nxd where n is the number of filtered studies and d is the number of features
#'@param calculated_values duckDB table, input `calculated_values` table
#'which is defined at the very beginning of this file. It is
#'enough to give as an input `calculated_values` table with the columns `nct_id`,
#' `number_of_primary_outcomes_to_measure`, `number_of_secondary_outcomes_to_measure` and
#' `number_of_other_outcomes_to_measure` only
plot_detailedness <- function(studies_filt, calculated_values){
  # getting the number of total outcomes for each trial
    # by summing up the number of primary, secondary and other outcomes
  # also the data is collected to the tibble so that it can be joined with studies tibble
  total_outcomes_data <- calculated_values |>
    select(nct_id,
           number_of_primary_outcomes_to_measure,
           number_of_secondary_outcomes_to_measure,
           number_of_other_outcomes_to_measure) |>
    replace_na(list(number_of_primary_outcomes_to_measure = 0,
                    number_of_secondary_outcomes_to_measure = 0,
                    number_of_other_outcomes_to_measure = 0)) |>
    collect() |>
    mutate(total_outcomes = rowSums(across(where(is.numeric)))) |>
    select(nct_id, total_outcomes)

  # joining number of outcomes, enrollment and sponsor type together
    # for each trial in get_studies() (aka studies_filt)
    #(enrollment and sponsor type are already in get_studies())
  detailedness_data <- left_join(studies_filt, total_outcomes_data, by = 'nct_id') |>
    select(nct_id, enrollment, total_outcomes, source_class)

  # saving number of observations, including na
  obs_with_na <- detailedness_data |> summarize(n = n()) |> as.numeric()

  # deleting missing data
  detailedness_data <- detailedness_data |>
    # have to get rid of na in enrollment, since not sure how to fill missing data there
    filter(!is.na(enrollment)) |>
    # similarly filtering out na source class since we don't know how we should fill in these values.
      # as unknown my mean that the sponsor does not want to be known and na is just missing info,
      # we cannot replace na onto Unknown
    filter(!is.na(source_class))

  # saving number of observations, without na
  obs_no_na <- detailedness_data |> summarize(n = n()) |> as.numeric()

  # missing data observations count
  missing_data_num <- obs_with_na - obs_no_na

  # now renaming sponsor types, so that the names are aligned with
    # the sponsor type names in the dropdown of the map
  detailedness_data$source_class[detailedness_data$source_class == "FED"] <- "Federal"
  detailedness_data$source_class[detailedness_data$source_class == "INDUSTRY"] <- "Industry"
  detailedness_data$source_class[detailedness_data$source_class == "INDIV"] <- "Individual"
  detailedness_data$source_class[detailedness_data$source_class == "NETWORK"] <- "Network"
  detailedness_data$source_class[detailedness_data$source_class == "OTHER"] <- "Other"
  detailedness_data$source_class[detailedness_data$source_class == "OTHER_GOV"] <- "Other_gov"
  detailedness_data$source_class[detailedness_data$source_class == "UNKNOWN"] <- "Unknown"
  # plotting the scatterplot
  # but first setting the title which includes the information about the missing data
  title_detailedness <- paste("Detailedness information by trial. Trials with missing data:", missing_data_num)
  ggplot(detailedness_data, aes(x = total_outcomes, y = 0.1 + log(enrollment, 10), col = source_class)) +
    geom_point() +
    labs(title=title_detailedness,
         x ="Total number of outcomes", y = "Enrollment, log10",
         col = "Sponsor Type") +
    theme(axis.title.x = element_text(size=16),
          axis.text.x  = element_text(size=12),
          axis.title.y = element_text(size=16),
          axis.text.y  = element_text(size=12),
          plot.title = element_text(size = 15),
          legend.title=element_text(size=12),
          legend.text=element_text(size=12))
}

