
# FUNCTION 1
#'@title Create a geographical map with the geographical distribution of the variable
#'@description this function will print the geographical distribution of some
#' specified variable over the region covered by the data by showing the map
#' of the region filled with the color corresponding the value of the
#' specified variable in the certain region
#'@param data sf_tibble/sf_data.frame, input sf dataset which must contain special
#' geometry column so that the geographical plot can be made
#'@param vis_col character, the variable (column of the data) to be analysed
#'@importFrom ggplot2 geom_sf theme_bw
#'@importFrom viridis scale_fill_viridis
#'@export
geographical_visualizer <- function(data, vis_col){
  geo_plot <- ggplot(data, aes(geometry = geometry, fill = !!as.name(vis_col))) +
    geom_sf() +
    scale_fill_viridis() +
    theme_bw()
  return(geo_plot)
}


# FUNCTION 2
#'@title Create the features from the geometric column for the further analysis
#'@description this function calculates the distances to each US state for each
#' US county by taking the minimum across distances between the center of some
#' county and centers of all counties in the state
#'@param county_data sf_tibble/sf_data.frame, input sf dataset which must contain special
#' geometry column
#'@importFrom sf st_distance
#'@importFrom dplyr filter
#'@export
feature_extraction <- function(county_data){
  states_vec <- county_data$STATE |> unique()
  county_num <- dim(county_data)[1]

  for (state in states_vec){
    df_state <- county_data |>
      filter(STATE == state)

    vec_min_dist <- st_distance(county_data$centr_coord, df_state$centr_coord) |>
      as.numeric() |>
      matrix(nrow = county_num) |>
      apply(1, min)

    colname <- paste('dist_to_', state, sep="")
    county_data[colname] <- vec_min_dist
  }

  return(county_data)
}


# FUNCTION 3
#'@title Transforming the data to the variables used in the modelling
#'@description this function transforms the data to create the final variables
#' which will be used in the further modelling.
#'@param df data.frame, dataset to be transformed
#'@importFrom dplyr group_by mutate
#'@importFrom stats sd
#'@export
transformation_modeling <- function(df){
  # zero in the dist means that the county is in this state
  # we only want to estimate the effect of the state on other states
  # so I replace 0 distance with 1000000 so that the final covariate for such values is 0
  # I will include the dummy variable for the states which
    # will show the average (not weighted) unemployment in the state
  # 'if no other states nearby'
  df[6:dim(df)[2]][df[6:dim(df)[2]] <= 0.01] <- 300000

  # capping etc
  df[6:dim(df)[2]][df[6:dim(df)[2]] >= 300000] <- 300000
  # then for dist columns do log |> * -1 |> + (-1 * max value)
  # calc unemployment rate, dummy for the states and z score unemployment rate normalizing by state!!!
  df_model <- log(300001) - log(1 + df[6:dim(df)[2]])

  states <- df$STATE |> unique()
  proximity_colnames <- rep('', length(states))
  for (i in 1:length(states)){
    colname <- paste('proximity_to_', states[i], sep = '')
    proximity_colnames[i] <- colname
  }
  colnames(df_model) <- proximity_colnames

  # adding back state and unemployment rate columns
  df_model$STATE <- df$STATE
  df_model$unemployment_rate <- df$unemployment_rate

  # adding dummy variables
  for (i in states){
    colname <- paste('in_', i, sep = '')
    df_model[colname] <- ifelse(df_model$STATE == i, 1, 0)
  }

  # adding mean and sd value of unemployment in the state (across counties)
  df_model <- df_model |>
    group_by(STATE) |>
    mutate(avg_state_unemp = mean(unemployment_rate)) |>
    mutate(sd_state_unemp = sd(unemployment_rate))

  return(df_model)
}


# FUNCTION 4
#'@title Forward selection for the linear regression model
#'@description this function performs forward selection from the set of variables
#' specified by user. The user also specifies the y columns and the dataset which contains
#' both y and X columns. The function returns the dataframe where each row is the iteration
#' var columns is the optimal variable added at the forward selection iteration
#' also the dataset shows AIC, r2 and r2_adj of the model after optimal variable
#' (optimal at the iteration) was added to the model
#'@param df data.frame, dataset which has all the data needed for the modelling
#'@param X_cols vector of characters, set of X variables to select from
#'@param y_col character, y column/target
#'@importFrom stats AIC lm as.formula
#'@export
forward_selection_linreg <- function(df, X_cols, y_col){
  vars <- c('const')
  AICs <- c()
  rsqs <- c()
  adjrsqs <- c()

  formulafs <- paste(y_col, ' ~ 1')
  modelfs <- lm(formulafs, data = df)

  AICs <- c(AICs, AIC(modelfs))
  rsqs <- c(rsqs, summary(modelfs)$r.squared)
  adjrsqs <- c(adjrsqs, summary(modelfs)$adj.r.squared)

  start_formula <- paste(y_col, ' ~ ')
  X_cols_left <- X_cols

  for (i in 1:length(X_cols)){
    AIC_opt <- 1e10
    for(var in X_cols_left){
      if (i == 1){
        formula <- paste(start_formula, var)
      } else {
        formula <- paste(start_formula, ' + ', var)
      }
      modelfs <- lm(as.formula(formula), data = df)
      AIC_cand <- AIC(modelfs)
      if (AIC_cand < AIC_opt){
        AIC_opt <- AIC_cand
        var_opt <- var
        rsq_opt <- summary(modelfs)$r.squared
        adjrsq_opt <- summary(modelfs)$adj.r.squared
        formula_opt <- formula
      }
    }

    vars <- c(vars, var_opt)
    AICs <- c(AICs, AIC_opt)
    rsqs <- c(rsqs, rsq_opt)
    adjrsqs <- c(adjrsqs, adjrsq_opt)

    start_formula <- formula_opt
    X_cols_left <- X_cols_left[X_cols_left != var_opt]
  }

  # choosing the set with the smallest AIC, but returning the whole information
  fs_df <- data.frame(var = vars, aic = AICs, rqs = rsqs, adjrsq = adjrsqs)
  return(fs_df)
}




















