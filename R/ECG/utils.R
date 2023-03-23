library(data.table)

# This function takes in the following arguments:
# - fitted_model: the output of the gfpop() funciton
# - signal: in the form of a dataframe with two named columns:
#   - 'timesteps': (int) indices of the timesteps
#   - 'values': (float) the values of the time series
# - sampling frequency of the signal in Hz (int)
# - x_label: the label to attach to the graph's x axis
# - y_label: the label to attach to the graph's y axis

plot_qrs_modelled_signal <- function(
  fitted_model,
  signal,
  sampling_frequency,
  x_label,
  y_label,
  state_codes,
  title
){
  ## Build a table that describes segments
  ##
  end.i <- fitted_model$changepoints
  start.i <- c(1, end.i[-length(end.i)]+1)

  # Create initial table with 4 columns
  segments.dt <- with(signal, data.table(
    timeStart = timesteps[start.i],
    timeEnd = timesteps[end.i],
    state = as.numeric(fitted_model$states),
    mean = fitted_model$parameters
  ))

  # Assign a letter to each state in a separate column
  segments.dt[, letter := state_codes[state + 1]]

  # Create a new table with two rwos per segment. For a given segment:
  # The "time" column has the beginning of the segment -0,5 on the first row
  # The "time" column gas the end of the segment + 0.5 on the first row
  # The "mean" column has a copy of the mean on both rows.
  mean.dt <- segments.dt[, data.table(
    timesteps=as.numeric(rbind(timeStart-0.5, timeEnd+0.5)),
    mean=as.numeric(rbind(mean, mean)) # rbind concatenates rows together
  )]

  model.dt <- segments.dt[, data.table(
      timesteps = ifelse(letter == 'Q', timeEnd, (timeStart+timeEnd)/2),
      values = mean,
      letter
    )]

  # Plot
  #------------------------------------------------------------------------------------------------
  ggplot()+
    theme_bw()+
    theme(panel.spacing=grid::unit(0, "lines"))+
    # Plot the ECG signal in grey
    geom_line(aes(
      timesteps/sampling_frequency, values),
      color="grey50",
      data=signal)+
    # Plot the mean-based model in blue
    geom_line(aes(
      timesteps/sampling_frequency, mean),
      data=mean.dt,
      color="blue")+
    # Plot the state labels on top of the model plot in blue
    geom_label(aes(
      timesteps/sampling_frequency, values,
      label=letter),
      color="blue",
      size=3,
      label.padding=grid::unit(0.1, "lines"),
      alpha=0.6,
      data=model.dt)+
    xlab(x_label)+
    ylab(y_label)+
    ggtitle(title)
}

scoring_state_ruptures <- function(
  signal,
  fitted_model,
  ground_truth,
  state_name,
  state_codes
){
  ## Build a table that describes segments
  ##
  end.i <- fitted_model$changepoints
  start.i <- c(1, end.i[-length(end.i)]+1)

  # Create initial table with 4 columns
  segments.dt <- with(signal, data.table(
    timeStart = timesteps[start.i],
    timeEnd = timesteps[end.i],
    state = as.numeric(fitted_model$states),
    mean = fitted_model$parameters
  ))

  # Assign a letter to each state in a separate column
  segments.dt[, letter := state_codes[state + 1]]

  found.dt <- segments.dt |> subset(letter == state_name)
  true_positiv <- 0
  
  for (time_truth in ground_truth) {
    if (nrow(found.dt |> subset(timeStart <= time_truth & time_truth <= timeEnd)) != 0){
      true_positiv <- true_positiv + 1
    }
  }
  false_pos <- nrow(found.dt) - true_positiv
  false_neg <- length(ground_truth) - true_positiv
  f1_score <- 2* true_positiv/(2*true_positiv + false_neg + false_pos)
  paste("F1 Score obtained : ", f1_score)
}
