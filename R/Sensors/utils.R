library(data.table)

# This function takes in the following arguments:
# - fitted_model: the output of the gfpop() funciton
# - signal: in the form of a dataframe with two named columns:
#   - 'timesteps': (int) indices of the timesteps
#   - 'temperature': (float) the values of the time series
# - x_label: the label to attach to the graph's x axis
# - y_label: the label to attach to the graph's y axis

plot_fault_modelled_signal <- function(
  fitted_model,
  signal,
  x_label,
  y_label,
  title
){
  ## Build a table that describes segments
  end.i <- fitted_model$changepoints
  start.i <- c(1, end.i[-length(end.i)]+1)

  segments.dt <- with(signal, data.table(
    timeStart = timesteps[start.i],
    timeEnd = timesteps[end.i],
    state = fitted_model$states,
    mean = fitted_model$parameters
  ))


  # Create a new table with two rwos per segment. For a given segment:
  # The "time" column has the beginning of the segment -0,5 on the first row
  # The "time" column gas the end of the segment + 0.5 on the first row
  # The "mean" column has a copy of the mean on both rows.
  mean.dt <- segments.dt[, data.table(
    timesteps=as.numeric(rbind(timeStart-0.5, timeEnd+0.5)),
    mean=as.numeric(rbind(mean, mean)) # rbind concatenates rows together
  )]

  model.dt <- segments.dt[, data.table(
      timesteps = ifelse(state == 'SHORT', timeEnd, (timeStart+timeEnd)/2),
      values = mean,
      state
    )]

  # Plot
  #------------------------------------------------------------------------------------------------
  ggplot()+
    theme_bw()+
    theme(panel.spacing=grid::unit(0, "lines"))+
    # Plot the temperature signal in grey
    geom_line(aes(
      timesteps, temperature),
      color="grey50",
      data=signal)+
    # Plot the mean-based model in blue
    geom_line(aes(
      timesteps, mean),
      data=mean.dt,
      color="blue")+
    # Plot the state labels on top of the model plot in blue
    geom_label(aes(
      timesteps, values,
      label=state),
      color="blue",
      size=3,
      label.padding=grid::unit(0.1, "lines"),
      alpha=0.6,
      data=model.dt)+
    xlab(x_label)+
    ylab(y_label)+
    ggtitle(title)
}
