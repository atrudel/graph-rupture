plot_qrs_modelled_signal <- function(
  fitted_model,
  signal,
  sampling_frequency,
  x_label,
  y_label
){
  ## Build a table that describes segments
  ##
  end.i <- fitted_model$changepoints
  start.i <- c(1, end.i[-length(end.i)]+1)

  # Create initial table with 4 columns
  segments.dt <- with(signal, data.table(
    timeStart=timesteps[start.i],
    timeEnd=timesteps[end.i],
    state=as.numeric(fitted_model$states),
    mean=fitted_model$parameters
  ))

  # Assign a letter to each state in a separate column
  segments.dt[, letter := c("beforeQ", "Q", "R", "S", "S1", "S2", "peak", "afterPeak", "foo")[state+1] ]

  # Create a new table with two rwos per segment. For a given segment:
  # The "time" column has the beginning of the segment -0,5 on the first row
  # The "time" column gas the end of the segment + 0.5 on the first row
  # The "mean" column has a copy of the mean on both rows.
  mean.dt <- segments.dt[, data.table(
    timesteps=as.numeric(rbind(timeStart-0.5, timeEnd+0.5)),
    mean=as.numeric(rbind(mean, mean)) # rbind concatenates rows together
  )]

  model.dt <- segments.dt[, data.table(
      timesteps=ifelse(letter=='Q', timeEnd, (timeStart+timeEnd)/2),
      values=mean,
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
    ylab(y_label)
}