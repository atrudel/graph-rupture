devtools::install_github("vrunge/gfpop.data")
library(gfpop)
library(gfpop.data)
data(ECG, package = "gfpop.data")
library(data.table)
library(ggplot2)


# Graph construction
#------------------------------------------------------------------------------------------------
heartbeatGraph <- gfpop::graph(
  gfpop::Edge(0, 1, type = "down", penalty = 8e7, gap=0),
  gfpop::Edge(1, 2, type = "up", penalty = 0, gap=2000),
  gfpop::Edge(2, 3, type = "down", penalty = 0, gap=5000),
  gfpop::Edge(3, 4, type = "up", penalty = 0, gap=2000),
  gfpop::Edge(4, 5, type = "up", penalty = 0, gap=1000),
  gfpop::Edge(5, 6, type = "up", penalty = 0, gap=0),
  gfpop::Edge(6, 7, type = "down", penalty = 0, gap=0),
  gfpop::Edge(7, 8, type = "down", penalty = 0, gap=0),
  gfpop::Edge(8, 0, type = "up", penalty =0, gap=0),
  all.null.edges = TRUE
)

# Fit graph to signal
fitted_model <- gfpop(data = ECG$data$millivolts, mygraph = heartbeatGraph, type = "mean")


plot_qrs_modelled_signal <- function(
  fitted_model,
  dataset,
  sampling_frequency
){
  ## Build a table that describes segments
  ##
  end.i <- fitted_model$changepoints
  start.i <- c(1, end.i[-length(end.i)]+1)

  # Create initial table with 4 columns
  segments.dt <- with(dataset, data.table(
    timeStart=time[start.i],
    timeEnd=time[end.i],
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
    time=as.numeric(rbind(timeStart-0.5, timeEnd+0.5)),
    mean=as.numeric(rbind(mean, mean)) # rbind concatenates rows together
  )]


  model.dt = segments.dt[, data.table(
      time=ifelse(letter=='Q', timeEnd, (timeStart+timeEnd)/2),
      millivolts=mean,
      letter
    )]

  truth.dt <- segments.dt[letter=="R", list(time=(timeStart+timeEnd)/2)]


  # Plot
  #------------------------------------------------------------------------------------------------
  ggplot()+
    # Plot the true R spikes in red
    geom_vline(aes(
      xintercept=time/sampling_frequency),
      color="red",
      data=truth.dt)+
    geom_text(aes(
      x, y, hjust=hjust, label="True R"),
      color="red",
      size=3,
      data=data.table(
        x=208.5, y=6500, hjust=1, label="True R"))+
    theme_bw()+
    theme(panel.spacing=grid::unit(0, "lines"))+
    # Plot the ECG signal in grey
    geom_line(aes(
      time/sampling_frequency, millivolts),
      color="grey50",
      data=ECG$data)+
    # Plot the mean-based model in blue
    geom_line(aes(
      time/sampling_frequency, mean),
      data=mean.dt,
      color="blue")+
    # Plot the state labels on top of the model plot in blue
    geom_label(aes(
      time/sampling_frequency, millivolts,
      label=letter),
      color="blue",
      size=3,
      label.padding=grid::unit(0.1, "lines"),
      alpha=0.6,
      data=model.dt)+
    coord_cartesian(xlim=c(52000, 52900)/sampling_frequency, expand=FALSE)+
    xlab("Time (seconds)")+
    ylab("Electrocardiogram activity (mV)")
}

gg <- plot_qrs_modelled_signal(
  fitted_model = fitted_model,
  dataset = ECG$data,
  sampling_frequency = 250
)

show(gg)
pdf("R/ECG/gfpop_ECG.pdf")
print(gg)
dev.off()