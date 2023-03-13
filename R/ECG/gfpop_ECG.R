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
fit <- gfpop(data = ECG$data$millivolts, mygraph = heartbeatGraph, type = "mean")


# Build a table that describes segments
#------------------------------------------------------------------------------------------------
end.i <- fit$changepoints
start.i <- c(1, end.i[-length(end.i)]+1)

# Create initial table with 4 columns
segments.dt <- with(ECG$data, data.table(
  timeStart=time[start.i],
  timeEnd=time[end.i],
  state=as.numeric(fit$states),
  mean=fit$parameters
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

samples.per.second <- 250
truth.dt <- segments.dt[letter=="R", list(time=(timeStart+timeEnd)/2)]


# Plot
#------------------------------------------------------------------------------------------------
gg <- ggplot()+
  geom_vline(aes(
    xintercept=time/samples.per.second),
    color="red",
    data=truth.dt)+
  # Plot the true R spikes in red
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
    time/samples.per.second, millivolts),
    color="grey50",
    data=ECG$data)+
  # Plot the mean-based model in blue
  geom_line(aes(
    time/samples.per.second, mean),
    data=mean.dt,
    color="blue")+
  # Plot the state labels on top of the model plot in blue
  geom_label(aes(
    time/samples.per.second, millivolts,
    label=letter),
    color="blue",
    size=3,
    label.padding=grid::unit(0.1, "lines"),
    alpha=0.6,
    data=model.dt)+
  coord_cartesian(xlim=c(52000, 52900)/samples.per.second, expand=FALSE)+
  xlab("Time (seconds)")+
  ylab("Electrocardiogram activity (mV)")

pdf("R/ECG/gfpop_ECG.pdf")
print(gg)
dev.off()