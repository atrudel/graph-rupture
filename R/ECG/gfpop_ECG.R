# devtools::install_github("vrunge/gfpop.data")
library(gfpop)
library(gfpop.data)
data(ECG, package = "gfpop.data")
library(ggplot2)
source("R/ECG/utils.R")


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
state_codes <- c("beforeQ", "Q", "R", "S", "S1", "S2", "peak", "afterPeak", "foo")

signal <- ECG$data
colnames(signal) <- c("timesteps", "values")


# Fit graph to signal
fitted_model <- gfpop(data = signal$values, mygraph = heartbeatGraph, type = "mean")

gg <- plot_qrs_modelled_signal(
  fitted_model = fitted_model,
  signal = signal,
  sampling_frequency = 250,
  x_label = "Time (seconds)",
  y_label = "Electrocardiogram activity (mV)",
  state_codes = state_codes
)

show(gg)
# pdf("R/ECG/figures/gfpop_ECG.pdf")
# print(gg)
# dev.off()