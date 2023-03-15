library(gfpop)
library(farff)
library(tidyverse)
library(ggplot2)
library(reshape2)
source("./ECG/utils.R")


# 4045 signals of 188
heartbeats <- read_csv("../data/ECG_kaggle/ptbdb_normal.csv", col_names = FALSE)
sampling_frequency <- 125

# Function that extracts a signal from the heartbeat dataset
extract_signal <- function(signal_index, heartbeats) {
  signal_length <- ncol(heartbeats)
  signal <- as.data.frame(t(heartbeats[signal_index,]))
  colnames(signal)[1] <- "values"
  signal$timesteps <- seq(1, signal_length, 1) #/ sampling_frequency
  signal
}

# Select signal index to display
idx <- 1
signal <- extract_signal(idx, heartbeats)
# Remove the first two steps that correspond to an incomplete peak at the beginning of the signal
signal <- signal[-c(1,2),]

# Create the graph
scaling_factor <- 1 # Scale the gaps by this factor to transfer the graph from one dataset to another
qrs_graph <- gfpop::graph(
  gfpop::Edge(0, 1, type = "down", penalty = 8e7, gap=0),
  gfpop::Edge(1, 2, type = "up", penalty = 0, gap=2*scaling_factor),
  gfpop::Edge(2, 3, type = "down", penalty = 0, gap=5*scaling_factor),
  gfpop::Edge(3, 4, type = "up", penalty = 0, gap=2*scaling_factor),
  gfpop::Edge(4, 5, type = "up", penalty = 0, gap=1*scaling_factor),
  gfpop::Edge(5, 6, type = "up", penalty = 0, gap=0),
  gfpop::Edge(6, 7, type = "down", penalty = 0, gap=0),
  gfpop::Edge(7, 8, type = "down", penalty = 0, gap=0),
  gfpop::Edge(8, 0, type = "up", penalty =0, gap=0),
  all.null.edges = TRUE
)
fitted_model <- gfpop(data=signal$values, mygraph = qrs_graph, type="mean")

# Plot the signal
gg <- plot_qrs_modelled_signal(
  fitted_model = fitted_model,
  signal = signal,
  sampling_frequency = sampling_frequency,
  x_label = "Time (seconds)",
  y_label = "Electrocardiogram"
)
show(gg)

# Save figure
# pdf("R/ECG/figures/Kaggle_ECG.pdf")
# print(gg)
# dev.off()


