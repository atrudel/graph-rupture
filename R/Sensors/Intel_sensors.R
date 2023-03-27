library(gfpop)
library(farff)
library(tidyverse)
library(ggplot2)
library(reshape2)
source("R/Sensors/utils.R")
library(data.table)


temperature_data <- read_csv("data/Sensors_Intel/signal_with_faults.csv", col_names = TRUE, col_types = "iTdlll")

signal <- temperature_data[c("timesteps", "temperature")]


up_penalty <- 1e2
short_penalty <- 1e2
short_gap <- 8


short_graph <- gfpop::graph(
  # Normal Signal
  gfpop::Edge("Down", "Down", type = "down"),   # Decay during day
  gfpop::Edge("Up", "Up", type = "up"),
  gfpop::Edge("Down", "Up", type = "up", penalty = up_penalty),
  gfpop::Edge("Up", "Down", type = "down"),

  # SHORT failure
  gfpop::Edge("Down", "SHORT", type="abs", gap=short_gap, penalty = short_penalty),
  gfpop::Edge("SHORT", "Down", type="abs", gap=short_gap),
  gfpop::Edge("Up", "SHORT", type="abs", gap=short_gap, penalty = short_penalty),
  gfpop::Edge("SHORT", "Up", type="abs", gap=short_gap),
  StartEnd(start="Down")
)
fitted_short_model <- gfpop(data = signal$temperature, mygraph = short_graph, type = "mean")

gg <- plot_fault_modelled_signal(
  fitted_model = fitted_short_model,
  signal = signal,
  x_label = "Time steps (30 min)",
  y_label = "Temperature (Celsius)",
  title = "Temperature mesasured by Mote #2 over 34 days"
)

show(gg)
pdf("R/Sensors/figures/sensor_short_faults.pdf")
print(gg)
dev.off()
