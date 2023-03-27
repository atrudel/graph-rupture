library(gfpop)
library(farff)
library(tidyverse)
library(ggplot2)
library(reshape2)
source("R/Sensors/utils.R")
library(data.table)


temperature_data <- read_csv("data/Sensors_Intel/signal_with_faults.csv", col_names = TRUE, col_types = "iTdlll")

signal <- temperature_data[c("timesteps", "temperature")]


up_penalty <- 1e4
short_penalty <- 1e4
# constant_penalty <- 1e5
short_gap <- 5


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
  #
  # # CONSTANT failure
  # gfpop::Edge("CONSTANT", "CONSTANT", type="null"),
  # gfpop::Edge("Down", "CONSTANT", type="abs", gap = 0, penalty = constant_penalty),
  # gfpop::Edge("CONSTANT", "Down", type="abs", gap = 0),
  # gfpop::Edge("Up", "CONSTANT", type="abs", gap = 0, penalty = constant_penalty),
  # gfpop::Edge("CONSTANT", "Up", type="abs", gap = 0),
  StartEnd(start="Down")
)
fitted_short_model <- gfpop(data = signal$temperature, mygraph = short_graph, type = "mean")

# plot.gfpop(fitted_model, data = signal$temperature)

gg <- plot_fault_modelled_signal(
  fitted_model = fitted_short_model,
  signal = signal,
  x_label = "Time steps (30 min)",
  y_label = "Temperature (Celsius)",
  title = "Temperature mesasured by Mote #2 over 34 days"
)

show(gg)
# pdf("R/Sensors_Intel/figures/sensor_faults.pdf")
# print(gg)
# dev.off()

#################################################################################################################
# Second graph

# noise_penalty <- 1e2
# constant_penalty <- 1e3
# noise_gap <- 2
#
# noise_constant_graph <- gfpop::graph(
#   # Transitions for the normal signal
#   gfpop::Edge("Normal", "Normal", type="null"),
#   gfpop::Edge("Normal", "NOISE", type="up", gap = noise_gap, penalty = noise_penalty),
#   gfpop::Edge("NOISE", "Normal", type="down", gap = noise_gap),
#   gfpop::Edge("Normal", "CONSTANT", type="down", penalty = constant_penalty),
#   gfpop::Edge("CONSTANT", "CONSTANT", type="null"),
#   gfpop::Edge("CONSTANT", "Normal", type="up"),
#   StartEnd("Normal"),
#   # Node for CONSTANT state with stdev of zero
#   gfpop::Node("CONSTANT", min=0, max=0.02)
# )
# stdev <- sdDiff(signal$temperature)
# fitted_constant_noise_model <- gfpop(data = signal$temperature, mygraph = noise_constant_graph, type = "variance")
#
# plot.gfpop(fitted_constant_noise_model, data = signal$temperature)
#
# gg <- plot_fault_modelled_signal(
#   fitted_model = fitted_constant_noise_model,
#   signal = signal,
#   x_label = "Time steps (30 min)",
#   y_label = "Temperature (Celsius)",
#   title = "Temperature mesasured by Mote #2 over 34 days"
# )
#
#
# show(gg)
# # pdf("R/Sensors_Intel/figures/sensor_faults.pdf")
# # print(gg)
# # dev.off()
