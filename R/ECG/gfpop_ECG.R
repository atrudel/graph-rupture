# devtools::install_github("vrunge/gfpop.data")
library(gfpop)
library(gfpop.data)
data(ECG, package = "gfpop.data")
library(ggplot2)
library(reshape2)
library(tidyverse)
source("R/ECG/utils.R")

## penalty
penalty <- 8e7
# Graph construction
#------------------------------------------------------------------------------------------------
scaling_factor <- 1000
heartbeatGraph <- gfpop::graph(
  gfpop::Edge(0, 1, type = "down", penalty = penalty, gap=0),
  gfpop::Edge(1, 2, type = "up", penalty = 0, gap=2*scaling_factor),
  gfpop::Edge(2, 3, type = "down", penalty = 0, gap=5*scaling_factor),
  gfpop::Edge(3, 4, type = "up", penalty = 0, gap=2*scaling_factor),
  gfpop::Edge(4, 5, type = "up", penalty = 0, gap=1*scaling_factor),
  gfpop::Edge(5, 6, type = "up", penalty = 0, gap=0),
  gfpop::Edge(6, 7, type = "down", penalty = 0, gap=0),
  gfpop::Edge(7, 8, type = "down", penalty = 0, gap=0),
  gfpop::Edge(8, 0, type = "up", penalty = 0, gap=0),
  all.null.edges = TRUE
)
state_codes <- c("beforeQ", "Q", "R", "S", "S1", "S2", "peak", "afterPeak", "foo")

signal <- ECG$data
colnames(signal) <- c("timesteps", "values")

fitted_model1 <- gfpop(data = signal$values, mygraph = heartbeatGraph, type = "mean")

gg <- plot_qrs_modelled_signal(
  fitted_model = fitted_model1,
  signal = signal,
  sampling_frequency = 250,
  x_label = "Time (seconds)",
  y_label = "Electrocardiogram activity (mV)",
  state_codes = state_codes,
  title = "Constraint"
)
show(gg)

pdf("R/ECG/figures/gfpop_ECG.pdf")
print(gg)
dev.off()


penalty <- 5e7 #2*sigma**2 *log(N)

# Graph construction
#------------------------------------------------------------------------------------------------
scaling_factor <- 1000
heartbeatGraph <- gfpop::graph(
  gfpop::Edge(0, 1, type = "down", penalty = penalty, gap=0),
  gfpop::Edge(1, 2, type = "up", penalty = 0, gap=2*scaling_factor),
  gfpop::Edge(2, 3, type = "down", penalty = 0, gap=5*scaling_factor),
  gfpop::Edge(3, 4, type = "up", penalty = 0, gap=2*scaling_factor),
  gfpop::Edge(4, 5, type = "up", penalty = 0, gap=1*scaling_factor),
  gfpop::Edge(5, 6, type = "up", penalty = 0, gap=0),
  gfpop::Edge(6, 7, type = "down", penalty = 0, gap=0),
  gfpop::Edge(7, 8, type = "down", penalty = 0, gap=0),
  gfpop::Edge(8, 0, type = "up", penalty = 0, gap=0),
  all.null.edges = TRUE
)
state_codes <- c("beforeQ", "Q", "R", "S", "S1", "S2", "peak", "afterPeak", "foo")

signal <- ECG$data
colnames(signal) <- c("timesteps", "values")

fitted_model1 <- gfpop(data = signal$values, mygraph = heartbeatGraph, type = "mean")

gg <- plot_qrs_modelled_signal(
  fitted_model = fitted_model1,
  signal = signal,
  sampling_frequency = 250,
  x_label = "Time (seconds)",
  y_label = "Electrocardiogram activity (mV)",
  state_codes = state_codes,
  title = "Constraint"
)
show(gg)
pdf("R/ECG/figures/gfpop_ECG_5e7.pdf")
print(gg)
dev.off()


#### Equal penalty
penalty <- 8e7

scaling_factor <- 1000
heartbeatGraph <- gfpop::graph(
  gfpop::Edge(0, 1, type = "down", penalty = penalty/9, gap=0),
  gfpop::Edge(1, 2, type = "up", penalty = penalty/9, gap=2*scaling_factor),
  gfpop::Edge(2, 3, type = "down", penalty = penalty/9, gap=5*scaling_factor),
  gfpop::Edge(3, 4, type = "up", penalty = penalty/9, gap=2*scaling_factor),
  gfpop::Edge(4, 5, type = "up", penalty = penalty/9, gap=1*scaling_factor),
  gfpop::Edge(5, 6, type = "up", penalty = penalty/9, gap=0),
  gfpop::Edge(6, 7, type = "down", penalty = penalty/9, gap=0),
  gfpop::Edge(7, 8, type = "down", penalty = penalty/9, gap=0),
  gfpop::Edge(8, 0, type = "up", penalty = penalty/9, gap=0),
  all.null.edges = TRUE
)
state_codes <- c("beforeQ", "Q", "R", "S", "S1", "S2", "peak", "afterPeak", "foo")

signal <- ECG$data
colnames(signal) <- c("timesteps", "values")

fitted_model1 <- gfpop(data = signal$values, mygraph = heartbeatGraph, type = "mean")

gg <- plot_qrs_modelled_signal(
  fitted_model = fitted_model1,
  signal = signal,
  sampling_frequency = 250,
  x_label = "Time (seconds)",
  y_label = "Electrocardiogram activity (mV)",
  state_codes = state_codes,
  title = "Constraint and equally penalized"
)
show(gg)

pdf("R/ECG/figures/gfpop_ECG_equally_penalized.pdf")
print(gg)
dev.off()

# F1 score as a function of penalty

betas <- exp(seq(from = -5, to = 25, by = 0.2))
df <- data.frame(penality=NA, f1_constraint=NA)[numeric(0), ]

for (beta in betas){
  heartbeatGraph <- gfpop::graph(
  gfpop::Edge(0, 1, type = "down", penalty = beta, gap=0),
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

  fitted_model1 <- gfpop(data = signal$values, mygraph = heartbeatGraph, type = "mean")
  
  f1_score_model1 <- scoring_state_ruptures(signal, fitted_model1, c(52130,52342,52556,52771, 52979), 'R', state_codes)
  df <- df %>% add_row(penality = beta, f1_constraint = f1_score_model1)
}

ploting <- plot_curve_log(df)
show(ploting)


pdf("R/ECG/figures/constraint_f1_penalty.pdf")
print(ploting)
dev.off()

# Graph with no constraint : 
penalty <- 8e7

simple_graph <- gfpop::graph(
  gfpop::Edge(0, 0, type = "up", penalty = penalty),
  gfpop::Edge(0, 0, type = "down", penalty = penalty),
  all.null.edges = TRUE
)

fitted_model2 <- gfpop(data = signal$values, mygraph = simple_graph, type = "mean")


gg2 <- plot_qrs_modelled_signal(
  fitted_model = fitted_model2,
  signal = signal,
  sampling_frequency = 250,
  x_label = "Time (seconds)",
  y_label = "Electrocardiogram activity (mV)",
  state_codes = c(''),
  title = "No constraint"
)

show(gg2)


pdf("R/ECG/figures/gfpop_ECG_simple_graph.pdf")
print(gg2)
dev.off()

# with another penalty
N <- length(signal$values)
sigma <- sdDiff(signal$values, method = "HALL") # std of signal
penalty <- 2*sigma**2 *log(N)


simple_graph <- gfpop::graph(
  gfpop::Edge(0, 0, type = "up", penalty = penalty),
  gfpop::Edge(0, 0, type = "down", penalty = penalty),
  all.null.edges = TRUE
)

fitted_model2 <- gfpop(data = signal$values, mygraph = simple_graph, type = "mean")


gg2 <- plot_qrs_modelled_signal(
  fitted_model = fitted_model2,
  signal = signal,
  sampling_frequency = 250,
  x_label = "Time (seconds)",
  y_label = "Electrocardiogram activity (mV)",
  state_codes = c(''),
  title = "No constraint"
)

show(gg2)


pdf("R/ECG/figures/gfpop_ECG_simple_graph_bicpenalty.pdf")
print(gg2)
dev.off()

#low penalty simple graph
penalty <- 1e5


simple_graph <- gfpop::graph(
  gfpop::Edge(0, 0, type = "up", penalty = penalty),
  gfpop::Edge(0, 0, type = "down", penalty = penalty),
  all.null.edges = TRUE
)

fitted_model2 <- gfpop(data = signal$values, mygraph = simple_graph, type = "mean")


gg2 <- plot_qrs_modelled_signal(
  fitted_model = fitted_model2,
  signal = signal,
  sampling_frequency = 250,
  x_label = "Time (seconds)",
  y_label = "Electrocardiogram activity (mV)",
  state_codes = c(''),
  title = "No constraint"
)

show(gg2)


pdf("R/ECG/figures/gfpop_ECG_simple_graph_lowpenalty.pdf")
print(gg2)
dev.off()


