library(gfpop)
library(farff)
library(tidyverse)
library(ggplot2)
library(reshape2)

# 4045 signals of 188
heartbeats <- read_csv("data/ECG_kaggle/ptbdb_normal.csv", col_names = FALSE)
sampling_frequency <- 125


display_signal <- function(signal_index, heartbeats) {
  signal_length <- ncol(heartbeats)
  signal <- as.data.frame(t(heartbeats[signal_index,]))
  colnames(signal)[1] <- 'signal_1'
  signal$time <- seq(1, signal_length, 1) / sampling_frequency
  ggplot(data=signal) + geom_line(aes(x=time, y=signal_1))
}

# Select signal index to display
idx <- 1
gg <- display_signal(idx, heartbeats)


# TODO: Do graph analysis
# myGraph <- graph(penalty = 2*log(n), type = "updown")
# gfpop(data = myData, mygraph = myGraph, type = "mean")


# Save figure
pdf("R/ECG/Kaggle_ECG.pdf")
print(gg)
dev.off()


