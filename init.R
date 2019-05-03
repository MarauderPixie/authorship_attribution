library(tidyverse)
library(tidytext)
# library(broom)
## needed for dendogramifications:
library(dendextend)
library(colorspace)

theme_set(hrbrthemes::theme_modern_rc())

### read data
# ANNTF
train <- read_csv("data_prepd/training_data_complete.csv")
# CL4Spies
arts <- readRDS("data_prepd/artist_sample.rds")
