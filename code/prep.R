# %%
library(profoc)
library(dplyr)
# library(rgenoud)
library(mlrMBO)
library(tidyverse)
library(tictoc)
tic()
library(plotly)
library(gt)
source("code/functions.R")
# %%

# %% Read Data
load("data/PEFP.RData")
y <- FF[, , 1, "Observed"]
experts <- FF[, , , dimnames(FF)[[4]] != "Observed"]
experts <- experts[, , , experts_set]

# Define ouput folders
experts_sel <- dimnames(experts)[[4]]
foldername <- paste(experts_sel[1], tail(experts_sel, 1), sep = "_")

folder_tex <- paste0("paper/anc/", foldername, "/")
dir.create(
    folder_tex,
    showWarnings = FALSE,
    recursive = TRUE
)

folder_res <- paste0("results/", foldername, "/")
dir.create(
    folder_res,
    showWarnings = FALSE,
    recursive = TRUE
)

dir.create(paste0(folder_res, model_class),
    showWarnings = FALSE, recursive = TRUE
)

dir.create(paste0(folder_res, "data"),
    showWarnings = FALSE, recursive = TRUE
)

folder_res_class <- paste0(folder_res, model_class, "/")
# %%

# %% Dimensions and Indices
T <- dim(experts)[1]
D <- dim(experts)[2]
P <- dim(experts)[3]
K <- dim(experts)[4]
# Burn in period see. marcjasz2022distributional P. 7
# "however the first 182 observations are used" ...
# https://arxiv.org/pdf/2207.02832.pdf
B <- 182

# Tuning
# forget_regret
# fixed_share / thresholding
# learning rate scaling
# smoothing
# %%
