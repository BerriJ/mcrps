# %% Define the set of experts:

# All experts -> results/JSU1_Norm4
experts_set <- 1:8

# Only the experts with a JSU assumption -> JSU1_JSU4
# experts_set <- 5:8

# Only the experts with a normal assumption -> Norm1_Norm4
# experts_set <- 1:4

# %%

# %% Compute all combination models
source("code/special_cases.R")
source("code/sampling_online_full.R")
source("code/sampling_online_sm_fr.R")
source("code/sampling_online_sm.R")
source("code/sampling_online_fr.R")

source("code/bayesian_fix_full.R")
source("code/bayesian_fix_sm_fr.R")
source("code/bayesian_fix_sm.R")
source("code/bayesian_fix_fr.R")

source("code/bayesian_online_full.R")
source("code/bayesian_online_sm_fr.R")
source("code/bayesian_online_sm.R")
source("code/bayesian_online_fr.R")
# %%

# %% Evaluate the results
source("code/evaluation.R")
# %%

# %% Produce plots, tables etc.
source("code/kupiec_plot.R") # Kupiec, Christoffersen test results
source("code/output_plots_2.R") # Knots, Splines etc.
source("code/output_plots.R") # Weight plots
source("code/output_tables.R") # Tables

source("code/corrplot.R") # Needs additional data. Refer to readme sec. Data
# %%
