# %%
model_class <- "bayesian_online"
# remotes::install_github("berrij/profoc@develop")
source("code/prep.R")
source("code/common.R")
# %%

# %%
param_set <- "forget"
# %%

# %% Run online on mlrmbo results
N <- 1
performance <- array(dim = c(N, 3, 3), dimnames = list(
    NULL,
    c("bewa", "ml_poly", "ewa"),
    c("loss", "stat", "p.val")
))
desc <- character(N)
spec_idx <- 1

for (meth in c("bewa", "ml_poly", "ewa")) {
    # Load previous run of mlrmbo
    load(paste0(
        folder_res, "/bayesian_fix/run_", meth, "_", param_set, ".rds"
    ))

    # %% Plot optimization path
    optpath <- as.data.frame(run$opt.path)
    plot(optpath$y, type = "l")

    # Take the first 750 points
    optpath <- head(optpath, 750)

    # Extract forget values
    forget_regret <- 2^optpath$forget_regret

    # %%
    start <- Sys.time()
    mod <- online(
        y = y,
        experts = experts,
        tau = 1:99 / 100,
        method = meth,
        forget_regret = forget_regret,
        parametergrid_max_combinations = 2500,
        trace = TRUE
    )
    end <- Sys.time()
    mod$duration <- difftime(end, start, units = "mins")

    save_model(mod, folder_res_class, meth, param_set, naive_err, B)
}
save_scores(model_class, param_set, performance, folder_res)
# %%
