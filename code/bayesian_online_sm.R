# %%
model_class <- "bayesian_online"
# remotes::install_github("berrij/profoc@develop")
source("code/prep.R")
source("code/common.R")
# %%

# %%
param_set <- "smooth" # Smooth + Forget regret
# %%

# %%
N <- 1
performance <- array(dim = c(N, 3, 3), dimnames = list(
    NULL,
    c("bewa", "ml_poly", "ewa"),
    c("loss", "stat", "p.val")
))
desc <- character(N)
spec_idx <- 0
# %% Performance and DM test

# %% Tuned - Profoc
spec_idx <- spec_idx + 1
meth <- "bewa"
for (meth in c("bewa", "ml_poly", "ewa")) {
    # Load previous run of mlrmbo
    load(paste0(
        folder_res, "/bayesian_fix/run_", meth, "_", param_set, ".rds"
    ))

    # %% Plot optimization path
    optpath <- as.data.frame(run$opt.path)
    # plot(optpath$y, type = "l")

    # Take the first 750 points
    optpath <- head(optpath, 750)

    # Extract relevant columns
    optpath <- optpath[, c("p_smooth_pr_lambda", "p_smooth_mv_lambda")]

    # Set up the user specified grids
    grid_general <- matrix(NA,
        nrow = 750, ncol = 11,
        dimnames = list(
            NULL,
            c(
                "forget_regret",
                "soft_threshold", "hard_threshold",
                "fixed_share",
                "basis_pr_idx", "basis_mv_idx",
                "hat_pr_idx", "hat_mv_idx",
                "gamma", "loss_share", "regret_share"
            )
        )
    )

    # Fill the grid
    grid_general[, "forget_regret"] <- 0
    grid_general[, "soft_threshold"] <- -Inf
    grid_general[, "hard_threshold"] <- -Inf
    grid_general[, "fixed_share"] <- 0
    grid_general[, "basis_pr_idx"] <- 1
    grid_general[, "basis_mv_idx"] <- 1
    grid_general[, "hat_pr_idx"] <- 1:nrow(optpath)
    grid_general[, "hat_mv_idx"] <- 1:nrow(optpath)
    grid_general[, "gamma"] <- 1
    grid_general[, "loss_share"] <- 0
    grid_general[, "regret_share"] <- 0

    start <- Sys.time()
    set.seed(1)
    mod <- online(
        y = y,
        experts = experts,
        tau = 1:99 / 100,
        method = meth,
        p_smooth_pr = list(
            lambda = 2^optpath$p_smooth_pr_lambda
        ),
        p_smooth_mv = list(
            lambda = 2^optpath$p_smooth_mv_lambda
        ),
        parametergrids = list(
            general = grid_general
        ),
        parametergrid_max_combinations = 2500,
        trace = TRUE,
        lead_time = 0
    )
    end <- Sys.time()
    mod$duration <- difftime(end, start, units = "mins")

    save_model(mod, folder_res_class, meth, param_set, naive_err, B)
}
save_scores(model_class, param_set, performance, folder_res)
# %%
