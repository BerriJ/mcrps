# %%
model_class <- "bayesian_online"
# remotes::install_github("berrij/profoc@develop")
source("code/prep.R")
source("code/common.R")
# %%

# %%
param_set <- "full" # Smooth + Forget regret
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
    optpath <- optpath[
        ,
        c(
            "forget_regret", "fixed_share", "soft_threshold",
            "hard_threshold", "gamma", "p_smooth_pr_lambda",
            "p_smooth_pr_mu",
            "p_smooth_pr_sigma",
            "p_smooth_pr_nonc",
            "p_smooth_pr_tailweight",
            "p_smooth_mv_lambda", "p_smooth_mv_mu",
            "p_smooth_mv_sigma",
            "p_smooth_mv_nonc",
            "p_smooth_mv_tailweight"
        )
    ]

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
    grid_general[, "forget_regret"] <- 2^optpath$forget_regret
    grid_general[, "soft_threshold"] <- 2^optpath$soft_threshold
    grid_general[, "hard_threshold"] <- 2^optpath$hard_threshold
    grid_general[, "fixed_share"] <- 2^optpath$fixed_share
    grid_general[, "basis_pr_idx"] <- 1
    grid_general[, "basis_mv_idx"] <- 1
    grid_general[, "hat_pr_idx"] <- 1:nrow(optpath)
    grid_general[, "hat_mv_idx"] <- 1:nrow(optpath)
    grid_general[, "gamma"] <- 2^optpath$gamma
    grid_general[, "loss_share"] <- 0
    grid_general[, "regret_share"] <- 0

    grid_p_smooth_pr <- matrix(NA,
        nrow = 750, ncol = 9,
        dimnames = list(
            NULL,
            c(
                "n",
                "mu",
                "sigma",
                "nonc",
                "tailw",
                "deg",
                "ndiff",
                "lambda",
                "periodic"
            )
        )
    )

    grid_p_smooth_pr[, "n"] <- 99
    grid_p_smooth_pr[, "mu"] <- optpath$p_smooth_pr_mu^3 / 2.1 + 0.5
    grid_p_smooth_pr[, "sigma"] <- optpath$p_smooth_pr_sigma^3 / 1.1 + 1
    grid_p_smooth_pr[, "nonc"] <- optpath$p_smooth_pr_nonc^3
    grid_p_smooth_pr[, "tailw"] <- optpath$p_smooth_pr_tailweight^3 / 1.1 + 1
    grid_p_smooth_pr[, "deg"] <- 1
    grid_p_smooth_pr[, "ndiff"] <- 1.5
    grid_p_smooth_pr[, "lambda"] <- 2^optpath$p_smooth_pr_lambda
    grid_p_smooth_pr[, "periodic"] <- FALSE

    grid_p_smooth_mv <- matrix(NA,
        nrow = 750, ncol = 9,
        dimnames = list(
            NULL,
            c(
                "n",
                "mu",
                "sigma",
                "nonc",
                "tailw",
                "deg",
                "ndiff",
                "lambda",
                "periodic"
            )
        )
    )

    grid_p_smooth_mv[, "n"] <- 24
    grid_p_smooth_mv[, "mu"] <- optpath$p_smooth_mv_mu^3 / 2.1 + 0.5
    grid_p_smooth_mv[, "sigma"] <- optpath$p_smooth_mv_sigma^3 / 1.1 + 1
    grid_p_smooth_mv[, "nonc"] <- optpath$p_smooth_mv_nonc^3
    grid_p_smooth_mv[, "tailw"] <- optpath$p_smooth_mv_tailweight^3 / 1.1 + 1
    grid_p_smooth_mv[, "deg"] <- 1
    grid_p_smooth_mv[, "ndiff"] <- 1.5
    grid_p_smooth_mv[, "lambda"] <- 2^optpath$p_smooth_mv_lambda
    grid_p_smooth_mv[, "periodic"] <- FALSE

    start <- Sys.time()
    set.seed(1)
    mod <- online(
        y = y,
        experts = experts,
        tau = 1:99 / 100,
        method = meth,
        parametergrids = list(
            general = grid_general,
            p_smooth_pr = grid_p_smooth_pr,
            p_smooth_mv = grid_p_smooth_mv
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
