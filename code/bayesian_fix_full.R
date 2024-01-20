# %%
model_class <- "bayesian_fix"
source("code/prep.R")
source("code/common.R") #
# %%

# %%
param_set <- "full"
# %%

# %% Make objective
N <- 1
performance <- array(dim = c(N, 3, 3), dimnames = list(
    NULL,
    c("bewa", "ml_poly", "ewa"),
    c("loss", "stat", "p.val")
))
desc <- character(N)
spec_idx <- 0

for (meth in c("bewa", "ml_poly", "ewa")) {
    obj.fun <- makeSingleObjectiveFunction(
        name = "profoc",
        fn = function(x) {
            mod <- online(
                y = y[1:B, ],
                experts = experts[1:B, , , ],
                tau = 1:99 / 100,
                method = meth,
                forget_regret = x["forget_regret"],
                fixed_share = x["fixed_share"],
                soft_threshold = x["soft_threshold"],
                hard_threshold = x["hard_threshold"],
                gamma = x["gamma"],
                p_smooth_pr = list(
                    lambda = x["p_smooth_pr_lambda"],
                    mu = x["p_smooth_pr_mu"],
                    sigma = x["p_smooth_pr_sigma"],
                    nonc = x["p_smooth_pr_nonc"],
                    tailweight = x["p_smooth_pr_tailweight"]
                ),
                p_smooth_mv = list(
                    lambda = x["p_smooth_mv_lambda"],
                    mu = x["p_smooth_mv_mu"],
                    sigma = x["p_smooth_mv_sigma"],
                    nonc = x["p_smooth_mv_nonc"],
                    tailweight = x["p_smooth_mv_tailweight"]
                ),
                trace = FALSE
            )

            idx <- 50:182 # sample(31:182, size = 50, replace = TRUE)

            score <- mean(mod$fore[idx, , ])
            return(score)
        },
        par.set = makeParamSet(
            makeNumericParam("forget_regret",
                lower = forget_regret_rng[1],
                upper = forget_regret_rng[2],
                trafo = function(x) 2^x
            ),
            makeNumericParam("fixed_share",
                lower = common_range_fs_thresh[1],
                upper = common_range_fs_thresh[2],
                trafo = function(x) 2^x
            ),
            makeNumericParam("soft_threshold",
                lower = common_range_fs_thresh[1],
                upper = common_range_fs_thresh[2],
                trafo = function(x) 2^x
            ),
            makeNumericParam("hard_threshold",
                lower = common_range_fs_thresh[1],
                upper = common_range_fs_thresh[2],
                trafo = function(x) 2^x
            ),
            makeNumericParam("gamma",
                lower = common_range_gamma_mu_sig_tail[1],
                upper = common_range_gamma_mu_sig_tail[2],
                trafo = function(x) 2^x
            ),
            makeNumericParam("p_smooth_pr_lambda",
                lower = lambda_range[1],
                upper = lambda_range[2],
                trafo = function(x) 2^x,
                special.vals = list(-Inf)
            ),
            makeNumericParam("p_smooth_pr_mu",
                lower = common_range_gamma_mu_sig_tail[1],
                upper = common_range_gamma_mu_sig_tail[2],
                trafo = function(x) x^3 / 2.1 + 0.5
            ),
            makeNumericParam("p_smooth_pr_sigma",
                lower = common_range_gamma_mu_sig_tail[1],
                upper = common_range_gamma_mu_sig_tail[2],
                trafo = function(x) x^3 / 1.1 + 1
            ),
            makeNumericParam("p_smooth_pr_nonc",
                lower = sm_nonc_range[1],
                upper = sm_nonc_range[2],
                trafo = function(x) x^3
            ),
            makeNumericParam("p_smooth_pr_tailweight",
                lower = common_range_gamma_mu_sig_tail[1],
                upper = common_range_gamma_mu_sig_tail[2],
                trafo = function(x) x^3 / 1.1 + 1
            ),
            makeNumericParam("p_smooth_mv_lambda",
                lower = lambda_range[1],
                upper = lambda_range[2],
                trafo = function(x) 2^x,
                special.vals = list(-Inf)
            ),
            makeNumericParam("p_smooth_mv_mu",
                lower = common_range_gamma_mu_sig_tail[1],
                upper = common_range_gamma_mu_sig_tail[2],
                trafo = function(x) x^3 / 2.1 + 0.5
            ),
            makeNumericParam("p_smooth_mv_sigma",
                lower = common_range_gamma_mu_sig_tail[1],
                upper = common_range_gamma_mu_sig_tail[2],
                trafo = function(x) x^3 / 1.1 + 1
            ),
            makeNumericParam("p_smooth_mv_nonc",
                lower = sm_nonc_range[1],
                upper = sm_nonc_range[2],
                trafo = function(x) x^3
            ),
            makeNumericParam("p_smooth_mv_tailweight",
                lower = common_range_gamma_mu_sig_tail[1],
                upper = common_range_gamma_mu_sig_tail[2],
                trafo = function(x) x^3 / 1.1 + 1
            )
        ),
        minimize = TRUE
    )

    control <- makeMBOControl(propose.points = 8)
    # 1000 evals need around 30 minutes
    control <- setMBOControlTermination(control, max.evals = 1000)

    des <- generateDesign(
        n = 20,
        par.set = getParamSet(obj.fun)
    )

    start <- Sys.time()
    run <- mbo(
        obj.fun,
        design = des,
        control = control,
        show.info = TRUE
    )

    save(run, file = paste0(
        folder_res_class, "run_", meth, "_", param_set, ".rds"
    ))

    optpath <- as.data.frame(run$opt.path)
    plot(optpath$y, type = "l")
    abline(v = which.min(optpath$y), col = "grey")
    # %%

    # %% Estimate best model
    head(optpath)
    optpath$forget_regret <- 2^optpath$forget_regret
    optpath$fixed_share <- 2^optpath$fixed_share
    optpath$soft_threshold <- 2^optpath$soft_threshold
    optpath$hard_threshold <- 2^optpath$hard_threshold
    optpath$gamma <- 2^optpath$gamma
    optpath$p_smooth_pr_lambda <- 2^optpath$p_smooth_pr_lambda
    optpath$p_smooth_pr_mu <- optpath$p_smooth_pr_mu^3 / 2.1 + 0.5
    optpath$p_smooth_pr_sigma <- optpath$p_smooth_pr_sigma^3 / 1.1 + 1
    optpath$p_smooth_pr_nonc <- optpath$p_smooth_pr_nonc^3
    optpath$p_smooth_pr_tailweight <- optpath$p_smooth_pr_tailweight^3 / 1.1 + 1

    optpath$p_smooth_mv_lambda <- 2^optpath$p_smooth_mv_lambda
    optpath$p_smooth_mv_mu <- optpath$p_smooth_mv_mu^3 / 2.1 + 0.5
    optpath$p_smooth_mv_sigma <- optpath$p_smooth_mv_sigma^3 / 1.1 + 1
    optpath$p_smooth_mv_nonc <- optpath$p_smooth_mv_nonc^3
    optpath$p_smooth_mv_tailweight <- optpath$p_smooth_mv_tailweight^3 / 1.1 + 1

    optpath <- optpath[order(optpath$y), ]
    head(optpath)

    # %% Tuned - MLRBO

    spec_idx <- spec_idx + 1
    x <- as.numeric(optpath[1, ])
    names(x) <- names(optpath[1, ])

    mod <- online(
        y = y,
        experts = experts,
        tau = 1:99 / 100,
        method = meth,
        forget_regret = x["forget_regret"],
        fixed_share = x["fixed_share"],
        soft_threshold = x["soft_threshold"],
        hard_threshold = x["hard_threshold"],
        gamma = x["gamma"],
        p_smooth_pr = list(
            lambda = x["p_smooth_pr_lambda"],
            mu = x["p_smooth_pr_mu"],
            sigma = x["p_smooth_pr_sigma"],
            nonc = x["p_smooth_pr_nonc"],
            tailweight = x["p_smooth_pr_tailweight"]
        ),
        p_smooth_mv = list(
            lambda = x["p_smooth_mv_lambda"],
            mu = x["p_smooth_mv_mu"],
            sigma = x["p_smooth_mv_sigma"],
            nonc = x["p_smooth_mv_nonc"],
            tailweight = x["p_smooth_mv_tailweight"]
        ),
        trace = TRUE
    )

    end <- Sys.time()
    mod$duration <- difftime(end, start, units = "mins")

    save_model(mod, folder_res_class, meth, param_set, naive_err, B)
}
save_scores(model_class, param_set, performance, folder_res)
# %%
