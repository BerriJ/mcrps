# %%
model_class <- "bayesian_fix"
source("code/prep.R")
source("code/common.R") #
# %%

# %%
param_set <- "forget"
# %%

# %% Make objective
N <- 1
performance <- array(dim = c(N, 3, 3), dimnames = list(
    NULL,
    c("bewa", "ml_poly", "ewa"),
    c("loss", "stat", "p.val")
))
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
        trace = TRUE
    )

    end <- Sys.time()
    mod$duration <- difftime(end, start, units = "mins")

    save_model(mod, folder_res_class, meth, param_set, naive_err, B)
}
save_scores(model_class, param_set, performance, folder_res)
# %%
