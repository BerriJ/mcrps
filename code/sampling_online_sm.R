# %%
# remotes::install_github("berrij/profoc@develop")
model_class <- "sampling_online"
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

for (meth in c("bewa", "ml_poly", "ewa")) {
    start <- Sys.time()
    set.seed(1)
    mod <- online(
        y = y,
        experts = experts,
        tau = 1:99 / 100,
        method = meth,
        p_smooth_pr = list(
            lambda = 2^seq(lambda_range[1], lambda_range[2], length.out = 16)
        ),
        p_smooth_mv = list(
            lambda = 2^seq(lambda_range[1], lambda_range[2], length.out = 16)
        ),
        parametergrid_max_combinations = 2500,
        trace = TRUE
    )
    end <- Sys.time()
    mod$duration <- difftime(end, start, units = "mins")

    save_model(mod, folder_res_class, meth, param_set, naive_err, B)
}
save_scores(model_class, param_set, performance, folder_res)
# %%
