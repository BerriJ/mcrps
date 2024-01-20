# %%
# remotes::install_github("berrij/profoc@develop")
model_class <- "sampling_online"
source("code/prep.R")
source("code/common.R")
# %%

# %%
param_set <- "full"
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
        forget_regret = 2^seq(forget_regret_rng[1], forget_regret_rng[2], length.out = 16),
        fixed_share = 2^seq(common_range_fs_thresh[1], common_range_fs_thresh[2], length.out = 16),
        soft_threshold = 2^seq(common_range_fs_thresh[1], common_range_fs_thresh[2], length.out = 16),
        hard_threshold = 2^seq(common_range_fs_thresh[1], common_range_fs_thresh[2], length.out = 16),
        gamma = 2^seq(common_range_gamma_mu_sig_tail[1], common_range_gamma_mu_sig_tail[2], length.out = 16),
        p_smooth_pr = list(
            lambda = 2^seq(lambda_range[1], lambda_range[2], length.out = 16),
            mu = (seq(common_range_gamma_mu_sig_tail[1], common_range_gamma_mu_sig_tail[2], length.out = 16)^3 / 2.1 + 0.5),
            sigma = seq(common_range_gamma_mu_sig_tail[1], common_range_gamma_mu_sig_tail[2], length.out = 16)^3 / 1.1 + 1,
            nonc = seq(sm_nonc_range[1], sm_nonc_range[2], length.out = 16)^3,
            tailweight = seq(common_range_gamma_mu_sig_tail[1], common_range_gamma_mu_sig_tail[2], length.out = 16)^3 / 1.1 + 1
        ),
        p_smooth_mv = list(
            lambda = 2^seq(lambda_range[1], lambda_range[2], length.out = 16),
            mu = seq(common_range_gamma_mu_sig_tail[1], common_range_gamma_mu_sig_tail[2], length.out = 16)^3 / 2.1 + 0.5,
            sigma = seq(common_range_gamma_mu_sig_tail[1], common_range_gamma_mu_sig_tail[2], length.out = 16)^3 / 1.1 + 1,
            nonc = seq(sm_nonc_range[1], sm_nonc_range[2], length.out = 16)^3,
            tailweight = seq(common_range_gamma_mu_sig_tail[1], common_range_gamma_mu_sig_tail[2], length.out = 16)^3 / 1.1 + 1
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
