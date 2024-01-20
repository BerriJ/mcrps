model_class <- "specials"

# %%
source("code/prep.R")
library(knitr)
library(kableExtra)
# %%

# %%
performance_comb <- data.frame(
    Spec = character(10),
    "bewa" = numeric(10),
    "ml_poly" = numeric(10),
    "ewa" = numeric(10)
)
# %%

# %% NAIVE
naive <- array(dim = c(3, dim(experts)[4] + 1), dimnames = list(
    c("loss", "stat", "p.val"),
    c(dimnames(experts)[[4]], "Naive")
))

spec_idx <- 1
start <- Sys.time()
mod <- online(
    y = y,
    experts = experts,
    tau = 1:99 / 100,
    fixed_share = 1
)
end <- Sys.time()

mod$duration <- difftime(end, start, units = "mins")

for (i in seq_len(dim(experts)[4])) {
    naive[, i] <- DM.test(
        EA = apply(mod$ex[(B + 1):T, , , ], c(1, 4), mean)[, i],
        EB = apply(mod$fore[(B + 1):T, , ], 1, mean)
    )
}

naive_err <- apply(mod$fore[(B + 1):T, , ], 1, mean)

save(naive_err, file = paste0(folder_res, "naive_err.rds"))

naive[, "Naive"] <- c(mean(mod$fore[(B + 1):T, , ]), NA, NA)

# Remove unused components before saving to disk
mod <- reduce_mod_size(mod)

save(mod, file = paste0(
    folder_res, "naive.rds"
))

save(y, file = paste0(
    folder_res, "data/y.rds"
))

save(experts, file = paste0(
    folder_res, "data/experts.rds"
))

table_naive <- naive %>%
    as_tibble() %>%
    head(1) %>%
    mutate_all(round, 3) %>%
    mutate_all(as.character) %>%
    kbl(
        caption = paste0("TODO \\label{tab:energy}"),
        digits = 2,
        # col.names = c(
        #     "Model",
        #     "$\\text{ES}^{\\text{All}}_{1-30}$"
        # ),
        linesep = "",
        # Dont replace any string, dataframe has to be valid latex code ...
        escape = FALSE,
        format = "latex",
        booktabs = TRUE,
        align = c("l", rep("r", ncol(naive) - 1))
    ) %>%
    kable_paper(full_width = FALSE)

for (i in 1:ncol(naive)) {
    table_naive <- table_naive %>%
        column_spec(i,
            background = ifelse(
                is.na(naive[2, i, drop = TRUE][-ncol(naive)]),
                "#FFFFFF",
                col_scale2(
                    naive[2, i, drop = TRUE][-ncol(naive)],
                    rng_t
                )
            ),
            bold = i == which.min(naive[1, ])
        )
}

table_naive %>%
    kable_styling(latex_options = c(
        "hold_position"
        # "scale_down"
    )) %>%
    save_kable(file = paste0(folder_tex, "/tab_naive.tex"))
# %%

# %%
N <- 5
performance <- array(dim = c(N, 3, 3), dimnames = list(
    NULL,
    c("bewa", "ml_poly", "ewa"),
    c("loss", "stat", "p.val")
))
desc <- character(N)
spec_idx <- 0
# %% Performance and DM test

# %% Constant
spec_idx <- spec_idx + 1
for (meth in c("bewa", "ml_poly", "ewa")) {
    start <- Sys.time()
    mod <- online(
        y = y,
        experts = experts,
        tau = 1:99 / 100,
        b_smooth_pr = list(
            knots = -1
        ),
        b_smooth_mv = list(
            knots = -1
        ),
        method = meth
    )
    end <- Sys.time()
    mod$duration <- difftime(end, start, units = "mins")

    performance[spec_idx, meth, ] <- DM.test(
        EA = apply(mod$fore[(B + 1):T, , ], 1, mean),
        EB = naive_err
    )
    # Remove unused components before saving to disk
    mod <- reduce_mod_size(mod)
    save(mod, file = paste0(
        folder_res_class, "mod_", meth, "_", "constant.rds"
    ))
}
desc[spec_idx] <- "Constant"

# %%

# %% B-Constant PR
spec_idx <- spec_idx + 1
for (meth in c("bewa", "ml_poly", "ewa")) {
    start <- Sys.time()
    mod <- online(
        y = y,
        experts = experts,
        tau = 1:99 / 100,
        b_smooth_pr = list(
            knots = -1
        ),
        method = meth
    )
    end <- Sys.time()
    mod$duration <- difftime(end, start, units = "mins")
    performance[spec_idx, meth, ] <- DM.test(
        EA = apply(mod$fore[(B + 1):T, , ], 1, mean),
        EB = naive_err
    )
    # Remove unused components before saving to disk
    mod <- reduce_mod_size(mod)
    save(mod, file = paste0(
        folder_res_class, "mod_", meth, "_", "b_constant_pr.rds"
    ))
}
desc[spec_idx] <- "B-Constant PR"
# %%

# %% B-Constant MV
spec_idx <- spec_idx + 1
for (meth in c("bewa", "ml_poly", "ewa")) {
    start <- Sys.time()
    mod <- online(
        y = y,
        experts = experts,
        tau = 1:99 / 100,
        b_smooth_mv = list(
            knots = -1
        ),
        method = meth
    )
    end <- Sys.time()
    mod$duration <- difftime(end, start, units = "mins")
    performance[spec_idx, meth, ] <- DM.test(
        EA = apply(mod$fore[(B + 1):T, , ], 1, mean),
        EB = naive_err
    )
    # Remove unused components before saving to disk
    mod <- reduce_mod_size(mod)
    save(mod, file = paste0(
        folder_res_class, "mod_", meth, "_b_constant_mv.rds"
    ))
}
desc[spec_idx] <- "B-Constant MV"
# %%

# %% Pointwise
spec_idx <- spec_idx + 1
for (meth in c("bewa", "ml_poly", "ewa")) {
    start <- Sys.time()
    mod <- online(
        y = y,
        experts = experts,
        tau = 1:99 / 100,
        method = meth
    )
    end <- Sys.time()
    mod$duration <- difftime(end, start, units = "mins")
    performance[spec_idx, meth, ] <- DM.test(
        EA = apply(mod$fore[(B + 1):T, , ], 1, mean),
        EB = naive_err
    )
    # Remove unused components before saving to disk
    mod <- reduce_mod_size(mod)
    save(mod, file = paste0(
        folder_res_class, "mod_", meth, "_pointwise.rds"
    ))
}
desc[spec_idx] <- "Pointwise"
# %%

# %% Follow the leader
spec_idx <- spec_idx + 1
for (meth in c("bewa", "ml_poly", "ewa")) {
    start <- Sys.time()
    mod <- online(
        y = y,
        experts = experts,
        tau = 1:99 / 100,
        forget_regret = 1,
        hard_threshold = 1,
        method = meth
    )
    end <- Sys.time()
    mod$duration <- difftime(end, start, units = "mins")
    performance[spec_idx, meth, ] <- DM.test(
        EA = apply(mod$fore[(B + 1):T, , ], 1, mean),
        EB = naive_err
    )
    # Remove unused components before saving to disk
    mod <- reduce_mod_size(mod)
    save(mod, file = paste0(
        folder_res_class, "mod_", meth, "_ftl.rds"
    ))
}
desc[spec_idx] <- "FTL"
# %%


# %%
save(performance, desc, file = paste0(folder_res, "results_specials.rds"))
# %%
