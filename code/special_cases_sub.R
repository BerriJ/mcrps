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

dates <- as.Date(dimnames(experts)[[1]])
subgroup <- dates >= "2020-05-01"


mod <- online(
    y = y[subgroup, ],
    experts = experts[subgroup, , , ],
    tau = 1:99 / 100,
    fixed_share = 1
)
end <- Sys.time()
mod$duration <- difftime(end, start, units = "mins")

# save(mod, file = paste0(
#     folder_res, "naive_sub.rds"
# ))

for (i in seq_len(dim(experts)[4])) {
    naive[, i] <- DM.test(
        EA = apply(mod$ex[, , , ], c(1, 4), mean)[, i],
        EB = apply(mod$fore[, , ], 1, mean)
    )
}

naive_err <- apply(mod$fore[, , ], 1, mean)

# save(naive_err, file = paste0(folder_res, "naive_err_sub.rds"))

naive[, "Naive"] <- c(mean(mod$fore[, , ]), NA, NA)

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
    save_kable(file = paste0(folder_tex, "/tab_naive_2005_2101.tex"))
# %%
