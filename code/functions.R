# %%
# Ensure consistency between plots
library(plotly)
library(ggplot2)
library(ggnewscale)
library(ggsci)
library(tidyr)
library(dplyr)
library(tikzDevice)
options(tikzDocumentDeclaration = "\\documentclass[preprint,12pt]{elsarticle}")

material_pals <- c(
    "red", "pink", "purple", "deep-purple", "indigo",
    "blue", "light-blue", "cyan", "teal", "green", "light-green", "lime",
    "yellow", "amber", "orange", "deep-orange", "brown", "grey", "blue-grey"
)
cols <- purrr::map(material_pals, ~ pal_material(.x)(10)) %>%
    purrr::reduce(cbind)
colnames(cols) <- material_pals

cols %>%
    as_tibble() %>%
    mutate(idx = as.factor(1:10)) %>%
    pivot_longer(-idx, names_to = "var", values_to = "val") %>%
    mutate(var = factor(var, levels = material_pals[19:1])) %>%
    ggplot() +
    xlab(NULL) +
    ylab(NULL) +
    geom_tile(aes(x = idx, y = var, fill = val)) +
    scale_fill_identity() +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    theme_minimal() -> plot_cols

linesize <- 1

text_size <- 16
width <- 8
height <- 4

col_gas <- "blue"
col_eua <- "green"
col_oil <- "amber"
col_coal <- "brown"
# %%

# %%
B <- 182
# %%

# %%
DM.test <- function(EA, EB, hmax = 1, power = 1) {
    ## as dm.test with alternative == "less"
    LA <- apply(abs(as.matrix(EA))^power, 1, sum)^(1 / power)
    LB <- apply(abs(as.matrix(EB))^power, 1, sum)^(1 / power)
    delta <- LA - LB
    delta.var <- var(delta) / length(delta) ## estimation of the variance
    STATISTIC <- mean(delta, na.rm = TRUE) / sqrt(delta.var)
    delta.len <- length(delta)
    k <- ((delta.len + 1 - 2 * hmax + (hmax / delta.len) * (hmax - 1)) / delta.len)^(1 / 2)
    STATISTIC <- STATISTIC * k
    PVAL <- pt(STATISTIC, df = delta.len - 1)
    return(c(mean_ea = mean(EA), stat = STATISTIC, p.val = PVAL))
}
# %%

# %% Coloring for all performance tables
rng_t <- c(-5, 5)

col_scale <- function(x, rng) {
    col <- scales::gradient_n_pal(
        c(
            cols[5, "green"],
            cols[5, "light-green"],
            cols[5, "yellow"],
            # cols[5, "amber"],
            cols[5, "orange"],
            # cols[5, "deep-orange"],
            cols[5, "red"]
        ),
        values = seq(rng[1], rng[2], length.out = 5)
    )(x)
    return(col)
}

col_scale2 <- function(x, rng_t) {
    ret <- x
    for (i in seq_along(x)) {
        if (x[i] <= rng_t[1]) {
            ret[i] <- col_scale(rng_t[1], rng_t)
        } else if (x[i] >= rng_t[2]) {
            ret[i] <- col_scale(rng_t[2], rng_t)
        } else {
            ret[i] <- col_scale(x[i], rng_t)
        }
    }
    return(ret)
}



cols_vals <- rng_t[1]:rng_t[2]
cols_hex <- substr(col_scale2(rng_t[1]:rng_t[2], rng_t), 2, 7)

col_note_low <- paste0(
    "\\\\colorbox[HTML]{",
    cols_hex[1],
    "}{$<$",
    cols_vals[1],
    "} ",
    collapse = ""
)

col_note_mid <- paste0(
    "\\\\colorbox[HTML]{",
    cols_hex[2:(length(cols_hex) - 1)],
    "}{",
    cols_vals[2:(length(cols_hex) - 1)],
    "} ",
    collapse = ""
)

col_note_high <- paste0(
    "\\\\colorbox[HTML]{",
    cols_hex[length(cols_hex)],
    "}{$>$",
    cols_vals[length(cols_vals)],
    "} ",
    collapse = ""
)

col_note <- paste0(col_note_low, col_note_mid, col_note_high, collapse = "")
# %%

# %%
extract_pargrid <- function(mod) {
    pargrid <- mod$parametergrid
    p_sm_pr <- mod$params_hat_pr[pargrid[, "hat_pr_idx"], ]
    if (is.null(dim(p_sm_pr))) p_sm_pr <- as.data.frame(t(p_sm_pr))
    p_sm_mv <- mod$params_hat_mv[pargrid[, "hat_mv_idx"], ]
    if (is.null(dim(p_sm_mv))) p_sm_mv <- as.data.frame(t(p_sm_mv))
    to_rmve <- c(
        which(colnames(mod$parametergrid) == "hat_pr_idx"),
        which(colnames(mod$parametergrid) == "hat_mv_idx"),
        which(colnames(mod$parametergrid) == "basis_pr_idx"),
        which(colnames(mod$parametergrid) == "basis_mv_idx")
    )
    pargrid <- mod$parametergrid[, -to_rmve]
    colnames(p_sm_pr) <- paste0("p_pr_", colnames(p_sm_pr))
    colnames(p_sm_mv) <- paste0("p_mv_", colnames(p_sm_mv))
    if (is.null(dim(pargrid))) pargrid <- as.data.frame(t(pargrid))
    pargrid <- cbind(pargrid, p_sm_pr, p_sm_mv)
    pargrid <- as.data.frame(pargrid)
    return(pargrid)
}
# %%

# %%
save_scores <- function(
    model_class,
    param_set,
    performance,
    folder_res) {
    desc <- paste0(model_class, param_set)
    save(performance, desc,
        file = paste0(folder_res, model_class, "_", param_set, ".rds")
    )
    return(0)
}
# %%

# %%
save_model <- function(mod, folder_res_class, meth, param_set, naive_err, B) {
    # Remove unused components before saving to disk
    mod <- reduce_mod_size(mod)

    save(mod, file = paste0(
        folder_res_class, "mod_", meth, "_", param_set, ".rds"
    ))

    performance[1, meth, ] <<- DM.test(
        EA = apply(mod$fore[(B + 1):T, , ], 1, mean),
        EB = naive_err
    )

    pars <- extract_pargrid(mod)
    pars_chosen <- pars[mod$opt_index, ]

    readr::write_csv(pars_chosen, paste0(
        folder_res_class, "pars_", meth, "_", param_set, ".csv"
    ))
}
# %%

# %%
get_hits <- function(obs, q_forecast_low, q_forecast_high) {
    N <- length(obs)
    hit_x <- numeric(N)
    hit_x[which((obs <= q_forecast_low) | (obs >= q_forecast_high))] <- 1L
    return(hit_x)
}

kupiec_test <- function(hit, int_width) {
    N <- length(hit)
    x <- sum(hit)
    rate <- x / N
    test <- -2 * log(((1 - int_width)^(N - x) * int_width^x) / ((1 - rate)^(N - x) * rate^x))
    if (is.na(test)) {
        test <- -2 * ((N - x) * log(1 - int_width) + x * log(int_width) -
            (N - x) * log(1 - rate) - x * log(rate))
    }
    pvalue <- 1 - pchisq(test, df = 1)
    LRpof <- c(test, pvalue)
    names(LRpof) <- c("Test", "Pvalue")
    return(LRpof)
}

christoffersen_test <- function(hit, int_width) {
    n00 <- n01 <- n10 <- n11 <- 0
    N <- length(hit)
    for (i in 2:N) {
        if (hit[i] == 0L & hit[i - 1L] == 0L) {
            n00 <- n00 + 1
        }
        if (hit[i] == 0L & hit[i - 1L] == 1L) {
            n01 <- n01 + 1
        }
        if (hit[i] == 1L & hit[i - 1L] == 0L) {
            n10 <- n10 + 1
        }
        if (hit[i] == 1L & hit[i - 1L] == 1L) {
            n11 <- n11 + 1
        }
    }
    pi0 <- n01 / (n00 + n01)
    pi1 <- n11 / (n10 + n11)
    pi <- (n01 + n11) / (n00 + n01 + n10 + n11)
    LRind <- -2 * log(((1 - pi)^(n00 + n10) * pi^(n01 + n11)) / ((1 -
        pi0)^n00 * pi0^n01 * (1 - pi1)^n10 * pi1^n11))
    if (is.nan(LRind)) {
        LRind <- -2 * ((n00 + n10) * log(1 - pi) + (n01 + n11) *
            log(pi) - n00 * log(1 - pi0) - n01 * log(pi0) - n10 *
            log(1 - pi1) - n11 * log(pi1))
    }
    LRpof <- kupiec_test(hit, int_width)["Test"]
    LRcc <- LRpof + LRind
    pvalue <- 1 - pchisq(LRcc, df = 2L)
    LRcc <- c(LRcc, pvalue)
    names(LRcc) <- c("Test", "Pvalue")
    return(LRcc)
}
# %%

# %%
reduce_mod_size <- function(mod) {
    mod$specification <- NULL
    mod$enames <- dimnames(mod$experts_loss)[["k"]]
    mod$experts_loss <- NULL
    return(mod)
}
# %%
