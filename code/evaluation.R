library(tidyr)
library(dplyr)
library(purrr)
library(stringr)

source("code/functions.R")

### Kupiec test

dirs <- list.dirs("results", recursive = FALSE)

files <- dirs %>%
    purrr::map(list.dirs) %>%
    purrr::map(list.files, full.names = TRUE) %>%
    purrr::map(.f = ~ .x[stringr::str_detect(.x, "mod_")]) %>%
    purrr::map(str_remove_all, "_ml_poly") %>%
    purrr::map(str_remove_all, "_bewa") %>%
    purrr::map(str_remove_all, "_ewa") %>%
    purrr::map(unique)

names(files) <- dirs

levels <- c(0.5, 0.9)
methods <- c("bewa", "ml_poly", "ewa")

d.i <- 1
for (d.i in seq_along(dirs)) {
    cat(dirs[d.i], "\n")

    load(paste0(dirs[d.i], "/naive_err.rds"))
    load(paste0(dirs[d.i], "/data/y.rds"))
    load(paste0(dirs[d.i], "/data/experts.rds"))

    coverage_tests <- array(
        dim = c(
            length(files[[d.i]]),
            24, length(methods),
            2,
            length(levels), 2
        ),
        dimnames = list(
            stringr::str_remove_all(files[[d.i]], paste0(dirs[d.i], "/")),
            sprintf(1:24, fmt = "%02d"),
            methods,
            c("kupiec", "christoffersen"),
            levels,
            c("stat", "p.val")
        )
    )

    quantile_crossings <- array(
        dim = c(
            length(files[[d.i]]),
            736,
            24,
            length(methods)
        ),
        dimnames = list(
            stringr::str_remove_all(files[[d.i]], paste0(dirs[d.i], "/")),
            NULL,
            NULL,
            methods
        )
    )

    loss_and_dm <- array(
        dim = c(length(files[[d.i]]), length(methods), 4),
        dimnames = list(
            stringr::str_remove_all(files[[d.i]], paste0(dirs[d.i], "/")),
            methods,
            c("loss", "stat", "p.val", "duration")
        )
    )

    files_sel <- files[[d.i]]

    f.i <- 1
    for (f.i in seq_along(files_sel)) {
        file <- files_sel[f.i]
        cat(file, "\n")
        m.i <- 1
        for (m.i in seq_along(methods)) {
            method <- methods[m.i]
            cat(method, "\n")
            load(str_replace(file, "mod_", paste0("mod_", methods[m.i], "_")))

            quantile_crossings[f.i, , , method] <- mod$predictions_got_sorted

            loss_and_dm[f.i, method, c("loss", "stat", "p.val")] <- DM.test(
                EA = apply(mod$fore[(B + 1):dim(mod$fore)[1], , ], 1, mean),
                EB = naive_err
            )

            loss_and_dm[f.i, method, "duration"] <- as.numeric(mod$duration)

            h.i <- 1
            for (h.i in 1:24) {
                # 50 Percent coverage
                preds_low_high <- mod$predictions[(B + 1):dim(mod$predictions)[1], h.i, c(25, 75)]
                obs <- y[(B + 1):dim(y)[1], h.i]

                hits <- get_hits(
                    obs = obs,
                    q_forecast_low = preds_low_high[, 1],
                    q_forecast_high = preds_low_high[, 2]
                )

                coverage_tests[f.i, h.i, method, "kupiec", 1, ] <-
                    kupiec_test(hits, int_width = 0.5)
                coverage_tests[f.i, h.i, method, "christoffersen", 1, ] <-
                    christoffersen_test(hits, int_width = 0.5)

                preds_low_high <- mod$predictions[(B + 1):dim(mod$predictions)[1], h.i, c(5, 95)]

                hits <- get_hits(
                    obs = obs,
                    q_forecast_low = preds_low_high[, 1],
                    q_forecast_high = preds_low_high[, 2]
                )

                coverage_tests[f.i, h.i, method, "kupiec", 2, ] <-
                    kupiec_test(hits, int_width = 0.1)
                coverage_tests[f.i, h.i, method, "christoffersen", 2, ] <-
                    christoffersen_test(hits, int_width = 0.1)
            }
        } # i.m
    } # f.i
    save(coverage_tests,
        file = paste0(dirs[d.i], "/coverage_tests.rds")
    )
    save(quantile_crossings,
        file = paste0(dirs[d.i], "/quantile_crossings.rds")
    )
    save(loss_and_dm,
        file = paste0(dirs[d.i], "/loss_and_dm.rds")
    )
} # d.i

d.i <- 1
for (d.i in seq_along(dirs)) {
    load(paste0(dirs[d.i], "/naive.rds"))
    load(paste0(dirs[d.i], "/data/y.rds"))
    load(paste0(dirs[d.i], "/data/experts.rds"))

    experts_ <- experts

    coverage_tests_experts_naive <- array(
        dim = c(
            dim(experts_)[4] + 1,
            24,
            length(methods),
            2,
            length(levels),
            2
        ),
        dimnames = list(
            c("Naive", mod$enames),
            sprintf(1:24, fmt = "%02d"),
            methods,
            c("kupiec", "christoffersen"),
            levels,
            c("stat", "p.val")
        )
    )
    i.m <- 1
    for (i.m in seq_along(methods)) {
        i.e <- 1
        # Naive
        h.i <- 1
        for (h.i in 1:24) {
            # 50 Percent coverage
            preds_low_high <- mod$predictions[(B + 1):dim(mod$predictions)[1], h.i, c(25, 75)]
            obs <- y[(B + 1):dim(y)[1], h.i]

            hits <- get_hits(
                obs = obs,
                q_forecast_low = preds_low_high[, 1],
                q_forecast_high = preds_low_high[, 2]
            )

            coverage_tests_experts_naive[i.e, h.i, i.m, "kupiec", 1, ] <-
                kupiec_test(hits, int_width = 0.5)
            coverage_tests_experts_naive[i.e, h.i, i.m, "christoffersen", 1, ] <-
                christoffersen_test(hits, int_width = 0.5)

            preds_low_high <- mod$predictions[(B + 1):dim(mod$predictions)[1], h.i, c(5, 95)]

            hits <- get_hits(
                obs = obs,
                q_forecast_low = preds_low_high[, 1],
                q_forecast_high = preds_low_high[, 2]
            )

            coverage_tests_experts_naive[i.e, h.i, i.m, "kupiec", 2, ] <-
                kupiec_test(hits, int_width = 0.1)
            coverage_tests_experts_naive[i.e, h.i, i.m, "christoffersen", 2, ] <-
                christoffersen_test(hits, int_width = 0.1)
        }

        i.e <- 2
        for (i.e in 1 + seq_len(dim(experts_)[4])) {
            h.i <- 1
            for (h.i in 1:24) {
                # 50 Percent coverage
                preds_low_high <- experts_[(B + 1):dim(experts_)[1], h.i, c(25, 75), i.e - 1]
                obs <- y[(B + 1):dim(y)[1], h.i]

                hits <- get_hits(
                    obs = obs,
                    q_forecast_low = preds_low_high[, 1],
                    q_forecast_high = preds_low_high[, 2]
                )

                coverage_tests_experts_naive[i.e, h.i, i.m, "kupiec", 1, ] <-
                    kupiec_test(hits, int_width = 0.5)
                coverage_tests_experts_naive[i.e, h.i, i.m, "christoffersen", 1, ] <-
                    christoffersen_test(hits, int_width = 0.5)

                preds_low_high <- experts_[(B + 1):dim(experts_)[1], h.i, c(5, 95), i.e - 1]

                hits <- get_hits(
                    obs = obs,
                    q_forecast_low = preds_low_high[, 1],
                    q_forecast_high = preds_low_high[, 2]
                )

                coverage_tests_experts_naive[i.e, h.i, i.m, "kupiec", 2, ] <-
                    kupiec_test(hits, int_width = 0.1)
                coverage_tests_experts_naive[i.e, h.i, i.m, "christoffersen", 2, ] <-
                    christoffersen_test(hits, int_width = 0.1)
            }
        }
    }

    save(coverage_tests_experts_naive,
        file = paste0(dirs[d.i], "/coverage_tests_experts_naive.rds")
    )
}
