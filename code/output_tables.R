# %%
library(tidyverse)
library(knitr)
library(kableExtra)
library(conflicted)
source("code/functions.R")
# %%

# %% Write loss and dm tables
B <- 182
# Get filepaths of the results
dirs <- list.dirs("results", recursive = FALSE)

d.i <- 1
for (d.i in seq_along(dirs)) {
    load(paste0(dirs[d.i], "/loss_and_dm.rds"))
    dimnames(loss_and_dm)[[2]] <- c("BOA", "ML-Poly", "EWA")
    loss_and_dm <- loss_and_dm[c(15, 17:16, 14:13, 1:12), , ]


    load(paste0(dirs[d.i], "/quantile_crossings.rds"))
    dimnames(quantile_crossings)[[4]] <- c("BOA", "ML-Poly", "EWA")
    quantile_crossings <- quantile_crossings[c(15, 17:16, 14:13, 1:12), , , ]

    performance_loss_tibble <- loss_and_dm[, , "loss"] %>%
        as_tibble() %>%
        mutate(
            Description = dimnames(loss_and_dm)[[1]],
            Description = str_remove(Description, ".rds"),
            Description = str_remove(Description, "mod_")
        ) %>%
        separate_wider_delim(
            Description,
            "/",
            names = c("Parameter Tuning", "Description")
        ) %>%
        select(Description, `Parameter Tuning`, everything()) %>%
        mutate(
            BOA = sprintf(round(BOA, 4), fmt = "%#.4f"),
            `ML-Poly` = sprintf(round(`ML-Poly`, 4), fmt = "%#.4f"),
            EWA = sprintf(round(EWA, 4), fmt = "%#.4f"),
            `Parameter Tuning` = str_replace_all(`Parameter Tuning`, "_", " "),
            `Parameter Tuning` = str_replace_all(`Parameter Tuning`, "specials", ""),
            `Parameter Tuning` = str_to_title(`Parameter Tuning`),
            Description = str_replace_all(Description, "_", " "),
            Description = str_to_title(Description),
            Description = str_replace_all(Description, "Ftl", "FTL")
        )

    i <- 1
    j <- 1
    for (j in 1:3) {
        cols_tmp <- col_scale2(
            loss_and_dm[, j, "stat", drop = TRUE],
            rng_t
        ) %>%
            str_remove_all("#")

        cols_ltx <- paste0("\\cellcolor[HTML]{", cols_tmp, "} ")

        best_idx <- which.min(loss_and_dm[, j, "loss"])
        cols_ltx[best_idx] <- paste0(
            cols_ltx[best_idx],
            "\\Bold "
        )

        # Add cols to the table
        performance_loss_tibble[, 2 + j] <- paste0(
            cols_ltx,
            performance_loss_tibble[, 2 + j, drop = TRUE]
        )

        for (i in seq_len(nrow(performance_loss_tibble))) {
            if (loss_and_dm[i, j, "p.val"] <= 0.001) {
                performance_loss_tibble[i, 2 + j] <- paste0(
                    performance_loss_tibble[i, 2 + j], " { $^{***}$}"
                )
            } else if (loss_and_dm[i, j, "p.val"] <= 0.01) {
                performance_loss_tibble[i, 2 + j] <- paste0(
                    performance_loss_tibble[i, 2 + j], " { $^{**}$}"
                )
            } else if (loss_and_dm[i, j, "p.val"] <= 0.05) {
                performance_loss_tibble[i, 2 + j] <- paste0(
                    performance_loss_tibble[i, 2 + j], " { $^{*}$}"
                )
            } else if (loss_and_dm[i, j, "p.val"] <= 0.1) {
                performance_loss_tibble[i, 2 + j] <- paste0(
                    performance_loss_tibble[i, 2 + j], "{ ${.}$}"
                )
            } else {
                performance_loss_tibble[i, 2 + j] <- paste0(
                    performance_loss_tibble[i, 2 + j], ""
                )
            }
        }
    }

    colnames(performance_loss_tibble) <- paste0(
        "{",
        colnames(performance_loss_tibble),
        "}"
    )

    table_performance <- performance_loss_tibble %>%
        kbl(
            caption = paste0("TODO"),
            digits = 4,
            linesep = "",
            # Dont replace any string, dataframe has to be valid latex code ...
            escape = FALSE,
            format = "latex",
            booktabs = TRUE,
            align = c("l", "l", rep("S[table-format=1.4{$^{***}$}]", ncol(loss_and_dm)))
        )


    table_performance %>%
        footnote(
            general = c(
                paste0(
                    "Coloring w.r.t. test statistic: ",
                    col_note,
                    collapse = ""
                ),
                "{$^{*}$}p<0.05; {$^{**}$}p<0.01; {$^{***}$}p<0.001;"
            ),
            general_title = "",
            escape = FALSE,
            threeparttable = TRUE,
            fixed_small_size = TRUE
        ) %>%
        kable_styling(latex_options = c(
            "hold_position"
            # "scale_down"
        )) %>%
        save_kable(file = paste0(
            "paper/anc/",
            stringr::str_remove(dirs[d.i], "results/"),
            "/tab_performance.tex"
        ))
    # table_fin[[1]] <- stringr::str_replace(table_fin[[1]], "\\{tablenotes\\}", "{tablenotes}[flushleft]")

    # Write duration table

    duration_tibble <- loss_and_dm[, , "duration"] %>%
        as_tibble() %>%
        mutate(
            Description = dimnames(loss_and_dm)[[1]],
            Description = str_remove(Description, ".rds"),
            Description = str_remove(Description, "mod_")
        ) %>%
        separate_wider_delim(
            Description,
            "/",
            names = c("Parameter Tuning", "Description")
        ) %>%
        select(Description, `Parameter Tuning`, everything()) %>%
        mutate(
            BOA = sprintf(round(BOA, 4), fmt = "%#.4f"),
            `ML-Poly` = sprintf(round(`ML-Poly`, 4), fmt = "%#.4f"),
            EWA = sprintf(round(EWA, 4), fmt = "%#.4f"),
            `Parameter Tuning` = str_to_title(`Parameter Tuning`),
            `Parameter Tuning` = str_replace_all(`Parameter Tuning`, "_", " "),
            `Parameter Tuning` = str_replace_all(`Parameter Tuning`, "Specials", ""),
            Description = str_replace_all(Description, "_", " "),
            Description = str_to_title(Description),
            Description = str_replace_all(Description, "Ftl", "FTL")
        )

    i <- 1
    j <- 1
    for (j in 1:3) {
        cols_tmp <- col_scale2(
            loss_and_dm[, j, "duration", drop = TRUE],
            range(unlist(loss_and_dm[, j, "duration", drop = TRUE]))
        ) %>%
            str_remove_all("#")

        cols_ltx <- paste0("\\cellcolor[HTML]{", cols_tmp, "} ")

        best_idx <- which.min(loss_and_dm[, j, "duration"])
        cols_ltx[best_idx] <- paste0(
            cols_ltx[best_idx],
            "\\Bold "
        )

        # Add cols to the table
        duration_tibble[, 2 + j] <- paste0(
            cols_ltx,
            duration_tibble[, 2 + j, drop = TRUE]
        )
    }

    table_duration <- duration_tibble %>%
        kbl(
            caption = paste0("TODO"),
            digits = 4,
            linesep = "",
            # Dont replace any string, dataframe has to be valid latex code ...
            escape = FALSE,
            format = "latex",
            booktabs = TRUE,
            align = c("l", "l", rep("c", ncol(loss_and_dm)))
        ) %>%
        kable_paper(full_width = FALSE)

    # for (i in 1:3) {
    #     table_duration <- table_duration %>%
    #         column_spec(i + 2,
    #             background =
    #                 col_scale2(
    #                     loss_and_dm[, i, "stat", drop = TRUE],
    #                     rng_t
    #                 ),
    #             bold = min(loss_and_dm[, i, "duration"]) == loss_and_dm[, i, "duration"]
    #         )
    # }

    table_duration %>%
        kable_styling(latex_options = c(
            "hold_position"
            # "scale_down"
        )) %>%
        save_kable(file = paste0(
            "paper/anc/",
            stringr::str_remove(dirs[d.i], "results/"),
            "/tab_duration.tex"
        ))


    table_crossings <- apply(
        apply(quantile_crossings[, -c(1:B), , ], c(1, 2, 4), sum) >= 1,
        MARGIN = c(1, 3),
        sum
    ) %>%
        as_tibble() %>%
        mutate(
            Description = dimnames(loss_and_dm)[[1]],
            Description = str_remove(Description, ".rds"),
            Description = str_remove(Description, "mod_")
        ) %>%
        separate_wider_delim(
            Description,
            "/",
            names = c("Parameter Tuning", "Description")
        ) %>%
        mutate(
            `Parameter Tuning` = str_replace_all(`Parameter Tuning`, "_", " "),
            `Parameter Tuning` = str_replace_all(`Parameter Tuning`, "specials", ""),
            `Parameter Tuning` = str_to_title(`Parameter Tuning`),
            Description = str_replace_all(Description, "_", " "),
            Description = str_to_title(Description),
            Description = str_replace_all(Description, "Ftl", "FTL")
        ) %>%
        select(Description, `Parameter Tuning`, everything())

    table_crossings %>%
        kbl(
            caption = paste0("TODO"),
            digits = 4,
            linesep = "",
            # Dont replace any string, dataframe has to be valid latex code ...
            escape = FALSE,
            format = "latex",
            booktabs = TRUE,
            align = c("l", "l", rep("c", ncol(loss_and_dm)))
        ) %>%
        kable_paper(full_width = FALSE) %>%
        kable_styling(latex_options = c(
            "hold_position"
            # "scale_down"
        )) %>%
        save_kable(file = paste0(
            "paper/anc/",
            stringr::str_remove(dirs[d.i], "results/"),
            "/tab_crossings.tex"
        ))
}
# %%
