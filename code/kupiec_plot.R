# %%
library(tidyverse)
library(conflicted)
source("code/functions.R")
conflict_prefer("layout", "plotly")
conflict_prefer("filter", "dplyr")
# %%

dirs <- list.dirs("results", recursive = FALSE)

d.i <- 2
for (d.i in seq_along(dirs)) {
    load(paste0(dirs[d.i], "/coverage_tests.rds"))
    load(paste0(dirs[d.i], "/coverage_tests_experts_naive.rds"))

    coverage_tests <- abind::abind(
        coverage_tests_experts_naive,
        coverage_tests,
        along = 1
    )

    dimnames(coverage_tests)[[1]] <-
        tibble(Description = dimnames(coverage_tests)[[1]]) %>%
        separate_wider_delim(
            Description,
            "/",
            names = c("Parameter Tuning", "Description"),
            too_few = "align_end"
        ) %>%
        mutate(
            `Parameter Tuning` = str_replace_all(`Parameter Tuning`, "_", " "),
            `Parameter Tuning` = str_to_title(`Parameter Tuning`),
            Description = str_remove(Description, ".rds"),
            Description = str_remove(Description, "mod_"),
            Description = str_replace_all(Description, "_", " "),
            Description = str_to_title(Description)
        ) %>%
        unite("Description", c("Parameter Tuning", "Description"), sep = " - ") %>%
        pull(Description) %>%
        stringr::str_remove_all("NA - ") %>%
        stringr::str_remove_all("Specials - ") %>%
        # stringr::str_replace_all("Bayesian Online -", "BO -") %>%
        # stringr::str_replace_all("Sampling Online -", "SO -") %>%
        # stringr::str_replace_all("Bayesian Fix -", "BF -") %>%
        stringr::str_replace_all("Smooth.forget", "Smooth.Forget") %>%
        stringr::str_replace_all("Jsu", "JSU")

    coverage_sub <- coverage_tests[, , "bewa", , , ]

    kc <- "christoffersen"
    for (kc in c("kupiec", "christoffersen")) {
        if (kc == "kupiec") {
            col_scale_values <- qchisq(1 - c(1, 0.1, 0.05, 0.01, 0.001), df = 1)
            col_scale_breaks <- (seq(0, ceiling(max(col_scale_values)), 2))
        } else {
            col_scale_values <- qchisq(1 - c(1, 0.1, 0.05, 0.01, 0.001), df = 2)
            col_scale_breaks <- seq(0, ceiling(max(col_scale_values)), 2)
        }

        coverage_plot <- array2DF(coverage_sub) %>%
            as_tibble() %>%
            filter(Var3 == kc) %>%
            pivot_wider(
                names_from = Var5,
                values_from = Value
            ) %>%
            mutate(
                Var1 = factor(Var1, levels = rev(dimnames(coverage_tests)[[1]])),
                Var3 = str_to_title(kc),
                p.val = as.numeric(p.val),
                significance = case_when(
                    p.val > 0.05 & p.val <= 0.1 ~ "0.1",
                    p.val > 0.01 & p.val <= 0.05 ~ "0.05",
                    p.val > 0.001 & p.val <= 0.01 ~ "0.01",
                    p.val <= 0.001 ~ "0.001"
                ),
                Var2 = factor(Var2, levels = sort(unique(Var2))),
                Var4 = paste0("Interval: ", as.numeric(Var4) * 100, "\\%")
            ) %>%
            ggplot(aes(
                x = Var2,
                y = Var1
            )) +
            geom_tile(
                aes(
                    fill = stat
                )
            ) +
            geom_point(
                aes(
                    shape = significance
                ),
                size = 2
            ) +
            facet_grid(
                Var4 ~ Var3 # ,
                # labeller = labeller(Loss = Loss_labs)
            ) +
            scale_fill_gradientn(
                name = "Statistic",
                colours = rev(c(
                    cols[5, "red"],
                    cols[5, "orange"],
                    cols[5, "yellow"],
                    cols[5, "light-green"],
                    cols[9, "green"]
                )),
                values = scales::rescale(col_scale_values),
                na.value = "grey15",
                guide = guide_colourbar(
                    title.vjust = 0.9, # barheight = 1
                    barwidth = 10,
                ),
                breaks = col_scale_breaks,
                limits = range(col_scale_values),
                oob = scales::oob_squish_any
            ) +
            # coord_fixed() +
            labs(x = "Hour", y = NULL) +
            scale_size(guide = "none") +
            scale_alpha_discrete(guide = "none", range = c(0.2, 1)) +
            scale_shape_manual(
                values = c(16, 17, 4, 3),
                name = "Significance",
                guide = guide_legend(override.aes = list(size = 3)),
                na.translate = FALSE
            ) +
            theme_minimal() +
            theme(
                axis.text.y = element_text(hjust = 0),
                text = element_text(size = text_size),
                legend.position = "bottom",
                legend.justification = "right",
                strip.background = element_rect(fill = "grey95", colour = "grey95")
            ) +
            scale_x_discrete(guide = guide_axis(angle = 45))

        coverage_plot

        tikz(
            file = paste0(
                "paper/anc/",
                stringr::str_remove(dirs[d.i], "results/"),
                "/coverage_plot_", kc, ".tex"
            ), standAlone = FALSE,
            timestamp = FALSE,
            width = width,
            height = height * 3
        )
        print(coverage_plot)
        dev.off()
    }
}
