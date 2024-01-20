# %%
library(profoc)
library(tidyverse)
library(conflicted)
library(plotly)
library(tikzDevice)
options(tikzDocumentDeclaration = "\\documentclass[preprint,12pt]{elsarticle}")
source("code/functions.R")
conflict_prefer("layout", "plotly")
conflict_prefer("filter", "dplyr")
# %%

# %%
x <- 1:99 / 100
i <- 1
n <- 5 # Number of inner knots
deg <- 2

knot_pars <- expand.grid(
    mu = c(0.25, 0.5, 0.75),
    sigma = 2^c(-2, 0, 2),
    nonc = c(-4, 0, 4),
    tailw = 2^c(-2, 0, 2)
)

knots <- profoc:::make_knots(n,
    mu = knot_pars[i, "mu"],
    sig = knot_pars[i, "sigma"],
    nonc = knot_pars[i, "nonc"],
    tailw = knot_pars[i, "tailw"],
    deg = deg
)

basis <- as.matrix(profoc:::make_basis_matrix(x, knots, deg)) %>%
    as_tibble() %>%
    mutate(
        index = x,
        mu = paste0("$\\mu = ", knot_pars[i, "mu"], "$"),
        sigma = paste0("$\\sigma = ", knot_pars[i, "sigma"], "$"),
        nonc = paste0("$c = ", knot_pars[i, "nonc"], "$"),
        tailw = paste0("$\\tau = ", knot_pars[i, "tailw"], "$")
    ) %>%
    pivot_longer(!all_of(c("mu", "sigma", "nonc", "tailw", "index")))

for (i in 2:nrow(knot_pars)) {
    knots <- profoc:::make_knots(n,
        mu = knot_pars[i, "mu"],
        sig = knot_pars[i, "sigma"],
        nonc = knot_pars[i, "nonc"],
        tailw = knot_pars[i, "tailw"],
        deg = deg
    )

    basis_temp <- as.matrix(profoc:::make_basis_matrix(x, knots, deg)) %>%
        as_tibble() %>%
        mutate(
            index = x,
            mu = paste0("$\\mu = ", knot_pars[i, "mu"], "$"),
            sigma = paste0("$\\sigma = ", knot_pars[i, "sigma"], "$"),
            nonc = paste0("$c = ", knot_pars[i, "nonc"], "$"),
            tailw = paste0("$\\tau = ", knot_pars[i, "tailw"], "$")
        ) %>%
        pivot_longer(!all_of(c("mu", "sigma", "nonc", "tailw", "index")))

    basis <- rbind(basis, basis_temp)
}

basis <- basis %>%
    mutate(
        sigma = factor(sigma,
            levels =
                paste0("$\\sigma = ", unique(knot_pars[, "sigma"]), "$")
        ),
        tailw = factor(tailw,
            levels =
                paste0("$\\tau = ", unique(knot_pars[, "tailw"]), "$")
        ),
        nonc = factor(nonc,
            levels =
                paste0("$c = ", unique(knot_pars[, "nonc"]), "$")
        )
    )

plot <- basis %>%
    filter(
        nonc == "$c = 0$",
        tailw == "$\\tau = 1$"
    ) %>%
    ggplot(aes(x = index, y = value, col = name)) +
    geom_line(linewidth = linesize) +
    theme_minimal() +
    xlab(NULL) +
    ylab(NULL) +
    facet_grid(sigma ~ mu) +
    scale_color_manual(
        values = as.character(cols[6, 1 + c(2, 4, 6, 8, 10, 12, 14, 16)])
    ) +
    theme(
        # plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        text = element_text(size = text_size),
        legend.key.width = unit(0.9, "inch"),
        legend.position = "none",
        panel.spacing.x = unit(4, "mm")
    )

plot

tikz(
    file = paste0("paper/anc/plots/knots.tex"),
    standAlone = FALSE,
    timestamp = FALSE,
    width = width,
    height = height
)
print(plot)
dev.off()

plot <- basis %>%
    mutate(sigma = factor(sigma,
        levels =
            paste0("$\\sigma = ", unique(knot_pars[, "sigma"]), "$")
    )) %>%
    filter(
        mu == "$\\mu = 0.5$",
        sigma == "$\\sigma = 1$"
    ) %>%
    ggplot(aes(x = index, y = value, col = name)) +
    geom_line(linewidth = linesize) +
    theme_minimal() +
    xlab(NULL) +
    ylab(NULL) +
    facet_grid(tailw ~ nonc) +
    scale_color_manual(
        values = as.character(cols[6, 1 + c(2, 4, 6, 8, 10, 12, 14, 16)])
    ) +
    theme(
        # plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        text = element_text(size = text_size),
        legend.key.width = unit(0.9, "inch"),
        legend.position = "none",
        panel.spacing.x = unit(4, "mm")
    )

plot

tikz(
    file = paste0("paper/anc/plots/knots2.tex"),
    standAlone = FALSE,
    timestamp = FALSE,
    width = width,
    height = height
)
print(plot)
dev.off()
# %%
