# %%
library(tidyverse)
library(conflicted)
library(plotly)
library(tikzDevice)
options(tikzDocumentDeclaration = "\\documentclass[preprint,12pt]{elsarticle}")
source("code/functions.R")
model_class <- "dummy"
source("code/prep.R")
conflict_prefer("layout", "plotly")
conflict_prefer("filter", "dplyr")
# %%

# %% Share properties important for consistency
zmax <- 0.35
colmax <- 0.5
# %%

# %% Plots specials
files <- list.files("results/JSU1_Norm4/specials", full.names = TRUE)
files <- files[str_detect(files, "bewa")]
files.names <- files %>%
    basename() %>%
    str_replace_all("_", ".") %>%
    str_remove(".rds") %>%
    str_remove("mod.")

dir.create("paper/anc/plots/specials", showWarnings = FALSE, recursive = TRUE)

f <- 1
for (f in seq_along(files)) {
    load(files[f])

    exp_sel <- "JSU1"

    plot <- plot_ly(
        x = 1:99 / 100,
        y = 1:24,
        z = mod$weights[T, , , which(dimnames(experts)[[4]] == exp_sel)],
        cmin = 0, # Important for color mapping
        cmax = colmax,
        type = "mesh3d",
        colors = colorRamp(c(
            cols[5, "deep-orange"],
            cols[5, "amber"],
            cols[5, "lime"],
            cols[5, "green"],
            cols[5, "teal"],
            cols[5, "indigo"],
            cols[5, "pink"]
        ))
    ) %>%
        layout(
            scene = list(
                zaxis = list(
                    range = c(0, zmax)
                )
            )
        ) %>%
        add_surface() %>%
        layout(
            title = paste0("Most recent weights ", exp_sel),
            scene = list(
                xaxis = list(title = "p"),
                yaxis = list(title = "h"),
                zaxis = list(title = "w")
            )
        ) %>%
        hide_colorbar() %>%
        config(displayModeBar = FALSE)

    plot

    plotly::save_image(plot, paste0("paper/anc/plots/specials/", files.names[f], "_", exp_sel, ".pdf"))
}
# %%


# %% Load best
file <- "results/JSU1_Norm4/bayesian_online/mod_ewa_smooth.forget.rds"

file.pars <- file %>%
    str_replace("mod", "pars") %>%
    str_replace("rds", "csv")
file.name <- file %>%
    str_replace_all("/", ".") %>%
    str_replace_all("_", ".") %>%
    str_remove(".rds") %>%
    str_remove("mod.") %>%
    str_remove("results.")
load(file)

pars <- read_csv(file.pars)

dir.create("paper/anc/plots/best", showWarnings = FALSE, recursive = TRUE)
# %%

# %% Plot 3D weights
exp_sel <- "JSU1"

plot <- plot_ly(
    x = 1:99 / 100,
    y = 1:24,
    z = mod$weights[T, , , which(dimnames(experts)[[4]] == exp_sel)],
    cmin = 0,
    cmax = colmax,
    type = "mesh3d",
    colors = colorRamp(c(
        cols[5, "deep-orange"],
        cols[5, "amber"],
        cols[5, "lime"],
        cols[5, "green"],
        cols[5, "teal"],
        cols[5, "indigo"],
        cols[5, "pink"]
    ))
) %>%
    layout(
        scene = list(
            zaxis = list(
                range = c(0, zmax)
            )
        )
    ) %>%
    add_surface() %>%
    layout(
        title = paste0("Most recent weights ", exp_sel),
        scene = list(
            xaxis = list(title = "p"),
            yaxis = list(title = "h"),
            zaxis = list(title = "w")
        )
    ) %>%
    hide_colorbar() %>%
    config(displayModeBar = FALSE)

plot

plotly::save_image(plot, paste0("paper/anc/plots/best/", file.name, "_", exp_sel, ".pdf"))
# %%

# %% Plots weights vs. time vs. quantile
h_ <- 17

dimnames(mod$weights) <- list(
    as.character(c(
        as.Date(dimnames(FF)[[1]]),
        as.Date(tail(dimnames(FF)[[1]], 1)) + 1
    )),
    1:24,
    as.character(1:99 / 100),
    dimnames(experts)[[4]]
)

weights <- as_tibble(ftable(mod$weights)) %>%
    select(date = Var1, hour = Var2, q = Var3, Expert = Var4, weight = Freq) %>%
    mutate(
        date = as.Date(date),
        hour = as.numeric(levels(hour))[hour],
        q = as.numeric(levels(q))[q]
    )

wplot <- weights %>%
    filter(hour == h_) %>%
    ggplot(aes(date, q, fill = weight)) +
    geom_raster(interpolate = TRUE) +
    facet_grid(
        Expert ~ . # , labeller = labeller(Mod = mod_labs)
    ) +
    theme_minimal() +
    theme(
        # plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        text = element_text(size = text_size),
        legend.key.height = unit(0.9, "inch")
    ) +
    scale_x_date(expand = c(0, 0)) +
    scale_fill_gradientn(
        oob = scales::squish,
        limits = c(0, 1),
        values = c(seq(0, 0.4, length.out = 8), 0.65, 1),
        colours = c(
            cols[8, "red"],
            cols[5, "deep-orange"],
            cols[5, "amber"],
            cols[5, "yellow"],
            cols[5, "lime"],
            cols[5, "light-green"],
            cols[5, "green"],
            cols[7, "green"],
            cols[9, "green"],
            cols[10, "green"]
        ),
        breaks = seq(0, 1, 0.1)
    ) +
    xlab("date") +
    ylab("probability") +
    scale_y_continuous(breaks = c(0.1, 0.5, 0.9))

wplot

# save(wplot, file = "presentation/23_06_ecmi/weights_hour.rds")

# wplot

tikz(
    file = paste0("paper/anc/plots/best/wplot_prob_", h_, "_", file.name, ".tex"), standAlone = FALSE,
    timestamp = FALSE,
    width = width,
    height = height * 1.5
)
print(wplot)
dev.off()
# %%

# %% # %% Plots weights vs. time vs. hour
q_ <- 0.5

wplot <- weights %>%
    dplyr::filter(q == q_) %>%
    mutate(hour = as.numeric(hour) - 1) %>%
    ggplot(aes(date, hour, fill = weight)) +
    geom_raster(interpolate = TRUE) +
    facet_grid(
        Expert ~ . # , labeller = labeller(Mod = mod_labs)
    ) +
    theme_minimal() +
    theme(
        # plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        text = element_text(size = text_size),
        legend.key.height = unit(0.9, "inch")
    ) +
    scale_x_date(expand = c(0, 0)) +
    scale_fill_gradientn(
        oob = scales::squish,
        limits = c(0, 1),
        values = c(seq(0, 0.4, length.out = 8), 0.65, 1),
        colours = c(
            cols[8, "red"],
            cols[5, "deep-orange"],
            cols[5, "amber"],
            cols[5, "yellow"],
            cols[5, "lime"],
            cols[5, "light-green"],
            cols[5, "green"],
            cols[7, "green"],
            cols[9, "green"],
            cols[10, "green"]
        ),
        breaks = seq(0, 1, 0.1)
    ) +
    xlab("date") +
    ylab("hour") +
    scale_y_continuous(breaks = c(0, 8, 16, 24))

# save(wplot, file = "presentation/23_06_ecmi/weights_prob.rds")

# wplot

tikz(
    file = paste0("paper/anc/plots/best/wplot_mv_", q_, "_", file.name, ".tex"),
    standAlone = FALSE,
    timestamp = FALSE,
    width = width,
    height = height * 1.5
)

print(wplot)
dev.off()
# %%

# %% Plots params
unique_vals_per_col <- apply(pars, 2, FUN = \(x) length(unique(x)))

dates <- as.Date(dimnames(experts)[[1]])

scaleFUN <- function(x) sprintf("%.2f", x)

pars_data <- pars[-1, unique_vals_per_col > 1] %>%
    mutate(dates = dates) %>%
    pivot_longer(-dates) %>%
    mutate(value = round(value, 4)) %>%
    mutate(name = str_replace_all(name, "_", ".")) %>%
    mutate(name = str_remove(name, "p."))

# pars_data[pars_data$name == "forget.regret", "name"] <- "Forget $\\theta$"
# pars_data[pars_data$name == "pr.lambda", "name"] <- "Smooth Pr $\\lambda^{text{pr}}$"
# pars_data[pars_data$name == "mv.lambda", "name"] <- "Smooth Mv $\\lambda^{text{mv}}$"

pars_plot <- pars_data %>%
    ggplot(aes(x = dates, y = value)) +
    geom_rect(aes(
        ymin = 0,
        ymax = value * 1.2,
        xmin = dates[1],
        xmax = dates[B],
        fill = "Burn-In"
    )) +
    geom_line(aes(color = name), linewidth = linesize, show.legend = FALSE) +
    scale_colour_manual(
        values = as.character(cols[5, c("pink", "amber", "green")])
    ) +
    facet_grid(name ~ .,
        scales = "free_y",
        # switch = "both"
    ) +
    scale_y_continuous(
        trans = "log2",
        labels = scaleFUN
    ) +
    theme_minimal() +
    theme(
        # plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        text = element_text(size = text_size),
        legend.key.width = unit(0.9, "inch"),
        legend.position = "none"
    ) +
    ylab(NULL) +
    xlab("date") +
    scale_fill_manual(NULL,
        values = as.character(cols[3, "grey"])
    )

pars_plot

# save(pars_plot, file = "presentation/23_06_ecmi/pars_plot.rds")

tikz(
    file = paste0("paper/anc/plots/best/pars_", file.name, ".tex"), standAlone = FALSE,
    timestamp = FALSE,
    width = width,
    height = height
)
print(pars_plot)
dev.off()
# %%
