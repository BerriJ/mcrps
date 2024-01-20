source("code/functions.R")

dirs <- list.dirs("data", recursive = FALSE)

files <- dirs %>%
    purrr::map(list.files, full.names = TRUE)

jsu <- files[[1]] %>%
    purrr::map(list.files, full.names = TRUE)

jsu_preds_list <- list()

norm <- files[[2]] %>%
    purrr::map(list.files, full.names = TRUE)

norm_preds_list <- jsu_preds_list

f.i <- 1
for (f.i in seq_along(jsu)) {
    jsu_preds_list[[f.i]] <- list()
    norm_preds_list[[f.i]] <- list()
    files_jsu <- jsu[[f.i]]
    files_norm <- norm[[f.i]]
    for (i in seq_along(files_jsu)) {
        jsu_preds_list[[f.i]][[i]] <- readr::read_delim(
            files_jsu[i],
            col_names = FALSE,
            progress = FALSE,
            show_col_types = FALSE
        ) # %>%
        # mutate_all(~ .x - mean(.x)) %>%
        # pivot_longer(everything()) %>%
        # pull(value)

        norm_preds_list[[f.i]][[i]] <- readr::read_delim(
            files_norm[i],
            col_names = FALSE,
            progress = FALSE,
            show_col_types = FALSE
        ) # %>%
        # mutate_all(~ .x - mean(.x)) %>%
        # pivot_longer(everything()) %>%
        # pull(value)
        cat(i, "\r")
    }
}

jsu_preds <- purrr::map(jsu_preds_list, bind_rows) %>%
    purrr::map(~ mutate_all(.x, ~ .x - mean(.x))) %>%
    purrr::map(~ pivot_longer(.x, everything())) %>%
    purrr::map(~ pull(.x, value)) %>%
    bind_cols()

norm_preds <- purrr::map(norm_preds_list, bind_rows) %>%
    purrr::map(~ mutate_all(.x, ~ .x - mean(.x))) %>%
    purrr::map(~ pivot_longer(.x, everything())) %>%
    purrr::map(~ pull(.x, value)) %>%
    bind_cols()

preds <- cbind(norm_preds, jsu_preds)

colnames(preds) <- c(
    paste0("norm", 1:dim(norm_preds)[2]),
    paste0("jsu", 1:dim(jsu_preds)[2])
)
set.seed(1)
# 2^26 ~37 Mio -> Takes around an hour to compute
idx <- sample.int(dim(preds)[1], 2^26)

# %%
M <- cor(preds[idx, ])
MD <- M
for (i in 1:dim(M)[1]) {
    for (j in i:dim(M)[1]) {
        if (j > i) {
            MD[i, j] <- dccpp::dcor(preds[idx, i], preds[idx, j])
        }
    }
    print(lubridate::now())
}
# %%

# %% ggplot corrplot

# We get MD from above
MD_ <- MD
MD2 <- MD
MD2[upper.tri(MD_)] <- TRUE
MD2[lower.tri(MD_)] <- FALSE

is_upper <- MD2 %>%
    as_tibble() %>%
    mutate(var1 = rownames(M)) %>%
    pivot_longer(-var1, names_to = "var2", values_to = "value") %>%
    pull(value)

colnames(MD_) <- paste0(LETTERS[1:dim(MD_)[2]], colnames(MD_))
rownames(MD_) <- paste0(LETTERS[dim(MD_)[2]:1], rownames(MD_))

diag(MD_) <- NA

plot_df <- MD_ %>%
    as_tibble() %>%
    mutate(var1 = rownames(MD_)) %>%
    pivot_longer(-var1, names_to = "var2", values_to = "value") %>%
    mutate(
        is_upper = is_upper
    ) %>%
    drop_na() %>%
    mutate(label = format(round(value, 2), nsmall = 2)) %>%
    filter(var1 != var2)

labx <- c(stringr::str_to_title(colnames(M)[1:4]), stringr::str_to_upper(colnames(M)[5:8]))
laby <- rev(labx)

upper_df <- plot_df %>% filter(is_upper == TRUE)
lower_df <- plot_df %>% filter(is_upper == FALSE)

plot <- ggplot() +
    geom_tile(data = upper_df, aes(x = var2, y = var1, fill = value)) +
    geom_text(data = upper_df, aes(x = var2, y = var1, label = label), size = 4) +
    ggplot2::scale_fill_gradientn(
        colours = c(
            cols[1:8, "blue"]
        ),
        breaks = seq(from = 0, to = 1, length.out = 5),
        limits = range(upper_df$value),
        guide = guide_colourbar(
            title = "Distance Correlation",
            barwidth = unit(0.2, "npc")
        )
    ) +
    new_scale_fill() +
    geom_tile(data = lower_df, aes(x = var2, y = var1, fill = value)) +
    geom_text(data = lower_df, aes(x = var2, y = var1, label = label), size = 4) +
    xlab(NULL) +
    ylab(NULL) +
    ggplot2::scale_fill_gradientn(
        colours = c(
            cols[1:9, "yellow"]
        ),
        breaks = seq(from = -1, to = 1, length.out = 5),
        limits = range(lower_df$value),
        guide = guide_colourbar(
            title = "Correlation",
            barwidth = unit(0.2, "npc")
        )
    ) +
    theme_minimal() +
    theme(
        axis.text.y = element_text(hjust = 0),
        legend.position = "none",
        legend.title = element_blank(),
        text = element_text(
            size = text_size,
        )
    ) +
    scale_x_discrete(labels = labx) +
    scale_y_discrete(labels = laby)

plot

dir.create("paper/anc/plots", showWarnings = FALSE, recursive = TRUE)

tikz(
    file = "paper/anc/plots/corrplot.tex", standAlone = FALSE,
    timestamp = FALSE,
    width = width,
    height = height
)
print(plot)
dev.off()
# %%
