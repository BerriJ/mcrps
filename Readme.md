## Multivariate Probabilistic CRPS Learning with an Application to Day-Ahead Electricity Prices   

This repository contains the code and data for reproducing the results, figures, and tables presented in [1]. A preprint is available on [arXiv](https://arxiv.org/abs/2303.10019).


.. [1] Berrisch, Jonathan, and Florian Ziel. "**Multivariate Probabilistic CRPS Learning with an Application to Day-Ahead Electricity Prices**" International Journal of Forecasting (Forthcoming).

## Replication

### System Requirements

We ran the code using the following system:

```
R version 4.3.2 (2023-10-31)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 22.04.3 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/atlas/libblas.so.3.10.3 
LAPACK: /usr/lib/x86_64-linux-gnu/atlas/liblapack.so.3.10.3;  LAPACK version 3.10.0
```

### Required R Packages

The following `R` snipped sets your CRAN Server to a snapshot provided by posit. This way, you can install the required packages in the exact version used for the paper.

```
options(repos = c(
    CRAN = "https://packagemanager.posit.co/cran/__linux__/jammy/2023-12-04")
    )
```

The following `R` code installs all required packages to run the code:

```
remotes::install_github("berrij/profoc@1.3.1")
install.packages(c(
    "mlrMBO",       # 1.1.5.1
    "dccpp",        # 0.1.0
    "tictoc",       # 1.2
    "conflicted",   # 1.2.0
    "ggplot2",      # 3.4.4
    "dplyr",        # 1.1.4   
    "purrr",        # 1.0.2
    "tidyr",        # 1.3.0
    "stringr",      # 1.5.1
    "readr",        # 2.1.4
    "tibble",       # 3.2.1
    "tikzDevice",   # 0.12.6
    "ggnewscale",   # 0.4.9
    "ggsci",        # 3.0.0
    "plotly",       # 4.10.3
    "gt",           # 0.10.0
    "knitr",        # 1.45
    "kableExtra"    # 1.3.4
    ))
```

### Data

The primary data (realizations and expert predictions as quantiles) used in this paper is available in the [data](data) folder. 

For recreating the [correlation plot](code/corrplot.R) we provide a sample of the first 100 (out of 10000) trajectories in the data folder: [data/preds_traj_sample.rds](data/preds_traj_sample.rds). The full dataset is about 4GB in size (the trajectories of the expert predictions) and therefore too big to be uploaded to github. We will provide it on request. If provided, the data should be placed in the data folder. The commented lines 1-55 in [correlation plot](code/corrplot.R) will read and process the data.

### Rerun the Code

To reproduce the results, figures, and tables presented in [1] you can run the `R` script [00_run_all.R](code/00_run_all.R) . This script will:

- Compute the discussed computation schemes (weights and predictions)
- It will store results in a "results" folder, which will be created in the root folder of this repository
- Evaluation results will also be stored in the "results" folder
- Figures and tables will be stored in a folder named `paper` (using subdirectories if appropriate).

Note that most plots are exported to latex files. 

Note that the tables produced by the code were merged together using this [latex document](paper/anc/JSU1_Norm4/tab_joined.tex). That latex document was then included in the paper.