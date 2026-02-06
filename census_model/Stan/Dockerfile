FROM rocker/r-base

RUN Rscript -e 'install.packages("cmdstanr", repos = c("https://stan-dev.r-universe.dev", getOption("repos")))' \
        && Rscript -e 'cmdstanr::install_cmdstan(cores = 4)'