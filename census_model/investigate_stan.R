library(cmdstanr)
library(posterior)
library(ggplot2)
library(dplyr)


abun_ts <- function(fit_path, origin) {
  fit <- readRDS(fit_path)

  N_array <- fit$summary(
    'N',
    ~ quantile2(.x, c(0.5, 0.025, 0.975))
  ) |>
    mutate(
      date = as.Date(row_number() - 1, origin = origin)
    )

  # cumulative sum. will need to move this to generated quants
  k <- fit$draws(paste0('state_probs[', 1:nrow(N_array), ',1]')) |>
    apply(3, identity)
  kk <- fit$draws("LambdaSuper") |>
    apply(3, identity)
  N_cum <- sweep(k, 1, kk, '*')
  N_cum <- (as.vector(apply(fit$draws("Nsuper"), 3, identity)) - N_cum) |>
    summarize_draws(~ quantile2(.x, c(0.5, 0.025, 0.975))) |>
    mutate(
      date = as.Date(row_number() - 1, origin = origin)
    )

  # acoustic telemetry detection prob
  det_prob <- fit$summary(
    c("pbar_raw", "p_sss"),
    ~ quantile2(.x, c(0.5, 0.025, 0.975))
  )

  bind_rows(
    N_array,
    N_cum,
    det_prob
  ) |>
    mutate(year = substr(origin, 1, 4))
}


abun_per_year <- list(
  `2020` = list(
    "census_model/Stan/2020/out2020_stan_omega0.RDS",
    '2020-08-15'
  ),
  `2021` = list(
    "~/Desktop/out2021_stan.RDS",
    '2021-08-15'
  ),
  `2022` = list(
    "census_model/Stan/2022/out2022_lmc_omega0.RDS",
    '2022-08-15'
  ),
  `2023` = list(
    "census_model/Stan/2023/out2023_lmc_omega0.RDS",
    '2023-08-04'
  ),
  `2024` = list(
    "census_model/Stan/2024/out2024_lmc_omega0.RDS",
    '2024-08-13'
  )
) |>
  lapply(function(.) abun_ts(.[[1]], .[[2]]))


abun_per_year <- abun_per_year |>
  bind_rows()


ggplot() +
  geom_pointrange(
    data = abun_per_year |>
      filter(grepl("^N|state", variable)) |>
      mutate(var = ifelse(grepl("^N", variable), "Daily", "Cumulative")),
    aes(
      x = lubridate::yday(date),
      y = q50,
      ymin = q2.5,
      ymax = q97.5,
      color = var
    )
  ) +
  facet_wrap(~year) +
  scale_color_brewer(palette = 'Dark2') +
  scale_x_date() +
  labs(color = '', x = '', y = 'Spawning Run Size') +
  theme(
    legend.position = c(0.75, 0.25),
    legend.text = element_text(size = 18),
    axis.text = element_text(size = 18),
    strip.text = element_text(size = 18),
    axis.title = element_text(size = 18)
  )

ggplot() +
  geom_pointrange(
    data = filter(abun_per_year, grepl("^pbar", variable)) |>
      mutate(
        reach_no = gsub('.*\\[(\\d)\\].*', '\\1', variable),
        Reach = case_when(
          reach_no == '1' ~ 'LNR',
          reach_no == '2' ~ "LMC",
          reach_no == "3" ~ "UMC",
          reach_no == "4" ~
            "UNR"
        )
      ),
    aes(
      x = year,
      ymin = q2.5,
      y = q50,
      ymax = q97.5,
      color = Reach
    ),
    position = position_dodge2(width = 0.25)
  ) +
  labs(
    x = '', y = "Detection Probability"
  ) +
    theme(
    legend.position = c(0.75, 0.25),
    legend.text = element_text(size = 18),
    axis.text = element_text(size = 18),
    strip.text = element_text(size = 18),
    axis.title = element_text(size = 18)
  )


abun_per_year |>
  filter(variable == 'p_sss')
# o0_2020 <- readRDS("census_model/Stan/2020/out2020_stan_omega0.RDS")
# o0_2021 <- readRDS("~/Desktop/out2021_stan.RDS")
# o0_2022_lmc <- readRDS("census_model/Stan/2022/out2022_lmc_omega0.RDS")
# o0_2023_lmc <- readRDS("census_model/Stan/2023/out2023_lmc_omega0.RDS")
# o0_2024_lmc <- readRDS("census_model/Stan/2024/out2024_lmc_omega0.RDS")

# o0_2024_lmc$summary('pbar_raw', ~ quantile2(.x, c(0.5, 0.025, 0.975)))

abun_per_year |> 
  filter(grepl("^N", variable)) |> 
  group_by(year) |> 
  filter(q50 == max(q50))
