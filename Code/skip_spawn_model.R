library(brms)
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)

nr_data <- read.csv("./Data/skip_spawn/spawn.csv") |>
  mutate(
    spawn = spawn == "y",
    river = "nanticoke",
    sex = toupper(sex),
    fishid = as.character(fishid)
  )

hager <- read.csv("./Data/skip_spawn/hager_spawn.csv") |>
  pivot_longer(cols = starts_with("X"), names_to = "year") |>
  filter(!value %in% c("", "d", "tag")) |>
  mutate(
    spawn = value == "y",
    year = as.numeric(gsub("X", "", year)),
    fishid = fish_id,
    river = "york",
    sex = toupper(sex)
  )

balazik <- readxl::read_excel(
  "./Data/skip_spawn/Spawning_intervals_updating_Mike-2024_EDITED.xlsx"
) |>
  rename_all(tolower) |>
  rename(fishid = "telemetry tag", river = "tag river") |>
  filter(
    population == "Fall",
    !is.na(year),
    collection_date < "2024-01-01",
    !fishid %in%
      c(
        28267 # NC stray
      )
  ) |>
  mutate(
    spawn = ifelse(spawn %in% c("y", "Y"), T, F),
    fishid = as.character(fishid),
    sex = toupper(sex)
  )

spawn_data <- bind_rows(
  nr_data[, c("fishid", "year", "sex", "spawn", "river")],
  hager[, c("fishid", "year", "sex", "spawn", "river")],
  balazik[, c("fishid", "year", "sex", "spawn", "river")]
) |>
  filter(
    !(fishid == 27545 & year >= 2018),
    !(fishid == 26235 & year >= 2019),
    !(fishid == 27545 & year >= 2018),
    !(fishid == 27547 & year >= 2019),
    !(fishid == 26354 & year >= 2019),
    !(fishid == 23900 & year >= 2019),
    !(fishid == 27545 & year >= 2018),
    !(fishid %in% c("21072", "21061", "14-009")),
    !(fishid == 21060 & year >= 2019),
    !(fishid == 26041 & year >= 2018),
    !(fishid == 21892 & year >= 2020),
    !(fishid == 21904 & year >= 2016),
    !(fishid == 21899 & year >= 2018),
    !(fishid == 16042 & year >= 2018)
  )

spawn_data <- spawn_data |>
  group_by(fishid) |>
  mutate(
    last_spawn = ifelse(
      row_number(fishid) == 1 |
        lag(spawn) == TRUE,
      1,
      NA
    ),
    no_spawn = as.factor(cumsum(!is.na(last_spawn)))
  ) |>
  group_by(fishid, no_spawn) |>
  mutate(last_spawn = 1:n()) |>
  ungroup() |>
  select(-no_spawn)

#include yrs post tagging?

## Skipping ahead... mod_int14 is the selected model
options(mc.cores = 30)

cat("1\n")
mod_int <- brm(
  spawn ~ sex * river * last_spawn + (1 | fishid),
  data = spawn_data,
  family = "bernoulli",
  prior = c(
    set_prior("normal(0,10)", class = "b")
  ),
  iter = 6000,
  warmup = 3500,
  save_pars = save_pars(all = TRUE)
) |>
  add_criterion("loo", moment_match = T) # might need reloo=T

cat("2\n")
mod_int2 <- update(
  mod_int,
  formula. = ~ . - sex:river:last_spawn,
  iter = 6000,
  warmup = 3500,
  save_pars = save_pars(all = TRUE)
) |>
  add_criterion("loo", moment_match = T)

cat("3\n")
mod_int3 <- update(
  mod_int,
  formula. = ~ . - sex:river:last_spawn - river:last_spawn,
  iter = 6000,
  warmup = 3500,
  save_pars = save_pars(all = TRUE)
) |>
  add_criterion("loo", moment_match = T)

cat("4\n")
mod_int4 <- update(
  mod_int,
  formula. = ~ . - sex:river:last_spawn - sex:last_spawn,
  iter = 6000,
  warmup = 3500,
  save_pars = save_pars(all = TRUE)
) |>
  add_criterion("loo", moment_match = T)

cat("5\n")
mod_int5 <- update(
  mod_int,
  formula. = ~ . - sex:river:last_spawn - sex:last_spawn - river:last_spawn,
  iter = 6000,
  warmup = 3500,
  save_pars = save_pars(all = TRUE)
) |>
  add_criterion("loo", moment_match = T)

cat("6\n")
mod_int6 <- update(
  mod_int,
  formula. = ~ . -
    sex:river:last_spawn -
    sex:last_spawn -
    river:last_spawn -
    sex:river,
  iter = 6000,
  warmup = 3500,
  save_pars = save_pars(all = TRUE)
) |>
  add_criterion("loo", moment_match = T)

cat("7\n")
mod_int7 <- update(
  mod_int,
  formula. = ~ . -
    sex:river:last_spawn -
    sex:last_spawn -
    river:last_spawn -
    sex:river -
    last_spawn,
  iter = 6000,
  warmup = 3500,
  save_pars = save_pars(all = TRUE)
) |>
  add_criterion("loo", moment_match = T)

cat("8\n")
mod_int8 <- update(
  mod_int,
  formula. = ~ . -
    sex:river:last_spawn -
    sex:last_spawn -
    river:last_spawn -
    sex:river -
    river,
  iter = 6000,
  warmup = 3500,
  save_pars = save_pars(all = TRUE)
) |>
  add_criterion("loo", moment_match = T)

cat("9\n")
mod_int9 <- update(
  mod_int,
  formula. = ~ . -
    sex:river:last_spawn -
    sex:last_spawn -
    river:last_spawn -
    sex:river -
    sex,
  iter = 6000,
  warmup = 3500,
  save_pars = save_pars(all = TRUE)
) |>
  add_criterion("loo", moment_match = T)


cat("10\n")
mod_int10 <- update(
  mod_int,
  formula. = ~ . -
    sex:river:last_spawn -
    sex:last_spawn -
    river:last_spawn -
    sex:river -
    last_spawn -
    sex,
  iter = 6000,
  warmup = 3500,
  save_pars = save_pars(all = TRUE)
) |>
  add_criterion("loo", moment_match = T)

cat("11\n")
mod_int11 <- update(
  mod_int,
  formula. = ~ . -
    sex:river:last_spawn -
    sex:last_spawn -
    river:last_spawn -
    sex:river -
    last_spawn -
    river,
  iter = 6000,
  warmup = 3500,
  save_pars = save_pars(all = TRUE)
) |>
  add_criterion("loo", moment_match = T)

cat("12\n")
mod_int12 <- update(
  mod_int,
  formula. = ~ . -
    sex:river:last_spawn -
    sex:last_spawn -
    river:last_spawn -
    sex:river -
    river -
    sex,
  iter = 6000,
  warmup = 3500,
  save_pars = save_pars(all = TRUE)
) |>
  add_criterion("loo", moment_match = T)

cat("13\n")
mod_int13 <- update(
  mod_int,
  formula. = ~ . -
    sex:river:last_spawn -
    sex:river,
  iter = 6000,
  warmup = 3500,
  save_pars = save_pars(all = TRUE)
) |>
  add_criterion("loo", moment_match = T)

cat("14\n")
mod_int14 <- update(
  mod_int,
  formula. = ~ . -
    sex:river:last_spawn -
    sex:river -
    river:last_spawn,
  iter = 6000,
  warmup = 3500,
  save_pars = save_pars(all = TRUE)
) |>
  add_criterion("loo", moment_match = T)
mod_int14b <- mod_int14 |>
  add_criterion("loo", moment_match = T, reloo = T, overwrite = T)

cat("15\n")
mod_int15 <- update(
  mod_int,
  formula. = ~ . -
    sex:river:last_spawn -
    sex:river -
    sex:last_spawn,
  iter = 6000,
  warmup = 3500,
  save_pars = save_pars(all = TRUE)
) |>
  add_criterion("loo", moment_match = T)

cat("16\n")
mod_int16 <- update(
  mod_int,
  formula. = ~ . -
    sex:river:last_spawn -
    river:last_spawn -
    sex:last_spawn -
    last_spawn,
  iter = 6000,
  warmup = 3500,
  save_pars = save_pars(all = TRUE)
) |>
  add_criterion("loo", moment_match = T)

cat("17\n")
mod_int17 <- update(
  mod_int,
  formula. = ~ . -
    sex:river:last_spawn -
    river:last_spawn -
    sex:river -
    river,
  iter = 6000,
  warmup = 3500,
  save_pars = save_pars(all = TRUE)
) |>
  add_criterion("loo", moment_match = T)

cat("18\n")
mod_int18 <- update(
  mod_int,
  formula. = ~ . -
    sex:river:last_spawn -
    sex:last_spawn -
    sex:river -
    sex,
  iter = 6000,
  warmup = 3500,
  save_pars = save_pars(all = TRUE)
) |>
  add_criterion("loo", moment_match = T)

loo_res <- loo_compare(
  mod_int,
  mod_int2,
  mod_int3,
  mod_int4,
  mod_int5,
  mod_int6,
  mod_int7,
  mod_int8,
  mod_int9,
  mod_int10,
  mod_int11,
  mod_int12,
  mod_int13,
  mod_int14,
  mod_int15,
  mod_int16,
  mod_int17,
  mod_int18
)

data.frame(
  elpd_diff = loo_res[, "elpd_diff"],
  elpd_lci = loo_res[, "elpd_diff"] - 1.96 * loo_res[, "se_diff"],
  elpd_uci = loo_res[, "elpd_diff"] + 1.96 * loo_res[, "se_diff"],
  eff_params = loo_res[, "p_loo"],
  eff_params_lci = loo_res[, "p_loo"] - 1.96 * loo_res[, "se_p_loo"],
  eff_params_uci = loo_res[, "p_loo"] + 1.96 * loo_res[, "se_p_loo"]
)

# save(mod_int, mod_int2, mod_int3, mod_int13, mod_int14, file = "spawn_model_20250605.RData")
# load('Data/skip_spawn/spawn_model_20250610.RData')
mod <- mod_int14
pp_check(mod)
pp_check(mod, type = "bars", ndraws = 100)
pp_check(mod, type = "stat")


# mod2 <- brm(spawn ~ 0 + sex + river + (1|fishid),
#            data = spawn_data, family = 'bernoulli',
#            iter = 3000, thin = 5, warmup = 2000, cores = 4)
#
# mod_int <- brm(spawn ~ 0 + sex*river + (1|fishid),
#            data = spawn_data, family = 'bernoulli',
#            iter = 3000, thin = 5, warmup = 2000, cores = 4,
#            prior = c(prior(normal(-1, 0.5), coef = 'sexF'),
#                      prior(normal(1, 0.5), coef = 'sexM')))

library(tidybayes)
library(dplyr)
library(ggplot2)

# times more likely to spawn
marginaleffects::avg_comparisons(
  mod, 
  variables = list(sex = 'pairwise'),
  newdata = marginaleffects::datagrid(last_spawn = 1),
  comparison = 'ratio')

get_variables(mod)

plot_dat <- mod %>%
  spread_draws(
    b_Intercept,
    b_sexM,
    b_rivernanticoke,
    b_riverRappahannock,
    b_riveryork,
    b_last_spawn,
    `b_sexM:last_spawn`,
    # `b_rivernanticoke:last_spawn`,
    # `b_riverRappahannock:last_spawn`,
    # `b_riveryork:last_spawn`,
    r_fishid[fishid, ]
  ) |>
  sample_draws(500) |>
  left_join(distinct(spawn_data, fishid, sex, river), by = "fishid") |>
  mutate(
    mu = case_when(
      river == "James" & sex == "F" ~ b_Intercept + b_last_spawn + r_fishid,
      river == "James" & sex == "M" ~
        b_Intercept + b_sexM + b_last_spawn + `b_sexM:last_spawn` + r_fishid,
      river == "nanticoke" & sex == "F" ~
        b_Intercept +
          b_rivernanticoke +
          b_last_spawn +
          # `b_rivernanticoke:last_spawn` +
          r_fishid,
      river == "nanticoke" & sex == "M" ~
        b_Intercept +
          b_sexM +
          b_rivernanticoke +
          b_last_spawn +
          `b_sexM:last_spawn` +
          # `b_rivernanticoke:last_spawn` +
          r_fishid,
      river == "york" & sex == "F" ~
        b_Intercept +
          b_riveryork +
          b_last_spawn +
          # `b_riveryork:last_spawn` +
          r_fishid,
      river == "york" & sex == "M" ~
        b_Intercept +
          b_sexM +
          b_riveryork +
          b_last_spawn +
          `b_sexM:last_spawn` +
          # `b_riveryork:last_spawn` +
          r_fishid,
      river == "Rappahannock" & sex == "F" ~
        b_Intercept +
          b_riverRappahannock +
          b_last_spawn +
          # `b_riverRappahannock:last_spawn` +
          r_fishid,
      river == "Rappahannock" & sex == "M" ~
        b_Intercept +
          b_sexM +
          b_riverRappahannock +
          b_last_spawn +
          `b_sexM:last_spawn` +
          # `b_riverRappahannock:last_spawn` +
          r_fishid
    ),
    river = case_when(
      river == 'nanticoke' ~ 'Nanticoke',
      river == 'york' ~ 'York',
      .default = river
    )
  ) |>
  ungroup()


# plot effects
marginaleffects::predictions(mod, by = "sex")
marginaleffects::predictions(mod, by = "river")
df <- marginaleffects::avg_predictions(
  mod,
  variables = list(last_spawn = 1:6, sex = c("F", "M"))
) |>
  data.frame()

# A <-
marginaleffects::plot_predictions(
  mod,
  condition = c("last_spawn", "river", "sex")
) +
  labs(
    x = "Years since previous spawning run",
    y = "Marginal probability of spawning run",
    color = "River of\ntagging",
    fill = "River of\ntagging"
  ) +
  theme_minimal() +
  scale_color_brewer(
    type = 'qual',
    palette = 'Dark2',
    labels = c('James', 'Nanticoke', 'Rappahannock', 'York')
  ) +
  scale_fill_brewer(
    type = 'qual',
    palette = 'Dark2',
    labels = c('James', 'Nanticoke', 'Rappahannock', 'York')
  ) +
  geom_pointrange(
    data = df,
    aes(x = last_spawn, y = estimate, ymin = conf.low, ymax = conf.high)
  ) +
  theme(
    strip.text = element_text(size = 18),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    legend.position = 'none',
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18)
  )


# B <-
marginaleffects::plot_predictions(mod, by = c("sex", "river")) +
  labs(x = "Sex", y = "Marginal probability of spawning run") +
  scale_color_brewer(
    type = 'qual',
    palette = 'Dark2',
    labels = c('James', 'Nanticoke', 'Rappahannock', 'York')
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 14)) 

library(patchwork)
B + A + plot_layout(axes = "collect", widths = c(1, 2))

# prob of spawning by sex and river
marginaleffects::avg_predictions(mod, by = c("sex"))
df <- marginaleffects::avg_predictions(
  mod,
  variables = list(last_spawn = 1, sex = c("F", "M"))
) |>
  data.frame()
marginaleffects::avg_predictions(
  mod,
  variables = list(last_spawn = 1)
) |>
  data.frame()

# prob of females spawning after 4.5yrs from Stevenson 1997
marginaleffects::avg_predictions(
  mod,
  variables = list(last_spawn = 1:6, sex = c("F", "M"))
)

marginaleffects::avg_predictions(mod_int, by = c("sex"))

library(patchwork)
sex <- conditional_effects(mod, "sex")
sex_york <- conditional_effects(
  mod,
  "sex",
  conditions = data.frame(river = "york")
)
sex <- plot(sex, plot = F)[[1]] +
  plot(sex_york, plot = F)[[1]]
theme_minimal()

rbind(sex$sex, sex_york$sex)
kk <- ggplot(data = rbind(sex$sex, sex_york$sex)) +
  geom_pointinterval(
    aes(
      y = sex,
      x = estimate__,
      xmin = lower__,
      xmax = upper__,
      color = interaction(sex, river)
    ),
    position = position_dodge()
  ) +
  xlim(0, 1) +
  labs(x = "Probability of spawning")


river <- conditional_effects(mod, "river")
river <- plot(river, plot = F)[[1]] +
  theme_minimal()

sex + river + plot_layout(axes = "collect")

# plot ranefs
fish <-
ggplot(
  data = filter(plot_dat),
  aes(
    x = bernoulli()$linkinv(mu),
    y = reorder(fishid, mu),
    color = interaction(sex, river)
  )
) +
  stat_pointinterval(size = 1) +
  coord_cartesian(xlim = c(0, 1), expand = F) +
  scale_color_brewer(
    type = 'qual',
    palette = 'Dark2',
    labels = c(
      'F.James',
      'M.James',
      'F.Nanticoke',
      'M.Nanticoke',
      'M.Rappahannock',
      'F.York',
      'M.York'
    )
  ) +
  scale_fill_brewer(
    type = 'qual',
    palette = 'Dark2',
    labels = c(
      'F.James',
      'M.James',
      'F.Nanticoke',
      'M.Nanticoke',
      'M.Rappahannock',
      'F.York',
      'M.York'
    )
  ) +
  labs(
    y = "Fish",
    x = "Probability of consecutive spawning runs",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    legend.position = c(0.1, 0.75),
    panel.grid.major.y = element_blank()
  )

(sex / river) | fish

## TL vs FL

jj <- j |>
  filter(!is.na(tl_mm) & !is.na(fl_mm)) |>
  distinct(fishid, .keep_all = T) |>
  mutate(tl_cm = tl_mm / 10, fl_cm = fl_mm / 10)

tlfl_mod_s <- brm(tl_cm ~ fl_cm * sex, data = jj)


########## figures for AFS 2024 presentation?
library(ggplot2)

spawn_data |>
  # mutate(river = ifelse(river == "nanticoke", "Nanticoke", "York")) |>
  ggplot() +
  geom_histogram(aes(x = last_spawn, fill = sex), position = position_dodge()) +
  facet_wrap(~river, ncol = 2, scales = "free_y") +
  theme_minimal() +
  labs(x = "Years since last spawn", y = "")

library(dplyr)

spawn_data |>
  distinct(fishid, river, sex) |>
  count(sex, river)

xtabs(~ last_spawn + sex + river, data = spawn_data)
