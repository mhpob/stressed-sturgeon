library(brms)
library(dplyr)
library(tidyr)
nr_data <- read.csv('./Data/skip_spawn/spawn.csv') |>
  mutate(
    spawn = spawn == 'y',
    river = 'nanticoke',
    sex = toupper(sex),
    fishid = as.character(fishid)
  )

hager <- read.csv('./Data/skip_spawn/hager_spawn.csv') |>
  pivot_longer(cols = starts_with('X'), names_to = 'year') |>
  filter(!value %in% c('', 'd', 'tag')) |>
  mutate(
    spawn = value == 'y',
    year = as.numeric(gsub('X', '', year)),
    fishid = fish_id,
    river = 'york',
    sex = toupper(sex)
  )

balazik <- readxl::read_excel(
  './Data/skip_spawn/Spawning_intervals_updating_Mike-2024.xlsx'
) |>
  rename_all(tolower) |>
  rename(fishid = 'telemetry tag', river = 'tag river') |>
  filter(population == 'Fall', !is.na(year)) |>
  mutate(
    spawn = ifelse(spawn == 'y', T, F),
    fishid = as.character(fishid),
    sex = toupper(sex)
  )

spawn_data <- bind_rows(
  nr_data[, c('fishid', 'year', 'sex', 'spawn', 'river')],
  hager[, c('fishid', 'year', 'sex', 'spawn', 'river')],
  balazik[, c('fishid', 'year', 'sex', 'spawn', 'river')]
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

## Skipping ahead... mod_int3 is the selected model

mod_int <- brm(
  spawn ~ sex * river * last_spawn + (1 | fishid),
  data = spawn_data,
  family = 'bernoulli',
  prior = c(
    set_prior('normal(0,10)', class = 'b')
  ),
  # iter = 6000,
  thin = 5,
  # warmup = 4000,
  cores = 4
)
mod_int2 <- update(mod_int, formula. = ~ . - sex:river:last_spawn)
mod_int2 <- add_criterion(mod_int2, 'loo')
mod_int3 <- update(
  mod_int,
  formula. = ~ . - sex:river:last_spawn - river:last_spawn,
  thin = 5,
  cores = 4
)
mod_int3 <- add_criterion(mod_int3, 'loo')
mod_int4 <- update(
  mod_int,
  formula. = ~ . - sex:river:last_spawn - sex:last_spawn,
  thin = 5,
  cores = 4
)
mod_int4 <- add_criterion(mod_int4, 'loo')

mod_int5 <- update(
  mod_int,
  formula. = ~ . - sex:river:last_spawn - sex:last_spawn - river:last_spawn,
  thin = 5,
  cores = 4
)
mod_int5 <- add_criterion(mod_int5, 'loo')

mod_int6 <- update(
  mod_int,
  formula. = ~ . -
    sex:river:last_spawn -
    sex:last_spawn -
    river:last_spawn -
    sex:river,
  thin = 5,
  cores = 4
)
mod_int6 <- add_criterion(mod_int6, 'loo')

mod_int7 <- update(
  mod_int,
  formula. = ~ . -
    sex:river:last_spawn -
    sex:last_spawn -
    river:last_spawn -
    sex:river -
    last_spawn,
  thin = 5,
  cores = 4
)
mod_int7 <- add_criterion(mod_int7, 'loo')

mod_int8 <- update(
  mod_int,
  formula. = ~ . -
    sex:river:last_spawn -
    sex:last_spawn -
    river:last_spawn -
    sex:river -
    river,
  thin = 5,
  cores = 4
)
mod_int8 <- add_criterion(mod_int8, 'loo')

mod_int9 <- update(
  mod_int,
  formula. = ~ . -
    sex:river:last_spawn -
    sex:last_spawn -
    river:last_spawn -
    sex:river -
    sex,
  thin = 5,
  cores = 4
)
mod_int9 <- add_criterion(mod_int9, 'loo')

mod_int10 <- update(
  mod_int,
  formula. = ~ . -
    sex:river:last_spawn -
    sex:last_spawn -
    river:last_spawn -
    sex:river -
    last_spawn -
    sex,
  thin = 5,
  cores = 4
)
mod_int10 <- add_criterion(mod_int10, 'loo')

mod_int11 <- update(
  mod_int,
  formula. = ~ . -
    sex:river:last_spawn -
    sex:last_spawn -
    river:last_spawn -
    sex:river -
    last_spawn -
    river,
  thin = 5,
  cores = 4
)
mod_int11 <- add_criterion(mod_int11, 'loo')

mod_int12 <- update(
  mod_int,
  formula. = ~ . -
    sex:river:last_spawn -
    sex:last_spawn -
    river:last_spawn -
    sex:river -
    river -
    sex,
  thin = 5,
  cores = 4
)
mod_int12 <- add_criterion(mod_int12, 'loo')
#
# mod_int13 <- update(mod_int13)
# mod_int13 <- add_criterion(mod_int13, 'loo')

loo_compare(
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
  mod_int13
)


pp_check(mod_int3, type = 'bars', ndraws = 100)
pp_check(mod_int3, type = 'stat')

mod <- brm(
  spawn ~ sex + river + (1 | fishid),
  data = spawn_data,
  family = 'bernoulli',
  iter = 3000,
  thin = 5,
  warmup = 2000,
  cores = 4
)


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

get_variables(mod_int3)[1:10]

plot_dat <- mod_int3 %>%
  spread_draws(
    b_Intercept,
    b_sexM,
    b_riveryork,
    b_last_spawn,
    `b_sexM:riveryork`,
    `b_sexM:last_spawn`,
    r_fishid[fishid, ]
  ) %>%
  # sample_draws(500) |>
  left_join(distinct(spawn_data, fishid, sex, river), by = 'fishid') |>
  mutate(
    mu = case_when(
      river == 'nanticoke' & sex == 'F' ~ b_Intercept + b_last_spawn + r_fishid,
      river == 'nanticoke' & sex == 'M' ~
        b_Intercept + b_sexM + b_last_spawn + `b_sexM:last_spawn` + r_fishid,
      river == 'york' & sex == 'F' ~
        b_Intercept + b_riveryork + b_last_spawn + r_fishid,
      river == 'york' & sex == 'M' ~
        b_Intercept +
          b_sexM +
          b_riveryork +
          b_last_spawn +
          `b_sexM:riveryork` +
          `b_sexM:last_spawn` +
          r_fishid
    )
  ) |>
  ungroup()
#
# plot_dat <- mod_int %>%
#   spread_draws(b_Intercept, b_sexM, b_riveryork, b_last_spawn, `b_sexM:riveryork`,
# r_fishid[fishid,]) %>%
#   # sample_draws(500) |>
#   left_join(distinct(spawn_data, fishid, sex, river), by = 'fishid') |>
#   mutate(mu = case_when(
#     river == 'nanticoke' & sex == 'M' ~ b_sexM + r_fishid,
#     river == 'nanticoke' & sex == 'F' ~ b_sexF + r_fishid,
#     area == 'york' & sex == 'M' ~
#       b_sexM + b_areayork + `b_sexM:areayork` + r_fishid,
#     area == 'york' & sex == 'F' ~
#       b_sexF + b_areayork + r_fishid)) |>
#   ungroup()

# plot effects
marginaleffects::predictions(mod, by = 'sex')
marginaleffects::predictions(mod, by = 'river')

# A <-
marginaleffects::plot_predictions(
  mod_int3,
  condition = c('last_spawn', 'river', 'sex')
) +
  labs(
    x = 'Years since previous spawning run',
    y = 'Marginal probability of spawning',
    color = 'River of\ntagging',
    fill = 'River of\ntagging'
  ) +
  theme_minimal() +
  scale_color_manual(
    values = c(
      I(rgb(120, 90, 236, maxColorValue = 255)),
      # I(rgb(186, 141, 228,  maxColorValue = 255)),
      I(rgb(222, 132, 11, maxColorValue = 255))
      # I(rgb(210, 182, 144,  maxColorValue = 255))
    )
  ) +
  scale_fill_manual(
    values = c(
      I(rgb(120, 90, 236, maxColorValue = 255)),
      # I(rgb(186, 141, 228,  maxColorValue = 255)),
      I(rgb(222, 132, 11, maxColorValue = 255))
      # I(rgb(210, 182, 144,  maxColorValue = 255))
    )
  )

# B <-
marginaleffects::plot_predictions(mod_int3, by = c('sex', 'river')) +
  labs(x = 'Sex', y = 'Marginal probability of spawning') +
  theme_minimal() +
  theme(legend.position = 'none') +
  scale_color_manual(
    values = c(
      I(rgb(120, 90, 236, maxColorValue = 255)),
      # I(rgb(186, 141, 228,  maxColorValue = 255)),
      I(rgb(222, 132, 11, maxColorValue = 255))
      # I(rgb(210, 182, 144,  maxColorValue = 255))
    )
  ) +
  scale_fill_manual(
    values = c(
      I(rgb(120, 90, 236, maxColorValue = 255)),
      # I(rgb(186, 141, 228,  maxColorValue = 255)),
      I(rgb(222, 132, 11, maxColorValue = 255))
      # I(rgb(210, 182, 144,  maxColorValue = 255))
    )
  )

library(patchwork)
B + A + plot_layout(axes = 'collect', widths = c(1, 2))

# prob of spawning by sex and river
avg_predictions(mod_int3, by = c('sex'))
# prob of females spawning after 4.5yrs from Stevenson 1997
avg_predictions(mod_int3, variables = list(last_spawn = 4.5, sex = 'F'))

avg_predictions(mod_int3, by = c('sex', 'river'))

library(patchwork)
sex <- conditional_effects(mod, 'sex')
sex_york <- conditional_effects(
  mod,
  'sex',
  conditions = data.frame(river = 'york')
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
  labs(x = 'Probability of spawning')


river <- conditional_effects(mod, 'river')
river <- plot(river, plot = F)[[1]] +
  theme_minimal()

sex + river + plot_layout(axes = 'collect')

# plot ranefs
# fish <-
ggplot(
  data = filter(plot_dat),
  aes(
    x = bernoulli()$linkinv(mu),
    y = reorder(fishid, mu),
    color = interaction(sex, river)
  )
) +
  stat_pointinterval() +
  coord_cartesian(xlim = c(0, 1), expand = F) +
  scale_color_manual(
    values = c(
      I(rgb(120, 90, 236, maxColorValue = 255)),
      I(rgb(186, 141, 228, maxColorValue = 255)),
      I(rgb(222, 132, 11, maxColorValue = 255)),
      I(rgb(210, 182, 144, maxColorValue = 255))
    )
  ) +
  labs(
    y = 'Fish',
    x = 'Probability of consecutive spawning runs',
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
  mutate(river = ifelse(river == 'nanticoke', 'Nanticoke', 'York')) |>
  ggplot() +
  geom_histogram(aes(x = last_spawn, fill = sex), position = position_dodge()) +
  facet_wrap(~river, ncol = 1) +
  theme_minimal() +
  labs(x = 'Years since last spawn', y = '')

library(dplyr)

spawn_data |>
  distinct(fishid, river, sex) |>
  count(sex, river)

xtabs(~ last_spawn + sex + river, data = spawn_data)
