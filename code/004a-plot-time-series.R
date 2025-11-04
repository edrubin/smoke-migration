

# Notes ----------------------------------------------------------------------------------


# Data notes -----------------------------------------------------------------------------
# 	- The 'full' sample in the contiguous US (including DC)


# Setup ----------------------------------------------------------------------------------
  # Load packages
  library(pacman)
  p_load(fastverse, cowplot, extrafont, viridis, here)
  fastverse_extend(topics = c('SP', 'ST', 'DT', 'VI'))
  # Load fonts
  loadfonts(quiet = TRUE)


# Load data: Cleaned CBG dataset ---------------------------------------------------------
  # Load visits and smoke data (full-US and West Coast samples)
  full_dt = here(
    'data-processed', 'for-analysis',
    'for-analysis-full.fst'
  ) %>% read_fst(
    columns = c(
      'cbg_home', #'county',
      'pop',
      'date_start', #'yr', 'mo', 'wk',
      'total_visits', 'visits_same_county',
      'any_smoke', 'any_smoke_low', 'any_smoke_medium', 'any_smoke_high'
    ),
    as.data.table = TRUE
  )
  # Load visits and smoke data (full-US and West Coast samples)
  west_dt = here(
    'data-processed', 'for-analysis',
    'for-analysis-westcoast.fst'
  ) %>% read_fst(
    columns = c(
      'cbg_home', #'county',
      'pop',
      'date_start', #'yr', 'mo', 'wk',
      'total_visits', 'visits_same_county',
      'any_smoke', 'any_smoke_low', 'any_smoke_medium', 'any_smoke_high'
    ),
    as.data.table = TRUE
  )


# Data work: Time-series summaries of smoke exposure and movement ------------------------
  # Add state variable
  full_dt[, state := str_sub(cbg_home, 1, 2)]
  west_dt[, state := str_sub(cbg_home, 1, 2)]
  # Build time-series dataset(s)
  setkey(full_dt, date_start)
  setkey(full_dt, date_start)
  # Summarize each panel by week
  full_time = full_dt[, .(
    full_pct_same_county = fsum(visits_same_county) / fsum(total_visits),
    full_pct_cbgs_smoke = fmean(any_smoke),
    full_pct_pop_smoke = fmean(any_smoke, w = pop),
    full_pct_cbgs_smoke_low = fmean(any_smoke_low),
    full_pct_pop_smoke_low = fmean(any_smoke_low, w = pop),
    full_pct_cbgs_smoke_medium = fmean(any_smoke_medium),
    full_pct_pop_smoke_medium = fmean(any_smoke_medium, w = pop),
    full_pct_cbgs_smoke_high = fmean(any_smoke_high),
    full_pct_pop_smoke_high = fmean(any_smoke_high, w = pop)
  ), .(date = date_start)]
  west_time = west_dt[, .(
    west_pct_same_county = fsum(visits_same_county) / fsum(total_visits),
    west_pct_cbgs_smoke = fmean(any_smoke),
    west_pct_pop_smoke = fmean(any_smoke, w = pop),
    west_pct_cbgs_smoke_low = fmean(any_smoke_low),
    west_pct_pop_smoke_low = fmean(any_smoke_low, w = pop),
    west_pct_cbgs_smoke_medium = fmean(any_smoke_medium),
    west_pct_pop_smoke_medium = fmean(any_smoke_medium, w = pop),
    west_pct_cbgs_smoke_high = fmean(any_smoke_high),
    west_pct_pop_smoke_high = fmean(any_smoke_high, w = pop)
  ), .(date = date_start)]
  # Combine datasets
  time_dt = merge(
    x = full_time,
    y = west_time,
    by = 'date'
  )


# Figure: Smoke exposure time series -----------------------------------------------------
  # The time-series plot
  gg_smoke = ggplot(
    data = time_dt,
    aes(x = date)
  ) +
  geom_area(
    aes(y = west_pct_pop_smoke_low, color = 'b', fill = 'b'),
    size = 0.25, alpha = 0.925
  ) +
  geom_area(
    aes(y = west_pct_pop_smoke_medium, color = 'c', fill = 'c'),
    size = 0.25, alpha = 0.925
  ) +
  geom_area(
    aes(y = west_pct_pop_smoke_high, color = 'd', fill = 'd'),
    size = 0.25, alpha = 0.925
  ) +
  geom_hline(yintercept = 0, size = 0.25) +
  scale_x_date('Week') +
  scale_y_continuous('Percent of population exposed to wildfire smoke', labels = percent) +
  scale_color_manual(
    'Smoke Density:',
    labels = c('Low', 'Medium', 'High'),
    values = magma(100)[c(5, 70, 90)] %>% rev()
  ) +
  scale_fill_manual(
    'Smoke Density:',
    labels = c('Low', 'Medium', 'High'),
    values = magma(100)[c(5, 70, 90)] %>% rev()
  ) +
  theme_minimal(base_family = 'Fira Sans Extra Condensed', base_size = 9) +
  theme(
    panel.grid.minor.y = element_blank(),
    legend.position = 'bottom'
  )
  # Save the histogram
  save_plot(
    # filename = here('figures', 'smoke-exposure-pop-west.png'),
    filename = here('figures', 'smoke-exposure-pop-west.pdf'),
    plot = gg_smoke,
    base_height = 3.3,
    base_asp = 2,
    dpi = 450
  )


# Figure: Smoke exposure time series -----------------------------------------------------
  # The time-series plot
  gg_mv = ggplot(
    data = time_dt,
    aes(x = date, y = 1 - west_pct_same_county)
  ) +
  geom_area(
    color = 'grey25',
    size = 0.3,
    alpha = 0.8
  ) +
  geom_hline(yintercept = 0, size = 0.25) +
  scale_x_date('Week') +
  scale_y_continuous('Percent of movement beyond home county', labels = percent) +
  theme_minimal(base_family = 'Fira Sans Extra Condensed', base_size = 9) +
  theme(
    panel.grid.minor.y = element_blank()
  )
  # Save the histogram
  save_plot(
    # filename = here('figures', 'movement-west.png'),
    filename = here('figures', 'movement-west.pdf'),
    plot = gg_mv,
    base_height = 3,
    base_width = 6.6,
    dpi = 450
  )