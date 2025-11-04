# Notes ----------------------------------------------------------------------------------
#   Goal:   Run simple, FE-adjusted event study for effect of smoke onset on migration.
#   Time:   ???


# Observations ---------------------------------------------------------------------------
#   - It looks like the difference (wrt to income) in smoke response comes from LOW smoke.
#   - CBG and tract effects are consistent with county effects—just smaller magnitude.


# Load and prep data ---------------------------------------------------------------------
  # Source the script that loads and prepares the data
  here::here('code', '002b-prep-analysis-data.R') |> source()


# Regression: Event study ----------------------------------------------------------------
  # Build event-study dataset
  # Set event-study length
  N = 5
  np = ceiling((N - 1) / 2)
  # For each CBG-year, find the first week of smoke
  first_smoke = collap(
    full_dt[any_smoke > 0, .(cbg_home, yr, date_start)],
    by = ~ cbg_home + yr,
    FUN = fmin,
  )
  # First CBGs with no smoke for a given year
# NOTE: 'no_smoke' will be empty when using 'any_smoke' to define smoke
  no_smoke = collap(
    full_dt[, .(cbg_home, yr, any_smoke)],
    by = ~ cbg_home + yr,
    FUN = fmax,
  )[any_smoke == 0]
  # Add month and week
  first_smoke[, `:=`(
    wk = week(date_start),
    mo = month(date_start)
  )]
  # Change 'date_state' name
  setnames(first_smoke, old = 'date_start', new = 'date_smoke_start')
  # Expand dataset to create event study: N weeks with (N-1)/2 pre, (N-1)/2 post
  first_smoke %<>% .[rep(seq_len(.N), each = N)]
  # Add event time (with a factor label too)
  first_smoke[, `:=`(
    event_time = -np:np,
    event_period = seq_len(.N)
  ), by = .(cbg_home, yr, date_smoke_start)]
  first_smoke[, `:=`(
    event_period = factor(
      event_period,
      levels = 1:N,
      labels = paste0('p', str_pad(1:N, 2, 'left', 0)),
      ordered = TRUE
    )
  )]
  # Create date variable with actual weeks implied by event time
  first_smoke[, date_start := date_smoke_start + (event_time * 7)]
  # Drop repeated variables
  first_smoke[, c('yr', 'mo', 'wk') := NULL]
  # Merge full dataset onto event-study setup
  event_trt = merge(
    x = first_smoke,
    y = full_dt,
    by = c('cbg_home', 'date_start'),
    all.x = TRUE,
    all.y = FALSE
  )
  # Add dummies for each event period
  for (p in event_trt[, funique(event_period)]) {
    set(
      x = event_trt,
      j = p,
      value = 1 * (event_trt[, event_period] == p)
    )
  }
  # Add indicator for above median income
  event_trt[, hh_inc_top := 1 * (hh_inc_q %in% paste0('q', 11:20))]
  # Add month and week that smoke starts
  event_trt[, `:=`(
    mo_smoke_start = month(date_smoke_start),
    wk_smoke_start = week(date_smoke_start)
  )]
  # Check the smoke and migration variables
  event_trt[, .(
    any_smoke = fmean(any_smoke),
    any_smoke_low = fmean(any_smoke_low),
    any_smoke_medium = fmean(any_smoke_medium),
    any_smoke_high = fmean(any_smoke_high),
    out_cbg = fmean(1 - pct_same_cbg),
    out_tract = fmean(1 - pct_same_tract),
    out_county = fmean(1 - pct_same_county),
    out_state = fmean(1 - pct_same_state),
    n = .N
  ), event_period]
  # Excluded period
  p_drop = paste0('p', str_pad(np, 2, 'left', 0))
  # Iterate over outcome variables
  for (i in 1:5) {
    # Build formula
    event_f = paste0(
      c(
        '1 - pct_same_county ~ ',
        'dist_median ~ ',
        'dist_p75 ~ ',
        'total_visits ~ ',
        'total_visits - visits_same_county ~ '
      )[i],
      paste(
        str_subset(names(event_trt), '^p[0-9]{2}') %>% setdiff(p_drop),
        collapse = ' + '
      ),
      ' | ',
      # ' wk_smoke_start + county ^ mo + county ^ yr'
      # ' cbg_home + wk_smoke_start ^ yr'
      # ' cbg_home + state ^ wk_smoke_start ^ yr'
      # ' cbg_home + state ^ wk_smoke_start ^ yr'
      ' cbg_home + wk_smoke_start ^ yr + state ^ yr + state ^ mo'
    ) %>%
    as.formula()
    # Estimate!
    event_est = feols(
      event_f,
      data = event_trt[county != '06073' & i_rural == 0],
      # data = event_trt[county != '06073' & i_rural == 0 & !(cbg_home %in% fire_dt$cbg)],
      # data = event_trt[county != '06073' & i_rural == 0][mo_smoke_start < 7],
      # data = event_trt[county != '06073' & i_rural == 0][mo_smoke_start >= 7],
      # data = event_trt[county != '06073' & hh_inc_q %in% c('q01', 'q02', 'q03') & i_rural == 0 & mo_smoke_start < 7],
      # data = event_trt[county != '06073' & hh_inc_q %in% c('q17', 'q18', 'q19') & i_rural == 0 & mo_smoke_start < 7],
      cluster = ~county + mo ^ yr,
      weights = ~pop
    )
    # Plot
    event_study = ggplot(
      data = event_est %>%
        broom::tidy(conf.int = TRUE) %>%
        dplyr::filter(term != 'any_smoke') %>%
        tibble::add_row(estimate = 0, conf.low = 0, conf.high = 0) %>%
        dplyr::mutate(time = c(setdiff(-np:np, -1), -1)),
      aes(x = time, y = estimate, ymin = conf.low, ymax = conf.high)
    ) +
    geom_hline(yintercept = 0, size = .25) +
    scale_x_continuous('Weeks relative to first smoke exposure') +
    scale_y_continuous(
      name = c(
        'Out migration:\nPercentage of visits outside of home county',
        'Out migration:\nMedian distance from home (km)',
        'Out migration:\n Distance from home (km, 75th pcntl.)',
        'Total POI visits',
        'Visits to non-home counties'
      )[i],
      labels = list(
        percent,
        comma,
        comma,
        comma,
        comma
      )[[i]]
    ) +
    geom_ribbon(fill = viridis::magma(100)[82], alpha = 0.1, color = NA) +
    geom_pointrange(color = viridis::magma(100)[82]) +
    theme_minimal(base_family = 'Fira Sans Extra Condensed', base_size = 9)
    # Save
    ggsave(
      filename = here(
        'figures',
        c(
          'event-study.pdf',
          'event-study-dist.pdf',
          'event-study-dist75.pdf',
          'event-study-total-visits.pdf',
          'event-study-visits-other-county.pdf'
        )[i]
      ),
      plot = event_study,
      height = 3,
      width = 3.2,
      dpi = 450
    )
    save_plot(
      filename = here(
        'figures', 'event-study',
        c(
          'event-study.pdf',
          'event-study-dist.pdf',
          'event-study-dist75.pdf',
          'event-study-total-visits.pdf',
          'event-study-visits-other-county.pdf'
        )[i]
      ),
      plot = event_study,
      base_height = 3,
      base_width = 3.2,
      dpi = 450
    )
  }
