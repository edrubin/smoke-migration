# Notes ----------------------------------------------------------------------------------
#   Goal:   Run raw (unadjusted) event study for effect of smoke onset on migration.
#   Time:   Short (1-2 minutes)


# Observations ---------------------------------------------------------------------------
#   - It looks like the difference (wrt to income) in smoke response comes from LOW smoke.
#   - CBG and tract effects are consistent with county effects—just smaller magnitude.


# Load and prep data ---------------------------------------------------------------------
  # Source the script that loads and prepares the data
  here::here('code', '002b-prep-analysis-data.R') |> source()


# Set up event study: Pair smoke and non-smoke weeks within CBG ------------------------
# ADJUST Toggle to subset to post-event periods with all smoke
  # post_all_smoke = TRUE
  post_all_smoke = FALSE
  # Build event-study dataset
  # Set event-study length
  n_pre = 5
  n_post = 5
  n = n_pre + n_post + 1
  # Build dataset for finding smoke 'periods'
  event_dt = full_dt[, .(cbg_home, date_start, any_smoke)]
  # Add a counter for all dates and smoke dates within each CBG
  setkey(event_dt, cbg_home, date_start)
  event_dt[, `:=`(
    counter_date = seq_len(.N),
    counter_smoke = cumsum(any_smoke)
  ), by = cbg_home]
  # Add lags and leads
  event_dt[, `:=`(
    counter_smoke_lag_n = flag(counter_smoke, n = n_pre),
    counter_smoke_lead_n = flag(counter_smoke, n = -n_post),
    any_smoke_lag_n = flag(any_smoke, n = n_pre),
    any_smoke_lead_n = flag(any_smoke, n = -n_post)
  ), by = cbg_home]
  # Subset 1: Periods with smoke at time '0' and no smoke n_pre periods before
  smoke_dt =
    event_dt[(
      any_smoke == 1 &
      counter_smoke_lag_n == counter_smoke - 1 &
      any_smoke_lag_n == 0
    )]
  # If desired: Subset to periods where all of 'post' included smoke
  if (post_all_smoke == TRUE) {
    smoke_dt %<>% .[counter_smoke_lead_n == counter_smoke + n_post]
  }
  # Subset 2: Periods without smoke at time '0' and no smoke before or after np periods
  nosmoke_dt =
    event_dt[(
      any_smoke == 0 &
      counter_smoke_lag_n == counter_smoke &
      counter_smoke_lead_n == counter_smoke &
      any_smoke_lag_n == 0
    )]
  # Add subset indicators and calendar week
  smoke_dt[, `:=`(
    subset = 'smoke',
    wk = week(date_start)
  )]
  nosmoke_dt[, `:=`(
    subset = 'nosmoke',
    wk = week(date_start)
  )]
  # Combine datasets
  event_dt = rbindlist(list(smoke_dt, nosmoke_dt), use.names = TRUE, fill = TRUE)
  # Find matches (match on week of year and home CBG)
  match_dt =
    merge(
      smoke_dt[, .(cbg_home, wk, in_smoke = 1)],
      nosmoke_dt[, .(cbg_home, wk, in_nosmoke = 1)],
      by = c('cbg_home', 'wk'),
      all.x = FALSE,
      all.y = FALSE,
      allow.cartesian = TRUE
    ) |>
    funique()
  # Subset to matches
  event_dt %<>%
    merge(
      y = match_dt[, .(cbg_home, wk)],
      by = c('cbg_home', 'wk'),
      all.x = FALSE,
      all.y = TRUE
    )
  # Drop 'wk' for now
  event_dt[, wk := NULL]
  # Drop unnecessary variables
  event_dt[, c(
    'any_smoke',
    'counter_date', 'counter_smoke',
    'counter_smoke_lag_n', 'counter_smoke_lead_n',
    'any_smoke_lag_n', 'any_smoke_lead_n'
  ) := NULL]
  rm(smoke_dt, nosmoke_dt)
  invisible(gc())
  # Change 'date_state' name
  setnames(event_dt, old = 'date_start', new = 'date_smoke_start')
  # Expand dataset to create event study: N weeks with (N-1)/2 pre, (N-1)/2 post
  event_dt %<>% .[rep(seq_len(.N), each = n)]
  # Add event time
  event_dt[, `:=`(
    event_time = -n_pre:n_post,
    event_period = seq_len(.N)
  ), by = .(cbg_home, date_smoke_start, subset)]
  event_dt[, `:=`(
    event_period = factor(
      event_period,
      levels = 1:n,
      labels = paste0('p', str_pad(1:n, 2, 'left', 0)),
      ordered = TRUE
    )
  )]
  # Create date variable with actual weeks implied by event time
  event_dt[, date_start := date_smoke_start + (event_time * 7)]
  # Add identifier for CBG by calendar-week (for fixed effects)
  event_dt[, `:=`(
    cbg_wk =
      paste0(
        cbg_home,
        '-',
        week(date_smoke_start) |> str_pad(2, 'left', 0)
      )
  )]
  # Merge full dataset onto event-study setup
  event_trt =
    merge(
      x = event_dt,
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
  # Clean up
  invisible(gc())
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
  # Excluded period: 1 before the end of the pre-period
  p_drop = paste0('p', str_pad(n_pre - 1, 2, 'left', 0))


# Estimate event study -------------------------------------------------------------------
# TODO Clean up in-progress code below
  # Estimate
  test =
    feols(
      c(
        1 - pct_same_county,
        1 - pct_same_state,
        total_visits - visits_same_county,
        total_visits - visits_same_state,
        any_smoke,
        total_visits
      ) ~
      p01 + p02 + p03 + p05 + p06 + p07 + p08 + p09 + p10 + p11 |
      cbg_wk + state ^ yr,
      data = event_trt[county != '06073' & i_rural == 0 & !(cbg_home %in% fire_dt$cbg)],
      cluster = ~county + mo ^ yr,
      weights = ~pop,
    )
  # Plot
  for (i in seq_along(test)) test[[i]] |> coefplot()


# Estimate event study: Raw coefficients -------------------------------------------------
  # Iterate over outcome variables
  for (i in 1:8) {
    # Build formula
    event_f =
      paste0(
        c(
          '1 - pct_same_county ~ ',
          '1 - pct_same_state ~ ',
          'dist_median ~ ',
          'dist_p75 ~ ',
          'total_visits ~ ',
          'total_visits - visits_same_county ~ ',
          'total_visits - visits_same_state ~ ',
          'any_smoke ~ '
        )[i],
        str_subset(names(event_trt), '^p[0-9]{2}$') |>
          str_subset(p_drop, negate = TRUE) |>
          paste(collapse = ' + ')
      ) |>
      as.formula()
    # Estimate!
    event_est =
      feols(
        event_f,
        # data = event_trt[county != '06073' & i_rural == 0],
        data = event_trt[county != '06073' & i_rural == 0 & !(cbg_home %in% fire_dt$cbg)],
        cluster = ~county + mo ^ yr,
        weights = ~pop,
        split = ~subset
      )
    # Create dataset for plotting event study
    # Build dataset with estimates and confidence intervals
    plot_dt = cbind(
      event_est %>% coeftable() %>% extract(, 3:5),
      event_est %>% confint() %>% extract(, 5:6)
    )
    setDT(plot_dt)
    setnames(plot_dt, c('sample', 'period', 'estimate', 'conf.low', 'conf.high'))
    # Drop intercept
    plot_dt %<>% fsubset(period != '(Intercept)')
    # Add omitted period
    plot_dt = rbindlist(list(
      plot_dt,
      data.table(
        sample = c('nosmoke', 'smoke'),
        period = rep(p_drop, 2),
        estimate = rep(0, 2),
        conf.low = rep(0, 2),
        conf.high = rep(0, 2)
      )
    ))
    # Make event time
    setorder(plot_dt, sample, period)
    plot_dt[, time := rep(-n_pre:n_post, 2)]
    # Plot
    event_study = ggplot(
      data = plot_dt,
      aes(
        x = time, y = estimate, ymin = conf.low, ymax = conf.high,
        fill = sample, color = sample
      )
    ) +
    geom_hline(yintercept = 0, size = .25) +
    geom_vline(xintercept = 0, size = .25, linetype = 'dashed') +
    scale_x_continuous('Weeks relative to first smoke exposure') +
    scale_y_continuous(
      name = c(
        'Out migration:\nPercentage of visits outside of home county',
        'Out migration:\nPercentage of visits outside of home state',
        'Out migration:\nMedian distance from home (km)',
        'Out migration:\n Distance from home (km, 75th pcntl.)',
        'Total POI visits',
        'Visits to non-home counties',
        'Visits to non-home states',
        'Smoke exposure:\nPercentage of population facing smoke'
      )[i],
      labels = list(
        percent,
        percent,
        comma,
        comma,
        comma,
        comma,
        comma,
        percent
      )[[i]]
    ) +
    geom_ribbon(
      aes(alpha = sample),
      color = NA, position = position_dodge2(width = .3)
    ) +
    geom_pointrange(position = position_dodge2(width = .3)) +
    theme_minimal(base_family = 'Fira Sans Extra Condensed', base_size = 9) +
    scale_fill_manual(
      '',
      values = c(viridis::magma(100)[82], viridis::magma(100)[22]),
      labels = c('No smoke', 'Smoke')
    ) +
    scale_color_manual(
      '',
      values = c(viridis::magma(100)[82], viridis::magma(100)[22]),
      labels = c('No smoke', 'Smoke')
    ) +
    scale_alpha_manual(
      '',
      values = c(.1, .04),
      labels = c('No smoke', 'Smoke')
    ) +
    theme(legend.position = 'none')
    # Plot
    event_study
    # Save
    ggsave(
      filename = here(
        'figures', 'event-study-raw',
        c(
          'event-study-county.pdf',
          'event-study-state.pdf',
          'event-study-dist.pdf',
          'event-study-dist75.pdf',
          'event-study-total-visits.pdf',
          'event-study-visits-other-county.pdf',
          'event-study-visits-other-state.pdf',
          'event-study-smoke.pdf'
        )[i]
      ),
      plot = event_study,
      height = 3,
      width = 6.4,
      dpi = 450
    )
  }
  # Save a legend that matches the plots
  ggsave(
    path = here('figures', 'event-study-raw'),
    filename = 'event-study-legend.pdf',
    plot = (
        event_study +
        theme_minimal(base_family = 'Fira Sans Extra Condensed', base_size = 12) +
        theme(legend.position = 'bottom')
      ) |>
      ggpubr::get_legend() |>
      ggpubr::as_ggplot(),
    height = 0.5,
    width = 2.5
  )
