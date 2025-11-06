# Notes ----------------------------------------------------------------------------------
#   Goal:   Run main regression-based analyses.
#   Time:   ~1 minute


# Observations ---------------------------------------------------------------------------
#   - It looks like the difference (wrt to income) in smoke response comes from LOW smoke.
#   - CBG and tract effects are consistent with county effects—just smaller magnitude.


# Load and prep data ---------------------------------------------------------------------
  # Source the script that loads and prepares the data
  here::here('code', '002b-prep-analysis-data.R') |> source()
  # Drop rural areas (focusing on urban)
  full_dt %<>% .[i_rural == 0]
  # Create an indicator for fire-affected areas
  full_dt[, fire := cbg_home %in% fire_dt[fire_year %in% 2018:2021, cbg]]
  # Create week-of-sample and month-of-sample variables
  full_dt[, `:=`(
    wos = paste0(yr, '-', sprintf('%02d', wk)),
    mos = paste0(yr, '-', sprintf('%02d', mo))
  )]


# Setup: Regression tables ---------------------------------------------------------------
  # Dictionary
  setFixest_dict(c(
    any_smoke = 'Any smoke',
    lag_any_smoke = 'Lag of any smoke',
    lag2_any_smoke = 'Lag$_2$ of any smoke',
    lag3_any_smoke = 'Lag$_3$ of any smoke',
    cbg_home = 'CBG',
    county = 'County',
    state = 'State',
    mo = 'Month',
    yr = 'Year',
    wk = 'Week',
    wos = 'Week of sample',
    mos = 'Month of sample',
    pop = 'CBG population',
    hospital_visits_total = 'Total hospital visits',
    `hospital_visits_total/total_visits` = 'Hospital-visits share',
    `100*hospital_visits_total/total_visits` = 'Hospital-visits share',
    `100*(1 - pct_same_county)` = 'Percent of visits beyond home county',
    `1 - pct_same_county` = 'Percent of visits beyond home county',
    dist_median = 'Median distance traveled',
    hh_inc_q1 = 'Income quarter 1',
    hh_inc_q2 = 'Income quarter 2',
    hh_inc_q3 = 'Income quarter 3',
    hh_inc_q4 = 'Income quarter 4'
  ))
  # New 'fitstat': Number of observations in millions
  fitstat_register(
    type = 'n_m',
    fun = function(est) round(est$nobs / 1e6, 2) %>% as.character(),
    alias = 'N obs. (millions)'
  )
  # New 'fitstat': Mean of dependent variable
  fitstat_register(
    type = 'y_mean',
    fun = function(est) {
      fmean(est$residuals + est$fitted.values) %>% round(2) %>% as.character()
    },
    alias = 'Mean of dependent variable'
  )


# Regressions: Lags ----------------------------------------------------------------------
  # Lag structure: Percent of visits out-of-county
  est_lag_pct =
    feols(
      100 * (1 - pct_same_county) ~
      csw(any_smoke, lag_any_smoke, lag2_any_smoke, lag3_any_smoke, lag4_any_smoke) |
      cbg_home +
      sw(
        wos,
        wos + state ^ yr + state ^ mo,
        wos + state ^ yr,
        state ^ wos
      ),
      data = full_dt,
      fsplit = ~fire %keep% 'FALSE',
      lean = TRUE,
      cluster = ~county + mo ^ yr,
      weights = ~pop
    )
  # Save results
  qsave(
    x = est_lag_pct,
    file = here('data-results', 'est-lag-pct.qs'),
    preset = 'high',
    nthreads = 16
  )
  # The lag structure for the 75th percentile of distance traveled
  est_lag_dist =
    feols(
      dist_p75 ~
      csw(any_smoke, lag_any_smoke, lag2_any_smoke, lag3_any_smoke, lag4_any_smoke) |
      cbg_home +
      sw(
        wos,
        wos + state ^ yr + state ^ mo,
        wos + state ^ yr,
        state ^ wos
      ),
      data = full_dt,
      fsplit = ~fire %keep% 'FALSE',
      lean = TRUE,
      cluster = ~county + mo ^ yr,
      weights = ~pop
    )
  # Save results
  qsave(
    x = est_lag_dist,
    file = here('data-results', 'est-lag-dist.qs'),
    preset = 'high',
    nthreads = 16
  )
  # etable(
  #   est_lag_pct, est_lag_dist,
  #   tex = TRUE,
  #   style.tex = style.tex("aer"),
  #   fitstat = ~ n_m + y_mean,
  #   digits = 2
  # )


# Regressions: Percentile heterogeneity --------------------------------------------------
  # Heterogeneity by CBG characteristics: Percent of visits out-of-county
  est_het_pct =
    feols(
      100 * (1 - pct_same_county) ~
      any_smoke +
      sw(
        any_smoke:hh_inc_p,
        any_smoke:shr_black_p,
        any_smoke:shr_hispanic_p,
        any_smoke:shr_white_p
      ) |
      cbg_home +
      sw(
        wos,
        wos + state ^ yr + state ^ mo,
        wos + state ^ yr,
        state ^ wos
      ),
      data = full_dt,
      fsplit = ~fire %keep% 0,
      lean = TRUE,
      cluster = ~county + mos,
      weights = ~pop
    )
  # Heterogeneity by CBG characteristics: 75th percentile of distance traveled
  est_het_dist =
    feols(
      dist_p75 ~
      any_smoke +
      sw(
        any_smoke:hh_inc_p,
        any_smoke:shr_black_p,
        any_smoke:shr_hispanic_p,
        any_smoke:shr_white_p
      ) |
      cbg_home +
      sw(
        wos,
        wos + state ^ yr + state ^ mo,
        wos + state ^ yr,
        state ^ wos
      ),
      data = full_dt,
      fsplit = ~fire %keep% 0,
      lean = TRUE,
      cluster = ~county + mos,
      weights = ~pop
    )
  # Save results
  qsave(
    x = est_het_pct,
    file = here('data-results', 'est-het-pct.qs'),
    preset = 'high',
    nthreads = 16
  )
  qsave(
    x = est_het_dist,
    file = here('data-results', 'est-het-dist.qs'),
    preset = 'high',
    nthreads = 16
  )


# Specification chart --------------------------------------------------------------------
  # Robustness: Fixed-effect specification
  fe_est = feols(
    1 - pct_same_county ~
    any_smoke + lag_any_smoke + lag2_any_smoke + lag3_any_smoke |
    sw(
      county + mo + yr,
      county ^ mo + county ^ yr,
      county + mo ^ yr,
      # county ^ mo + mo ^ yr,
      # county ^ mo + mo ^ yr + county ^ yr,
      # county ^ mo ^ yr,
      # county + wk + yr,
      # county ^ wk + county ^ yr,
      # county + wk ^ yr,
      # county ^ wk + wk ^ yr,
      # county ^ wk + wk ^ yr + county ^ yr,
      # county ^ wk ^ yr,
# TODO: Add more FE specifications
      cbg_home + mo + yr,
      cbg_home + county ^ mo + county ^ yr,
      cbg_home + mo ^ yr,
      cbg_home + wk + yr,
      cbg_home + wk ^ yr
    ),
    data = full_dt,
    fsplit = ~fire %keep% 0,
    cluster = ~county + mo,
    weights = ~pop
  )
  # Create a table of the results and specifications
  fe_dt =
    lapply(
      X = seq_len(fe_est %>% length()),
      FUN = function(i) {
        # Get results (with confidence intervals)
        est_dt = broom::tidy(fe_est[[i]], conf.int = TRUE) %>% setDT()
        # Add fixed effects
        est_dt[, controls := fe_est[[i]]$fixef_vars %>% paste(collapse = ', ')]
      }
    ) %>%
    rbindlist()
  # Plot specification curve for 'any smoke'
  plot_specs(
    df = fe_dt[term == 'any_smoke'],
    choices = 'controls'
  )
  # Plot specification curve for the lag of 'any smoke'
  plot_specs(
    df = fe_dt[term == 'lag_any_smoke'],
    choices = 'controls'
  )
  # Plot specification curve for the second lag of 'any smoke'
  plot_specs(
    df = fe_dt[term == 'lag2_any_smoke'],
    choices = 'controls'
  )
  plot_specs(
    df = fe_dt[term == 'lag3_any_smoke'],
    choices = 'controls'
  )

# TODO: Add additional specification choices, e.g.,
#   - type of smoke
#   - population-weighting
#   - urban/rural subsets

  # Check the denominator (total visits)
  feols(
    c(total_visits, log(total_visits)) ~
    csw(any_smoke, lag_any_smoke, lag2_any_smoke, lag3_any_smoke) |
    # county + mo + yr,
    county ^ mo + county ^ yr,
    # county + mo ^ yr,
    data = full_dt,
    cluster = ~county + mo,
    weights = ~pop
  ) %>% etable()

  # Check rural/urban split
  feols(
    1 - pct_same_county ~
    any_smoke + lag_any_smoke + lag2_any_smoke |
    # county + mo + yr,
    # county ^ mo + county ^ yr,
    county ^ mo ^ yr,
    # county + mo ^ yr,
    data = full_dt,
    fsplit = ~i_rural,
    cluster = ~county + mo,
    weights = ~pop
  ) %>% etable()

  # Het. by (CBG's) household income quantile and rural-urban split
  feols(
    # total_visits ~
    1 - pct_same_county ~
    # hh_inc_q1 +
    hh_inc_q2 +
    hh_inc_q3 +
    hh_inc_q4 +
    # any_smoke +
    hh_inc_q1 : any_smoke +
    hh_inc_q2 : any_smoke +
    hh_inc_q3 : any_smoke +
    hh_inc_q4 : any_smoke |
    county + mo + yr,
    # county ^ mo + county ^ yr,
    # county + mo ^ yr,
    # county ^ hh_inc_q + mo ^ hh_inc_q + yr ^ hh_inc_q,
    data = full_dt,
    cluster = ~county + mo,
    fsplit = ~i_rural,
    weights = ~pop
  ) %>% etable()

  # Urban counties: Estimate as separate regressions
  feols(
    # total_visits ~
    1 - pct_same_county ~
    any_smoke |
    # county + mo + yr,
    county ^ mo + county ^ yr,
    # county + mo ^ yr,
    # county ^ shr_white_q + mo ^ shr_white_q + yr ^ shr_white_q,
    data = full_dt[i_rural == 0],
    cluster = ~county + mo,
    fsplit = ~hh_inc_q,
    weights = ~pop
  ) %>% etable()

  # Het. by 'share white' quantile and rural-urban split
  feols(
    # total_visits ~
    1 - pct_same_county ~
    # shr_white_q1 +
    shr_white_q2 +
    shr_white_q3 +
    shr_white_q4 +
    # any_smoke +
    shr_white_q1 : any_smoke +
    shr_white_q2 : any_smoke +
    shr_white_q3 : any_smoke +
    shr_white_q4 : any_smoke |
    # county + mo + yr,
    county ^ mo + county ^ yr,
    # county + mo ^ yr,
    # county ^ shr_white_q + mo ^ shr_white_q + yr ^ shr_white_q,
    data = full_dt,
    cluster = ~county + mo,
    fsplit = ~i_rural,
    weights = ~pop
  ) %>% etable()

  # Het. by finer income quantiles
  hh20 = feols(
    # total_visits ~
    1 - pct_same_county ~
    hh_inc_q01 +
    hh_inc_q02 +
    hh_inc_q03 +
    hh_inc_q04 +
    hh_inc_q05 +
    hh_inc_q06 +
    hh_inc_q07 +
    hh_inc_q08 +
    hh_inc_q09 +
    # hh_inc_q10 +
    hh_inc_q11 +
    hh_inc_q12 +
    hh_inc_q13 +
    hh_inc_q14 +
    hh_inc_q15 +
    hh_inc_q16 +
    hh_inc_q17 +
    hh_inc_q18 +
    hh_inc_q19 +
    hh_inc_q20 +
    # any_smoke +
    hh_inc_q01 : any_smoke +
    hh_inc_q02 : any_smoke +
    hh_inc_q03 : any_smoke +
    hh_inc_q04 : any_smoke +
    hh_inc_q05 : any_smoke +
    hh_inc_q06 : any_smoke +
    hh_inc_q07 : any_smoke +
    hh_inc_q08 : any_smoke +
    hh_inc_q09 : any_smoke +
    hh_inc_q10 : any_smoke +
    hh_inc_q11 : any_smoke +
    hh_inc_q12 : any_smoke +
    hh_inc_q13 : any_smoke +
    hh_inc_q14 : any_smoke +
    hh_inc_q15 : any_smoke +
    hh_inc_q16 : any_smoke +
    hh_inc_q17 : any_smoke +
    hh_inc_q18 : any_smoke +
    hh_inc_q19 : any_smoke +
    hh_inc_q20 : any_smoke |
    # county + mo + yr,
    county ^ mo + county ^ yr,
    # county + mo ^ yr,
    # county ^ shr_white_q + mo ^ shr_white_q + yr ^ shr_white_q,
    data = full_dt[i_rural == 0],
    cluster = ~county + mo,
    # fsplit = ~i_rural,
    weights = ~pop
  )
  # Process results for figure
  hh20_dt =
    hh20 %>% broom::tidy(
      conf.int = TRUE
    ) %>%
    tibble::add_row(
      term = 'hh_inc_q10',
      estimate = 0,
      conf.low = 0,
      conf.high = 0
    ) %>%
    setDT()
  hh20_dt[, `:=`(
    smoke = str_detect(term, 'smoke'),
    q = str_extract(term, '(?<=q)[0-9]{2}')
  )]
  hh20_dt[, lab := ifelse(smoke == TRUE, 'Response to smoke', 'Average trends')]
  # Define colors
  c1 = viridis::magma(10)[2]
  c2 = viridis::magma(10)[7]
  # Plot 1: Response to smoke
  hh20p1 = ggplot(
    data = hh20_dt[smoke == TRUE],
    aes(
      x = as.numeric(q),
      y = estimate, ymin = conf.low, ymax = conf.high,
    )
  ) +
  scale_y_continuous(
    'Pct. visits away from home county when smokey',
    labels = scales::percent
  ) +
  scale_x_continuous('Income quantile') +
  geom_hline(yintercept = 0, size = .25) +
  geom_pointrange(color = c2) +
  geom_ribbon(fill = c2, alpha = 0.1, color = NA) +
  theme_minimal(base_family = 'Fira Sans Extra Condensed') +
  theme(legend.position = 'none') +
  ggtitle(
    'Differences in mobility and smoke response for 20 income quantiles',
    subtitle = 'Quantile\'s responses to smoke'
  )
  # Plot 2:
  hh20p2 = ggplot(
    data = hh20_dt[smoke == FALSE],
    aes(
      x = as.numeric(q),
      y = estimate, ymin = conf.low, ymax = conf.high,
    )
  ) +
  scale_y_continuous('Pct. visits away from home county', labels = scales::percent) +
  scale_x_continuous('Income quantile') +
  geom_hline(yintercept = 0, size = .25) +
  geom_pointrange(color = c1) +
  geom_ribbon(fill = c1, alpha = 0.1, color = NA) +
  theme_minimal(base_family = 'Fira Sans Extra Condensed') +
  theme(legend.position = 'none') +
  ggtitle('', subtitle = 'Income quantiles\' relative differences in travel')
  # Combine
  hh20p1 / hh20p2


  # Het. by finer income quantiles
  hi20 = feols(
    # total_visits ~
    1 - pct_same_county ~
    shr_hispanic_q01 +
    shr_hispanic_q02 +
    shr_hispanic_q03 +
    shr_hispanic_q04 +
    shr_hispanic_q05 +
    shr_hispanic_q06 +
    shr_hispanic_q07 +
    shr_hispanic_q08 +
    shr_hispanic_q09 +
    # shr_hispanic_q10 +
    shr_hispanic_q11 +
    shr_hispanic_q12 +
    shr_hispanic_q13 +
    shr_hispanic_q14 +
    shr_hispanic_q15 +
    shr_hispanic_q16 +
    shr_hispanic_q17 +
    shr_hispanic_q18 +
    shr_hispanic_q19 +
    shr_hispanic_q20 +
    # any_smoke +
    shr_hispanic_q01 : any_smoke +
    shr_hispanic_q02 : any_smoke +
    shr_hispanic_q03 : any_smoke +
    shr_hispanic_q04 : any_smoke +
    shr_hispanic_q05 : any_smoke +
    shr_hispanic_q06 : any_smoke +
    shr_hispanic_q07 : any_smoke +
    shr_hispanic_q08 : any_smoke +
    shr_hispanic_q09 : any_smoke +
    shr_hispanic_q10 : any_smoke +
    shr_hispanic_q11 : any_smoke +
    shr_hispanic_q12 : any_smoke +
    shr_hispanic_q13 : any_smoke +
    shr_hispanic_q14 : any_smoke +
    shr_hispanic_q15 : any_smoke +
    shr_hispanic_q16 : any_smoke +
    shr_hispanic_q17 : any_smoke +
    shr_hispanic_q18 : any_smoke +
    shr_hispanic_q19 : any_smoke +
    shr_hispanic_q20 : any_smoke |
    # county + mo + yr,
    county ^ mo + county ^ yr,
    # county + mo ^ yr,
    data = full_dt[i_rural == 0],
    cluster = ~county + mo,
    # fsplit = ~i_rural,
    weights = ~pop
  )
  # Process results for figure
  hi20_dt =
    hi20 %>% broom::tidy(
      conf.int = TRUE
    ) %>%
    tibble::add_row(
      term = 'shr_hispanic_q10',
      estimate = 0,
      conf.low = 0,
      conf.high = 0
    ) %>%
    setDT()
  hi20_dt[, `:=`(
    smoke = str_detect(term, 'smoke'),
    q = str_extract(term, '(?<=q)[0-9]{2}')
  )]
  hi20_dt[, lab := ifelse(smoke == TRUE, 'Response to smoke', 'Average trends')]
  # Define colors
  c1 = viridis::magma(10)[2]
  c2 = viridis::magma(10)[7]
  # Plot 1: Response to smoke
  hi20p1 = ggplot(
    data = hi20_dt[smoke == TRUE],
    aes(
      x = as.numeric(q),
      y = estimate, ymin = conf.low, ymax = conf.high,
    )
  ) +
  scale_y_continuous(
    'Pct. visits away from home county when smokey',
    labels = scales::percent
  ) +
  scale_x_continuous('\'hiite\' quantile') +
  geom_hline(yintercept = 0, size = .25) +
  geom_pointrange(color = c2) +
  geom_ribbon(fill = c2, alpha = 0.1, color = NA) +
  theme_minimal(base_family = 'Fira Sans Extra Condensed') +
  theme(legend.position = 'none') +
  ggtitle(
    'Differences in mobility and smoke response for 20 Hispaniic quantiles',
    subtitle = 'Quantile\'s responses to smoke'
  )
  # Plot 2:
  hi20p2 = ggplot(
    data = hi20_dt[smoke == FALSE],
    aes(
      x = as.numeric(q),
      y = estimate, ymin = conf.low, ymax = conf.high,
    )
  ) +
  scale_y_continuous('Pct. visits away from home county', labels = scales::percent) +
  scale_x_continuous('\'White\' quantile') +
  geom_hline(yintercept = 0, size = .25) +
  geom_pointrange(color = c1) +
  geom_ribbon(fill = c1, alpha = 0.1, color = NA) +
  theme_minimal(base_family = 'Fira Sans Extra Condensed') +
  theme(legend.position = 'none') +
  ggtitle('', subtitle = 'Hispanic quantiles\' relative differences in travel')
  # Combine
  hi20p1 / hi20p2

  # Robustness: Het. by 'share white' quantile with and without county-size controls
  feols(
    # total_visits ~
    1 - pct_same_county ~
    # shr_white_q1 +
    shr_white_q2 +
    shr_white_q3 +
    shr_white_q4 +
    # any_smoke +
    shr_white_q1 : any_smoke +
    shr_white_q2 : any_smoke +
    shr_white_q3 : any_smoke +
    shr_white_q4 : any_smoke + sw0(area_cb, area_tl) |
    # county + mo + yr,
    county ^ mo + county ^ yr,
    # county + mo ^ yr,
    # county ^ shr_white_q + mo ^ shr_white_q + yr ^ shr_white_q,
    data = full_dt,
    cluster = ~county + mo,
    weights = ~pop
  ) %>% etable()

  feols(
    # total_visits ~
    1 - pct_same_county ~
    # shr_black_q1 +
    shr_black_q2 +
    shr_black_q3 +
    shr_black_q4 +
    # any_smoke +
    shr_black_q1 : any_smoke +
    shr_black_q2 : any_smoke +
    shr_black_q3 : any_smoke +
    shr_black_q4 : any_smoke |
    # county + mo + yr,
    county ^ mo + county ^ yr,
    # county + mo ^ yr,
    # county ^ shr_black_q + mo ^ shr_black_q + yr ^ shr_black_q,
    data = full_dt,
    cluster = ~county + mo,
    fsplit = ~i_rural,
    weights = ~pop
  ) %>% etable()

  feols(
    # total_visits ~
    1 - pct_same_county ~
    # shr_hispanic_q1 +
    shr_hispanic_q2 +
    shr_hispanic_q3 +
    shr_hispanic_q4 +
    any_smoke +
    # shr_hispanic_q1 : any_smoke +
    shr_hispanic_q2 : any_smoke +
    shr_hispanic_q3 : any_smoke +
    shr_hispanic_q4 : any_smoke |
    # county + mo + yr,
    county ^ mo + county ^ yr,
    # county + mo ^ yr,
    # county ^ shr_hispanic_q + mo ^ shr_hispanic_q + yr ^ shr_hispanic_q,
    data = full_dt,
    cluster = ~county + mo,
    fsplit = ~i_rural,
    weights = ~pop
  ) %>% etable()

  feols(
    # total_visits ~
    1 - pct_same_county ~
    # shr_employed_q1 +
    shr_employed_q2 +
    shr_employed_q3 +
    shr_employed_q4 +
    # any_smoke +
    shr_employed_q1 : any_smoke +
    shr_employed_q2 : any_smoke +
    shr_employed_q3 : any_smoke +
    shr_employed_q4 : any_smoke |
    # county + mo + yr,
    county ^ mo + county ^ yr,
    # county + mo ^ yr,
    # county ^ shr_employed_q + mo ^ shr_employed_q + yr ^ shr_employed_q,
    data = full_dt,
    cluster = ~county + mo,
    fsplit = ~i_rural,
    weights = ~pop
  ) %>% etable()

  feols(
    # total_visits ~
    1 - pct_same_county ~
    # pop_q1 +
    pop_q2 +
    pop_q3 +
    pop_q4 +
    # any_smoke +
    pop_q1 : any_smoke +
    pop_q2 : any_smoke +
    pop_q3 : any_smoke +
    pop_q4 : any_smoke |
    # county + mo + yr,
    county ^ mo + county ^ yr,
    # county + mo ^ yr,
    # county ^ pop_q + mo ^ pop_q + yr ^ pop_q,
    data = full_dt,
    cluster = ~county + mo,
    fsplit = ~i_rural,
    weights = ~pop
  ) %>% etable()

  lapply(
    X = paste0('q', 1:4),
    FUN = function(q) {
      feols(
        1 - pct_same_county ~
        any_mh_smoke + lag_any_mh_smoke + lag2_any_mh_smoke + lag3_any_mh_smoke |
        # county + mo + yr,
        county ^ mo + county ^ yr,
        # county + mo ^ yr,
        data = full_dt[hh_inc_q == q],
        cluster = ~county + mo,
        weights = ~pop
      )
    }
  ) %>% etable()
