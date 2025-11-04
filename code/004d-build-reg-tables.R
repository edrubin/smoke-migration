

# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(
    parallel, fastverse, fixest, qs,
    magrittr, here
  )
  fastverse_extend(topics = c('DT', 'ST', 'VI'))
  # Fix collapse's F issue
  F = FALSE

# Setup: Regression tables ---------------------------------------------------------------
  # Dictionary
  setFixest_dict(c(
    any_smoke = 'Any smoke',
    lag_any_smoke = 'Lag of any smoke',
    lag2_any_smoke = 'Lag$_2$ of any smoke',
    lag3_any_smoke = 'Lag$_3$ of any smoke',
    lag4_any_smoke = 'Lag$_4$ of any smoke',
    county = 'County',
    mo = 'Month',
    yr = 'Year',
    wk = 'Week',
    pop = 'CBG population',
    `100*(1 - pct_same_county)` = 'Percent of visits beyond home county',
    `1 - pct_same_county` = 'Percent of visits beyond home county',
    dist_median = 'Median distance traveled',
    dist_p75 = '75$^\\text{th} percentile of distance traveled (km)$',
    # hh_inc_p = 'Income percentile',
    # shr_black_p = 'Pct. Black percentile',
    # shr_hispanic_p = 'Pct. Hispanic percentile',
    # shr_white_p = 'Pct. White percentile'
    hh_inc_p = 'Het. percentile',
    shr_black_p = 'Het. percentile',
    shr_hispanic_p = 'Het. percentile',
    shr_white_p = 'Het. percentile'
  ))
  # New 'fitstat': Number of observations in millions
  fitstat_register(
    type = 'n_m', 
    fun = function(est) round(est$nobs/1e6, 2) %>% as.character(),
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

# Load data: Regression results ----------------------------------------------------------
  # Lags: Percent out of county
  est_lag_pct = qread(
    file = here('data-results', 'est-lag-pct.qs'),
    nthreads = 16
  )
  # Lags: 75th percentile distance traveled
  est_lag_dist = qread(
    file = here('data-results', 'est-lag-dist.qs'),
    nthreads = 16
  )
  # Percentile-heterogeneity: Percent out of county
  est_het_pct = qread(
    file = here('data-results', 'est-het-pct.qs'),
    nthreads = 16
  )
  # Percentile-heterogeneity: 75th percentile distance traveled
  est_het_dist = qread(
    file = here('data-results', 'est-het-dist.qs'),
    nthreads = 16
  )

# Make tables ----------------------------------------------------------------------------

  # Main results 
  etable(
    # est_lag_pct[1], est_het_pct[1:4],
    # est_lag_pct[6], est_het_pct[5:8],
    est_lag_dist[1], est_het_dist[1:4],
    # est_lag_dist[6], est_het_dist[5:8],
    tex = TRUE,
    # style.tex = style.tex("aer"),
    # fitstat = ~ n_m + y_mean,
    digits = 2
  )

  # Lags
  etable(
    est_lag_pct[1:5],
    tex = TRUE,
    style.tex = style.tex("aer"),
    # fitstat = ~ n_m + y_mean,
    digits = 2
  )
  etable(
    est_lag_pct[1:5],
    tex = TRUE,
    style.tex = style.tex("aer"),
    fitstat = ~ n_m + y_mean,
    digits = 2
  )

