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


# Setup: Regression tables ---------------------------------------------------------------
  # Dictionary
  setFixest_dict(c(
    any_smoke = 'Any smoke',
    lag_any_smoke = 'Lag of any smoke',
    lag2_any_smoke = 'Lag$_2$ of any smoke',
    lag3_any_smoke = 'Lag$_3$ of any smoke',
    lag4_any_smoke = 'Lag$_4$ of any smoke',
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
    total_visits = 'Total visits',
    `total_visits - visits_same_county` = 'Visits beyond home county',
    visits_same_county = 'Visits within home county',
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
  # Latex table
  etable(
    est_lag_pct[fixef = 3, sample = 'FALSE'],
    tex = TRUE,
    style.tex = style.tex('aer'),
    fitstat = ~ n_m + y_mean,
    digits = 2
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
  # Latex table
  etable(
    est_lag_dist[fixef = 3, sample = 'FALSE'],
    tex = TRUE,
    style.tex = style.tex('aer'),
    fitstat = ~ n_m + y_mean,
    digits = 2
  )
  # Clean up
  rm(est_lag_pct, est_lag_dist)
  invisible(gc())


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
      fsplit = ~fire %keep% 'FALSE',
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
  # Latex table
  etable(
    est_het_pct[fixef = 3, sample = 'FALSE'],
    tex = TRUE,
    style.tex = style.tex('aer'),
    fitstat = ~ n_m + y_mean,
    digits = 2
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
      fsplit = ~fire %keep% 'FALSE',
      lean = TRUE,
      cluster = ~county + mos,
      weights = ~pop
    )
  # Save results
  qsave(
    x = est_het_dist,
    file = here('data-results', 'est-het-dist.qs'),
    preset = 'high',
    nthreads = 16
  )
  # Latex table
  etable(
    est_het_dist[fixef = 3, sample = 'FALSE'],
    tex = TRUE,
    style.tex = style.tex('aer'),
    fitstat = ~ n_m + y_mean,
    digits = 2
  )
  # Clean up
  rm(est_het_pct, est_het_dist)
  invisible(gc())


# Regressions: Decompose fraction with heterogeneity -------------------------------------
  # Estimates for numerator (visits to other counties) and denominator (total visits)
  est_het_visits =
    feols(
      c(total_visits, total_visits - visits_same_county, visits_same_county) ~
      any_smoke +
      sw(
        1,
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
      fsplit = ~fire %keep% 'FALSE',
      cluster = ~county + mos,
      weights = ~pop
    )
  # Save results
  qsave(
    x = est_het_visits,
    file = here('data-results', 'est-het-frac.qs'),
    preset = 'high',
    nthreads = 16
  )
  # Latex table
  etable(
    est_het_visits[lhs = 1:3, fixef = 3, rhs = 1, sample = 'FALSE'],
    tex = TRUE,
    style.tex = style.tex('aer'),
    fitstat = ~ n_m + y_mean,
    digits = 2
  )
  # Clean up
  rm(est_het_pct, est_het_dist)
  invisible(gc())


# Compare smoke effects on quartiles -----------------------------------------------------
  # Effect of smoke on distance-traveled quartiles
  est_dist_p =
    feols(
      c(dist_p25, dist_median, dist_p75) ~
      any_smoke |
      cbg_home + wos + state ^ yr,
      data = full_dt[fire == FALSE],
      lean = TRUE,
      cluster = ~county + mo ^ yr,
      weights = ~pop
    )
  # Save results
  qsave(
    x = est_dist_p,
    file = here('data-results', 'est-dist-p.qs'),
    preset = 'high',
    nthreads = 16
  )
  # Latex table
  etable(
    est_dist_p,
    tex = TRUE,
    style.tex = style.tex('aer'),
    fitstat = ~ n_m + y_mean,
    digits = 2
  )
  # Clean up
  rm(est_dist_p)
  invisible(gc())


# Smoke exposure by demographic ----------------------------------------------------------
  # Regress smoke exposure on CBG demographics (varying fixed effects)
  est_smoke =
    feols(
      any_smoke ~
      sw(
        hh_inc_p,
        shr_black_p,
        shr_hispanic_p,
        shr_white_p
      ) |
      sw(
        1,
        wos,
        wos + state ^ yr + state ^ mo,
        wos + state ^ yr,
        state ^ wos,
        state,
        yr,
        state ^ yr
      ),
      data = full_dt[fire == FALSE],
      lean = TRUE,
      cluster = ~county + mo ^ yr,
      weights = ~pop
    )
  # Save results
  qsave(
    x = est_smoke,
    file = here('data-results', 'est-smoke.qs'),
    preset = 'high',
    nthreads = 16
  )
  # Latex table
  etable(
    est_smoke[fixef = 4],
    tex = TRUE,
    style.tex = style.tex('aer'),
    fitstat = ~ n_m + y_mean,
    digits = 3
  )
  # Clean up
  rm(est_smoke)
  invisible(gc())
