# Notes ----------------------------------------------------------------------------------
#   Goal:   Build regression tables for main results


# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(
    parallel, fastverse, fixest, qs, stringr,
    magrittr, here
  )


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


# Load data: Regression results ----------------------------------------------------------
  # Lags: Percent out of county
  est_lag_pct =
    qread(
      file = here('data-results', 'est-lag-pct.qs'),
      nthreads = 16
    )
  # Lags: 75th percentile distance traveled
  est_lag_dist =
    qread(
      file = here('data-results', 'est-lag-dist.qs'),
      nthreads = 16
    )
  # Percentile-heterogeneity: Percent out of county
  est_het_pct =
    qread(
      file = here('data-results', 'est-het-pct.qs'),
      nthreads = 16
    )
  # Percentile-heterogeneity: 75th percentile distance traveled
  est_het_dist =
    qread(
      file = here('data-results', 'est-het-dist.qs'),
      nthreads = 16
    )
  # Decompose visits: Numerator and denominator
  est_het_visits =
    qread(
      file = here('data-results', 'est-het-frac.qs'),
      nthreads = 16
    )


# Make tables ----------------------------------------------------------------------------
  # Main results: Percent out of county
  etable(
    est_lag_pct[fixef = 3, sample = 'FALSE', rhs = 1],
    est_het_pct[fixef = 3, sample = 'FALSE'],
    tex = TRUE,
    style.tex = style.tex('aer'),
    fitstat = ~ n_m + y_mean,
    digits = 2
  )
  # Main results: 75th percentile distance traveled
  etable(
    est_lag_dist[fixef = 3, sample = 'FALSE', rhs = 1],
    est_het_dist[fixef = 3, sample = 'FALSE'],
    tex = TRUE,
    style.tex = style.tex('aer'),
    fitstat = ~ n_m + y_mean,
    digits = 2
  )
  # Lags: Percent out of county
  etable(
    est_lag_pct[fixef = 3, sample = 'FALSE'],
    tex = TRUE,
    style.tex = style.tex('aer'),
    fitstat = ~ n_m + y_mean,
    digits = 2
  )
  # Lags: 75th percentile distance traveled
  etable(
    est_lag_dist[fixef = 3, sample = 'FALSE'],
    tex = TRUE,
    style.tex = style.tex('aer'),
    fitstat = ~ n_m + y_mean,
    digits = 2
  )
  # Decompose visits: Numerator and denominator
  etable(
    est_het_visits[lhs = 1:3, fixef = 3, rhs = 1, sample = 'FALSE'],
    tex = TRUE,
    style.tex = style.tex('aer'),
    fitstat = ~ n_m + y_mean,
    digits = 2
  )
  # Robustness to FE spec: Percent out of county
  etable(
    est_lag_pct[fixef = 2, sample = 'FALSE', rhs = 1],
    est_het_pct[fixef = 2, sample = 'FALSE'],
    tex = TRUE,
    style.tex = style.tex('aer'),
    fitstat = ~ n_m + y_mean,
    digits = 2
  )
  # Robustness to FE spec: 75th percentile distance traveled
  etable(
    est_lag_dist[fixef = 2, sample = 'FALSE', rhs = 1],
    est_het_dist[fixef = 2, sample = 'FALSE'],
    tex = TRUE,
    style.tex = style.tex('aer'),
    fitstat = ~ n_m + y_mean,
    digits = 2
  )



# Test where percentile heterogeneity differs from zero ----------------------------------
  # Function to test significance at different percentiles
  test_fun =
    function(est) {
      # Extract the coefficients and covariance matrix for the first two parameters
      beta = est[[1]]$coefficients[1:2]
      sigma = est[[1]]$cov.scaled[1:2, 1:2]
      # Calculate the p-value for each percentile
      lapply(X = seq(0, 1, by = .01), FUN = function(p) {
        # Calculate the linear combination of coefficients
        combo = (c(1, p) * beta) |> sum()
        # Calculate the standard error of the linear combination
        combo_se = sqrt(t(c(1, p)) %*% sigma %*% c(1, p))
        # Calculate the z-score and p-value
        z = combo / combo_se
        # Return the results as a data.table
        data.table(
          p = p,
          combo = combo |> as.numeric(),
          se = combo_se |> as.numeric(),
          p_val = 2 * pnorm(-abs(z)) |> as.numeric()
        )
      }) |>
      rbindlist(use.names = TRUE, fill = TRUE)
    }
  # Income
  est_het_pct[fixef = 3, sample = 'FALSE', rhs = 1] %>% test_fun() %>% .[p_val < .05]
  est_het_dist[fixef = 3, sample = 'FALSE', rhs = 1] %>% test_fun() %>% .[p_val < .05]
  # Black
  est_het_pct[fixef = 3, sample = 'FALSE', rhs = 2] %>% test_fun() %>% .[p_val < .05]
  est_het_dist[fixef = 3, sample = 'FALSE', rhs = 2] %>% test_fun() %>% .[p_val < .05]
  # Hispanic
  est_het_pct[fixef = 3, sample = 'FALSE', rhs = 3] %>% test_fun() %>% .[p_val < .05]
  est_het_dist[fixef = 3, sample = 'FALSE', rhs = 3] %>% test_fun() %>% .[p_val < .05]
  # White
  est_het_pct[fixef = 3, sample = 'FALSE', rhs = 4] %>% test_fun() %>% .[p_val < .05]
  est_het_dist[fixef = 3, sample = 'FALSE', rhs = 4] %>% test_fun() %>% .[p_val < .05]
