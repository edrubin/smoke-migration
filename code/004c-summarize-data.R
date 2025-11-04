# Notes ----------------------------------------------------------------------------------
#   Goal: Summary statistics and tables for paper
#   Note: Some formatting done by hand after producing the tables.


# Setup ----------------------------------------------------------------------------------
  # Load packages
  pacman::p_load(fastverse, fixest, vtable, here)
  fastverse_extend(topics = c('VI', 'ST'))


# Load data: Analysis dataset (west coast) -----------------------------------------------
  # Read the data
  full_dt = here(
    'data-processed', 'for-analysis',
    'for-analysis-westcoast.fst'
  ) %>% read_fst(as.data.table = TRUE)


# Data work: Collapse to CBG -------------------------------------------------------------
  # Summarize CBGs (across weeks)
  setkey(full_dt, cbg_home)
  cbg_dt = full_dt %>% .[, .(
    total_visits = fsum(total_visits),
    total_visits_same_cbg = fsum(visits_same_cbg),
    total_visits_same_county = fsum(visits_same_county),
    weeks_smoke = fsum(any_smoke),
    pop = fmedian(pop),
    pop_black = fmedian(pop_black),
    pop_hispanic = fmedian(pop_hispanic),
    pop_white = fmedian(pop_white),
    pop_rural = fmedian(pop_rural),
    pop_urban = fmedian(pop_urban),
    shr_black = 100 * fmedian(pop_black / pop),
    shr_hispanic = 100 * fmedian(pop_hispanic / pop),
    shr_white = 100 * fmedian(pop_white / pop),
    shr_rural = 100 * fmedian(pop_rural / (pop_rural + pop_urban)),
    shr_urban = 100 * fmedian(pop_urban / (pop_rural + pop_urban)),
    hh_inc = fmedian(hh_inc),
    dist_p75 = fmedian(dist_p75)
  ), by = cbg_home]


# Summary: Rural/urban -------------------------------------------------------------------
  # Urban share of CBGs
  cbg_dt[, fmean(pop_urban > pop_rural)]
  # Urban share of population
  cbg_dt[, fsum((shr_urban > shr_rural) * pop) / fsum(pop)]
  cbg_dt[(shr_urban > shr_rural), fsum(pop)]
  # Urban share of the visits
  cbg_dt[, fsum((shr_urban > shr_rural) * total_visits) / fsum(total_visits)]
  cbg_dt[(shr_urban > shr_rural), fsum(total_visits)]


# Summary table: CBG level ---------------------------------------------------------------
  st(
    # data = cbg_dt[pop_urban > pop_rural],
    data = cbg_dt,
    out = 'latex',
    vars = c(
      'POI VISITS',
        'total_visits',
        'total_visits_same_county',
        'total_visits_same_cbg',
        'dist_p75',
      'SMOKE',
        'weeks_smoke',
      'POPULATION',
        'pop',
        'pop_black',
        'pop_hispanic',
        'pop_white',
        'pop_rural',
        'pop_urban',
      'POPULATION SHARES',
        'shr_black',
        'shr_hispanic',
        'shr_white',
        'shr_rural',
        'shr_urban',
      'INCOME',
        'hh_inc'
    ),
    labels = c(
      'POI visit counts',
        'All',
        'Within home county',
        'Within home CBG',
        'Dist traveled (km)',
      'Smoke',
        'Weeks of smoke',
      'Population counts',
        'Total',
        'Black',
        'Hispanic',
        'White',
        'Rural',
        'Urban',
      'Population shares',
        'Black',
        'Hispanic',
        'White',
        'Rural',
        'Urban',
      'Income',
        'Med. HH income'
    ),
    # out = 'viewer',
    summ = c(
      'notNA(x)',
      'fmean(x)',
      'fsd(x)',
      'fmin(x)',
      # 'fnth(x, 0.25)',
      'fmedian(x)',
      # 'fnth(x, 0.75)',
      'fmax(x)'
    ),
    summ.names = c(
      'N obs.',
      'Mean',
      'Stnd. Dev.',
      'Min.',
      # '25th Pctl.',
      'Median',
      # '75th Pctl.',
      'Max.'
    ),
    digits = 1,
    fixed.digits = FALSE
  )


# Summary table: CBG-by-week -------------------------------------------------------------
  st(
    # data = full_dt[pop_urban > pop_rural, .(
    data = full_dt[, .(
      total_visits, visits_same_county, visits_same_cbg, dist_p75,
      any_smoke = any_smoke * 100,
      any_smoke_low = any_smoke_low * 100,
      any_smoke_medium = any_smoke_medium * 100,
      any_smoke_high = any_smoke_high * 100
    )],
    out = 'latex',
    vars = c(
      'POI VISITS',
        'total_visits',
        'visits_same_county',
        'visits_same_cbg',
        'dist_p75',
      'SMOKE',
        'any_smoke',
        'any_smoke_low',
        'any_smoke_medium',
        'any_smoke_high'
    ),
    labels = c(
      'POI visit counts',
        'All',
        'Within home county',
        'Within home CBG',
        'Dist traveled (km)',
      'Smoke',
        'Any smoke',
        'Any low smoke',
        'Any medium smoke',
        'Any high smoke'
    ),
    # out = 'viewer',
    summ = c(
      'notNA(x)',
      'fmean(x)',
      'fsd(x)',
      'fmin(x)',
      # 'fnth(x, 0.25)',
      'fmedian(x)',
      # 'fnth(x, 0.75)',
      'fmax(x)'
    ),
    summ.names = c(
      'N obs.',
      'Mean',
      'Stnd. Dev.',
      'Min.',
      # '25th Pctl.',
      'Median',
      # '75th Pctl.',
      'Max.'
    ),
    digits = 1,
    fixed.digits = FALSE
  )