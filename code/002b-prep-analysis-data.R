# Notes ----------------------------------------------------------------------------------
#   Goal:   Prepare data for analysis. Several scripts use these prepped datasets.
#   Note:   Does not perform any analyses.
#   Time:   ~ 28 sec


# Data notes -----------------------------------------------------------------------------
#   - We drop CBGs missing Census data.
#     This choice drops 2.5% of CBGs and 1.5% of the population and visits.


# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(
    parallel, fastverse, fst, gtools, fastDummies, fixest, qs,
    cowplot, extrafont, magrittr, here
  )
  fastverse_extend(topics = c('DT', 'ST', 'VI'))
  # p_load(specr, patchwork)
  p_load(patchwork)
  # Load fonts
  suppressMessages(
    font_import(paths = '~/.fonts', pattern = 'FiraSansExtra', prompt = FALSE)
  )
  loadfonts(quiet = TRUE)
  # Add directory of SafeGraph data
  dir_sg = '/media/edwardrubin/Data/SafeGraph'


# Load data: West Coast data -------------------------------------------------------------
  # Load the dataset
  full_dt = here(
    'data-processed', 'for-analysis', 'for-analysis-westcoast.fst'
  ) %>% read_fst(as.data.table = TRUE)


# Load data: CBG fire exposures ----------------------------------------------------------
  # Load the dataset
  fire_dt = here(
    'data-processed', 'for-analysis', 'cbg-fire-exposure-westcoast.fst'
  ) %>% read_fst(as.data.table = TRUE)


# Data work: Quick cleaning --------------------------------------------------------------
  # Create an indicator for rural (more than 50% rural population)
  full_dt[, i_rural := as.integer(pop_rural / (pop_rural + pop_urban) >= 0.5)]
  # CBG-level dataset
  cbg_dt = full_dt[, .(
    pop = fmedian(pop),
    i_rural = fmax(i_rural),
    shr_black = fmedian(shr_black),
    shr_hispanic = fmedian(shr_hispanic),
    shr_white = fmedian(shr_white),
    shr_employed = fmedian(shr_employed),
    shr_unemployed = fmedian(shr_unemployed),
    hh_inc = fmedian(hh_inc)
  ), by = cbg_home]
  # Drop CBGs missing Census data
  cbg_dt %<>% na.omit()
  full_dt %<>% .[cbg_home %in% cbg_dt[, cbg_home]]


# Data work: Calculate quantiles ---------------------------------------------------------
  # CBG-level dataset
  cbg_dt = full_dt[, .(
    pop = fmedian(pop),
    shr_black = fmedian(shr_black),
    shr_hispanic = fmedian(shr_hispanic),
    shr_white = fmedian(shr_white),
    shr_employed = fmedian(shr_employed),
    shr_unemployed = fmedian(shr_unemployed),
    hh_inc = fmedian(hh_inc)
  ), by = cbg_home]
  # Drop CBGs missing Census data
  cbg_dt %<>% na.omit()
  # Calculate quantiles
  nq = 20
  lab = paste0('q', str_pad(1:nq, nchar(nq), 'left', '0'))
  cols = names(cbg_dt)[-1]
  cbg_dt[, (cols) :=
    lapply(X = .SD, quantcut, q = nq, na.rm = TRUE, ordered_result = TRUE),
    .SDcols = cols
  ]
  for (j in 2:ncol(cbg_dt)) {
    # Find the number of unique values
    nu = fndistinct(cbg_dt[[j]])
    # Recode as quantiles
    set(
      x = cbg_dt,
      j = j,
      value = factor(
        cbg_dt[[j]],
        labels = paste0('q', str_pad(1:nu, nchar(nu), 'left', 0))
      )
    )
  }
  # Expand quantiles factors to indicators
  cbg_dt %<>% dummy_cols(select_columns = names(cbg_dt)[-1])
  # Fix names so they are not duplicated from full dataset
  setnames(cbg_dt, old = names(cbg_dt)[2:8], new = paste0(names(cbg_dt)[2:8], '_q'))
  # Merge with full dataset
  full_dt %<>% merge(
    y = cbg_dt,
    by = 'cbg_home',
    all.x = FALSE,
    all.y = FALSE
  )
  # CBG-level dataset
  cbg_dt = full_dt[, .(
    pop = fmedian(pop),
    shr_black = fmedian(shr_black),
    shr_hispanic = fmedian(shr_hispanic),
    shr_white = fmedian(shr_white),
    shr_employed = fmedian(shr_employed),
    shr_unemployed = fmedian(shr_unemployed),
    hh_inc = fmedian(hh_inc)
  ), by = cbg_home]
  # Drop CBGs missing Census data
  cbg_dt %<>% na.omit()
  # Calculate percentile (first find rank; then divide by N)
  cbg_dt[, `:=`(
    shr_black_p = frankv(shr_black, ties = 'first', na.last = 'keep'),
    shr_hispanic_p = frankv(shr_hispanic, ties = 'first', na.last = 'keep'),
    shr_white_p = frankv(shr_white, ties = 'first', na.last = 'keep'),
    hh_inc_p = frankv(hh_inc, ties = 'first', na.last = 'keep')
  )]
  cbg_dt[, `:=`(
    shr_black_p = shr_black_p / fmax(shr_black_p),
    shr_hispanic_p = shr_hispanic_p / fmax(shr_hispanic_p),
    shr_white_p = shr_white_p / fmax(shr_white_p),
    hh_inc_p = hh_inc_p / fmax(hh_inc_p)
  )]
  # Merge again!
  full_dt %<>% merge(
    y = cbg_dt[, .(cbg_home, shr_black_p, shr_hispanic_p, shr_white_p, hh_inc_p)],
    by = 'cbg_home',
    all.x = FALSE,
    all.y = FALSE
  )
