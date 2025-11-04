# Notes ----------------------------------------------------------------------------------
#   Goal:   Make plots showing how various CBG characteristics vary by income


# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(fastverse, patchwork, extrafont, fixest, here)
  fastverse_extend(topics = c('ST', 'VI'))
  # Load desired fonts
  # remotes::install_version('Rttf2pt1', version = '1.3.8')
  # font_import(
  #   paths = '/home/edwardrubin/.local/share/fonts/',
  #   pattern = 'Fira'
  # )
  # font_import(
  #   paths = '/Users/edwardarubin/Library/Fonts/skyfonts-google',
  #   pattern = 'Fira'
  # )


# Load data: Main analysis file ----------------------------------------------------------
  # Load the dataset
  full_dt = here(
    'data-processed', 'for-analysis', 'for-analysis-westcoast.fst'
    # 'data-processed', 'for-analysis', 'for-analysis-full.fst'
  ) %>% read_fst(as.data.table = TRUE)


# Data work: Collapse to CBG -------------------------------------------------------------
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
  # Drop full dataset
  rm(full_dt)
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


# Load data: CBG descriptions ------------------------------------------------------------
  # Load the raw data
  desc_dt = here(
    'data-raw', 'census', 'cbg-descr', 'cbg-descr.csv'
  ) %>% fread()


# Data work: Code NHGIS file -------------------------------------------------------------
  # Code data
  desc_dt %<>% .[, .(
    # CBG code to match other data
    cbg_home = paste0(
      str_pad(STATEA, 2, 'left', 0),
      str_pad(COUNTYA, 3, 'left', 0),
      str_pad(TRACTA, 6, 'left', 0),
      str_pad(BLKGRPA, 1, 'left', 0)
    ),
    # Education: Pct high school graduates (and higher)
    educ_pct_grad_hs = (ALWGE017 + ALWGE018 + ALWGE019 + ALWGE020 +
      ALWGE021 + ALWGE022 + ALWGE023 + ALWGE024 + ALWGE025) / ALWGE001,
    # Education: Pct college graduates (and higher)
    educ_pct_grad_co = (ALWGE021 + ALWGE022 + ALWGE023 + ALWGE024 + ALWGE025) / ALWGE001,
    # Health: Pct of households with at least one person who has a disability
    health_pct_disability = (ALYWE003 + ALYWE006) / ALYWE001,
    # Health: Pct uninsured
    health_pct_uninsured = (AL1WE017 + AL1WE033 + AL1WE050 + AL1WE066) / AL1WE001,
    # Housing: Pct of housing that is renter occupied
    housing_pct_renter = ALZLE003 / ALZLE001,
    # Housing: Pct mobile home
    housing_pct_mblhome = AL0AE010 / AL0AE001,
    # Housing: Weighted age
    housing_avg_age =
      AL0DE002 / AL0DE001 * (2015 - 2014) +
      AL0DE003 / AL0DE001 * (2015 - 2010) +
      AL0DE004 / AL0DE001 * (2015 - 2000) +
      AL0DE005 / AL0DE001 * (2015 - 1990) +
      AL0DE006 / AL0DE001 * (2015 - 1980) +
      AL0DE007 / AL0DE001 * (2015 - 1970) +
      AL0DE008 / AL0DE001 * (2015 - 1960) +
      AL0DE009 / AL0DE001 * (2015 - 1950) +
      AL0DE010 / AL0DE001 * (2015 - 1940) +
      AL0DE011 / AL0DE001 * (2015 - 1939),
    # Housing: Pct no internet
    housing_pct_nointernet = AL2FE008 / AL2FE001,
    # Transport: Pct no vehicle
    transport_pct_novehicle = (AL0NE003 + AL0NE010) / AL0NE001,
    # Heating: Pct no heat
    heat_pct_none = AL0JE010 / AL0JE001,
    # Heating: Pct natural gas
    heat_pct_gas = AL0JE002 / AL0JE001,
    # Heating: Pct electricity (from utility)
    heat_pct_electricity = AL0JE004 / AL0JE001
  )]


# Merge data -----------------------------------------------------------------------------
  # Merge!
  cbg_dt %<>% merge(
    y = desc_dt,
    by = 'cbg_home',
    all.x = TRUE,
    all.y = FALSE
  )


# Regressions ----------------------------------------------------------------------------
  # Income percentile
  feols(
    c(
      shr_unemployed, educ_pct_grad_hs, educ_pct_grad_co,
      health_pct_disability, health_pct_uninsured,
      housing_avg_age, housing_pct_renter, housing_pct_mblhome, housing_pct_nointernet,
      transport_pct_novehicle,
      heat_pct_none, heat_pct_gas, heat_pct_electricity
    ) ~ hh_inc_p,
    data = cbg_dt[i_rural == 0]
  ) %>% etable()


# Data work: Create percentile datasets --------------------------------------------------
  # Add percentile groups
  cbg_dt[, `:=`(
    grp_inc = round(hh_inc_p * 50, 0) / 50,
    grp_black = round(shr_black_p * 50, 0) / 50,
    grp_hispanic = round(shr_hispanic_p * 50, 0) / 50,
    grp_white = round(shr_white_p * 50, 0) / 50
  )]
  # Summarize by percentile groups
  inc_dt = cbg_dt[i_rural == 0] %>% collap(
    FUN = list(
      avg = function(x) fmean(x),
      q1 = function(x) fnth(x, n = 0.250),
      q2 = function(x) fnth(x, n = 0.500),
      q3 = function(x) fnth(x, n = 0.750),
      p10 = function(x) fnth(x, n = 0.100),
      p90 = function(x) fnth(x, n = 0.900)
    ),
    by = ~grp_inc,
    cols = 4:25
  )
  black_dt = cbg_dt[i_rural == 0] %>% collap(
    FUN = list(
      avg = function(x) fmean(x),
      q1 = function(x) fnth(x, n = 0.250),
      q2 = function(x) fnth(x, n = 0.500),
      q3 = function(x) fnth(x, n = 0.750),
      p10 = function(x) fnth(x, n = 0.100),
      p90 = function(x) fnth(x, n = 0.900)
    ),
    by = ~grp_black,
    cols = 4:25
  )
  hispanic_dt = cbg_dt[i_rural == 0] %>% collap(
    FUN = list(
      avg = function(x) fmean(x),
      q1 = function(x) fnth(x, n = 0.250),
      q2 = function(x) fnth(x, n = 0.500),
      q3 = function(x) fnth(x, n = 0.750),
      p10 = function(x) fnth(x, n = 0.100),
      p90 = function(x) fnth(x, n = 0.900)
    ),
    by = ~grp_hispanic,
    cols = 4:25
  )
  white_dt = cbg_dt[i_rural == 0] %>% collap(
    FUN = list(
      avg = function(x) fmean(x),
      q1 = function(x) fnth(x, n = 0.250),
      q2 = function(x) fnth(x, n = 0.500),
      q3 = function(x) fnth(x, n = 0.750),
      p10 = function(x) fnth(x, n = 0.100),
      p90 = function(x) fnth(x, n = 0.900)
    ),
    by = ~grp_white,
    cols = 4:25
  )


# Define variables, labels, and colors for plots -----------------------------------------
  # Choose variables
  target_vars = c(
    'educ_pct_grad_co',
    'health_pct_disability',
    'health_pct_uninsured',
    'housing_pct_renter',
    'housing_pct_nointernet',
    'transport_pct_novehicle',
    'shr_black',
    'shr_hispanic',
    'shr_white',
    'hh_inc'
  )
  # Define labels
  labels = c(
    '% Pop. w/ coll. degrees',
    '% HHs w/ disability',
    '% Pop. uninsured',
    '% Rentals',
    '% HHs w/out internet',
    '% Pop. w/out a vehicle',
    '% Pop. Black',
    '% Pop. Hispanic',
    '% Pop. White',
    'Median HH income'
  )
  # Define vector of colors for plots
  cols = viridis::magma(100)[seq(1, 85, length.out = length(target_vars)) %>% round(0)]


# Figure: Income -------------------------------------------------------------------------
  plot_v = lapply(
    X = target_vars,
    FUN = function(v) {
      # Find the color
      vc = cols[which(target_vars == v)]
      # Find y-axis label
      yl = labels[which(target_vars == v)]
      # Plot!
      ggplot(
        data = inc_dt,
        aes_string(x = 'grp_inc', y = paste0('q2.', v)),
      ) +
      geom_ribbon(
        aes_string(ymin = paste0('p10.', v), ymax = paste0('p90.', v)),
        alpha = 0.08,
        fill = vc
      ) +
      geom_ribbon(
        aes_string(ymin = paste0('q1.', v), ymax = paste0('q3.', v)),
        alpha = 0.09,
        fill = vc
      ) +
      geom_point(
        color = vc,
        size = 1.5
      ) +
      geom_hline(yintercept = 0) +
      scale_x_continuous(
        '\nIncome percentile (CBG median HH)',
        labels = percent
      ) +
      scale_y_continuous(
        '',
        labels = percent
      ) +
      ggtitle('', yl) +
      theme_minimal(base_family = 'Fira Sans Extra Condensed', base_size = 9) +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_text(margin = margin(0, -3, 0, 0, 'pt')),
        plot.margin = margin(0, 1, 0, 0, 'pt')
      )
    }
  )
  # Fix income plot
  if ('hh_inc' %in% target_vars) {
    # Find it
    i = which(target_vars == 'hh_inc')
    # Fix the labels (change from percent to thousands of dollars)
    plot_v[[i]] =
      plot_v[[i]] +
      scale_y_continuous('', labels = . %>% dollar(scale = 1e-3, suffix = 'K'))
  }
  # Remove unwanted x-axis labels
  for (i in 1:5) {
    plot_v[[i]] =
      plot_v[[i]] +
      theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  }
  for (i in c(6:7, 9:10)) {
    plot_v[[i]] =
      plot_v[[i]] +
      theme(axis.title.x = element_blank())
  }
  # Save the joint
  ggsave(
    plot = wrap_plots(plot_v, ncol = 5),
    filename = here('figures', 'dimensions', 'dimensions-by-income.pdf'),
    width = 9,
    height = 5,
    device = cairo_pdf
  )
  embed_fonts(
    file = here('figures', 'dimensions', 'dimensions-by-income.pdf'),
    outfile = here('figures', 'dimensions', 'dimensions-by-income.pdf')
  )


# Figure: Share Black --------------------------------------------------------------------
  # Iterate over variables
  plot_v = lapply(
    X = target_vars,
    FUN = function(v) {
      # Find the color
      vc = cols[which(target_vars == v)]
      # Find y-axis label
      yl = labels[which(target_vars == v)]
      # Plot!
      ggplot(
        data = black_dt,
        aes_string(x = 'grp_black', y = paste0('q2.', v)),
      ) +
      geom_ribbon(
        aes_string(ymin = paste0('p10.', v), ymax = paste0('p90.', v)),
        alpha = 0.08,
        fill = vc
      ) +
      geom_ribbon(
        aes_string(ymin = paste0('q1.', v), ymax = paste0('q3.', v)),
        alpha = 0.09,
        fill = vc
      ) +
      geom_point(
        color = vc,
        size = 1.5
      ) +
      geom_hline(yintercept = 0) +
      scale_x_continuous(
        '\nShare Black percentile',
        labels = percent
      ) +
      scale_y_continuous(
        '',
        labels = percent
      ) +
      ggtitle('', yl) +
      theme_minimal(base_family = 'Fira Sans Extra Condensed', base_size = 9) +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_text(margin = margin(0, -3, 0, 0, 'pt')),
        plot.margin = margin(0, 1, 0, 0, 'pt')
      )
    }
  )
  # Fix income plot
  if ('hh_inc' %in% target_vars) {
    # Find it
    i = which(target_vars == 'hh_inc')
    # Fix the labels (change from percent to thousands of dollars)
    plot_v[[i]] =
      plot_v[[i]] +
      scale_y_continuous('', labels = . %>% dollar(scale = 1e-3, suffix = 'K'))
  }
  # Remove unwanted x-axis labels
  for (i in 1:5) {
    plot_v[[i]] =
      plot_v[[i]] +
      theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  }
  for (i in c(6:7, 9:10)) {
    plot_v[[i]] = plot_v[[i]] + theme(axis.title.x = element_blank())
  }
  # Save the joint
  ggsave(
    plot = wrap_plots(plot_v, ncol = 5),
    filename = here('figures', 'dimensions', 'dimensions-by-black.pdf'),
    width = 9,
    height = 5,
    device = cairo_pdf
  )
  embed_fonts(
    file = here('figures', 'dimensions', 'dimensions-by-black.pdf'),
    outfile = here('figures', 'dimensions', 'dimensions-by-black.pdf')
  )


# Figure: Share Hispanic -----------------------------------------------------------------
  # Iterate over variables
  plot_v = lapply(
    X = target_vars,
    FUN = function(v) {
      # Find the color
      vc = cols[which(target_vars == v)]
      # Find y-axis label
      yl = labels[which(target_vars == v)]
      # Plot!
      ggplot(
        data = hispanic_dt,
        aes_string(x = 'grp_hispanic', y = paste0('q2.', v)),
      ) +
      geom_ribbon(
        aes_string(ymin = paste0('p10.', v), ymax = paste0('p90.', v)),
        alpha = 0.08,
        fill = vc
      ) +
      geom_ribbon(
        aes_string(ymin = paste0('q1.', v), ymax = paste0('q3.', v)),
        alpha = 0.09,
        fill = vc
      ) +
      geom_point(
        color = vc,
        size = 1.5
      ) +
      geom_hline(yintercept = 0) +
      scale_x_continuous(
        '\nShare Hispanic percentile',
        labels = percent
      ) +
      scale_y_continuous(
        '',
        labels = percent
      ) +
      ggtitle('', yl) +
      theme_minimal(base_family = 'Fira Sans Extra Condensed', base_size = 9) +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_text(margin = margin(0, -3, 0, 0, 'pt')),
        plot.margin = margin(0, 1, 0, 0, 'pt')
      )
    }
  )
  # Fix income plot
  if ('hh_inc' %in% target_vars) {
    # Find it
    i = which(target_vars == 'hh_inc')
    # Fix the labels (change from percent to thousands of dollars)
    plot_v[[i]] =
      plot_v[[i]] +
      scale_y_continuous('', labels = . %>% dollar(scale = 1e-3, suffix = 'K'))
  }
  # Remove unwanted x-axis labels
  for (i in 1:5) {
    plot_v[[i]] =
      plot_v[[i]] +
      theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  }
  for (i in c(6:7, 9:10)) {
    plot_v[[i]] = plot_v[[i]] + theme(axis.title.x = element_blank())
  }
  # Save the joint
  ggsave(
    plot = wrap_plots(plot_v, ncol = 5),
    filename = here('figures', 'dimensions', 'dimensions-by-hispanic.pdf'),
    width = 9,
    height = 5,
    device = cairo_pdf
  )
  embed_fonts(
    file = here('figures', 'dimensions', 'dimensions-by-hispanic.pdf'),
    outfile = here('figures', 'dimensions', 'dimensions-by-hispanic.pdf')
  )


# Figure: Share White --------------------------------------------------------------------
  # Iterate over variables
  plot_v = lapply(
    X = target_vars,
    FUN = function(v) {
      # Find the color
      vc = cols[which(target_vars == v)]
      # Find y-axis label
      yl = labels[which(target_vars == v)]
      # Plot!
      ggplot(
        data = white_dt,
        aes_string(x = 'grp_white', y = paste0('q2.', v)),
      ) +
      geom_ribbon(
        aes_string(ymin = paste0('p10.', v), ymax = paste0('p90.', v)),
        alpha = 0.08,
        fill = vc
      ) +
      geom_ribbon(
        aes_string(ymin = paste0('q1.', v), ymax = paste0('q3.', v)),
        alpha = 0.09,
        fill = vc
      ) +
      geom_point(
        color = vc,
        size = 1.5
      ) +
      geom_hline(yintercept = 0) +
      scale_x_continuous(
        '\nShare White percentile',
        labels = percent
      ) +
      scale_y_continuous(
        '',
        labels = percent
      ) +
      ggtitle('', yl) +
      theme_minimal(base_family = 'Fira Sans Extra Condensed', base_size = 9) +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_text(margin = margin(0, -3, 0, 0, 'pt')),
        plot.margin = margin(0, 1, 0, 0, 'pt')
      )
    }
  )
  # Fix income plot
  if ('hh_inc' %in% target_vars) {
    # Find it
    i = which(target_vars == 'hh_inc')
    # Fix the labels (change from percent to thousands of dollars)
    plot_v[[i]] =
      plot_v[[i]] +
      scale_y_continuous('', labels = . %>% dollar(scale = 1e-3, suffix = 'K'))
  }
  # Remove unwanted x-axis labels
  for (i in 1:5) {
    plot_v[[i]] =
      plot_v[[i]] +
      theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  }
  for (i in c(6:7, 9:10)) {
    plot_v[[i]] = plot_v[[i]] + theme(axis.title.x = element_blank())
  }
  # Save the joint
  ggsave(
    plot = wrap_plots(plot_v, ncol = 5),
    filename = here('figures', 'dimensions', 'dimensions-by-white.pdf'),
    width = 9,
    height = 5,
    device = cairo_pdf
  )
  embed_fonts(
    file = here('figures', 'dimensions', 'dimensions-by-white.pdf'),
    outfile = here('figures', 'dimensions', 'dimensions-by-white.pdf')
  )