# Notes ----------------------------------------------------------------------------------
#   Goal:   Estimate heterogeneity in smoke-based migration for socioeconomic quantiles.
#   Time:   ~20 minutes


# Data notes -----------------------------------------------------------------------------
#   - We drop CBGs missing Census data.
#     This choice drops 2.5% of CBGs and 1.5% of the population and visits.


# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(parallel, fastverse, gtools, extrafont, fastDummies, fst, fixest, magrittr, here)
  fastverse_extend(topics = c('DT', 'ST', 'VI'))
  p_load(patchwork)
  # Fix 'extrafont' issue
  # remove.packages('Rttf2pt1')
  # remotes::install_version("Rttf2pt1", version = "1.3.8")
  # Import desired fonts
  # Load fonts
  suppressMessages(
    font_import(
      paths = here('fonts'),
      pattern = 'FiraSansExtra',
      prompt = FALSE
    )
  )
  loadfonts(quiet = TRUE)


# Load data: CSA crosswalk ---------------------------------------------------------------
  # Load the crosswalk
  csa_xwalk =
    here(
      'data-raw', 'census', 'qcew-county-msa-csa-crosswalk.csv'
    ) %>%
    fread() %>%
    janitor::clean_names()
  # Pad county codes
  csa_xwalk[, county_code := county_code %>% str_pad(5, 'left', 0)]
  # Restrict to west-coast states
  csa_xwalk %<>% .[str_sub(county_code, 1, 2) %in% c('06', '41', '53')]


# Load data: West-coast data -------------------------------------------------------------
  # Load the dataset
  full_dt =
    here(
      'data-processed', 'for-analysis', 'for-analysis-westcoast.fst'
      # 'data-processed', 'for-analysis', 'for-analysis-full.fst'
    ) |>
    read_fst(as.data.table = TRUE)


# Load data: CBG fire exposures ----------------------------------------------------------
  # Load the dataset
  fire_dt =
    here(
      'data-processed', 'for-analysis', 'cbg-fire-exposure-westcoast.fst'
    ) |>
    read_fst(as.data.table = TRUE)


# Data work: Quick cleaning --------------------------------------------------------------
  # Create an indicator for fire-affected areas
  full_dt[, fire := cbg_home %in% fire_dt[fire_year %in% 2018:2021, cbg]]
  # Create an indicator for rural (more than 50% rural population)
  full_dt[, i_rural := as.integer(pop_rural / (pop_rural + pop_urban) >= 0.5)]
  # Create week-of-sample and month-of-sample variables
  full_dt[, `:=`(
    wos = paste0(yr, '-', sprintf('%02d', wk)),
    mos = paste0(yr, '-', sprintf('%02d', mo))
  )]
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
  # Indicators for major CSAs
  full_dt[, `:=`(
    i_csa_sf =
      1 * (county %in% csa_xwalk[str_detect(csa_title, 'San Francisco'), county_code]),
    i_csa_la =
      1 * (county %in% csa_xwalk[str_detect(csa_title, 'Los Angeles'), county_code]),
    i_msa_sandiego =
      1 * (county %in% csa_xwalk[str_detect(msa_title, 'San Diego'), county_code]),
    i_csa_sacramento =
      1 * (county %in% csa_xwalk[str_detect(csa_title, 'Sacramento'), county_code]),
    i_csa_seattle =
      1 * (county %in% csa_xwalk[str_detect(csa_title, 'Seattle'), county_code]),
    i_csa_portland =
      1 * (county %in% csa_xwalk[str_detect(csa_title, 'Portland'), county_code])
  )]



# Subset to non-fire-affected CBGs -------------------------------------------------------
# ADJUST Comment out for 'full' sample
# NOTE Filenames to do not reflect this subsetting; files will be overwritten
  # Subset
  full_dt %<>% .[fire == FALSE]
  full_dt[, fire := NULL]


# Function: Het. with many quantile bins -------------------------------------------------
  plot_q = function(
    nq, outcome, het_var, het_label,
    smoke, fe_spec, cl_spec,
    subset_var = NULL, subset = NULL,
    save_fig = TRUE,
    save_parts = FALSE,
    file_type = 'pdf'
  ) {
    # CBG-level dataset for desired variable
    cbg_dt = full_dt[, c('cbg_home', het_var), with = FALSE] %>% funique()
    # Calculate quantiles
    cols = names(cbg_dt)[-1]
    cbg_dt[, (cols) :=
      lapply(X = .SD, quantcut, q = nq, na.rm = TRUE, ordered_result = TRUE),
      .SDcols = cols
    ]
    # Add labels to the quantiles
    for (j in 2:ncol(cbg_dt)) {
      # Find the number of unique values
      nu = fndistinct(cbg_dt[[j]])
      # Recode as quantiles
      set(
        x = cbg_dt,
        j = j,
        value = factor(
          x = cbg_dt[[j]],
          labels = paste0('q', str_pad(1:nu, nchar(nu), 'left', 0)),
          ordered = TRUE
        )
      )
    }
    # Expand quantiles factors to indicators
    cbg_dt %<>% dummy_cols(select_columns = names(cbg_dt)[-1])
    # Fix names so they are not duplicated from full dataset
    setnames(cbg_dt, old = het_var, new = paste0(het_var, '_q'))
    # Merge with full dataset
    q_dt = merge(
      x = full_dt,
      y = cbg_dt,
      by = 'cbg_home',
      all.x = FALSE,
      all.y = FALSE
    )
    # Enforce subsetting (if requested)
    if (!is.null(subset)) {
      q_dt = q_dt[which(q_dt[[subset_var]] %in% subset)]
    }
    # Find the heterogeneity variables (and their number)
    vars_x = cbg_dt %>% names() %>% str_subset(paste0(het_var, '_q[0-9]+'))
    n_x = length(vars_x)
    # Create a formula for the fixed-effects smoke het. regression
    formula_fe = paste0(
      outcome,
      ' ~ ',
      # Het. bins
      paste(vars_x[-median(seq_len(n_x))], collapse = ' + '),
      ' + ',
      # Het. bins interacted with smoke
      paste(paste(vars_x, smoke, sep = ':'), collapse = ' + '),
      ' | ',
      # Fixed effects
      fe_spec
    ) %>%
    as.formula()
    # Create a formula for the non-fixed-effects, non-smoke regression
    formula_migration = paste0(
      outcome,
      ' ~ ',
      '-1 + ',
      # Het. bins
      paste(vars_x, collapse = ' + ')
    ) %>%
    as.formula()
    # Run the FE smoke regression
    est_smoke = feols(
      fml = formula_fe,
      data = q_dt,
      cluster = cl_spec,
      weights = ~pop
    )
    # Estimate migration levels (no fixed effects or smoke)
    est_migration = feols(
      fml = formula_migration,
      data = q_dt,
      cluster = cl_spec,
      weights = ~pop
    )
    # Process results for figure
    est_dt =
      est_smoke %>% broom::tidy(
        conf.int = TRUE
      ) %>%
      tibble::add_row(
        term = vars_x[median(seq_len(n_x))],
        estimate = 0,
        conf.low = 0,
        conf.high = 0
      ) %>%
      setDT()
    est_dt[, `:=`(
      smoke = str_detect(term, 'smoke'),
      q = str_extract(term, '(?<=q)[0-9]+'),
      est = 'fe'
    )]
    # Repeat for migration levels
    mig_dt =
      est_migration %>%
      broom::tidy(conf.int = TRUE) %>%
      setDT()
    mig_dt[, `:=`(
      q = str_extract(term, '(?<=q)[0-9]+'),
      est = 'lvl'
    )]
    # Combine datasets
    est_dt = rbindlist(list(est_dt, mig_dt), use.names = TRUE, fill = TRUE)
    # 'q' to numeric
    est_dt[, `:=`(q = q %>% as.numeric())]
    # Create a percentile from the quantile and the number of quantiles
    est_dt[, `:=`(p = q * (1 / nq))]
    # Make sure we hit 100%
    est_dt[, `:=`(p = p + (1 - max(p)))]
    setorder(est_dt, smoke, q)
    # Define colors
    c1 = viridis::magma(100)[20]
    c2 = viridis::magma(100)[70]
    c3 = viridis::magma(100)[85]
    # Plot 1: Response to smoke
    p1 = ggplot(
      data = est_dt[smoke == TRUE],
      aes(
        x = p,
        y = estimate, ymin = conf.low, ymax = conf.high,
      )
    ) +
    scale_y_continuous(
      name = ifelse(
        str_detect(outcome, pattern = 'dist'),
        'Distance traveled\n(km, 75th percentile)',
        '% visits away from\nhome county when smokey'
      ),
      labels = ifelse(
        str_detect(outcome, pattern = 'dist'),
        scales::comma,
        scales::percent
      )
    ) +
    scale_x_continuous(paste(het_label, 'quantile')) +
    geom_hline(yintercept = 0, size = 0.125) +
    geom_pointrange(color = c2, fatten = 0.7, size = 0.2) +
    geom_ribbon(fill = c2, alpha = 0.1, color = NA) +
    theme_minimal(base_family = 'Fira Sans Extra Condensed', base_size = 8) +
    theme(legend.position = 'none') +
    ggtitle(
      '',
      # paste(
      #   'Differences in mobility and smoke response across',
      #   ifelse(het_var == 'hh_inc', 'income', 'race'),
      #   'quantiles'
      # ),
      subtitle = 'Quantiles\' responses to smoke'
    )
    # Plot 2: Quantile's fixed effects
    p2 = ggplot(
      data = est_dt[smoke == FALSE],
      aes(
        x = p,
        y = estimate, ymin = conf.low, ymax = conf.high,
      )
    ) +
    scale_y_continuous(
      name = ifelse(
        str_detect(outcome, pattern = 'dist'),
        'Distance traveled\n(km, 75th percentile)',
        '% visits away from\nhome county when smokey'
      ),
      labels = ifelse(
        str_detect(outcome, pattern = 'dist'),
        scales::comma,
        scales::percent
      )
    ) +
    scale_x_continuous(paste(het_label, 'quantile')) +
    geom_hline(yintercept = 0, size = 0.125) +
    geom_pointrange(color = c1, fatten = 0.7, size = 0.2) +
    geom_ribbon(fill = c1, alpha = 0.1, color = NA) +
    theme_minimal(base_family = 'Fira Sans Extra Condensed', base_size = 8) +
    theme(legend.position = 'none') +
    ggtitle(
      '',
      subtitle =
        paste(
          het_label,
          'quantiles\' out-of-county travel, relative to median'
        )
    )
    # Plot 3: Levels of migration (estimated without fixed effects)
    p3 = ggplot(
      data = est_dt[is.na(smoke)],
      aes(
        x = p,
        y = estimate, ymin = conf.low, ymax = conf.high,
      )
    ) +
    scale_y_continuous(
      name = ifelse(
        str_detect(outcome, pattern = 'dist'),
        'Distance traveled\n(km, 75th percentile)',
        '% visits away from\nhome county when smokey'
      ),
      labels = ifelse(
        str_detect(outcome, pattern = 'dist'),
        scales::comma,
        scales::percent
      )
    ) +
    scale_x_continuous(paste(het_label, 'quantile')) +
    geom_pointrange(color = c1, fatten = 0.7, size = 0.2) +
    geom_ribbon(fill = c1, alpha = 0.1, color = NA) +
    theme_minimal(base_family = 'Fira Sans Extra Condensed', base_size = 8) +
    theme(legend.position = 'none') +
    ggtitle('', subtitle = paste(het_label, 'quantiles\' out-of-county travel'))
    # Plot 4: Quantile's demographicis
    dem_dt = q_dt[, c(het_var, paste0(het_var, '_q')), with = FALSE]
    setnames(dem_dt, c('val', 'q'))
    dem_dt %<>% .[, .(med_val = fmedian(val)), q]
    dem_dt[, q := as.numeric(str_remove(q, 'q'))]
    dem_dt[, `:=`(p = q * (1 / nq))]
    dem_dt[, `:=`(p = p + (1 - max(p)))]
    p4 = ggplot(
      data = dem_dt,
      aes(x = p, y = med_val)
    ) +
    geom_hline(yintercept = 0, size = 0.125) +
    geom_line(color = c3, size = 1.5, alpha = 0.2) +
    geom_point(color = c3, size = 1) +
    scale_y_continuous(
      het_label,
      labels = ifelse(het_var == 'hh_inc', scales::dollar, scales::percent)
    ) +
    scale_x_continuous(paste(het_label, 'percentile'), labels = percent) +
    theme_minimal(base_family = 'Fira Sans Extra Condensed', base_size = 8) +
    ggtitle('', subtitle = paste('Quantiles\' median', str_to_lower(het_label)))
    # If requested: Save individual parts of the graph
    if (save_parts == TRUE) {
      # Save the datasets required for plotting
      qs::qsave(
        est_dt,
        here(
          'data-figures', 'percentile-graphs',
          paste0(
            str_remove_all(het_var, '[^a-z]'), '-',
            'q:', nq,
            ifelse(
              !is.null(subset),
              paste0(
                '-sub:',
                subset_var %>% str_remove_all('[^a-z]') %>% str_remove('^i'),
                subset
              ),
              ''
            ),
            '-est_dt.qs'
          )
        )
      )
      qs::qsave(
        dem_dt,
        here(
          'data-figures', 'percentile-graphs',
          paste0(
            str_remove_all(het_var, '[^a-z]'), '-',
            'q:', nq,
            ifelse(
              !is.null(subset),
              paste0(
                '-sub:',
                subset_var %>% str_remove_all('[^a-z]') %>% str_remove('^i'),
                subset
              ),
              ''
            ),
            '-dem_dt.qs'
          )
        )
      )
    }
    # Save figure (if requested)
    if (save_fig == TRUE) {
      # Combine figures
      fig = cowplot::plot_grid(
        p1 + theme(axis.text.x = element_blank(), axis.title.x = element_blank()),
        # p2 + theme(axis.text.x = element_blank(), axis.title.x = element_blank()),
        p3 + theme(axis.text.x = element_blank(), axis.title.x = element_blank()),
        p4,
        align = 'v',
        labels = as.roman(1:3) %>% str_to_lower() %>% paste0('.'),
        label_size = 11,
        nrow = 3
      )
      # Save
      the_file = here(
        'figures', 'percentile-graphs',
        paste0(
          fcase(
            outcome == '1 - pct_same_county', 'pctnonco-',
            outcome == 'dist_p75', 'dist75-'
          ),
          str_remove_all(het_var, '[^a-z]'), '-',
          'q:', nq,
          ifelse(
            !is.null(subset),
            paste0(
              '-sub:',
              subset_var %>% str_remove_all('[^a-z]') %>% str_remove('^i'),
              subset
            ),
            ''
          ),
          paste0('.', file_type)
        )
      )
      cowplot::save_plot(
        filename = the_file,
        plot = fig,
        base_width = 3,
        base_height = 7
      )
      # if (file_type == 'pdf') {
      #   embed_fonts(the_file, outfile = str_replace(the_file, '\\.pdf', '-embed\\.pdf'))
      # }
    }
  }


# Income, % out of county ----------------------------------------------------------------
  # Income (All)
  inc_all = plot_q(
    nq = 50,
    outcome = '1 - pct_same_county',
    het_var = 'hh_inc',
    het_label = 'Income',
    smoke = 'any_smoke',
    fe_spec = 'cbg_home + wos + state ^ yr',
    cl_spec = 'county + mos',
    save_parts = FALSE,
    save_fig = TRUE
  )
  # Income (Urban)
  inc_urban = plot_q(
    nq = 50,
    outcome = '1 - pct_same_county',
    het_var = 'hh_inc',
    het_label = 'Income',
    smoke = 'any_smoke',
    fe_spec = 'cbg_home + wos + state ^ yr',
    cl_spec = 'county + mos',
    subset_var = 'i_rural',
    subset = 0,
    save_parts = FALSE,
    save_fig = TRUE
  )
  # Income (Rural)
  inc_rural = plot_q(
    nq = 50,
    outcome = '1 - pct_same_county',
    het_var = 'hh_inc',
    het_label = 'Income',
    smoke = 'any_smoke',
    fe_spec = 'cbg_home + wos + state ^ yr',
    cl_spec = 'county + mos',
    subset_var = 'i_rural',
    subset = 1,
    save_parts = FALSE,
    save_fig = TRUE
  )
  # Income (Los Angeles)
  inc_la = plot_q(
    nq = 50,
    outcome = '1 - pct_same_county',
    het_var = 'hh_inc',
    het_label = 'Income',
    smoke = 'any_smoke',
    fe_spec = 'cbg_home + wos + state ^ yr',
    cl_spec = 'county + mos',
    subset_var = 'i_csa_la',
    subset = 1,
    save_parts = FALSE,
    save_fig = TRUE
  )
  # Income (San Francisco)
  inc_sf = plot_q(
    nq = 50,
    outcome = '1 - pct_same_county',
    het_var = 'hh_inc',
    het_label = 'Income',
    smoke = 'any_smoke',
    fe_spec = 'cbg_home + wos + state ^ yr',
    cl_spec = 'county + mos',
    subset_var = 'i_csa_sf',
    subset = 1,
    save_parts = FALSE,
    save_fig = TRUE
  )


# Share Black, % out of county -----------------------------------------------------------
  # Share Black (All)
  bl_all = plot_q(
    nq = 50,
    outcome = '1 - pct_same_county',
    het_var = 'shr_black',
    het_label = 'Share Black',
    smoke = 'any_smoke',
    fe_spec = 'cbg_home + wos + state ^ yr',
    cl_spec = 'county + mos',
    save_parts = FALSE,
    save_fig = TRUE
  )
  # Share Black (Urban)
  bl_urban = plot_q(
    nq = 50,
    outcome = '1 - pct_same_county',
    het_var = 'shr_black',
    het_label = 'Share Black',
    smoke = 'any_smoke',
    fe_spec = 'cbg_home + wos + state ^ yr',
    cl_spec = 'county + mos',
    subset_var = 'i_rural',
    subset = 0,
    save_parts = FALSE,
    save_fig = TRUE
  )
  # Share Black (LA)
  bl_la = plot_q(
    nq = 50,
    outcome = '1 - pct_same_county',
    het_var = 'shr_black',
    het_label = 'Share Black',
    smoke = 'any_smoke',
    fe_spec = 'cbg_home + wos + state ^ yr',
    cl_spec = 'county + mos',
    subset_var = 'i_csa_la',
    subset = 1,
    save_parts = FALSE,
    save_fig = TRUE
  )
  # Share Black (SF)
  bl_sf = plot_q(
    nq = 50,
    outcome = '1 - pct_same_county',
    het_var = 'shr_black',
    het_label = 'Share Black',
    smoke = 'any_smoke',
    fe_spec = 'cbg_home + wos + state ^ yr',
    cl_spec = 'county + mos',
    subset_var = 'i_csa_sf',
    subset = 1,
    save_parts = FALSE,
    save_fig = TRUE
  )


# Share Hispanic, % out of county --------------------------------------------------------
  # Share Hispanic (All)
  hi_all = plot_q(
    nq = 50,
    outcome = '1 - pct_same_county',
    het_var = 'shr_hispanic',
    het_label = 'Share Hispanic',
    smoke = 'any_smoke',
    fe_spec = 'cbg_home + wos + state ^ yr',
    cl_spec = 'county + mos',
    save_parts = FALSE,
    save_fig = TRUE
  )
  # Share Hispanic (Urban)
  hi_urban = plot_q(
    nq = 50,
    outcome = '1 - pct_same_county',
    het_var = 'shr_hispanic',
    het_label = 'Share Hispanic',
    smoke = 'any_smoke',
    fe_spec = 'cbg_home + wos + state ^ yr',
    cl_spec = 'county + mos',
    subset_var = 'i_rural',
    subset = 0,
    save_parts = FALSE,
    save_fig = TRUE
  )
  # Share Hispanic (Rural)
  hi_rural = plot_q(
    nq = 50,
    outcome = '1 - pct_same_county',
    het_var = 'shr_hispanic',
    het_label = 'Share Hispanic',
    smoke = 'any_smoke',
    fe_spec = 'cbg_home + wos + state ^ yr',
    cl_spec = 'county + mos',
    subset_var = 'i_rural',
    subset = 1,
    save_parts = FALSE,
    save_fig = TRUE
  )
  # Share Hispanic (LA)
  hi_la = plot_q(
    nq = 50,
    outcome = '1 - pct_same_county',
    het_var = 'shr_hispanic',
    het_label = 'Share Hispanic',
    smoke = 'any_smoke',
    fe_spec = 'cbg_home + wos + state ^ yr',
    cl_spec = 'county + mos',
    subset_var = 'i_csa_la',
    subset = 1,
    save_parts = FALSE,
    save_fig = TRUE
  )
  # Share Hispanic (SF)
  hi_sf = plot_q(
    nq = 50,
    outcome = '1 - pct_same_county',
    het_var = 'shr_hispanic',
    het_label = 'Share Hispanic',
    smoke = 'any_smoke',
    fe_spec = 'cbg_home + wos + state ^ yr',
    cl_spec = 'county + mos',
    subset_var = 'i_csa_sf',
    subset = 1,
    save_parts = FALSE,
    save_fig = TRUE
  )


# Share White, % out of county -----------------------------------------------------------
  # Share White (All)
  wh_all = plot_q(
    nq = 50,
    outcome = '1 - pct_same_county',
    het_var = 'shr_white',
    het_label = 'Share White',
    smoke = 'any_smoke',
    fe_spec = 'cbg_home + wos + state ^ yr',
    cl_spec = 'county + mos',
    save_parts = FALSE,
    save_fig = TRUE
  )
  # Share White (Urban)
  wh_urban = plot_q(
    nq = 50,
    outcome = '1 - pct_same_county',
    het_var = 'shr_white',
    het_label = 'Share White',
    smoke = 'any_smoke',
    fe_spec = 'cbg_home + wos + state ^ yr',
    cl_spec = 'county + mos',
    subset_var = 'i_rural',
    subset = 0,
    save_parts = FALSE,
    save_fig = TRUE
  )
  # Share White (Rural)
  wh_rural = plot_q(
    nq = 50,
    outcome = '1 - pct_same_county',
    het_var = 'shr_white',
    het_label = 'Share White',
    smoke = 'any_smoke',
    fe_spec = 'cbg_home + wos + state ^ yr',
    cl_spec = 'county + mos',
    subset_var = 'i_rural',
    subset = 1,
    save_parts = FALSE,
    save_fig = TRUE
  )
  # Share White (LA)
  wh_la = plot_q(
    nq = 50,
    outcome = '1 - pct_same_county',
    het_var = 'shr_white',
    het_label = 'Share White',
    smoke = 'any_smoke',
    fe_spec = 'cbg_home + wos + state ^ yr',
    cl_spec = 'county + mos',
    subset_var = 'i_csa_la',
    subset = 1,
    save_parts = FALSE,
    save_fig = TRUE
  )
  # Share White (SF)
  wh_sf = plot_q(
    nq = 50,
    outcome = '1 - pct_same_county',
    het_var = 'shr_white',
    het_label = 'Share White',
    smoke = 'any_smoke',
    fe_spec = 'cbg_home + wos + state ^ yr',
    cl_spec = 'county + mos',
    subset_var = 'i_csa_sf',
    subset = 1,
    save_parts = FALSE,
    save_fig = TRUE
  )


# Income, distance traveled --------------------------------------------------------------
  # Income (All)
  inc_all = plot_q(
    nq = 50,
    outcome = 'dist_p75',
    het_var = 'hh_inc',
    het_label = 'Income',
    smoke = 'any_smoke',
    fe_spec = 'cbg_home + wos + state ^ yr',
    cl_spec = 'county + mos',
    save_parts = FALSE,
    save_fig = TRUE
  )
  # Income (Urban)
  inc_urban = plot_q(
    nq = 50,
    outcome = 'dist_p75',
    het_var = 'hh_inc',
    het_label = 'Income',
    smoke = 'any_smoke',
    fe_spec = 'cbg_home + wos + state ^ yr',
    cl_spec = 'county + mos',
    subset_var = 'i_rural',
    subset = 0,
    save_parts = FALSE,
    save_fig = TRUE
  )


# Share Black, distance traveled ---------------------------------------------------------
  # Share Black (All)
  bl_all = plot_q(
    nq = 50,
    outcome = 'dist_p75',
    het_var = 'shr_black',
    het_label = 'Share Black',
    smoke = 'any_smoke',
    fe_spec = 'cbg_home + wos + state ^ yr',
    cl_spec = 'county + mos',
    save_parts = FALSE,
    save_fig = TRUE
  )
  # Share Black (Urban)
  bl_urban = plot_q(
    nq = 50,
    outcome = 'dist_p75',
    het_var = 'shr_black',
    het_label = 'Share Black',
    smoke = 'any_smoke',
    fe_spec = 'cbg_home + wos + state ^ yr',
    cl_spec = 'county + mos',
    subset_var = 'i_rural',
    subset = 0,
    save_parts = FALSE,
    save_fig = TRUE
  )


# Share Hispanic, distance traveled ------------------------------------------------------
  # Share Hispanic (All)
  hi_all = plot_q(
    nq = 50,
    outcome = 'dist_p75',
    het_var = 'shr_hispanic',
    het_label = 'Share Hispanic',
    smoke = 'any_smoke',
    fe_spec = 'cbg_home + wos + state ^ yr',
    cl_spec = 'county + mos',
    save_parts = FALSE,
    save_fig = TRUE
  )
  # Share Hispanic (Urban)
  hi_urban = plot_q(
    nq = 50,
    outcome = 'dist_p75',
    het_var = 'shr_hispanic',
    het_label = 'Share Hispanic',
    smoke = 'any_smoke',
    fe_spec = 'cbg_home + wos + state ^ yr',
    cl_spec = 'county + mos',
    subset_var = 'i_rural',
    subset = 0,
    save_parts = FALSE,
    save_fig = TRUE
  )


# Share White, distance traveled ---------------------------------------------------------
  # Share White (All)
  wh_all = plot_q(
    nq = 50,
    outcome = 'dist_p75',
    het_var = 'shr_white',
    het_label = 'Share White',
    smoke = 'any_smoke',
    fe_spec = 'cbg_home + wos + state ^ yr',
    cl_spec = 'county + mos',
    save_parts = FALSE,
    save_fig = TRUE
  )
  # Share White (Urban)
  wh_urban = plot_q(
    nq = 50,
    outcome = 'dist_p75',
    het_var = 'shr_white',
    het_label = 'Share White',
    smoke = 'any_smoke',
    fe_spec = 'cbg_home + wos + state ^ yr',
    cl_spec = 'county + mos',
    subset_var = 'i_rural',
    subset = 0,
    save_parts = FALSE,
    save_fig = TRUE
  )
