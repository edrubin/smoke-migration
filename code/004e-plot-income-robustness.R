# Notes ----------------------------------------------------------------------------------
#   Goal:   Estimate and plot the robustness of the main income heterogeneity results
#           with respect to interacting fixed effects with income bins.
#   Time:   < 2 minutes


# Load and prep data ---------------------------------------------------------------------
  # Source the script that loads and prepares the data
  here::here('code', '002b-prep-analysis-data.R') |> source()
  # Drop rural areas (focusing on urban)
  full_dt %<>% .[i_rural == 0]


# Estimate model -------------------------------------------------------------------------
  # Estimate main heterogeneity model with indicators interacted with FEs
  est_inc_q =
    feols(
      c(100 * (1 - pct_same_county), dist_p75) ~
      any_smoke:hh_inc_q |
      sw(
        cbg_home + wos + state ^ yr,
        cbg_home + wos + state ^ yr + mo ^ hh_inc_q,
        cbg_home + wos + state ^ yr + wk ^ hh_inc_q
      ),
      data = full_dt[fire == FALSE],
      lean = TRUE,
      cluster = ~county + mos,
      weights = ~pop
    )

  # Grab data for figure
  est_dt =
    lapply(
      X = seq_along(est_inc_q),
      FUN = function(i) {
        est_inc_q[[i]] |>
          broom::tidy(conf.int = TRUE) |>
          fmutate(
            outcome = est_inc_q[[i]]$model_info$lhs,
            fixef = est_inc_q[[i]]$model_info$fixef
          )
      }
    ) |> rbindlist(use.names = TRUE, fill = TRUE)
  est_dt[, c('std.error', 'statistic', 'p.value') := NULL]
  # Clean up outcome labels
  est_dt[, outcome := ifelse(str_detect(outcome, 'dist'), 'dist75', 'pctout')]
  # Clean up fixed effects
  est_dt[, fixef := str_replace(fixef, 'cbg_home', 'CBG')]
  est_dt[, fixef := str_replace(fixef, 'wos', 'W.o.S.')]
  est_dt[, fixef := str_replace(fixef, '\\^', ' × ')]
  est_dt[, fixef := str_replace(fixef, 'state', 'St.')]
  est_dt[, fixef := str_replace(fixef, 'yr', 'Yr.')]
  est_dt[, fixef := str_replace(fixef, 'mo', 'Mo.')]
  est_dt[, fixef := str_replace(fixef, 'wk', 'Wk.')]
  est_dt[, fixef := str_replace(fixef, 'hh_inc_q', 'Inc. Grp.')]
  # Grab bin numbers
  est_dt[, bin := str_extract(term, '\\d+') |> as.numeric()]
  # Add number for each fixef
  est_dt[, fixef := factor(fixef, ordered = TRUE)]
  est_dt[, fixef_num := as.numeric(fixef)]
  # Create 'x' variable and shift the bins slightly by fixed-effect group
  est_dt[, x := bin * 5 - 2.5]
  est_dt[fixef_num == 2, x := x - .75]
  est_dt[fixef_num == 3, x := x + .75]


# Plot the results -----------------------------------------------------------------------
  # Plot the results for income
  fig_pct =
    ggplot(
      data = est_dt[outcome == 'pctout'],
      aes(
        x = x / 100,
        y = estimate / 100,
        ymin = conf.low / 100,
        ymax = conf.high / 100,
        color = as.factor(fixef),
        fill = as.factor(fixef)
      )
    ) +
    scale_y_continuous(
      '% visits away from\nhome county when smokey',
      labels =  scales::percent
    ) +
    scale_x_continuous(
      'Income percentile (vintile)',
      labels = scales::percent
    ) +
    geom_hline(yintercept = 0, size = 0.125) +
    geom_pointrange(fatten = 0.7, size = 0.2) +
    geom_ribbon(alpha = 0.1, color = NA) +
    theme_minimal(base_family = 'Fira Sans Extra Condensed', base_size = 8) +
    scale_color_manual(
      'Fixed effects',
      values = viridis::magma(100)[c(20, 50, 80)],
      labels = c('\"Stnd.\"', 'Stnd. + Inc. × Mo.', 'Stnd. + Inc. × Wk.')
    ) +
    scale_fill_manual(
      'Fixed effects',
      values = viridis::magma(100)[c(20, 50, 80)],
      labels = c('\"Stnd.\"', 'Stnd. + Inc. × Mo.', 'Stnd. + Inc. × Wk.')
    ) +
    theme(legend.position = 'none')
  # Save
  cowplot::save_plot(
    filename = here('figures', 'robust-fe-pct-inc.pdf'),
    device = grDevices::cairo_pdf,
    plot = fig_pct,
    base_width = 6,
    base_height = 3.5
  )
  cowplot::save_plot(
    filename = here('figures', 'robust-fe-pct-inc.png'),
    plot = fig_pct,
    base_width = 6,
    base_height = 3.5
  )
  # Plot the results for distance
  fig_dist =
    ggplot(
      data = est_dt[outcome != 'pctout'],
      aes(
        x = x / 100,
        y = estimate,
        ymin = conf.low,
        ymax = conf.high,
        color = as.factor(fixef),
        fill = as.factor(fixef)
      )
    ) +
    scale_y_continuous(
      'Distance traveled\n(km, 75th percentile)',
      labels =  scales::comma
    ) +
    scale_x_continuous(
      'Income percentile (vintile)',
      labels = scales::percent
    ) +
    geom_hline(yintercept = 0, size = 0.125) +
    geom_pointrange(fatten = 0.7, size = 0.2) +
    geom_ribbon(alpha = 0.1, color = NA) +
    theme_minimal(base_family = 'Fira Sans Extra Condensed', base_size = 8) +
    scale_color_manual(
      'Fixed effects',
      values = viridis::magma(100)[c(20, 50, 80)],
      labels = c('\"Stnd.\"', 'Stnd. + Inc. × Mo.', 'Stnd. + Inc. × Wk.')
    ) +
    scale_fill_manual(
      'Fixed effects',
      values = viridis::magma(100)[c(20, 50, 80)],
      labels = c('\"Stnd.\"', 'Stnd. + Inc. × Mo.', 'Stnd. + Inc. × Wk.')
    ) +
    theme(legend.position = 'none')
  # Save
  cowplot::save_plot(
    filename = here('figures', 'robust-fe-dist-inc.pdf'),
    device = grDevices::cairo_pdf,
    plot = fig_dist,
    base_width = 6,
    base_height = 3.5
  )
  cowplot::save_plot(
    filename = here('figures', 'robust-fe-dist-inc.png'),
    device = 'png',
    plot = fig_dist,
    base_width = 6,
    base_height = 3.5
  )
