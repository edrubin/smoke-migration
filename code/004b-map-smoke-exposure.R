# Notes ----------------------------------------------------------------------------------
#   Goal:   Create a map (shp, hist, legend) of West Coast CBGs' smoke exposures


# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(parallel, fastverse, here, cowplot)
  fastverse_extend(topics = c('DT', 'ST', 'SP', 'VI'))
  extrafont::loadfonts(quiet = TRUE)
  # Add directory of SafeGraph data
  dir_sg = '/media/edwardrubin/Data/SafeGraph'


# Load data: West Coast data -------------------------------------------------------------
  # Load the dataset
  smoke_panel = here(
    'data-processed', 'for-analysis', 'cbg-visit-smoke-week.fst'
  ) %>% read_fst(
    columns = c(
      'cbg_home', 'date_start',
      'n_smoke_files', 'smoke_low_home', 'smoke_medium_home', 'smoke_high_home'
    ),
    as.data.table = TRUE
  )


# Data work: Weekly CBG summaries --------------------------------------------------------
  # Determine if the CBG received any smoke in the given week
  smoke_panel[, `:=`(
    any_smoke = as.numeric(smoke_low_home + smoke_medium_home + smoke_high_home > 0)
  )]
  # Find the number of observations for each CBG
  smoke_panel[, n_weeks := .N, cbg_home]
  # Collapse smoke exposure to CBG level cross-sectional dataset
  cbg_smoke = smoke_panel[, .(
    wks_obs = ffirst(n_weeks),
    wks_smoke = fsum(any_smoke)
  ), by = .(cbg = cbg_home)]
  # Add column with NAs for CBGs missing more than 10 weeks
  cbg_smoke[, wks_smoke_na := wks_smoke]
  cbg_smoke[wks_obs < 209 - 209 * 0.05, wks_smoke_na := NA]
  # Add the state (helpful for later merge)
  cbg_smoke[, state := str_sub(cbg, 1, 2)]


# Load data: Census CBG shapefile --------------------------------------------------------
  # Load the 2016 CBG shapefiles from SG (for West Coast)
  cbg_shp = file.path(
    dir_sg, 'data-other', 'census', 'US_blck_grp_2016',
    'US_blck_grp_2016.shp'
  ) %>% st_read(
    query = 'SELECT GEOID FROM "US_blck_grp_2016" WHERE STATEFP IN (\'06\', \'41\', \'53\')'
  )


# Data work: Merge CBG shapefile with CBG smoke data -------------------------------------
  # Convert shapefile to data table
  setDT(cbg_shp)
  setnames(cbg_shp, old = 'GEOID', new = 'cbg')
  # Key both datasets
  setkey(cbg_shp, cbg)
  setkey(cbg_smoke, cbg)
  # Merge
  cbg_shp %<>% merge(
    y = cbg_smoke[state %in% c('06', '41', '53')],
    by = 'cbg',
    all.x = TRUE,
    all.y = FALSE
  )
  # Drop 'state'
  cbg_shp[, state := NULL]
  # Convert shapefile back to spatial
  cbg_shp %<>% st_as_sf()


# Save data ------------------------------------------------------------------------------
  # Save the new West Coast CBG shapefile
  st_write(
    cbg_shp,
    here('data-processed', 'cbg-smoke-west-shp', 'cbg-smoke-west-shp.shp'),
    delete_layer = TRUE
  )


# Figure: Histogram of CBG exposure ------------------------------------------------------
  # Grab the data table
  cbg_dt = cbg_shp %T>% setDT()
  # Make the histogram
  h = ggplot(
    data = cbg_dt,
    aes(x = wks_smoke, fill = wks_smoke)
  ) +
  geom_histogram(
    color = NA,
    size = 0.2,
    bins = 131,
    fill = viridis::magma(131)
  ) +
  geom_hline(yintercept = 0, size = 0.1) +
  scale_x_continuous('Weeks exposed to wildfire smoke, 2018-2021', labels = comma) +
  scale_y_continuous('Number of CBGs', labels = comma) +
  theme_minimal(base_family = 'Fira Sans Extra Condensed', base_size = 6.5) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )
  # Save the histogram
  save_plot(
    # filename = here('figures', 'cbg-smoke-hist.png'),
    filename = here('figures', 'cbg-smoke-hist.pdf'),
    plot = h + coord_cartesian(xlim = c(40, 100)),
    base_height = 1.75,
    base_asp = 1.518,
    dpi = 450
  )


# Figure: Legend for CBG exposure --------------------------------------------------------
  # Grab the data table
  legend_dt = cbg_dt[, .(
    y = 1,
    x = seq(fmin(wks_smoke), fmax(wks_smoke), by = 0.1)
  )]
  # Create the gradient
  h2 = ggplot(
    data = legend_dt,
    aes(x = x, y = y, fill = x)
  ) +
  geom_raster() +
  scale_fill_viridis_c(option = 'magma') +
  theme_void(base_family = 'Fira Sans Extra Condensed', base_size = 6.5) +
  theme(legend.position = 'none')
  # Save
  save_plot(
    # filename = here('figures', 'cbg-smoke-legend.png'),
    filename = here('figures', 'cbg-smoke-legend.pdf'),
    plot = h2 + coord_cartesian(xlim = c(40, 100)),
    base_height = 0.25,
    base_width = 2.65,
    dpi = 450
  )