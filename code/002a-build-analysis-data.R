

# Notes ----------------------------------------------------------------------------------
#   Goal: Create (mostly) clean (West Coast) dataset ready for analysis.
#   Time: 5-10 minutes


# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(parallel, fastverse, fst, fixest, magrittr, here)
  fastverse_extend(topics = c('SP', 'DT', 'ST'))
  # Fix collapse's F issue
  F = FALSE
  # Add directory of SafeGraph data
  dir_sg = '/media/edwardrubin/Data/SafeGraph'


# Load data: County shapefiles -----------------------------------------------------------
  # Load TigerLines county shape file
  tl_sf = here(
    "data-raw", "census", "tl_2016_us_county", "tl_2016_us_county.shp"
  ) %>% st_read(
    stringsAsFactors = F,
    # Limit to counties in contiguous 48 states (plus DC)
    query = paste(
      "SELECT GEOID", 
      "FROM \"tl_2016_us_county\"",
      "WHERE STATEFP NOT IN ('02', '15', '60', '66', '69', '72', '78')"
    )
  ) %>% dplyr::transmute(fips = GEOID)
  # Load cartographic boundary county shape file
  cb_sf = here(
    "data-raw", "census", "cb_2016_us_county_500k", "cb_2016_us_county_500k.shp"
  ) %>% st_read(
    stringsAsFactors = F,
    # Limit to counties in contiguous 48 states (plus DC)
    query = paste(
      "SELECT GEOID", 
      "FROM \"cb_2016_us_county_500k\"",
      "WHERE STATEFP NOT IN ('02', '15', '60', '66', '69', '72', '78')"
    )
  ) %>% dplyr::transmute(fips = GEOID)


# Data work: County areas ----------------------------------------------------------------
  # Add county area to each of the spatial datasets
  tl_areas = tl_sf %>% st_area()
  cb_areas = cb_sf %>% st_area()
  # Convert shapefiles to data tables (for merging)
  setDT(tl_sf)
  setDT(cb_sf)
  tl_sf[, `:=`(area_tl = tl_areas, geometry = NULL)]
  cb_sf[, `:=`(area_cb = cb_areas, geometry = NULL)]
  # Create dataset of areas
  area_dt = merge(
    x = tl_sf,
    y = cb_sf,
    by = 'fips',
    all = TRUE
  )
  # Change name of 'fips' to 'county'
  setnames(area_dt, old = 'fips', new = 'county')


# Load data: Smoke-week dataset ----------------------------------------------------------
  # Load the dataset
  full_dt = here(
    'data-processed', 'for-analysis', 'cbg-visit-smoke-week.fst'
  ) %>% read_fst(as.data.table = TRUE)


# Load data: Hospital visits ------------------------------------------------------------
  # Load the dataset
  hospitals_dt = here(
    'data-processed', 'cbg-hospital-visits', 'all-cbg-hospital-visits.fst'
  ) %>% read_fst(as.data.table = TRUE)


# Merge: Movement and hospital visits ----------------------------------------------------
  # Key the datasets
  setkey(full_dt, cbg_home, date_start)
  setkey(hospitals_dt, cbg_home, date_start)
  # Merge
  full_dt %<>% merge(
    y = hospitals_dt,
    by = c('cbg_home', 'date_start'),
    all.x = TRUE,
    all.y = FALSE
  )
  # Fill in NAs of visits with 0s (missing means and empty intersection)
  full_dt[is.na(hospital_visits_total), `:=`(
    hospital_visits_total = 0,
    hospital_visits_same_cbg = 0,
    hospital_visits_same_tract = 0,
    hospital_visits_same_county = 0,
    hospital_visits_same_state = 0,
    n_hospitals = 0
  )]


# Data work: Change names ----------------------------------------------------------------
  # Rename smoke variables
  setnames(
    full_dt,
    old = c('smoke_low_home', 'smoke_medium_home', 'smoke_high_home'),
    new = c('ndays_smoke_low', 'ndays_smoke_medium', 'ndays_smoke_high')
  )


# Data work: Create variables ------------------------------------------------------------
  # Any smoke
  full_dt[, `:=`(
    any_smoke = 1 * (ndays_smoke_low + ndays_smoke_medium + ndays_smoke_high > 0),
    any_smoke_low = 1 * (ndays_smoke_low > 0),
    any_smoke_medium = 1 * (ndays_smoke_medium > 0),
    any_smoke_high = 1 * (ndays_smoke_high > 0),
    any_smoke_mh = 1 * (ndays_smoke_medium + ndays_smoke_high > 0)
  )]
  # Shares
  full_dt[, `:=`(
    pct_smoke_low = ndays_smoke_low / n_smoke_files,
    pct_smoke_medium = ndays_smoke_medium / n_smoke_files,
    pct_smoke_high = ndays_smoke_high / n_smoke_files,
    pct_same_cbg = visits_same_cbg / total_visits,
    pct_same_tract = visits_same_tract / total_visits,
    pct_same_county = visits_same_county / total_visits,
    pct_same_state = visits_same_state / total_visits
  )]
  # Fixed effects: Spatial/administrative
  full_dt[, `:=`(
    state = str_sub(cbg_home, 1, 2) %>% factor(),
    county = str_sub(cbg_home, 1, 5) %>% factor()
  )]
  # Fixed effects: Temporal
  full_dt[, `:=`(
    yr = year(date_start),
    mo = month(date_start),
    wk = week(date_start)
  )]
  # Drop non-CONUS states
  full_dt %<>% .[!(state %in% c('02', '15', '60', '66', '69', '72', '78'))]
  # Add mean visits
  full_dt[, mean_visits := fmean(total_visits), by = cbg_home]


# Data work: Balance the panel -----------------------------------------------------------
  # Count CBGs' numbers of weeks in the sample
  cbg_dt = full_dt[, .(
    n_weeks = .N,
    total_visits = fsum(total_visits)
  ), by = cbg_home]
  # Only keep CBGs with all weeks (98.5% of CBGs and 99.8% of population)
  # For visits: we keep 22,815,726,329 out of 22,870,872,528 visits
  max_weeks = cbg_dt[,fmax(n_weeks)]
  cbg_dt %<>% .[n_weeks == max_weeks]
  full_dt %<>% .[cbg_home %in% cbg_dt[, cbg_home]]


# Data work: Add lags --------------------------------------------------------------------
  # Add lags for smoke
  setorder(full_dt, cbg_home, date_start)
  full_dt[, `:=`(
    lag_any_smoke = flag(any_smoke, n = 1),
    lag_any_smoke_low = flag(any_smoke_low, n = 1),
    lag_any_smoke_medium = flag(any_smoke_medium, n = 1),
    lag_any_smoke_high = flag(any_smoke_high, n = 1),
    lag_any_smoke_mh = flag(any_smoke_mh, n = 1),
    lag_smoke_low_home = flag(ndays_smoke_low, n = 1),
    lag_smoke_medium_home = flag(ndays_smoke_medium, n = 1),
    lag_smoke_high_home = flag(ndays_smoke_high, n = 1),
    lag_pct_smoke_low = flag(pct_smoke_low, n = 1),
    lag_pct_smoke_medium = flag(pct_smoke_medium, n = 1),
    lag_pct_smoke_high = flag(pct_smoke_high, n = 1)
  ), by = cbg_home]
  full_dt[, `:=`(
    lag2_any_smoke = flag(any_smoke, n = 2),
    lag2_any_smoke_low = flag(any_smoke_low, n = 2),
    lag2_any_smoke_medium = flag(any_smoke_medium, n = 2),
    lag2_any_smoke_high = flag(any_smoke_high, n = 2),
    lag2_any_smoke_mh = flag(any_smoke_mh, n = 2),
    lag2_smoke_low_home = flag(ndays_smoke_low, n = 2),
    lag2_smoke_medium_home = flag(ndays_smoke_medium, n = 2),
    lag2_smoke_high_home = flag(ndays_smoke_high, n = 2),
    lag2_pct_smoke_low = flag(pct_smoke_low, n = 2),
    lag2_pct_smoke_medium = flag(pct_smoke_medium, n = 2),
    lag2_pct_smoke_high = flag(pct_smoke_high, n = 2)
  ), by = cbg_home]
  full_dt[, `:=`(
    lag3_any_smoke = flag(any_smoke, n = 3),
    lag3_any_smoke_low = flag(any_smoke_low, n = 3),
    lag3_any_smoke_medium = flag(any_smoke_medium, n = 3),
    lag3_any_smoke_high = flag(any_smoke_high, n = 3),
    lag3_any_smoke_mh = flag(any_smoke_mh, n = 3),
    lag3_smoke_low_home = flag(ndays_smoke_low, n = 3),
    lag3_smoke_medium_home = flag(ndays_smoke_medium, n = 3),
    lag3_smoke_high_home = flag(ndays_smoke_high, n = 3),
    lag3_pct_smoke_low = flag(pct_smoke_low, n = 3),
    lag3_pct_smoke_medium = flag(pct_smoke_medium, n = 3),
    lag3_pct_smoke_high = flag(pct_smoke_high, n = 3)
  ), by = cbg_home]
  full_dt[, `:=`(
    lag4_any_smoke = flag(any_smoke, n = 4),
    lag4_any_smoke_low = flag(any_smoke_low, n = 4),
    lag4_any_smoke_medium = flag(any_smoke_medium, n = 4),
    lag4_any_smoke_high = flag(any_smoke_high, n = 4),
    lag4_any_smoke_mh = flag(any_smoke_mh, n = 4),
    lag4_smoke_low_home = flag(ndays_smoke_low, n = 4),
    lag4_smoke_medium_home = flag(ndays_smoke_medium, n = 4),
    lag4_smoke_high_home = flag(ndays_smoke_high, n = 4),
    lag4_pct_smoke_low = flag(pct_smoke_low, n = 4),
    lag4_pct_smoke_medium = flag(pct_smoke_medium, n = 4),
    lag4_pct_smoke_high = flag(pct_smoke_high, n = 4)
  ), by = cbg_home]


# Data work: Add leads -------------------------------------------------------------------
  # Add lags for smoke
  setorder(full_dt, cbg_home, date_start)
  full_dt[, `:=`(
    lead_any_smoke = flag(any_smoke, n = -1),
    lead_any_smoke_low = flag(any_smoke_low, n = -1),
    lead_any_smoke_medium = flag(any_smoke_medium, n = -1),
    lead_any_smoke_high = flag(any_smoke_high, n = -1),
    lead_any_smoke_mh = flag(any_smoke_mh, n = -1),
    lead_smoke_low_home = flag(ndays_smoke_low, n = -1),
    lead_smoke_medium_home = flag(ndays_smoke_medium, n = -1),
    lead_smoke_high_home = flag(ndays_smoke_high, n = -1),
    lead_pct_smoke_low = flag(pct_smoke_low, n = -1),
    lead_pct_smoke_medium = flag(pct_smoke_medium, n = -1),
    lead_pct_smoke_high = flag(pct_smoke_high, n = -1)
  ), by = cbg_home]
  full_dt[, `:=`(
    lead2_any_smoke = flag(any_smoke, n = -2),
    lead2_any_smoke_low = flag(any_smoke_low, n = -2),
    lead2_any_smoke_medium = flag(any_smoke_medium, n = -2),
    lead2_any_smoke_high = flag(any_smoke_high, n = -2),
    lead2_any_smoke_mh = flag(any_smoke_mh, n = -2),
    lead2_smoke_low_home = flag(ndays_smoke_low, n = -2),
    lead2_smoke_medium_home = flag(ndays_smoke_medium, n = -2),
    lead2_smoke_high_home = flag(ndays_smoke_high, n = -2),
    lead2_pct_smoke_low = flag(pct_smoke_low, n = -2),
    lead2_pct_smoke_medium = flag(pct_smoke_medium, n = -2),
    lead2_pct_smoke_high = flag(pct_smoke_high, n = -2)
  ), by = cbg_home]
  full_dt[, `:=`(
    lead3_any_smoke = flag(any_smoke, n = -3),
    lead3_any_smoke_low = flag(any_smoke_low, n = -3),
    lead3_any_smoke_medium = flag(any_smoke_medium, n = -3),
    lead3_any_smoke_high = flag(any_smoke_high, n = -3),
    lead3_any_smoke_mh = flag(any_smoke_mh, n = -3),
    lead3_smoke_low_home = flag(ndays_smoke_low, n = -3),
    lead3_smoke_medium_home = flag(ndays_smoke_medium, n = -3),
    lead3_smoke_high_home = flag(ndays_smoke_high, n = -3),
    lead3_pct_smoke_low = flag(pct_smoke_low, n = -3),
    lead3_pct_smoke_medium = flag(pct_smoke_medium, n = -3),
    lead3_pct_smoke_high = flag(pct_smoke_high, n = -3)
  ), by = cbg_home]
  full_dt[, `:=`(
    lead4_any_smoke = flag(any_smoke, n = -4),
    lead4_any_smoke_low = flag(any_smoke_low, n = -4),
    lead4_any_smoke_medium = flag(any_smoke_medium, n = -4),
    lead4_any_smoke_high = flag(any_smoke_high, n = -4),
    lead4_any_smoke_mh = flag(any_smoke_mh, n = -4),
    lead4_smoke_low_home = flag(ndays_smoke_low, n = -4),
    lead4_smoke_medium_home = flag(ndays_smoke_medium, n = -4),
    lead4_smoke_high_home = flag(ndays_smoke_high, n = -4),
    lead4_pct_smoke_low = flag(pct_smoke_low, n = -4),
    lead4_pct_smoke_medium = flag(pct_smoke_medium, n = -4),
    lead4_pct_smoke_high = flag(pct_smoke_high, n = -4)
  ), by = cbg_home]


# Load data: Census datasets from SafeGraph ----------------------------------------------
  # Load datasets
  c01 = file.path(
    dir_sg,
    'sg-census', 'safegraph_open_census_data_2019', 'data',
    'cbg_b01.csv'
  ) %>% fread(
    select = c(census_block_group = 'character', B01003e1 = 'integer')
  )
  setnames(c01, c('cbg', 'pop'))
  c02 = file.path(
    dir_sg,
    'sg-census', 'safegraph_open_census_data_2019', 'data',
    'cbg_b02.csv'
  ) %>% fread(
    select = c(census_block_group = 'character', B02008e1 = 'integer', B02009e1 = 'integer')
  )
  setnames(c02, c('cbg', 'pop_white', 'pop_black'))
  c03 = file.path(
    dir_sg,
    'sg-census', 'safegraph_open_census_data_2019', 'data',
    'cbg_b03.csv'
  ) %>% fread(
    select = c(census_block_group = 'character', B03002e12 = 'integer')
  )
  setnames(c03, c('cbg', 'pop_hispanic'))
  c19 = file.path(
    dir_sg,
    'sg-census', 'safegraph_open_census_data_2019', 'data',
    'cbg_b19.csv'
  ) %>% fread(
    select = c(census_block_group = 'character', B19013e1 = 'integer')
  )
  setnames(c19, c('cbg', 'hh_inc'))
  c23 = file.path(
    dir_sg,
    'sg-census', 'safegraph_open_census_data_2019', 'data',
    'cbg_b23.csv'
  ) %>% fread(
    select = c(census_block_group = 'character', B23025e1 = 'integer', B23025e2 = 'integer')
  )
  setnames(c23, c('cbg', 'pop_employable', 'pop_employed'))
  # Merge datasets
  cbg_dt = merge(
    x = c01,
    y = c02,
    by = 'cbg',
    all = T
  ) %>% merge(
    y = c03,
    by = 'cbg',
    all = T
  ) %>% merge(
    y = c19,
    by = 'cbg',
    all = T
  ) %>% merge(
    y = c23,
    by = 'cbg',
    all = T
  )
  # Add shares
  cbg_dt[, `:=`(
    shr_white = pop_white / pop,
    shr_black = pop_black / pop,
    shr_hispanic = pop_hispanic / pop,
    shr_employed = pop_employed / pop,
    shr_unemployed = (pop_employable - pop_employed) / pop_employable
  )]
  # Join to full dataset
  setkey(cbg_dt, cbg)
  setkey(full_dt, cbg_home)
  full_dt %<>% merge(
    y = cbg_dt,
    by.x = 'cbg_home',
    by.y = 'cbg',
    all.x = TRUE,
    all.y = FALSE
  )
  # Clean up
  invisible(gc())


# Load data: Census data from NHGIS ------------------------------------------------------
  # Load the dataset descriptions
  nhgis_desc = here(
    'data-raw', 'nhgis', 'nhgis0036_ds172_2010_blck_grp.csv'
  ) %>% fread(nrows = 2)
  # Load the dataset
  nhgis_dt = here(
    'data-raw', 'nhgis', 'nhgis0036_ds172_2010_blck_grp.csv'
  ) %>% fread(skip = 2, header = F)
  setnames(nhgis_dt, names(nhgis_desc))
  # Create CBG FIPS code
  nhgis_dt %<>% .[, .(
    cbg_home = paste0(
      str_pad(STATEA, 2, 'left', 0),
      str_pad(COUNTYA, 3, 'left', 0),
      str_pad(TRACTA, 6, 'left', 0),
      str_pad(BLKGRPA, 1, 'left', 0)
    ),
    pop_rural = as.integer(H7W005),
    pop_urban = as.integer(H7W002),
    pop_urban_area = as.integer(H7W003),
    pop_urban_cluster = as.integer(H7W004)
  )]
  # Merge to the full dataset
  setkey(nhgis_dt, cbg_home)
  setkey(full_dt, cbg_home)
  full_dt %<>% merge(
    y = nhgis_dt,
    by = 'cbg_home',
    all.x = TRUE,
    all.y = FALSE
  )
  # Clean up
  invisible(gc())


# Data work: Add county areas ------------------------------------------------------------
  # Merge
  full_dt %<>% merge(
    y = area_dt,
    by = 'county',
    all.x = TRUE,
    all.y = FALSE,
    sort = FALSE
  )
  # Clean up
  invisible(gc())


# Save data ------------------------------------------------------------------------------
  # Save full dataset
  write_fst(
    x = full_dt,
    path = here('data-processed', 'for-analysis', 'for-analysis-full.fst'),
    compress = 100
  )
  # Save West Coast dataset
  write_fst(
    x = full_dt[state %in% c('06', '41', '53')],
    path = here('data-processed', 'for-analysis', 'for-analysis-westcoast.fst'),
    compress = 100
  )
  