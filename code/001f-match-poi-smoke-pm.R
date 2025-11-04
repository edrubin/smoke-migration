# Notes ----------------------------------------------------------------------------------
#   Goal:   Merge SafeGraph POI weekly visits with grid of smoke-based PM exposure.
#           NOTE: Source of data is Burke et al. (2022, Env. Sci. & Technology)
#   Time:   Under two hours?


# Data notes -----------------------------------------------------------------------------
#   - Six out of 7,887,724 POIs do not match to Burke et al.'s grid. Two are in the ocean,
#     three are in Canada. One is near the Gulf Coast.
#   - Visits (by CBG origin) are weekly; PM are daily. We average across PM in a week.
#   - We use CBG centroids for grid match. Should be accurate for urban CBGs.
#   - Two CBGs (060839900000 and 061119901000) do not match a grid cell.


# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(tidyverse, fastverse, parallel, here)
  fastverse_extend(topics = c('ST', 'TV', 'IO', 'DT', 'SP'))


# Load data: Burke et al. grid -----------------------------------------------------------
  # Load the grid (defined by Burke et al.)
  grid_sf =
    here(
      'data-processed', 'pm-smoke-burke', '10km_grid',
      '10km_grid_wgs84/10km_grid_wgs84.shp'
    ) |>
    st_read() |>
    select(ID)


# Load data: POI locations ---------------------------------------------------------------
  # Load the previously constructed POI-location dataset
  poi_dt = here(
    'data-processed', 'geo-summaries',
    'placekey-coordinates.fst'
  ) %>% read_fst(as.data.table = TRUE)
  # Drop POIs with mismatched lat/lon
  poi_dt %<>% .[flag_lat == FALSE & flag_lon == FALSE]
  # Drop unwanted variables
  poi_dt[, c('flag_lat', 'flag_lon') := NULL]


# Data work: Match POIs to grid cells ----------------------------------------------------
  # Grab bounding box of the grid
  grid_bbox = grid_sf %>% st_bbox() %>% unlist()
  # Drop POIs outside of the bounding box of the grid (outside contiguous US)
  poi_dt %<>% .[(
    between(lon, grid_bbox$xmin, grid_bbox$xmax) &
    between(lat, grid_bbox$ymin, grid_bbox$ymax)
  )]
  # Create SF object
  poi_sf = poi_dt %>% st_as_sf(
    coords = c('lon', 'lat'),
    crs = st_crs(4326)
  )
  # Make sure CRS matches across grid and cells
  grid_sf %<>% st_transform(crs = st_crs(poi_sf))
  # Spatially match (intersect) POIs to grid cells
# NOTE: Takes about 4 minutes for the 7.89 million POIs
  poi_grid = st_join(
    x = poi_sf,
    y = grid_sf,
    join = st_intersects,
    left = TRUE
  )
  # Convert to data table
  setDT(poi_grid)
  # Clean names
  setnames(poi_grid, old = 'ID', new = 'grid_id')
  # Drop geometry
  poi_grid[, geometry := NULL]
  # Save the cross walk
  write_fst(
    x = poi_grid,
    path = here('data-processed', 'pm-smoke-burke', 'xwalk-poi-grid.fst'),
    compress = 100
  )


# Load data: CBG centroids ---------------------------------------------------------------
  cbg_dt = here(
    'data-processed', 'geo-summaries', 'sg-cbg-centroids-dt.fst'
  ) %>% read_fst(as.data.table = TRUE)
  # Subset to West Coast CBGs
  cbg_dt %<>% .[str_sub(cbg, 1, 2) %in% c('06', '41', '53')]
  # To SF
  cbg_sf = cbg_dt %>% st_as_sf(coords = c('lon', 'lat'), crs = st_crs(4326))


# Data work: Match CBG centroids to grid cells -------------------------------------------
  # Spatially match (intersect) CBGs to grid cells
# NOTE: Takes about 4 seconds for the 30,629 West-Coast CBGs
  cbg_grid = st_join(
    x = cbg_sf,
    y = grid_sf,
    join = st_intersects,
    left = TRUE
  )
  # Convert to data table
  setDT(cbg_grid)
  # Clean names
  setnames(cbg_grid, old = 'ID', new = 'grid_id')
  # Drop geometry
  cbg_grid[, geometry := NULL]
  # Save the cross walk
  write_fst(
    x = cbg_grid,
    path = here('data-processed', 'pm-smoke-burke', 'xwalk-cbg-grid.fst'),
    compress = 100
  )


# Clean up -------------------------------------------------------------------------------
  # Clean up
  rm(grid_sf, poi_dt, poi_sf, cbg_dt, cbg_sf)
  invisible(gc())


# Load data: Daily PM from smoke ---------------------------------------------------------
  # Load the dataset
  pm_dt = here(
    'data-processed', 'pm-smoke-burke', '10km_grid',
    'smokePM2pt5_predictions_on_smokedays_daily_10km_20060101-20201231.csv'
  ) %>% fread()
  # Change names
  setnames(pm_dt, c('grid_id', 'date', 'pm'))
  # Format date
  pm_dt[, date := date %>% ymd()]


# Data work: Match visits to PM exposure -------------------------------------------------
# NOTE: This step took ~100 minutes without parallelization
  # Find files (weekly files of POI visits)
  week_v =
    here(
      'data-processed', 'poi-smoke-week'
    ) |>
    dir() |>
    str_remove_all('\\.fst') |>
    ymd()
  # Iterate over already-processed files
  smoke_pm =
    mclapply(X = week_v, mc.cores = 10, FUN = function(w) {
      # Find days in week 'w'
      w_days = seq.Date(from = w, by = '1 day', length = 7)
      # Convert week 'w' to character
      wc = w %>% as.character() %>% str_remove_all('-')
      # Load POI visits for week 'w'
      w_dt = here(
        'data-processed', 'poi-smoke-week',
        paste0(str_remove_all(w, '-'), '.fst')
      ) %>% read_fst(
        as.data.table = TRUE,
        columns = c('cbg_poi', 'cbg_home', 'placekey', 'visits')
      )
      # Restrict to visits where visitors' home CBGs are on the West Coast
      w_dt %<>% .[str_sub(cbg_home, 1, 2) %in% c('06', '41', '53')]
      # Iterate over days in 'w', calculating smoke-based PM exposure for visits
      w_pm = lapply(X = w_days, FUN = function(d) {
        # Copy w_dt
        wd_dt = copy(w_dt)
        # Add date
        wd_dt[, date := d]
        # Key to merge with cross walk
        setkey(wd_dt, placekey)
        setkey(poi_grid, placekey)
        # Merge with cross walk
        wd_dt %<>% merge(poi_grid, by = 'placekey')
        # Key to merge with PM dataset
        setkey(wd_dt, grid_id)
        setkey(pm_dt, date, grid_id)
        # Merge with PM dataset
        wd_dt %<>% merge(
          pm_dt[date == d],
          by = c('grid_id', 'date'),
          all.x = TRUE,
          all.y = FALSE
        )
        # Replace missing values with 0 (no smoke-based PM)
        wd_dt[is.na(pm), pm := 0]
        # Return the daily dataset
        return(wd_dt)
      }) |>
      rbindlist(use.names = TRUE, fill = TRUE)
      # Repeat iterations for CBG centroids (counterfactual of no travel)
      w_pm_cbg = lapply(X = w_days, FUN = function(d) {
        # Copy the CBG-grid crosswalk
        cbg_d = copy(cbg_grid)
        # Drop CBGs that did not match to grid cells (spurious PM = 0)
        cbg_d %<>% .[!is.na(grid_id)]
        # Add date
        cbg_d[, date := d]
        # Key to merge with PM dataset
        setkey(cbg_d, grid_id)
        setkey(pm_dt, date, grid_id)
        # Merge with PM dataset
        cbg_d %<>% merge(
          pm_dt[date == d],
          by = c('grid_id', 'date'),
          all.x = TRUE,
          all.y = FALSE
        )
        # Replace missing values with 0 (no smoke-based PM)
        cbg_d[is.na(pm), pm := 0]
        # Return the daily dataset
        return(cbg_d)
      }) %>%
      rbindlist(use.names = TRUE, fill = TRUE)
      # Drop 'w_dt'
      rm(w_dt)
      # Summarize PM exposure tables (by home for POI visits) and merge!
      w_dt = merge(
        x = w_pm_cbg[, .(smoke_pm_home = fmean(x = pm)), by = .(cbg_home = cbg)],
        y = w_pm[, .(
          smoke_pm_visits = fmean(x = pm, w = visits),
          date_start = w
        ), by = cbg_home],
        by = 'cbg_home',
        all.x = TRUE,
        all.y = TRUE
      )
      # Return the week's summary
      return(w_dt)
    })
  # Convert list of daily CBG PM exposure measures to data table
  # Note: This step is to fix missing dates (when a CBG had no visits)
  exp_dt = lapply(
    X = seq_along(smoke_pm),
    FUN = function(i) {
      # Grab the ith element of smoke_pm
      i_dt = smoke_pm[[i]]
      # Grab the date
      i_d = i_dt[1, date_start]
      # Fill in missing dates (there's only one date in i_dt)
      i_dt[is.na(date_start), date_start := i_d]
      # Return the updated data table
      return(i_dt)
    }
  ) %>% rbindlist(use.names = TRUE, fill = TRUE)
  # Find the two CBGs that did not match a grid cell
  tmp = exp_dt[, .N, cbg_home]
  # Drop the two CBGs
  exp_dt %<>% .[!(cbg_home %in% tmp[N < 209, cbg_home])]


# Save -----------------------------------------------------------------------------------
  # Save the weekly smoke-based PM exposure dataset
  write_fst(
    x = exp_dt,
    path = here('data-processed', 'pm-smoke-burke', 'cbg-week-pm-smoke.fst'),
    compress = 100
  )
