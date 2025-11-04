# Notes ----------------------------------------------------------------------------------
#   Goal: Collapse weekly POI-smoke data to weekly home-CBG visit summaries.
#   Time: 22 minutes


# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(parallel, fastverse, fst, magrittr, here)
  fastverse_extend(topics = c('DT', 'ST'))
  # Add directory of SafeGraph data
  dir_sg = '/media/edwardrubin/Data/SafeGraph'


# Load data: CBG table -------------------------------------------------------------------
  # Load CBG-centroid data table
  cbg_dt = here(
    'data-processed', 'geo-summaries', 'sg-cbg-centroids-dt.fst'
  ) %>% read_fst(as.data.table = TRUE)


# Data work: Collapse weekly POI-smoke files ---------------------------------------------
  # Find weekly files
  w_files = here('data-processed', 'poi-smoke-week') %>% dir(pattern = '[0-9]{8}\\.fst$')
  # Determine which files have already finished
  finished = NULL
  # finished = here('data-processed', 'cbg-smoke-week') %>% dir()
  # Remove the finished files from the to-do list
  todo = setdiff(w_files, finished)
  # Iterating over weekly POI-smoke files (in to-do list):
  #   1. Load the file
  #   2. Collapse to home-CBG by week level, summarizing visits' proximity to home CBG
  blah = mclapply(
    X = todo,
    mc.cores = 8,
    FUN = function(f) {
      # Load data
      w_dt = here('data-processed', 'poi-smoke-week', f) %>% read_fst(as.data.table = TRUE)
      # Drop observations missing their CBGs
      w_dt %<>% .[(cbg_poi != '') & (cbg_home != '')]
      # Add lat/lon of home and POI CBGs
      # First: Add POI lat/lon via merge with CBG centroid data table
      setnames(cbg_dt, c('cbg_poi', 'lon_poi', 'lat_poi'))
      setkey(w_dt, cbg_poi)
      setkey(cbg_dt, cbg_poi)
      # Merge lat/lon onto w_dt
      w_dt %<>% merge(
        y = cbg_dt,
        by = 'cbg_poi',
        all.x = TRUE,
        all.y = FALSE
      )
      invisible(gc())
      # Now merge HOME lat/lon (via CBG centroid data table)
      setnames(cbg_dt, c('cbg_home', 'lon_home', 'lat_home'))
      setkey(w_dt, cbg_home)
      setkey(cbg_dt, cbg_home)
      # Merge lat/lon onto w_dt
      w_dt %<>% merge(
        y = cbg_dt,
        by = 'cbg_home',
        all.x = TRUE,
        all.y = FALSE
      )
      invisible(gc())
      # Calculate distances (starts in meters; converted to km)
      w_dt[, `:=`(
        dist = geodist::geodist(
          x = w_dt[, .(lon_poi, lat_poi)],
          y = w_dt[, .(lon_home, lat_home)],
          paired = TRUE,
          measure = 'haversine'
        ) / 1e3
      )]
      # Determine whether the home and visits POIs match at CBG, tract, county, or state
      w_dt[, `:=`(
        visits_same_state = visits * (str_sub(cbg_poi, 1, 2) == str_sub(cbg_home, 1, 2)),
        visits_same_county = visits * (str_sub(cbg_poi, 1, 5) == str_sub(cbg_home, 1, 5)),
        visits_same_tract = visits * (str_sub(cbg_poi, 1, 11) == str_sub(cbg_home, 1, 11)),
        visits_same_cbg = visits * (cbg_poi == cbg_home)
      )]
      # Collapse the dataset to the home CBG level
      visits_dt = collap(
        X = w_dt,
        ~ cbg_home,
        w = ~ visits,
        custom = list(
          fsum_uw = c(
            total_visits = 'visits',
            'visits_same_cbg',
            'visits_same_tract',
            'visits_same_county',
            'visits_same_state'
          ),
          fmax_uw = c(
            'smoke_low_home',
            'smoke_medium_home',
            'smoke_high_home',
            'n_smoke_files'
          ),
          fmean = c(dist_mean = 'dist'),
          fmedian = c(dist_median = 'dist')
        ),
        keep.col.order = FALSE,
        give.names = FALSE,
        keep.w = FALSE
      )
      # Grab 25th and 75th percentiles for each CBGs' distance-traveled distribution
      visits_dist = w_dt[, .(
        dist_p25 = fnth(dist, n = 0.25, w = visits),
        dist_p75 = fnth(dist, n = 0.75, w = visits)
      ), by = cbg_home]
      # Merge distance summary on to visits_dt
      visits_dt %<>% merge(y = visits_dist, by = 'cbg_home', all = TRUE)
      # Add the week's start date
      visits_dt[, date_start := w_dt[1, date_start]]
      # Remove the POI dataset
      rm(w_dt, visits_dist)
      invisible(gc())
      # Save the file
      write_fst(
        x = visits_dt,
        path = here('data-processed', 'cbg-smoke-week', f),
        compress = 100
      )
      # Return nothing
      return('nothing')
    }
  )