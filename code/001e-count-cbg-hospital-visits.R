

# Notes ----------------------------------------------------------------------------------
#   Goal:   Calculate CBGs' weekly visits to hospitals (summing over POIs).
#   Time:   ~10 minutes


# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(parallel, fastverse, fst, magrittr, here)
  fastverse_extend(topics = c('DT', 'ST'))
  # Fix collapse's F issue
  F = FALSE
  # Add directory of SafeGraph data
  dir_sg = '/media/edwardrubin/Data/SafeGraph'


# Load data: Hospital placekeys ----------------------------------------------------------
  # Load the hospitals' placekeys
  hospital_keys = here(
    'data-processed', 'poi-summaries', 'sg-hospital-pois.csv'
  ) %>% fread()
  # Key the dataset
  setkey(hospital_keys, placekey)


# Data work: Collapse weekly POI-CBG files -----------------------------------------------
  # Find weekly files
  w_files = here('data-processed', 'poi-smoke-week') %>% dir(pattern = '[0-9]{8}\\.fst$')
  # Determine which files have already finished
  finished = NULL
  # finished = here('data-processed', 'cbg-hospital-visits', 'weekly') %>% dir()
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
      w_dt = here(
        'data-processed', 'poi-smoke-week', f
      ) %>% read_fst(
        columns = c('placekey', 'cbg_poi', 'cbg_home', 'date_start', 'visits'),
        as.data.table = T
      )
      # Drop observations missing their CBGs
      w_dt %<>% .[(cbg_poi != '') & (cbg_home != '')]
      # Grab hospitals only
      w_dt %<>% merge(
        hospital_keys[,'placekey'],
        by = 'placekey',
        all.x = FALSE,
        all.y = FALSE
      )
      # Determine whether the hospital visits occur in home CBG, tract, county, or state
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
        custom = list(
          fsum_uw = c(
            hospital_visits_total = 'visits',
            hospital_visits_same_cbg = 'visits_same_cbg',
            hospital_visits_same_tract = 'visits_same_tract',
            hospital_visits_same_county = 'visits_same_county',
            hospital_visits_same_state = 'visits_same_state'
          ),
          fndistinct_uw = c(n_hospitals = 'placekey')
        ),
        keep.col.order = FALSE,
        give.names = FALSE
      )
      # Add the week's start date
      visits_dt[, date_start := w_dt[1,date_start]]
      # Remove the POI dataset
      rm(w_dt); invisible(gc())
      # Save the file
      write_fst(
        x = visits_dt,
        path = here('data-processed', 'cbg-hospital-visits', 'weekly', f),
        compress = 100
      )
      # Return nothing
      return('nothing')
    }
  )


# Data work: Combine weekly visits files -------------------------------------------------
  # Find weekly files
  files = here(
    'data-processed', 'cbg-hospital-visits', 'weekly'
  ) %>% dir(pattern = '[0-9]{8}\\.fst$', full.names = TRUE)
  # Load them
  visits_dt = mclapply(
    X = files,
    FUN = read_fst,
    as.data.table = TRUE,
    mc.cores = 32
  ) %>% rbindlist(use.names = TRUE, fill = TRUE)
  

# Save -----------------------------------------------------------------------------------
  # Save the dataset
  write_fst(
    x = visits_dt,
    path = here('data-processed', 'cbg-hospital-visits', 'all-cbg-hospital-visits.fst'),
    compress = 100
  )
