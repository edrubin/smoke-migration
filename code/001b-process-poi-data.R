

# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(
    tidyverse, 
    qs, fst, collapse, data.table, 
    splitstackshape, MetricsWeighted, lubridate, parallel,
    magrittr, here
  )
  # Fix collapse's F issue
  F = FALSE
  # Add directory of SafeGraph data
  dir_sg = '/media/edwardrubin/Data/SafeGraph'


# Data work: Load and merge SG visits with smoke data ------------------------------------
  # Find the weeks
  file_dt = data.table(
    file_name = file.path(dir_sg, "data-transformed", "weekly") %>% dir()
  )
  # Add the week
  file_dt[, wk := str_sub(file_name, 17, 26) %>% ymd()]
  # Find the completed weeks
  weeks_done = here(
    'data-processed', 'poi-smoke-week'
  ) %>% dir() %>% str_remove_all('\\.fst') %>% ymd()
  # Drop completed weeks
  file_dt %<>% .[! (wk %in% weeks_done)]
  # Iterate over weeks...
  blah = lapply(
  # blah = mclapply(
    X = file_dt[,wk], 
    # mc.cores = 4,
    FUN = function(w) {
      # Load the data from week 'w'
      w_dt = file.path(
        dir_sg,
        "data-transformed", "weekly", 
        paste0("weekly-patterns-", as.character(w), ".fst")
      ) %>% read_fst(
        columns = c(
          "date_range_start", "placekey",
          # "raw_visit_counts", "raw_visitor_counts", 
          "poi_cbg", "visitor_home_cbgs"
        ),
        as.data.table = T
      )
      # Drop POIs where 'visitor_home_cbgs' is '{}'
      w_dt %<>% .[visitor_home_cbgs != '{}']
      # Remove curly brackets and escaped quotation marks
      w_dt[, `:=`(
        visitor_home_cbgs = visitor_home_cbgs %>% str_remove_all("\\{|\"|\\}")
      )]
      # Split visitor CBGs into rows
      w_dt = cSplit(
        indt = w_dt,
        sep = ",",
        splitCols = "visitor_home_cbgs",
        direction = "long",
        drop = T,
        type.convert = F
      )
      # Drop Canadian CBGs: they begin with letters instead (CA) of numbers
      w_dt %<>% .[
        (!str_detect(visitor_home_cbgs, '^[^0-9]')) & (!str_detect(poi_cbg, '^[^0-9]'))
      ]
      # Split the new column
      w_dt[, c("cbg_home", "visits") := tstrsplit(
        x = visitor_home_cbgs,
        split = ":",
        type.convert = F
      )]
      # Drop 'visitor_home_cbgs' column
      w_dt[, visitor_home_cbgs := NULL]
      # Rename
      setnames(w_dt, old = 'poi_cbg', new = 'cbg_poi')
      # The week's days
      w_days = seq.Date(from = w, length.out = 7, by = '1 day')
      # The week's smoke files
      w_files = w_days %>% 
        str_remove_all('-') %>% 
        paste0('.fst') %>%
        here('data-processed', 'cbg-smoke-day', .)
      # Only grab the files that exist
      w_files = w_files[file.exists(w_files)]
      # Load and stack the files
      w_smoke = lapply(
        X = w_files, FUN = read_fst, as.data.table = T
      ) %>% rbindlist(use.names = T, fill = T)
      # Collapse to CBG-week level, summing the daily smoke indicators
      w_smoke %<>% collap(FUN = fsum, ~ geoid)
      # Key datasets for home CBG merge
      setkey(w_smoke, geoid)
      setkey(w_dt, cbg_home)
      # Merge on home CBG
      w_dt %<>% merge(
        x = .,
        y = w_smoke,
        by.x = 'cbg_home',
        by.y = 'geoid',
        all.x = TRUE,
        all.y = FALSE
      )
      # Fix names
      setnames(
        w_dt,
        old = c('smoke_low', 'smoke_medium', 'smoke_high'),
        new = c('smoke_low_home', 'smoke_medium_home', 'smoke_high_home')
      )
      # Merge on POI CBG
      setkey(w_dt, cbg_poi)
      w_dt %<>% merge(
        x = .,
        y = w_smoke,
        by.x = 'cbg_poi',
        by.y = 'geoid',
        all.x = TRUE,
        all.y = FALSE
      )
      # Fix names
      setnames(
        w_dt,
        old = c('smoke_low', 'smoke_medium', 'smoke_high'),
        new = c('smoke_low_poi', 'smoke_medium_poi', 'smoke_high_poi')
      )
      # Fill zeros for no smoke
      w_dt[is.na(smoke_low_home), smoke_low_home := 0]
      w_dt[is.na(smoke_medium_home), smoke_medium_home := 0]
      w_dt[is.na(smoke_high_home), smoke_high_home := 0]
      w_dt[is.na(smoke_low_poi), smoke_low_poi := 0]
      w_dt[is.na(smoke_medium_poi), smoke_medium_poi := 0]
      w_dt[is.na(smoke_high_poi), smoke_high_poi := 0]
      # Convert visits to integer
      w_dt[, visits := visits %>% as.integer()]
      # Add variable for the number of smoke files in the week
      w_dt[, n_smoke_files := w_files %>% length()]
      # Change name of date
      setnames(w_dt, old = 'date_range_start', new = 'date_start')
      # Save the dataset
      write_fst(
        x = w_dt,
        path = here(
          'data-processed', 'poi-smoke-week',
          paste0(str_remove_all(w, '-'), '.fst')
        ),
        compress = 100
      )
      # Return nothing
      return('nothing')
    }
  )

