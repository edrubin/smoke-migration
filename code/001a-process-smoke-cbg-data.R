
# Notes ----------------------------------------------------------------------------------
#   Goal:   Join smoke and CBG dta
#   Time:   About 40 minutes for 2018-2021


# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(
    tidyverse, fst, collapse, data.table, 
    sf, lubridate, parallel,
    magrittr, here
  )
  # Solve S2 issues in sf
  sf_use_s2(FALSE)
  # Fix collapse's F issue
  F = FALSE
  # Set directories
  dir_cbg = '/media/edwardrubin/Data/Census/BG2016'
  # Define CRS of Lambert
  crs_lambert = st_crs(
    'ESRI:102004',
    paste0(
      '+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 +x_0=0 +y_0=0',
      '+datum=NAD83 +units=m +no_defs'
    )
  )


# Load data: CBGs ------------------------------------------------------------------------
  # Find CBG data
  cbg_files = dir_cbg %>% dir(pattern = '^tl_2016_[0-9]{2}_bg')
  cbg_files %<>% str_subset('02|15|60|66|69|72|78', negate = T)
  # Load all CBGs
  cbg_dt = mclapply(
    X = cbg_files,
    FUN = function(i) {
      # Load the file
      i_sf = file.path(dir_cbg, i) %>%
        st_read(query = paste0('SELECT GEOID FROM \"', i, '\"'))
      # Cast to MULTIPOLYGON and project to Lambert
      i_sf %<>% st_cast('MULTIPOLYGON') %>% st_transform(crs = crs_lambert)
      # Convert to data.table
      setDT(i_sf)
      # Return
      return(i_sf)
    },
    mc.cores = 26
  ) %>% rbindlist(use.names = T, fill = T)
  # Clean names
  setnames(cbg_dt, old = 'GEOID', new = 'geoid')
  # Convert back to 'sf'
  cbg_sf = cbg_dt %>% st_as_sf()


# Data work: Find unique values of smoke density -----------------------------------------
  # Find the relevant days (files)
  days_smoke = seq.Date(
    from = ymd('20171201'),
    to = ymd('20211130'),
    by = '1 day'
  ) %>% str_remove_all('-')
  # Find days with smoke data (a few are missing days)
  days_smoke = intersect(here('data-raw', 'noaa') %>% dir(), days_smoke)
  # Iterate over days, searching for unique values of smoke density
  smoke_values = mclapply(
    X = days_smoke,
    FUN = function(d) {
      tryCatch(
        expr = here(
          'data-raw', 'noaa', d,
          paste0('hms_smoke', d, '.shp')
        ) %>% st_read() %>% as.data.table() %>% .[,unique(Density)],
        error = function(e) d
      )
    },
    mc.cores = 16
  ) %>% unlist() %>% funique()


# Data work: Merge daily smoke to CBGs ---------------------------------------------------
  # Find smoke days
  days_smoke = seq.Date(
    from = ymd('20171201'),
    to = ymd('20211130'),
    by = '1 day'
  ) %>% str_remove_all('-')
  # Find days with smoke data (a few are missing days)
  days_smoke = intersect(here('data-raw', 'noaa') %>% dir(), days_smoke)
  # Find completed days
  # days_done = here('data-processed', 'cbg-smoke-day') %>% dir() %>% str_remove("\\.fst")
  days_done = c()
  # Define days to process
  days_todo = setdiff(days_smoke, days_done)
  # Remove 2018030 (missing density; only has plume outlines)
  days_todo %<>% setdiff('20180303')
  # Counts of CBGs by state
  st_fips = copy(cbg_sf)
  setDT(st_fips)
  st_fips[, geometry := NULL]
  st_fips %<>% .[,.N,.(st = str_sub(geoid, 1, 2))]
  setorder(st_fips, -N)
  # Four largest states and then start with smallest
  st_fips = rbindlist(list(
    st_fips[1:4],
    st_fips[.N:5]
  ))
  # Iterate over smoke days
  blah = mclapply(
    X = days_todo,
    FUN = function(d) {
      # Load the smoke shape
      smoke_sf = here(
        'data-raw', 'noaa', d,
        paste0('hms_smoke', d, '.shp')
      ) %>% st_read() %>% st_transform(crs = crs_lambert)
      # Iterate over states
      cbg_smoke = mclapply(
        X = st_fips[,st],
        FUN = function(s) {
          # Match CBGs to smoke by smoke density
          st_dt = st_join(
            x = cbg_sf %>% filter(str_detect(geoid, paste0("^", s))),
            y = smoke_sf,
            join = st_intersects,
            left = TRUE
          )
          # Convert to data table
          setDT(st_dt)
          # Drop missing densities
          st_dt %<>% .[!is.na(Density)]
          # Indicator for whether CBG encountered smoke
          st_dt %>% .[, .(
            smoke_low = fmax(Density == 5),
            smoke_medium = fmax(Density == 16),
            smoke_high = fmax(Density == 27)
          ), by = geoid]
        },
        mc.cores = 5
      ) %>% rbindlist(use.names = T, fill = T)
      # Save!
      write_fst(
        x = cbg_smoke,
        path = here('data-processed', 'cbg-smoke-day', paste0(d, '.fst')),
        compress = 100
      )
      # Clean up
      rm(smoke_sf, cbg_smoke)
      invisible(gc())
      # Export success
      return('success')
    },
    mc.cores = 9
  )