

# Notes ----------------------------------------------------------------------------------
#   Goals:
#     1. Find all placekeys in SafeGraph data.
#     2. Include placekeys' coordinates (lat/lon).
#     3. Flag placekeys whose coordinates move more than 0.1 degrees.
#   Time: 4 minutes (using 48 cores)


# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(parallel, fastverse, fst, magrittr, here)
  fastverse_extend(topics = c('DT', 'ST'))
  # Fix collapse's F issue
  F = FALSE
  # Add directory of SafeGraph data
  dir_sg = '/media/edwardrubin/Data/SafeGraph'


# Load data: POI geographic data ---------------------------------------------------------
  # Find the data
  files = file.path(
    dir_sg, 'core', 'core_poi'
  ) %>% dir(
    pattern = 'csv.gz',
    full.names = TRUE,
    recursive = TRUE
  )
  # Read in the 'core' POI data
  poi_dt = mclapply(
    X = files,
    FUN = function(i) {
      # Read the file
      i_dt = fread(
        i,
        nThread = 1,
        select = c('placekey', 'latitude', 'longitude'),
        colClasses = c(
          'placekey' = 'factor',
          'latitude' = 'numeric',
          'longitude' = 'numeric'
        )
      )
      # Add the file's date
      i_dt[, file_date := str_extract(i, '202[01]/[0-9]{2}/[0-9]{2}') %>% ymd()]
      # Return the dataset
      return(i_dt)
    },
    mc.cores = 48
  ) %>% rbindlist(use.names = TRUE, fill = TRUE)


# Data work: Aggregate to POI level ------------------------------------------------------
  # Force uniqueness
  poi_dt %<>% funique()
  # Take the min/mean/median/mode/max/last of latitude and longitude (by placekey)
  setkey(poi_dt, placekey, file_date)
  poi_sum = collap(
    X = poi_dt,
    FUN = list(fmin, fmean, fmedian, fmode, fmax, flast),
    longitude + latitude ~ placekey,
    parallel = TRUE,
    mc.cores = 12
  )
  # Flag POIs where the min and max latitude differ by more than 0.01 degrees (~1 km)
  poi_sum[, flag_lat := abs(fmin.latitude - fmax.latitude) > 0.01]
  # Repeat flag for longitude
  poi_sum[, flag_lon := abs(fmin.longitude - fmax.longitude) > 0.01]
# NOTE: Flags 27,722 POI (49.8% are flagged for both lat and lon)
  # Focus on the median lat and lon
  poi_sum %<>% .[, .(
    placekey,
    lat = fmedian.latitude,
    lon = fmedian.longitude,
    flag_lat,
    flag_lon
  )]
  

# Save data ------------------------------------------------------------------------------
  # Save 
  write_fst(
    x = poi_sum,
    path = here('data-processed', 'geo-summaries', 'placekey-coordinates.fst'),
    compress = 100
  )