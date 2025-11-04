# Notes ----------------------------------------------------------------------------------
#   Goal:   Find centroids of all SafeGraph CBGs


# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(parallel, fastverse, geojsonsf, magrittr, here)
  fastverse_extend(topics = c('DT', 'ST', 'SP', 'IO'))
  # Add directory of SafeGraph data
  dir_sg = '/media/edwardrubin/Data/SafeGraph'


# Load data: Census block group GeoJSON --------------------------------------------------
  # Load SafeGraph's CBG file (as 'sf')
  cbg_sf = file.path(
    dir_sg, 'sg-census', 'safegraph_open_census_data_2010_to_2019_geometry', 'cbg.geojson'
  ) %>% geojson_sf()


# Data work: Centroids -------------------------------------------------------------------
  # Calculate centroids
  cbg_centroids = cbg_sf %>% dplyr::select(cbg = CensusBlockGroup) %>% st_centroid()
  # Save centroids dataset
  qsave(
    x = cbg_centroids,
    file = here('data-processed', 'geo-summaries', 'sg-cbg-centroids.qs'),
    preset = 'high'
  )
  # Drop polygon dataset and clean up
  rm(cbg_sf)
  invisible(gc())


# Data work: Distances between centroids -------------------------------------------------
  # Load centroids
  cbg_centroids = here(
    'data-processed', 'geo-summaries', 'sg-cbg-centroids.qs'
  ) %>% qread()
  # Convert centroid lat/lon to data table
  cbg_dt = cbg_centroids
  setDT(cbg_dt)
  # Load centroids again
  cbg_centroids = here(
    'data-processed', 'geo-summaries', 'sg-cbg-centroids.qs'
  ) %>% qread()
  # Bind coordinates
  cbg_dt = cbind(
    cbg_dt,
    cbg_centroids %>% st_coordinates() %>% as.data.table()
  )
  # Drop geometry
  cbg_dt[, geometry := NULL]
  # Rename columns
  setnames(cbg_dt, old = c('X', 'Y'), new = c('lon', 'lat'))
  # Save
  write_fst(
    x = cbg_dt,
    path = here('data-processed', 'geo-summaries', 'sg-cbg-centroids-dt.fst'),
    compress = 100
  )