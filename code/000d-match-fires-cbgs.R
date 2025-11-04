# Notes ----------------------------------------------------------------------------------
#   Goal:   Match CBGs to historic fire perimeters
#   Time:   1 minute


# Data notes -----------------------------------------------------------------------------
#   - Fire data source:
#       https://wfdss.usgs.gov/wfdss/wfdss_data_downloads.shtml


# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(parallel, fastverse, geojsonsf, magrittr, here)
  fastverse_extend(topics = c('DT', 'ST', 'SP', 'IO'))
  sf_use_s2(FALSE)
  # Add directory of SafeGraph data
  dir_sg = '/media/edwardrubin/Data/SafeGraph'


# Load data: Census block group GeoJSON --------------------------------------------------
  # Load SafeGraph's CBG file (as 'sf')
  cbg_sf = file.path(
    dir_sg, 'sg-census', 'safegraph_open_census_data_2010_to_2019_geometry', 'cbg.geojson'
  ) %>% geojson_sf()
  # Rename
  cbg_sf %<>% dplyr::transmute(
    state = StateFIPS,
    county = CountyFIPS,
    cbg = CensusBlockGroup
  )
  # Subset to the West Coast
  cbg_sf %<>% dplyr::filter(state %in% c('06', '41', '53'))


# Load data: Fire perimeters, 2010-2019 --------------------------------------------------
  # First set of fires
  fire1 = here(
    'data-raw', 'usgs', 'WFDSSHistoricFirePerimeters_2010_2019',
    'WFDSSHistoricFirePerimeters_2010_2019.shp'
  ) %>% st_read()
  fire1 %<>% janitor::clean_names()
  # Grab desired variables
  fire1 %<>% dplyr::select(id = objectid, fire_year, acres = gis_acres)


# Load data: Fire perimeters, 2020 -------------------------------------------------------
  # Load
  fire2 = here(
    'data-raw', 'usgs', 'WFDSSHistoricFirePerimeters_2020',
    'WFDSSHistoricFirePerimeters_2020.shp'
  ) %>% st_read()
  fire2 %<>% janitor::clean_names()
  # Grab desired variables
  fire2 %<>% dplyr::select(id = objectid, fire_year, acres = gis_acres)


# Load data: Fire perimeters, full history -----------------------------------------------
# NOTE: The 'full history' only fills in 2020 and 2021 (and 2022)
  # Load
  fire3 = here(
    'data-raw', 'usgs', 'WFIGSWildlandFirePerimeters_Full_History',
    'FH_Perimeter.shp'
  ) %>% st_read()
  fire3 %<>% janitor::clean_names()
  # Grab desired variables
  fire3 %<>% dplyr::transmute(
    id = objectid,
    fire_year = irwin_fi_9 %>% ymd() %>% year(),
    acres = poly_gis_ac
  )


# Data work: Combine fire datasets -------------------------------------------------------
  # Combine
  fire_sf = rbindlist(list(
    as.data.table(fire1),
    as.data.table(fire2),
    as.data.table(fire3)
  ), use.names = TRUE, fill = TRUE) %>% st_as_sf()
  # Get centroids
  fire_center = fire_sf %>% st_centroid()
  # Data table of fire centroids
  fire_center = cbind(
    data.table(id = fire_center$id),
    fire_center %>% st_coordinates() %>% as.data.table()
  )
  setnames(fire_center, c('id', 'x', 'y'))
  # Limit to fires longitude to -126 to -112 (CA border with AZ is around 115)
  fire_center %<>% .[between(x, -126, -112)]
  # Limit to fires south of 50th latitude (a bit above the Canadian border; drops AK)
  fire_center %<>% .[y < 50]
  # Apply these restrictions the actual fire perimeters
  fire_sf %<>% dplyr::filter(id %in% fire_center[, id])


# Data work: Find intersections ----------------------------------------------------------
  # Project fires to match CBG CRS
  fire_sf %<>% st_transform(crs = st_crs(cbg_sf))
  # Merge!
  cbg_fire = st_join(
    x = fire_sf,
    y = cbg_sf
  )


# Data work: Summarize CBG-by-fire year --------------------------------------------------
  # Convert to data table
  cbg_dt = data.table::copy(cbg_fire)
  setDT(cbg_dt)
  # Drop geometry
  cbg_dt[, geometry := NULL]
  # Create county FIPS; drop state and county
  cbg_dt[, co := paste0(state, county)]
  cbg_dt[, c('state', 'county') := NULL]
  # Drop observations missing the CBG (fires beyond CA, OR, and WA)
  cbg_dt %<>% .[!is.na(cbg)]
  # Summarize by CBG and fire-year
  cbg_dt %<>% .[, .(
    n_fires = .N,
    total_fire_acres = fsum(acres),
    max_fire_acres = fmax(acres)
  ), by = .(cbg, fire_year)]


# Save data ------------------------------------------------------------------------------
  # Save the dataset
  write_fst(
    x = cbg_dt,
    path = here('data-processed', 'for-analysis', 'cbg-fire-exposure-westcoast.fst'),
    compress = 100
  )
