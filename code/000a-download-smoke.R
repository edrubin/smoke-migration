# Notes ----------------------------------------------------------------------------------
#   Goal:   Download NOAA smoke shapefiles


# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(stringr, lubridate, here, magrittr)
  # Option for download timeout
  options(timeout = 10)


# Function: Download smoke data ----------------------------------------------------------
  # Create the function
  func_query = function(date) {
    y = date |> year()
    m = date |> month() |> str_pad(2, 'left', 0)
    d = date |> day() |> str_pad(2, 'left', 0)
    datestring = paste0(y, m, d)
    # URL of NOAA site
    smoke_shape_url = paste0(
      'https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/',
      y,
      '/',
      m,
      '/hms_smoke',
      datestring,
      '.zip'
    )
    # Download just the shape file
    download.file(
      smoke_shape_url,
      here('data-raw', 'noaa', 'smoke-tmp.zip')
    )
    # Unzip
    unzip(
      here('data-raw', 'noaa', 'smoke-tmp.zip'),
      exdir = here('data-raw', 'noaa', datestring)
    )
    #
    unlink(here('data-raw', 'noaa', 'smoke-tmp.zip'))
    # Return
    return('success')
  }


# Download smoke data --------------------------------------------------------------------
  # Iterate over target dates
  date_v = seq.Date(
    from = ymd('20171201'),
    to = ymd('20211231'),
    by = '1 day'
  )
  tmp = lapply(
    X = date_v,
    FUN = function(i) tryCatch(func_query(i), error = function(e) NULL)
  )
  # Try missing dates
  date_na = here('data-raw', 'noaa') %>% dir() %>% setdiff(str_remove_all(date_v, '-'), .)
  tmp = lapply(
    X = date_na,
    FUN = function(i) tryCatch(func_query(i), error = function(e) NULL)
  )
