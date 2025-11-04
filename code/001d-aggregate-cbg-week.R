# Notes ----------------------------------------------------------------------------------
#   Goal:   Combine (stack) individual weekly CBG-level datasets into one dataset
#   Time:   1 minute


# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(parallel, fastverse, fst, magrittr, here)
  fastverse_extend(topics = c('DT', 'ST'))
  # Add directory of SafeGraph data
  dir_sg = '/media/edwardrubin/Data/SafeGraph'


# Load data: Weekly CBG datasets ---------------------------------------------------------
  # Find the files
  files = here('data-processed', 'cbg-smoke-week') |> dir(full.names = TRUE)
  # Load them
  full_dt =
    mclapply(
      X = files,
      FUN = read_fst,
      as.data.table = TRUE,
      mc.cores = 32
    ) |>
    rbindlist(use.names = TRUE, fill = TRUE)


# Save -----------------------------------------------------------------------------------
  # Save the dataset
  write_fst(
    x = full_dt,
    path = here('data-processed', 'for-analysis', 'cbg-visit-smoke-week.fst'),
    compress = 100
  )
