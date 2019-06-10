
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, tidyverse, stringr, rgeos, gtools, velox, sf, foreach, doSNOW)

# Initial setup 
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Function
extMskNC <- function(vr, yr){
  # vr <- 'tmin'
  # yr <- 1981
  fle <- grep(vr, fls, value = T) %>%
    grep(yr, ., value = T)
  bck <- raster::brick(fle)
  nms <- names(bck) %>% gsub('\\.', '_', .)
  print(paste0('To convert to velox object ', vr, ' ', yr))
  vlx <- velox(bck)
  vlx$crop(ext)
  stk <- vlx$as.RasterStack()
  lyr <- unstack(stk)
  Map('writeRaster', x = lyr, filename = paste0('../_2019/_asc/_bsn/', vr, '_', nms, '.asc'), overwrite = TRUE)
  print(paste0('To convert to a table ', vr, ' ', yr))
  coords <- vlx$getCoordinates() %>%
    as_data_frame() %>%
    setNames(c('Lon', 'Lat'))
  sp <- SpatialPoints(coords)
  vls <- vlx$extract_points(sp = sp) %>%
    cbind(coordinates(sp)) %>%
    as.tibble() %>%
    setNames(c(month.name, sp %>% coordinates %>% colnames)) %>%
    mutate(yr = yr,
           vr = vr,
           id = 1:nrow(.)) %>%
    gather(month, value, -Lon, -Lat, -yr, -vr, -id)
  write_csv(vls, paste0('../_2019/_tbl/_vls/', vr, '_', yr, 'cabuyaro.csv'))
  print('Done')
  return(vls)
}

# Load data
fls <- list.files('//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_guatemala/_data/_nc/_world', full.names = TRUE, pattern = '.nc$')
bsn <- shapefile('../_2019/basins_geo.shp')
ext <- extent(bsn)
vrs <- c('pet', 'ppt', 'tmax', 'tmin')
yrs <- basename(fls) %>% readr::parse_number() %>% unique()

# Potential Evapotranspiration
cl <- makeCluster(length(yrs)/2)
registerDoSNOW(cl)
# registerDoMC(length(yrs)/2)
pet <- foreach(i = 1:length(yrs), .packages = c('raster', 'rgdal', 'tidyverse', 'velox'), .verbose = TRUE) %dopar% {
  extMskNC(vr = 'pet', yr = yrs[i])
}
stopCluster(cl)

# Precipitation
cl <- makeCluster(length(yrs)/2)
registerDoSNOW(cl)
ppt <- foreach(i = 1:length(yrs), .packages = c('raster', 'rgdal', 'tidyverse', 'velox'), .verbose = TRUE) %dopar% {
  extMskNC(vr = 'ppt', yr = yrs[i])
}
stopCluster(cl)

# Maximum temperature
cl <- makeCluster(length(yrs)/2)
registerDoSNOW(cl)
tmax <- foreach(i = 1:length(yrs), .packages = c('raster', 'rgdal', 'tidyverse', 'velox'), .verbose = TRUE) %dopar% {
  extMskNC(vr = 'tmax', yr = yrs[i])
}
stopCluster(cl)

# Minimum temperature
cl <- makeCluster(length(yrs)/2)
registerDoSNOW(cl)
tmin <- foreach(i = 1:length(yrs), .packages = c('raster', 'rgdal', 'tidyverse', 'velox'), .verbose = TRUE) %dopar% {
  extMskNC(vr = 'tmin', yr = yrs[i])
}
stopCluster(cl)



