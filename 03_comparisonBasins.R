

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, purrr, readxl)

# Initial setup  ----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)
geo <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'

# Functions to create -----------------------------------------------------
makeGraph <- function(df, vr){
  # df <- znl_vls
  # vr <- 'bio_1'
  df_sub <- df %>%
    filter(variable == vr)
  nme <- bios %>%
    filter(nme == vr) %>%
    pull(2)
  gg <- ggplot(data = df_sub, aes(x =  year, y = value, group = basin, colour = basin)) +
    geom_line(size = 1.1) +
    theme(legend.position = 'top', 
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),  
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          legend.title = element_blank(),
          legend.text = element_text(size = 10)) +
    ylab(nme) +
    xlab('') +
    scale_color_manual(values=c("#181716", "#B5B2AE"))
  ggsave(plot = gg, filename = paste0('../_png/_bio/gg_', vr, '.png'), units = 'cm', width = 12, height = 9, dpi = 300)
  print('Done!!!')
}

# Load data ---------------------------------------------------------------
bsn <- shapefile('../_shp/_base/basins_prj.shp')
bsn_bff <- shapefile('../_shp/_base/basins_bff_geo.shp')
fls <- list.files('../_tif/_bio', full.names = TRUE, pattern = '.asc$')

bios <- read_excel('../_tbl/bioclimatic_variables.xlsx')[,2:3]

tua <- bsn[bsn@data$name %in% 'Tua',]
cby <- bsn[bsn@data$name %in% 'Cabuyaro',]

# Data process ------------------------------------------------------------
# Stack data
stk <- stack(fls)
bsn <- spTransform(x = bsn, CRSobj = geo)

# Crop and mask
stk_cut <- raster::crop(stk, bsn_bff) %>% raster::mask(bsn_bff)
msk <- stk_cut[[1]] * 0 + 1

# Rasterize the basin's shapefile
bsn_bff@data$gid <- 1:2
bsn_msk <- rasterize(bsn_bff, msk, field = 'gid')

# Zonal statistics (it retunrs a dataframe with the mean of the bioclimatic variables)
tst <- raster::zonal(stk_cut[[1]], bsn_msk, fun = 'mean')
znl_vls <- raster::zonal(stk_cut, bsn_msk, fun = 'mean')

# Tidy the dataframe
znl_vls <- znl_vls %>%
  as.data.frame() %>% 
  mutate(basin = c('Cabuyaro', 'Tua')) %>%
  as_data_frame()
znl_vls <- znl_vls %>% 
  gather(var, value, -basin, -zone)

vrs <- paste0('bio_', 1:33)
znl_vls <- znl_vls %>%
  mutate(year = str_sub(var, start = 2, end = 5) %>% as.numeric(),
         variable = str_sub(var, start = 16, end = nchar(var))) %>%
  dplyr::select(basin, variable, year, value) %>%
  mutate(variable = factor(variable, levels = vrs),
         basin = as.factor(basin))
bios$nme <- paste0('bio_', 1:33)

write.csv(znl_vls, '../_tbl/_bio/zonal_values_basins_bios.csv', row.names = FALSE)

znl_vls_2 <- znl_vls %>% 
  spread(key = basin, value = value) %>%
  mutate(difference = Tua - Cabuyaro)

smm_vls <- znl_vls_2 %>%
  group_by(variable) %>%
  summarize(diff = mean(difference)) %>%
  ungroup()

write.csv(znl_vls_2, '../_tbl/_bio/zonal_values_basins_bios_difference.csv', row.names = FALSE)
write.csv(smm_vls, '../_tbl/_bio/summary_difference_bios.csv', row.names = FALSE)

# To create the graphs ----------------------------------------------------
ggs <- lapply(1:length(vrs), function(k) makeGraph(df = znl_vls, vr = vrs[k]))












