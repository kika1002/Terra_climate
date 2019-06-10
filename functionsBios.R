
loadRaster <- function(files, var){
  stk <- grep(var, files, value = T) %>% 
    mixedsort() %>%
    stack()
}

makeBiovar <- function(fls, yr){
  # yr <- 1981
  fls.sub <- grep(yr, fls, value = T)
  vars <- c('pet', 'ppt', 'tmax', 'tmean', 'tmin')
  prec <- loadRaster(files = fls.sub, var = 'ppt')
  tmax <- loadRaster(files = fls.sub, var = 'tmax')
  # tmean <- loadRaster(files = fls.sub, var = 'tmean')
  tmin <- loadRaster(files = fls.sub, var = 'tmin')
  pet <- loadRaster(files = fls.sub, var = 'pet')
  
  # for(i in 1:length(vars)){
  #   eval(parse(text = paste0(vars[i], '<- loadRaster(var = vars[', i, '])')))
  # }
  # 
  # bioclim_list <- vector('list', 35)
  
  # Creating the tmean
  tmean <- (tmax + tmin) / 2
  
  # Making the bioclimatics
  bioclim <- biovars(prec, tmin, tmax)
  bioclim <- unstack(bioclim)
  Map('writeRaster', x = bioclim, filename = paste0('./_bios/', yr, '_bio_', 1:19, '.asc'), overwrite = TRUE)
  precmatrix <- as.matrix(prec)
  
  def <- prec - pet
  bio20ies <-  t(apply(precmatrix, 1, cumDry))
  zeroraster <- prec[[1]] * 0
  bio20 <- zeroraster
  values(bio20) <- bio20ies[,1]
  writeRaster(bio20, paste0('./_bios/', yr, '_bio_20.asc'), overwrite = TRUE)
  
  DefAndTemp <- cbind(as.matrix(def), as.matrix(tmean), as.matrix(tmax))
  biovalues <- t(apply(DefAndTemp, 1, cumTemp))
  
  ETPAndTemp <- cbind(as.matrix(pet),as.matrix(tmean))
  etpbios2 <-  t(apply(ETPAndTemp ,1, etpvars2))
  
  ETPAndPrec <- cbind(as.matrix(pet),as.matrix(prec))
  etpbios <-  t(apply(ETPAndPrec,1,etpvars1))
  
  rm(ETPAndPrec)
  rm(DefAndTemp)
  rm(ETPAndTemp)
  
  # Bio 21: Consecutive Months with less Prec than PET
  bio21 <- zeroraster
  values(bio21) <- biovalues[,1]
  writeRaster(bio21, paste0('./_bios/', yr, '_bio_21.asc'), overwrite = T, NAflag = -9999)
  
  # Bio 22: Sum of water deficit during dry season
  bio22 <- zeroraster
  values(bio22) <- biovalues[,2]
  writeRaster(bio22, paste0('./_bios/', yr, '_bio_22.asc'), overwrite = T, NAflag = -9999)
  
  # Bio 23: Mean temperature during growing season
  bio23 <- zeroraster
  values(bio23) <- biovalues[,3]
  writeRaster(bio23, paste0('./_bios/', yr, '_bio_23.asc'), overwrite = T, NAflag = -9999)
  
  # BIO 24: Max dry season temperature
  bio24 <- zeroraster
  values(bio24) <- biovalues[,4]
  writeRaster(bio24, paste0('./_bios/', yr, '_bio_24.asc'), overwrite = T, NAflag = -9999)
  
  # BIO 25: Annual PET
  bio25 <- zeroraster
  values(bio25) <- round(etpbios[,1], digits = 2)
  writeRaster(bio25, paste0('./_bios/', yr, '_bio_25.asc'), overwrite = T, NAflag = -9999)
  
  # Bio 26: PET seasonality (Coefficient of Variation)
  bio26 <- zeroraster
  values(bio26) <- round(etpbios[,2], digits=  2)
  writeRaster(bio26, paste0('./_bios/', yr, '_bio_26.asc'), overwrite = T, NAflag = -9999)
  
  # Bio 27: Max PET
  bio27 <- zeroraster
  values(bio27) <- round(etpbios[,3], digits = 2)
  writeRaster(bio27, paste0('./_bios/', yr, '_bio_27.asc'), overwrite = T, NAflag = -9999)
  
  # Bio 28: Min PET
  bio28 <- zeroraster
  values(bio28) <- round(etpbios[,4], digits = 2)
  writeRaster(bio28, paste0('./_bios/', yr, '_bio_28.asc'), overwrite = T, NAflag = -9999)
  
  # Bio 29: Range of PET (PETmax-PETmin)
  bio29 <- zeroraster
  values(bio29) <- round(etpbios[,5], digits = 2)
  writeRaster(bio29, paste0('./_bios/', yr, '_bio_29.asc'), overwrite = T, NAflag = -9999)
  
  # Bio 30: PET of wettest quarter
  bio30 <- zeroraster
  values(bio30) <- round(etpbios[,6], digits = 2)
  writeRaster(bio30, paste0('./_bios/', yr, '_bio_30.asc'), overwrite = T, NAflag = -9999)
  
  # Bio 31:	PET of driest quarter
  bio31 <- zeroraster
  values(bio31) <- round(etpbios[,7], digits = 2)
  writeRaster(bio31, paste0('./_bios/', yr, '_bio_31.asc'), overwrite = T, NAflag = -9999)
  
  # Bio 32:	PET of warmest quarter
  bio32 <- zeroraster
  values(bio32) <- round(etpbios2[,1], digits = 2)
  writeRaster(bio32, paste0('./_bios/', yr, '_bio_32.asc'), overwrite = T, NAflag = -9999)
  
  # Bio 33:	PET of coldest quarter
  bio33 <- zeroraster
  values(bio33) <- round(etpbios2[,2], digits = 2)
  writeRaster(bio33, paste0('./_bios/', yr, '_bio_33.asc'), overwrite = T, NAflag = -9999)
  
  rm(biovalues)
  
  bioclim_list <- stack(bioclim, bio20, bio21, bio22, bio23, bio24,
                        bio25, bio26, bio27, bio28, bio29, bio30, bio31,
                        bio32, bio33)
  print('Done!')
  # return(bioclim_list)
}
