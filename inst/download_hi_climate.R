# load needed packages
# ----

library(raster)

# all non-precip climate variables
# ----

baseURL <- 'http://evapotranspiration.geography.hawaii.edu/assets/files/ASCIIFiles/'


# relative URLs for different climate data
urlz <- readLines('inst/hi_urlz.txt')

# set up a temporary directory
d <- tempdir()

# download all data files
download.file(paste0(baseURL, urlz),
              destfile = file.path(d, urlz),
              method = 'libcurl')

# see if anything didn't work and try it again
wtf <- warnings()
wtf <- sapply(urlz, grepl, x = paste(names(unclass(wtf)), collapse = ''))
if(any(wtf)) {
    tryagain <- urlz[wtf]

    download.file(paste0(baseURL, tryagain),
                  destfile = file.path(d, tryagain),
                  method = 'libcurl')
}

# unpack compressed data files
system(sprintf('cp inst/download_hi_climate.sh %s', d))
system(sprintf('cd %s; sh download_hi_climate.sh', d))



# names of files to read-in
ff <- file.path(d, gsub('.rar', '', urlz))

# extract only file names pertaining to annual data
x <- sapply(ff, function(f) {
    x <- list.files(f, pattern = '\\.txt', full.names = TRUE)
    if(length(x) < 1) {
        return(NA)
    } else if(length(x) == 1) {
        return(x)
    } else {
        return(grep('ann', x, value = TRUE))
    }
})

x <- x[!is.na(x)]

# wind speed is hourly so deal with the separately
i <- which(sapply(x, function(w) any(grepl('wind_sd', w))))
wind <- x[[i]]
x <- x[-i]

wind <- stack(wind)
wind <- calc(wind, fun = mean, na.rm = TRUE)

# now stack all raters
hiClim <- stack(x)
hiClim <- addLayer(hiClim, wind)
names(hiClim) <- c(gsub('.*/|\\.txt', '', unlist(x)), 'wind')




# precip
# ----

precipBase <- 'http://rainfall.geography.hawaii.edu/assets/files/GISLayers/'
precip <- 'StateASCIIGrids_mm.zip'

# download and unzip
download.file(paste0(precipBase, precip),
              destfile = file.path(d, precip))
unzip(file.path(d, precip), exdir = file.path(d, 'precip'))

# path to annual precip data
p <- list.files(file.path(d, 'precip'), pattern = 'ann.txt', full.names = TRUE)
hiPrecip <- raster(p)

# add to other climate data
oldnames <- names(hiClim)
hiClim <- addLayer(hiClim, hiPrecip)
names(hiClim) <- c(oldnames, 'precip_mm')


# write out data
# ----

# write raw raster
writeRaster(hiClim, filename = 'inst/hiClim.tiff', driver = 'GeoTiff')

# read back in data as brick
hiClimBrick <- brick('inst/hiClim.tiff')

# save brick object to data
save(hiClimBrick, file = 'data/hiClimBrick.rda')

