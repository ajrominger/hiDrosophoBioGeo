# ----
# load needed packages
library(sp)
library(rgeos)
library(lubridate)

# this might need to be installed from github
k <- require(kokua)
if(!k) {
    devtools::install_github('hawaiiDimensions/kokua')
    library(kokua)
}


# ----
# keep a list to use in reporting
keepTrack <- list()

reportCols <- c('REFERENCE', 'SPECIES', 'ISLAND', 'POINT_Y', 'POINT_X',
                'RESERVE', 'TYPE', 'LOCALITY')

# ----
# read data
dataDate <- as.Date('2021-07-02')
keepTrack$dataDate <- dataDate

drosoph <- read.csv(sprintf('inst/raw_data_%s.csv', as.character(dataDate)),
                    as.is = TRUE)

# record keeping
keepTrack$nrowInitial <- nrow(drosoph)
nRecPerSpp <- aggregate(list(nrec = rep(1, nrow(drosoph))),
                        list(species = drosoph$SPECIES),
                        sum)
keepTrack$nRecPerSppInitial <- nRecPerSpp


# ----
# clean location data

# clean up special characters in X and convert to numeric
drosoph$POINT_X <- gsub('−', '-', drosoph$POINT_X)
drosoph$POINT_X <- as.numeric(drosoph$POINT_X)

# fix wrong long
drosoph$POINT_X <- -abs(drosoph$POINT_X)

# clean island names
keepTrack$islandNameOld <- unique(drosoph$ISLAND)

drosoph$ISLAND[grep('Hawa|Big', drosoph$ISLAND)] <- 'Hawaii'

# add info to record keeper
keepTrack$islandNameNew <- unique(drosoph$ISLAND)
keepTrack$geoInitial <- unique(drosoph[, c('ISLAND', 'LOCALITY')])


# ----
# retain only needed columns
colWeWant <- c('REFERENCE', 'SPECIES', 'ISLAND', 'POINT_Y', 'POINT_X',
               'RESERVE', 'TYPE', 'LOCALITY', 'ELEV_FT', 'ACC', 'DATE_',
               'YEAR', 'NUM', 'COLLECTOR', 'NOTES', 'GROUP', 'SUBGROUP')
drosoph <- drosoph[, colWeWant]

# ----
# trim whitespace
drosoph$REFERENCE <- trimws(drosoph$REFERENCE)
drosoph$SPECIES <- trimws(drosoph$SPECIES)
drosoph$ISLAND <- trimws(drosoph$ISLAND)
drosoph$LOCALITY <- trimws(drosoph$LOCALITY)
drosoph$GROUP <- trimws(drosoph$GROUP)
drosoph$SUBGROUP <- trimws(drosoph$SUBGROUP)


# ----
# fix species names

# map of incorrect names to correct names
nameFix <- data.frame(old_name = c('silvestrs'),
                      new_name = c('silvestris'))

# replace incorrect names with correct
newName <- nameFix$new_name[match(drosoph$SPECIES, nameFix$old_name)]
newName[is.na(newName)] <- drosoph$SPECIES[is.na(newName)]

drosoph$SPECIES <- newName

# add to record keeping
keepTrack$nameFix <- nameFix


# ----
# clean up dates

# place-holders
stubDate <- as.Date('1000-01-01')
cleanDate <- rep(stubDate, nrow(drosoph))

# indices for the different formats that dates are in
slashInd <- grep('/', drosoph$DATE_)
dashInd <- grep('-', drosoph$DATE_)

# populate clean dates vector
cleanDate[slashInd] <- as.Date(drosoph$DATE_[slashInd], format = '%m/%d/%y')
cleanDate[dashInd] <- as.Date(drosoph$DATE_[dashInd], format = '%d-%b-%y')

# check for cases where parsed year is in future and correct
badYear <- year(cleanDate) > year(dataDate)
year(cleanDate[badYear]) <- year(cleanDate[badYear]) - 100

# record keeping
keepTrack$dateHash <- unique(cbind(drosoph$DATE_, as.character(cleanDate)))

# add back to data.frame
drosoph$DATE_ <- cleanDate

# change name of date column
names(drosoph)[names(drosoph) == 'DATE_'] <- 'DATE'

# make clean year column
drosoph$YEAR <- year(drosoph$DATE)

# remove entries with no date (with record keeping)
keepTrack$noDate <- drosoph[drosoph$DATE == stubDate, reportCols]
drosoph <- drosoph[drosoph$DATE != stubDate, ]

# record initial collection frequency by year
nRecPerYr <- aggregate(list(nrec = rep(1, nrow(drosoph))),
                       list(year = drosoph$YEAR),
                       sum)
keepTrack$nRecPerYrInitial <- nRecPerYr


# ----
# de-duplicate
keyCol <- c('SPECIES', 'ISLAND', 'POINT_Y', 'POINT_X', 'LOCALITY', 'DATE')
drosoph <- drosoph[!duplicated(drosoph[, keyCol]), ]

keepTrack$keyCol <- keyCol
keepTrack$nrowDeDup <- nrow(drosoph)


# ----
# drop records with bad accuracy and then drop accuracy column
drosoph <- drosoph[drosoph$ACC != 'C', ]
drosoph <- drosoph[, names(drosoph) != 'ACC']

keepTrack$nrowBadACC <- nrow(drosoph)


# ----
# make into spatial points data frame
coordsInd <- c(grep('POINT_X', names(drosoph)), grep('POINT_Y', names(drosoph)))

# the projection
p4s <- '+proj=longlat +datum=WGS84 +no_defs'

# make spatial object
drosoph <- SpatialPointsDataFrame(coords = drosoph[, coordsInd], data = drosoph,
                                  coords.nrs = coordsInd,
                                  proj4string = CRS(p4s))

# # ----
# check that coordinates are accurate at least to island

# outlines of Hawaiian islands, projected to be same as `drosoph`
data('islands', package = 'kokua')
islands <- spTransform(islands, CRS(proj4string(drosoph)))

# loop over islands anch check points within polys
islandCheck <- lapply(unique(drosoph$ISLAND), function(i) {
    # extract one island from data
    d <- coordinates(drosoph[drosoph$ISLAND == i, ])
    d <- SpatialPoints(d, CRS(proj4string(drosoph)))

    # see if those points fall within the island polygon
    o <- gWithin(d, islands[grep(i, islands$island, ignore.case = TRUE), ],
                 byid = TRUE)

    # return data.frame with point ID and boolean
    return(data.frame(ID = colnames(o), within = as.logical(o)))
})

islandCheck <- do.call(rbind, islandCheck)

# retain only those that passed the island check (with record keeping)
keepTrack$outsideIsland <- drosoph@data[islandCheck$ID[!islandCheck$within],
                                        reportCols]

drosoph <- drosoph[islandCheck$ID[islandCheck$within], ]


# ----
# save to /data
save(drosoph, file = 'data/drosoph.RData')

# data object record keeping
keepTrack$proj <- proj4string(drosoph)
keepTrack$class <- class(drosoph)

# final records per spp
nRecPerSpp <- aggregate(list(nrec = rep(1, nrow(drosoph))),
                        list(species = drosoph$SPECIES),
                        sum)
keepTrack$nRecPerSppFinal <- nRecPerSpp

names(keepTrack$nRecPerSppInitial)[2] <- 'nrec_initial'
names(keepTrack$nRecPerSppFinal)[2] <- 'nrec_final'

keepTrack$perSpp <- with(keepTrack, merge(nRecPerSppInitial, nRecPerSppFinal))

# final records per year
nRecPerYr <- aggregate(list(nrec = rep(1, nrow(drosoph))),
                       list(year = drosoph$YEAR),
                       sum)
keepTrack$nRecPerYrFinal <- nRecPerYr

names(keepTrack$nRecPerYrInitial)[2] <- 'nrec_initial'
names(keepTrack$nRecPerYrFinal)[2] <- 'nrec_final'

keepTrack$perYr <- with(keepTrack, merge(nRecPerYrInitial, nRecPerYrFinal))

# final localities
keepTrack$geoFinal <- unique(drosoph@data[, c('ISLAND', 'LOCALITY')])
geoTemp <- rbind(keepTrack$geoInitial, keepTrack$geoFinal)
geoTemp <- geoTemp[order(geoTemp$ISLAND), ]
keepTrack$geoLost <- geoTemp[!(duplicated(geoTemp) |
                                   duplicated(geoTemp, fromLast = TRUE)), ]

# ----
# write data processing report

# save record keeping object
save(keepTrack, file = 'inst/keepTrack.RData')

# render report
rmarkdown::render('inst/data_cleaning_report.Rmd')

# remove record keeping object (it was only needed for rendering the report)
system('rm inst/keepTrack.RData')
