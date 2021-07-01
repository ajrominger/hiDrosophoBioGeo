# ----
# load needed packages
library(sp)
# library(rgeos)
library(lubridate)

# # this might need to be installed from github
# k <- require(kokua)
# if(!k) {
#     devtools::install_github('hawaiiDimensions/kokua')
#     library(kokua)
# }


# ----
# read data
drosoph <- read.csv('inst/raw_data_2021-06-30.csv', as.is = TRUE)


# ----
# clean location data

# clean up special characters in X and convert to numeric
drosoph$POINT_X <- gsub('−', '-', drosoph$POINT_X)
drosoph$POINT_X <- as.numeric(drosoph$POINT_X)

# fix wrong long
drosoph$POINT_X <- -abs(drosoph$POINT_X)


# ----
# retain only needed columns
colWeWant <- c('REFERENCE', 'SPECIES', 'ISLAND', 'POINT_Y', 'POINT_X',
               'RESERVE', 'TYPE', 'LOCALITY', 'ELEV_FT', 'ACC', 'DATE_',
               'YEAR', 'RECENT', 'NUM', 'COLLECTOR', 'NOTES', 'GROUP',
               'SUBGROUP')
drosoph <- drosoph[, colWeWant]

# ----
# clean up dates

# to check if parsed dates are in the future
dataDate <- as.Date('2021-06-30')

# place-holders
stubDate <- as.Date('1000-01-01')
cleanDate <- rep(stubDate, nrow(drosoph))

# indeces for the different formats dates are in
slashInd <- grep('/', drosoph$DATE_)
dashInd <- grep('-', drosoph$DATE_)

# populate clean dates vector
cleanDate[slashInd] <- as.Date(drosoph$DATE_[slashInd], format = '%m/%d/%y')
cleanDate[dashInd] <- as.Date(drosoph$DATE_[dashInd], format = '%d-%b-%y')

# check for cases where parsed year is in future and correct
badYear <- year(cleanDate) > year(dataDate)
year(cleanDate[badYear]) <- year(cleanDate[badYear]) - 100

drosoph[cleanDate == stubDate, ]

# add back to data.frame
drosoph$DATE_ <- cleanDate

# change name of date column
names(drosoph)[names(drosoph) == 'DATE_'] <- 'DATE'

# make clean year column
drosoph$YEAR <- year(drosoph$DATE)

# remove entries with no date
drosoph <- drosoph[drosoph$DATE != stubDate, ]

# ----
# de-duplicate
keyCol <- c('SPECIES', 'ISLAND', 'POINT_Y', 'POINT_X', 'LOCALITY', 'DATE')
drosoph <- drosoph[!duplicated(drosoph[, keyCol]), ]


# ----
# drop records with bad accuracy and then drop accuracy column
drosoph <- drosoph[drosoph$ACC != 'C', ]
drosoph <- drosoph[, names(drosoph) != 'ACC']


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
# # check that coordinates are accurate at least to island
# coordinates(drosoph)
#
# gWithin(drosoph, spTransform(islands, CRS(proj4string(drosoph))))


# ----
# save to /data

save(drosoph, file = 'data/drosoph.RData')
