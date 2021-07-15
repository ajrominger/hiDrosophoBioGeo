# ----
# load and clean phylogeny

library(ape)

tre <- read.tree('inst/magnacca_price_2015_tree.new')

# clean up names
tre$tip.label <- gsub('\\.', '_', tre$tip.label)
tre$tip.label <- gsub('#.*', '', tre$tip.label)

# drop multi-island tips (only Mo and Ma get multiple tips so just drop one)
tre <- drop.tip(tre, grep('Ma', tre$tip.label))

# remove island signifier
tre$tip.label <- gsub('Mo', '', tre$tip.label)

# extract picture wing clade
tre <- extract.clade(tre, 167) # extract at 168 to remove primaeva as well


# ----
# match to names in `drosoph`

# load spatial data
load('data/drosoph.RData')

# extract unique names
spp <- unique(drosoph$SPECIES)

# tips not in spatial data
treNotInDat <- sort(tre$tip.label[!(tre$tip.label %in% spp)])

# spatial data not in tree
datNotInTre <- sort(spp[!(spp %in% tre$tip.label)])

# combine the two for visual comparison
lets <- sort(unique(substr(c(treNotInDat, datNotInTre), 1, 1)))

miss <- lapply(lets, function(l) {
    x <- datNotInTre[substr(datNotInTre, 1, 1) == l]
    y <- treNotInDat[substr(treNotInDat, 1, 1) == l]

    n <- length(x) - length(y)

    if(n < 0) {
        x <- c(x, rep('', -n))
    } else {
        y <- c(y, rep('', n))
    }

    cbind(x, y)
})

miss <- do.call(rbind, miss)

colnames(miss) <- c('Present in spatial data not tree',
                    'Present in tree not spatial data')


# save info for reporting
keepTrack <- list(miss = miss)

# evaluate how many
keepTrack$nlost <- mean(!(drosoph$SPECIES %in% tre$tip.label))

# ----
# write data processing report

# save record keeping object
save(keepTrack, file = 'inst/keepTrack.RData')

# render report
rmarkdown::render('inst/phylo_cleaning_report.Rmd')

# remove record keeping object (it was only needed for rendering the report)
system('rm inst/keepTrack.RData')
