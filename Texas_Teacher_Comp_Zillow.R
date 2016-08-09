################################################################################
# Texas_Teacher_Comp_Zillow.R                                                  #
# Contact: ian@blincoe.xyz                                                     #
# R version 3.3.1                                                              #
# Description: Get Zillow Index data and plot results                          #
################################################################################

src.dir <- 'C:/Users/blincoeshirey/Documents/Ian/Analysis/Texas-Teacher-Comp'
working.dir <- 'C:/Users/blincoeshirey/Documents/Ian/Analysis/Texas-Teacher-Comp-WD'

setwd(working.dir)

## View ZHVI at metro level, city-level ZHVI does not have a single entry for Ft. Worth or Houston
zillow.metro.url.stub <- 'http://files.zillowstatic.com/research/public/Metro/'

download.file(paste0(zillow.metro.url.stub, 'Metro_Zhvi_AllHomes.csv'), 'ZHVI.csv')

zhvi.dat <- read.csv('ZHVI.csv', stringsAsFactors = FALSE)

keep.metro <- c('Dallas-Fort Worth, TX', 'Houston, TX', 'San Antonio, TX', 'Austin, TX', 'El Paso, TX')
zhvi.dat <- zhvi.dat[zhvi.dat$RegionName %in% keep.metro, ]

barplot(zhvi.dat$X2016.06)
barplot(zri.dat$X2016.06)


## Grab ZRI at the metro level
download.file(paste0(zillow.metro.url.stub, 'Metro_Zri_AllHomesPlusMultifamily.csv'), 'ZRI.csv')

zri.dat <- read.csv('ZRI.csv', stringsAsFactors = FALSE)

keep.metro <- c('Dallas-Fort Worth, TX', 'Houston, TX', 'San Antonio, TX', 'Austin, TX', 'El Paso, TX')
zri.dat <- zri.dat[zri.dat$RegionName %in% keep.metro, ]


## ZHVI Plot
metro.plot.names <- c('Dallas / Ft. Worth', 'Houston', 'San Antonio', 'Austin', 'El Paso')

zhvi.plot.path <- paste0(src.dir, '/Plots/ZHVIBarplot.png')
png(file = zhvi.plot.path, width = 650, height = 350)
zhvi.plot <- barplot(zhvi.dat$X2016.06,
                     yaxt = 'n', names.arg = metro.plot.names,
                     ylim = c(0, max(zhvi.dat$X2016.06) * 1.1),
                     col = 'grey40', space = 0.65)
midpoints <- zhvi.plot
text(x = midpoints[, 1],
     y = zhvi.dat$X2016.06 + 10000,
     labels = paste0(sprintf('$%.1f', zhvi.dat$X2016.06 / 1000), 'K'))
abline(h = 0)

dev.off()


## ZRI Plot
zri.plot.path <- paste0(src.dir, '/Plots/ZRIBarplot.png')
png(file = zri.plot.path, width = 650, height = 350)
zri.plot <- barplot(zri.dat$X2016.06,
                     yaxt = 'n', names.arg = metro.plot.names,
                     ylim = c(0, max(zri.dat$X2016.06) * 1.1),
                     col = 'grey40', space = 0.65)
midpoints <- zri.plot
text(x = midpoints[, 1],
     y = zri.dat$X2016.06 + 100,
     labels = sprintf('$%.0f', zri.dat$X2016.06))
abline(h = 0)

dev.off()
