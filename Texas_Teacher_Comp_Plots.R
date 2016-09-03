################################################################################
## Texas_Teacher_Comp_Plots.R                                                 ##
## Contact: ian@blincoe.xyz                                                   ##
## R version 3.3.1                                                            ##
## Description: Compile master dataset and generate plots                     ##
################################################################################

library('sqldf')

source.dir <- 'C:/Users/blincoeshirey/Documents/Ian/Analysis/Texas-Teacher-Comp'
working.dir <- 'C:/Users/blincoeshirey/Documents/Ian/Analysis/Texas-Teacher-Comp-WD'

setwd(working.dir)

load.csv <- function(filename) {
    file.path <- paste0(source.dir, '/Data/', filename, '.csv')
    out.file <- read.csv(file.path, stringsAsFactors = FALSE)
    return(out.file)
}

## Load base dataset
TEA.dat <- load.csv('TEADistrictSalaryRecapture')

## Load and merge updated recapture info
UpdatedRecapture.dat <- load.csv('UpdatedRecapture')
UpdatedRecapture.dat$RecaptureAmount <- as.numeric(sub('\\$', '', gsub(',', '', UpdatedRecapture.dat$RecaptureAmount)))
TEA.dat <- merge(TEA.dat, UpdatedRecapture.dat[ , c('DistrictID', 'SchoolYear', 'RecaptureAmount')], all.x = TRUE)
TEA.dat$RecapturePaid <- ifelse(is.na(TEA.dat$RecaptureAmount), TEA.dat$RecapturePaid, TEA.dat$RecaptureAmount)

## Load and merge AltCWI info
AltCWI <- load.csv('AltCWI')
AltCWIAustin <- AltCWI[AltCWI$POWPUMA == 5400, ]
AltCWIAustin$POWPUMA <- 5300
AltCWIAustin$AltCWI <- 1
AltCWI <- rbind(AltCWI, AltCWIAustin)
SchoolYearIndexYear <- load.csv('SchoolYearIndexYear')
AltCWI <- merge(AltCWI, SchoolYearIndexYear)
MapCountyPOWPUMA <- load.csv('MapCountyPOWPUMA')
AltCWI$POWPUMAType <- ifelse(AltCWI$IndexYear <= 2011, 'POWPUMA2K', 'POWPUMA12')

AltCWI <- merge(AltCWI, MapCountyPOWPUMA)

AltCWI$lAltCWI <- log(AltCWI$AltCWI)

StateLevelAltCWIQuery <- 'select SchoolYear, sum(lAltCWI * Weight) / sum(Weight) as lStateLevelAltCWI
                from AltCWI group by SchoolYear'
StateLevelAltCWI <- sqldf(StateLevelAltCWIQuery)
StateLevelAltCWI$StateLevelAltCWI <- exp(StateLevelAltCWI$lStateLevelAltCWI)
StateLevelAltCWI <- StateLevelAltCWI[ , c('SchoolYear', 'StateLevelAltCWI')]

AltCWIQuery <- 'select CountyName, SchoolYear, sum(lAltCWI * Weight) / sum(Weight) as lAltCWI
                from AltCWI group by CountyName, SchoolYear'
AltCWI <- sqldf(AltCWIQuery)
AltCWI$AltCWI <- exp(AltCWI$lAltCWI)

MapDistrictCounty <- load.csv('MapDistrictCounty')
AltCWI <- merge(AltCWI, MapDistrictCounty)
AltCWI <- AltCWI[, c('DistrictID', 'SchoolYear', 'AltCWI')]
TEA.dat <- merge(TEA.dat, AltCWI, all.x = 'TRUE')
TEA.dat <- merge(TEA.dat, StateLevelAltCWI)
TEA.dat$AltCWI <- ifelse(is.na(TEA.dat$AltCWI), TEA.dat$StateLevelAltCWI, TEA.dat$AltCWI)


## Load, transform, and merge Zillow info
ZillowIndex <- load.csv('ZillowIndex')
IndexMonthSchoolYear <- load.csv('IndexMonthSchoolYear')

Zillow <- TEA.dat[ , c('SchoolYear', 'DistrictID')]
Zillow <- merge(Zillow, IndexMonthSchoolYear)
Zillow <- merge(Zillow, MapDistrictCounty)
Zillow <- Zillow[ , c('SchoolYear', 'IndexMonth', 'CountyName')]
Zillow <- unique(Zillow)

## State
ZRIState <- ZillowIndex[ZillowIndex$RegionType == 'State' & ZillowIndex$IndexType == 'ZRI', c('IndexMonth', 'IndexValue')]
ZHVIState <- ZillowIndex[ZillowIndex$RegionType == 'State' & ZillowIndex$IndexType == 'ZHVI', c('IndexMonth', 'IndexValue')]

ZRIState$ZRIState <- ZRIState$IndexValue
ZRIState <- ZRIState[ , c('IndexMonth', 'ZRIState')]

ZHVIState$ZHVIState <- ZHVIState$IndexValue
ZHVIState <- ZHVIState[ , c('IndexMonth', 'ZHVIState')]

Zillow <- merge(Zillow, ZRIState, all.x = TRUE)
Zillow <- merge(Zillow, ZHVIState, all.x = TRUE)


## County
ZRICounty <- ZillowIndex[ZillowIndex$RegionType == 'County' & ZillowIndex$IndexType == 'ZRI', c('RegionName', 'IndexMonth', 'IndexValue')]
ZHVICounty <- ZillowIndex[ZillowIndex$RegionType == 'County' & ZillowIndex$IndexType == 'ZHVI', c('RegionName', 'IndexMonth', 'IndexValue')]

ZRICounty$ZRICounty <- ZRICounty$IndexValue
ZRICounty$CountyName <- ZRICounty$RegionName
ZRICounty <- ZRICounty[ , c('CountyName', 'IndexMonth', 'ZRICounty')]

ZHVICounty$ZHVICounty <- ZHVICounty$IndexValue
ZHVICounty$CountyName <- ZHVICounty$RegionName
ZHVICounty <- ZHVICounty[ , c('CountyName', 'IndexMonth', 'ZHVICounty')]

Zillow <- merge(Zillow, ZRICounty, all.x = TRUE)
Zillow <- merge(Zillow, ZHVICounty, all.x = TRUE)


## Metro
MapCountyMetro <- load.csv('MapCountyMetro')

ZRIMetro <- ZillowIndex[ZillowIndex$RegionType == 'Metro' & ZillowIndex$IndexType == 'ZRI', c('RegionName', 'IndexMonth', 'IndexValue')]
ZHVIMetro <- ZillowIndex[ZillowIndex$RegionType == 'Metro' & ZillowIndex$IndexType == 'ZHVI', c('RegionName', 'IndexMonth', 'IndexValue')]

ZRIMetro$ZRIMetro <- ZRIMetro$IndexValue
ZRIMetro$MetroName <- ZRIMetro$RegionName
ZRIMetro <- merge(ZRIMetro, MapCountyMetro[ , c('MetroName', 'CountyName')])
ZRIMetro <- ZRIMetro[ , c('CountyName', 'IndexMonth', 'ZRIMetro')]

ZHVIMetro$ZHVIMetro <- ZHVIMetro$IndexValue
ZHVIMetro$MetroName <- ZHVIMetro$RegionName
ZHVIMetro <- merge(ZHVIMetro, MapCountyMetro[ , c('MetroName', 'CountyName')])
ZHVIMetro <- ZHVIMetro[ , c('CountyName', 'IndexMonth', 'ZHVIMetro')]

Zillow <- merge(Zillow, ZRIMetro, all.x = TRUE)
Zillow <- merge(Zillow, ZHVIMetro, all.x = TRUE)

## Backfill Zillow Index
Zillow <- Zillow[order(Zillow$CountyName, rev(Zillow$IndexMonth)), ]
Zillow$ZRIStateGrowthRate <- c(NA, diff(Zillow$ZRIState) / Zillow$ZRIState[1:(nrow(Zillow) - 1)])
Zillow$ZRIMetroGrowthRate <- c(NA, diff(Zillow$ZRIMetro) / Zillow$ZRIMetro[1:(nrow(Zillow) - 1)])
Zillow$ZRICountyGrowthRate <- c(NA, diff(Zillow$ZRICounty) / Zillow$ZRICounty[1:(nrow(Zillow) - 1)])
Zillow$ZHVIStateGrowthRate <- c(NA, diff(Zillow$ZHVIState) / Zillow$ZHVIState[1:(nrow(Zillow) - 1)])
Zillow$ZHVIMetroGrowthRate <- c(NA, diff(Zillow$ZHVIMetro) / Zillow$ZHVIMetro[1:(nrow(Zillow) - 1)])
Zillow$ZHVICountyGrowthRate <- c(NA, diff(Zillow$ZHVICounty) / Zillow$ZHVICounty[1:(nrow(Zillow) - 1)])

Zillow$ZRIGrowthRate <- ifelse(is.na(Zillow$ZRICountyGrowthRate),
                               ifelse(is.na(Zillow$ZRIMetroGrowthRate),
                                      Zillow$ZRIStateGrowthRate,
                                      Zillow$ZRIMetroGrowthRate),
                               Zillow$ZRICountyGrowthRate)
Zillow$ZHVIGrowthRate <- ifelse(is.na(Zillow$ZHVICountyGrowthRate),
                                ifelse(is.na(Zillow$ZHVIMetroGrowthRate),
                                       Zillow$ZHVIStateGrowthRate,
                                       Zillow$ZHVIMetroGrowthRate),
                                Zillow$ZHVICountyGrowthRate)

Zillow$ZRIChosen <- ifelse(is.na(Zillow$ZRICounty),
                           ifelse(is.na(Zillow$ZRIMetro),
                                  Zillow$ZRIState,
                                  Zillow$ZRIMetro),
                           Zillow$ZRICounty)
Zillow$ZHVIChosen <- ifelse(is.na(Zillow$ZHVICounty),
                            ifelse(is.na(Zillow$ZHVIMetro),
                                   Zillow$ZHVIState,
                                   Zillow$ZHVIMetro),
                            Zillow$ZHVICounty)

Zillow$ZRI <- ifelse(Zillow$IndexMonth == '2016-06-01', Zillow$ZRIChosen,
                     (1 + Zillow$ZRIGrowthRate) * c(NA, Zillow$ZRIChosen[1:(nrow(Zillow) - 1)]))
Zillow$ZHVI <- ifelse(Zillow$IndexMonth == '2016-06-01', Zillow$ZHVIChosen,
                      (1 + Zillow$ZHVIGrowthRate) * c(NA, Zillow$ZHVIChosen[1:(nrow(Zillow) - 1)]))

ZillowQuery <- 'select CountyName, SchoolYear, avg(ZRI) as ZRI, avg(ZHVI) as ZHVI
                from Zillow group by CountyName, SchoolYear'
Zillow <- sqldf(ZillowQuery)

Zillow <- merge(Zillow, MapDistrictCounty)

TEA.dat <- merge(TEA.dat, Zillow[ , c('DistrictID', 'SchoolYear', 'ZRI', 'ZHVI')], all.x = TRUE)

TEA.dat$TeacherSalary <- as.numeric(TEA.dat$TeacherSalary)

##write.csv(TEA.dat, 'temp.csv')


## Generate plots

## District FTE Count distribution
TEA.dat$TeacherFTEBin <- cut(TEA.dat$TeacherFTE, breaks = c(0, 100, 1000, Inf), labels = c('Fewer than 100', '100 to 1000', 'More than 1000'))

bin.counts <- table(TEA.dat[TEA.dat$SchoolYear == '2015-16', 'TeacherFTEBin'])

bin.counts.plot.path <- paste0(source.dir, '/Plots/binCountBarplot2.png')
png(file = bin.counts.plot.path, width = 650, height = 350)
bin.counts.plot <- barplot(bin.counts, yaxt = 'n', ylim = c(0, max(bin.counts) * 1.1),
                           col = 'grey40', space = 0.85,
                           main = 'Number of Teachers per District: 2015-16 School Year')
midpoints <- bin.counts.plot
text(x = midpoints[, 1], y = bin.counts + 35, labels = bin.counts)
abline(h = 0)
dev.off()


## 2015-16 Salary level for Major Urban districts

top.five.districts <- TEA.dat[TEA.dat$SchoolYear == '2015-16' & TEA.dat$DistrictClassification == 'MAJOR URBAN', c('DistrictID', 'DistrictName', 'TeacherFTE', 'TeacherSalary')]
top.five.districts <- top.five.districts[order(-top.five.districts$TeacherFTE), ]
top.five.districts <- top.five.districts[1:5, ]
top.five.districts$AverageSalary <- top.five.districts$TeacherSalary / top.five.districts$TeacherFTE

other.mu.districts <- TEA.dat[TEA.dat$SchoolYear == '2015-16' & TEA.dat$DistrictClassification == 'MAJOR URBAN' & !(TEA.dat$DistrictID %in% top.five.districts$DistrictID), c('DistrictID', 'DistrictName', 'TeacherFTE', 'TeacherSalary')]
other.mu.districts.average.salary <- sum(other.mu.districts$TeacherSalary) / sum(other.mu.districts$TeacherFTE)

ms.districts <- TEA.dat[TEA.dat$SchoolYear == '2015-16' & TEA.dat$DistrictClassification == 'MAJOR SUBURBAN', c('DistrictID', 'DistrictName', 'TeacherFTE', 'TeacherSalary')]
ms.districts.average.salary <- sum(ms.districts$TeacherSalary) / sum(ms.districts$TeacherFTE)

salary.plot.data <- rbind(top.five.districts[ , c('DistrictName', 'AverageSalary')],
                          c('Oth. Major Urban', other.mu.districts.average.salary),
                          c('All Major Suburban', ms.districts.average.salary),
                          c('', NA))
salary.plot.data$AverageSalary <- as.numeric(salary.plot.data$AverageSalary)

statewide.average.salary <- sum(TEA.dat[TEA.dat$SchoolYear == '2015-16', 'TeacherSalary']) / sum(TEA.dat[TEA.dat$SchoolYear == '2015-16', 'TeacherFTE'])

salary.plot.names <- c('Houston\n ISD', 'Dallas\n ISD', 'Northside\n ISD', 'Austin\n ISD', 'Ft. Worth\n ISD', 'Oth. Major\n Urban', 'All Major\n Suburban', '')

salary.plot.path <- paste0(source.dir, '/Plots/AverageSalaryBarplot2.png')
png(file = salary.plot.path, width = 650, height = 350)
salary.plot <- barplot(salary.plot.data$AverageSalary,
                       yaxt = 'n', names.arg = salary.plot.names,
                       ylim = c(0, max(salary.plot.data$AverageSalary, na.rm = TRUE) * 1.1),
                       col = 'grey40', space = 0.65,
                       main = 'Comparable District Average Salaries: 2015-16 School Year')

midpoints <- salary.plot
text(x = midpoints[, 1],
     y = salary.plot.data$AverageSalary + 2000,
     labels = paste0(sprintf('$%.1f', salary.plot.data$AverageSalary / 1000), 'K'))
abline(h = 0)
abline(h = statewide.average.salary, lty = 2, col = 'grey35')
statewide.average.label <- paste0('Statewide\n Average\n (',
                                  sprintf('$%.1f', statewide.average.salary / 1000),
                                  'K)')
text(x = 12.6, y = statewide.average.salary - 6000, labels = statewide.average.label)

dev.off()


## Zillow home values

zxi.district.list <- c('HOUSTON ISD', 'DALLAS ISD', 'NORTHSIDE ISD', 'AUSTIN ISD', 'FORT WORTH ISD', 'EL PASO ISD')
zxi.dat <- TEA.dat[TEA.dat$DistrictName %in% zxi.district.list & TEA.dat$SchoolYear == '2015-16' & TEA.dat$DistrictClassification == 'MAJOR URBAN', ]

zxi.dat <- zxi.dat[order(-zxi.dat$TeacherFTE), ]

zxi.plot.names <- c('Houston', 'Dallas', 'San Antonio', 'Austin', 'Ft. Worth', 'El Paso')

## ZHVI Plot
zhvi.plot.path <- paste0(source.dir, '/Plots/ZHVIBarplot2.png')
png(file = zhvi.plot.path, width = 650, height = 350)
zhvi.plot <- barplot(zxi.dat$ZHVI,
                     yaxt = 'n', names.arg = zxi.plot.names,
                     ylim = c(0, max(zxi.dat$ZHVI) * 1.1),
                     col = 'grey40', space = 0.65,
                     main = 'Zillow Home Value Index for Large Texas Cities - 2015-16 School Year')
midpoints <- zhvi.plot
text(x = midpoints[, 1],
     y = zxi.dat$ZHVI + 10000,
     labels = paste0(sprintf('$%.1f', zxi.dat$ZHVI / 1000), 'K'))
abline(h = 0)

dev.off()


## ZRI Plot
zri.plot.path <- paste0(source.dir, '/Plots/ZRIBarplot2.png')
png(file = zri.plot.path, width = 650, height = 350)
zri.plot <- barplot(zxi.dat$ZRI,
                     yaxt = 'n', names.arg = zxi.plot.names,
                     ylim = c(0, max(zxi.dat$ZRI) * 1.1),
                     col = 'grey40', space = 0.65,
                     main = 'Zillow Rental Index for Large Texas Cities - 2015-16 School Year')
midpoints <- zri.plot
text(x = midpoints[, 1],
     y = zxi.dat$ZRI + 100,
     labels = sprintf('$%.0f', zxi.dat$ZRI))
abline(h = 0)

dev.off()


## Cost adjusted salaries plot

austin.zri <- TEA.dat[TEA.dat$SchoolYear == '2015-16' & TEA.dat$DistrictName == 'AUSTIN ISD', 'ZRI']

top.five.districts <- TEA.dat[TEA.dat$SchoolYear == '2015-16' & TEA.dat$DistrictClassification == 'MAJOR URBAN', ]
top.five.districts <- top.five.districts[order(-top.five.districts$TeacherFTE), ]
top.five.districts <- top.five.districts[1:5, ]
top.five.districts$ZRIAdjSalary <- top.five.districts$TeacherSalary * austin.zri / (top.five.districts$ZRI * top.five.districts$TeacherFTE)
top.five.districts$AltCWIAdjSalary <- top.five.districts$TeacherSalary / (top.five.districts$AltCWI * top.five.districts$TeacherFTE)

other.mu.districts <- TEA.dat[TEA.dat$SchoolYear == '2015-16' & TEA.dat$DistrictClassification == 'MAJOR URBAN' & !(TEA.dat$DistrictID %in% top.five.districts$DistrictID), ]
other.mu.districts.zri.adj.salary <- sum(other.mu.districts$TeacherSalary * austin.zri / other.mu.districts$ZRI) / sum(other.mu.districts$TeacherFTE)
other.mu.districts.alt.cwi.adj.salary <- sum(other.mu.districts$TeacherSalary / other.mu.districts$AltCWI) / sum(other.mu.districts$TeacherFTE)

ms.districts <- TEA.dat[TEA.dat$SchoolYear == '2015-16' & TEA.dat$DistrictClassification == 'MAJOR SUBURBAN', ]
ms.districts.zri.adj.salary <- sum(ms.districts$TeacherSalary * austin.zri / ms.districts$ZRI) / sum(ms.districts$TeacherFTE)
ms.districts.alt.cwi.adj.salary <- sum(ms.districts$TeacherSalary / ms.districts$AltCWI) / sum(ms.districts$TeacherFTE)

salary.plot.data <- rbind(top.five.districts[ , c('DistrictName', 'ZRIAdjSalary', 'AltCWIAdjSalary')],
                          c('Oth. Major Urban', other.mu.districts.zri.adj.salary, other.mu.districts.alt.cwi.adj.salary),
                          c('All Major Suburban', ms.districts.zri.adj.salary, ms.districts.alt.cwi.adj.salary),
                          c('', NA))
salary.plot.data$ZRIAdjSalary <- as.numeric(salary.plot.data$ZRIAdjSalary)
salary.plot.data$AltCWIAdjSalary <- as.numeric(salary.plot.data$AltCWIAdjSalary)

statewide.zri.adj.salary <- sum(TEA.dat[TEA.dat$SchoolYear == '2015-16', 'TeacherSalary'] * austin.zri / TEA.dat[TEA.dat$SchoolYear == '2015-16', 'ZRI']) / sum(TEA.dat[TEA.dat$SchoolYear == '2015-16', 'TeacherFTE'])

statewide.alt.cwi.adj.salary <- sum(TEA.dat[TEA.dat$SchoolYear == '2015-16', 'TeacherSalary'] / TEA.dat[TEA.dat$SchoolYear == '2015-16', 'AltCWI']) / sum(TEA.dat[TEA.dat$SchoolYear == '2015-16', 'TeacherFTE'])

salary.plot.names <- c('Houston\n ISD', 'Dallas\n ISD', 'Northside\n ISD', 'Austin\n ISD', 'Ft. Worth\n ISD', 'Oth. Major\n Urban', 'All Major\n Suburban', '')

salary.plot.path <- paste0(source.dir, '/Plots/AdjAverageSalaryBarplot.png')
png(file = salary.plot.path, width = 650, height = 350)
salary.plot <- barplot(t(as.matrix(salary.plot.data[ , c('ZRIAdjSalary', 'AltCWIAdjSalary')])),
                       yaxt = 'n', names.arg = salary.plot.names,
                       ylim = c(0, max(salary.plot.data[ , c('ZRIAdjSalary', 'AltCWIAdjSalary')], na.rm = TRUE) * 1.1),
                       col = t(c('grey40', 'grey60')), #space = 0.65,
                       main = 'Comparable District Cost-Adjusted Average Salaries: 2015-16 School Year',
                       beside = TRUE)

midpoints <- salary.plot
text(x = midpoints[1, ],
     y = salary.plot.data$ZRIAdjSalary + 2000,
     labels = paste0(sprintf('$%.0f', salary.plot.data$ZRIAdjSalary / 1000), 'K'),
     cex = 0.8, adj = c(0.65, 0.40))
text(x = midpoints[2, ],
     y = salary.plot.data$AltCWIAdjSalary + 2000,
     labels = paste0(sprintf('$%.0f', salary.plot.data$AltCWIAdjSalary / 1000), 'K'),
     cex = 0.8, adj = c(0.35, 0.40))
abline(h = 0)
abline(h = statewide.zri.adj.salary, lty = 2, col = 'grey35')
abline(h = statewide.alt.cwi.adj.salary, lty = 2, col = 'grey55')
statewide.zri.adj.label <- paste0('Statewide ZRI\n Adj. Average\n (',
                                  sprintf('$%.0f', statewide.zri.adj.salary / 1000),
                                  'K)')
statewide.alt.cwi.adj.label <- paste0('Statewide Alt CWI\n Adj. Average\n (',
                                  sprintf('$%.0f', statewide.alt.cwi.adj.salary / 1000),
                                  'K)')
text(x = 23, y = statewide.zri.adj.salary + 8000, labels = statewide.zri.adj.label, cex = 0.8)
text(x = 23, y = statewide.alt.cwi.adj.salary - 6500, labels = statewide.alt.cwi.adj.label, cex = 0.8)
legend('topleft', c('ZRI Adj. Salary', 'CWI Adj. Salary'), fill = c('grey40', 'grey60'),
       cex = 0.8, bty = 'n')

dev.off()

