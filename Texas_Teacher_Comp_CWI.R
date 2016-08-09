################################################################################
## Texas_Teacher_Comp_CWI.R                                                   ##
## Contact: ian@blincoe.xyz                                                   ##
## R version 3.3.1                                                            ##
## Description: Pull Census American Community Survey data, build wage index, ##
##   and plot results                                                         ##
################################################################################

src.dir <- 'C:/Users/blincoeshirey/Documents/Ian/Analysis/Texas-Teacher-Comp'
working.dir <- 'C:/Users/blincoeshirey/Documents/Ian/Analysis/Texas-Teacher-Comp-WD'

setwd(working.dir)

url.stub <- 'http://www2.census.gov/programs-surveys/acs/data/pums/'

extract.files <- function(year) {
    year.str <- as.character(year)
    dest.zipfile <- paste0('csv_ptx_', year.str, '.zip')
    if (year <= 2006) {
        url <- paste0(url.stub, year.str, '/csv_ptx.zip')
    } else {
        url <- paste0(url.stub, year.str, '/1-Year/csv_ptx.zip')
    }
    download.file(url = url, destfile = dest.zipfile)

    file <- paste0('ss', substring(year.str, 3, 4), 'ptx.csv')

    unzip(dest.zipfile, files = file)

    read.file <- read.csv(file = file)
    return(read.file)
}

keep.list <- c('SEX', 'AGEP', 'RAC2P', 'HISP', 'ENG', 'SCHL', 'ESR', 'OCCP', 'INDP', 'COW', 'WAGP', 'WKW', 'WKHP', 'POWPUMA', 'POWSP', 'HINS1', 'PUMA')

beg.year <- 2005
end.year <- 2014
for (year in beg.year:end.year) {
    if (year == beg.year) {
        raw.dat <- extract.files(year)
        for (item in keep.list) {
            if (!(item %in% colnames(raw.dat))) {
                raw.dat[, item] <- rep(NA, nrow(raw.dat))
            }
        }
        raw.dat <- subset(raw.dat, select = keep.list)
        raw.dat$SurveyYear <- year
    } else {
        join.dat <- extract.files(year)
        for (item in keep.list) {
            if (!(item %in% colnames(join.dat))) {
                join.dat[, item] <- rep(NA, nrow(join.dat))
            }
        }
        join.dat <- subset(join.dat, select = keep.list)
        join.dat$SurveyYear <- year
        raw.dat <- rbind(raw.dat, join.dat)
    }
}

## 2005 & on has good POWPUMA info
dat <- raw.dat[raw.dat$SurveyYear >= 2005, ]

## Only pick Texas places of work
dat <- dat[dat$POWSP == 48, ]

## Exclude self-employed, unemployed
dat <- dat[dat$COW %in% c(1, 2, 3, 4, 5), ]

## Only include those with college degrees
dat <- dat[(dat$SurveyYear <= 2007 & dat$SCHL %in% 13:16) | (dat$SurveyYear >= 2008 & dat$SCHL %in% 21:24), ]

## Exclude less than half-time workers
dat <- dat[dat$WKHP >= 30, ]

## Exclude super low-wage earners
dat <- dat[dat$WAGP > 5000, ]

## Exclude Non-English Speakers
dat <- dat[!(dat$ENG %in% c(3, 4)), ]

## Define Model Variables
dat$MV.lWage <- log(dat$WAGP)
dat$MV.Female <- ifelse(dat$SEX == 2, 1, 0)
dat$MV.MastersDegree <- ifelse((dat$SurveyYear <= 2007 & dat$SCHL == 14) | (dat$SurveyYear >= 2008 & dat$SCHL == 22), 1, 0)
dat$MV.ProfDegree <- ifelse((dat$SurveyYear <= 2007 & dat$SCHL == 15) | (dat$SurveyYear >= 2008 & dat$SCHL == 23), 1, 0)
dat$MV.DoctDegree <- ifelse((dat$SurveyYear <= 2007 & dat$SCHL == 16) | (dat$SurveyYear >= 2008 & dat$SCHL == 24), 1, 0)
dat$MV.HoursWorked <- dat$WKHP
dat$MV.WeeksWorked <- dat$WKW
dat$MV.Age <- dat$AGEP
dat$MV.AgeSq <- dat$MV.Age ^ 2
dat$MV.RaceBlack <- ifelse(dat$RAC2P == 2, 1, 0)
dat$MV.RaceAmIndian <- ifelse(dat$RAC2P %in% 3:37, 1, 0)
dat$MV.RaceAsChinese <- ifelse(dat$RAC2P == 43, 1, 0)

RaceCode.EastAsian <- c(44, 46, 48, 49, 52)
RaceCode.SouthEastAsian <- c(41, 42, 45, 47, 50, 51, 56, 57)
RaceCode.SouthAsian <- c(38, 39, 40, 53, 54, 55)
RaceCode.PacIsland <- 60:66

dat$MV.RaceEastAsian <- ifelse(dat$RAC2P %in% RaceCode.EastAsian, 1, 0)
dat$MV.RaceSouthEastAsian <- ifelse(dat$RAC2P %in% RaceCode.SouthEastAsian, 1, 0)
dat$MV.RaceSouthAsian <- ifelse(dat$RAC2P %in% RaceCode.SouthAsian, 1, 0)
dat$MV.RacePacIsland <- ifelse(dat$RAC2P %in% RaceCode.PacIsland, 1, 0)
dat$MV.RaceOtherAsian <- ifelse(dat$RAC2P %in% 58:59, 1, 0)
dat$MV.RaceOther <- ifelse(dat$RAC2P == 67, 1, 0)
dat$MV.RaceMultiple <- ifelse(dat$RAC2P == 68, 1, 0)

dat$MV.OccupationFactor <- factor(dat$OCCP)
dat$MV.PlaceOfWorkFactor <- factor(dat$POWPUMA)

ACM.OccupationCodes <- c(800, 860, 620, 630, 640, 650, 8740, 6010, 1300, 1640, 3130, 3255, 3256,
                         3258, 3150, 3160, 2200, 2340, 2000, 2400, 2040, 2840, 2830, 2810, 1010)
dat$ACM.Occupation <- ifelse(dat$OCCP %in% ACM.OccupationCodes, 1, 0)

dat$Occupation.Teacher <- ifelse(dat$OCCP %in% c(2310, 2320), 1, 0)

run.model <- function(year) {
    model <- lm(data = dat[dat$SurveyYear == year & dat$Occupation.Teacher == 0, ],
                formula = MV.lWage ~ MV.Female +
                    MV.MastersDegree + MV.ProfDegree + MV.DoctDegree +
                    MV.HoursWorked + MV.WeeksWorked + MV.Age + MV.AgeSq +
                    MV.RaceBlack + MV.RaceAmIndian + MV.RaceAsChinese + MV.RaceEastAsian +
                    MV.RaceSouthEastAsian + MV.RaceSouthAsian + MV.RacePacIsland +
                    MV.RaceOtherAsian + MV.RaceOther + MV.RaceMultiple +
                    MV.OccupationFactor + MV.PlaceOfWorkFactor - 1)
    model.summary <- coef(summary(model))

    location.list <- c('Dallas', 'Ft. Worth', 'Houston', 'Austin', 'San Antonio', 'El Paso')
    location.code <- c('2300', '2500', '4600', '5300', '5900', '3300')
    if (year <= 2011) {location.code[3] <- '4690'}
    out.matrix <- matrix(ncol = 3, nrow = 6)
    colnames(out.matrix) <- c('Location', 'Coef', 'SE')
    out.dat <- data.frame(out.matrix)
    for (i in 1:6) {
        coef.name <- paste0('MV.PlaceOfWorkFactor', location.code[i])
        out.dat$Location[i] <- location.list[i]
        out.dat$Coef[i] <- model.summary[coef.name, 1]
        out.dat$SE[i] <- model.summary[coef.name, 2]
    }
    return(out.dat)
}

for (year in beg.year:end.year) {
    if (year == beg.year) {
        final.out.dat <- run.model(year)
        final.out.dat$SurveyYear <- year
    } else {
        join.out.dat <- run.model(year)
        join.out.dat$SurveyYear <- year
        final.out.dat <- rbind(final.out.dat, join.out.dat)
    }
}


## Plot results

plot.names <- c('Dallas', 'Ft. Worth', 'Houston', 'San Antonio', 'El Paso', '')

cwi.relative.austin <- exp(final.out.dat$Coef[final.out.dat$Location %in% plot.names & final.out.dat$SurveyYear == 2014] - final.out.dat$Coef[final.out.dat$Location == 'Austin' & final.out.dat$SurveyYear == 2014])

cwi.relative.austin.se <- (final.out.dat$SE[final.out.dat$Location %in% plot.names & final.out.dat$SurveyYear == 2014] ^ 2 + final.out.dat$SE[final.out.dat$Location == 'Austin' & final.out.dat$SurveyYear == 2014] ^ 2) ^ (1 / 2)

cwi.relative.austin.lci <- cwi.relative.austin * exp(-2 * cwi.relative.austin.se)
cwi.relative.austin.uci <- cwi.relative.austin * exp(2 * cwi.relative.austin.se)

cwi.relative.austin[6] <- NA
cwi.relative.austin.lci[6] <- NA
cwi.relative.austin.uci[6] <- NA

cwi.plot.path <- paste0(src.dir, '/Plots/CWIBarplot.png')
png(file = cwi.plot.path, width = 650, height = 350)
cwi.plot <- barplot(cwi.relative.austin,
                    yaxt = 'n', names.arg = plot.names,
                    ylim = c(0, max(cwi.relative.austin[1:5]) * 1.1),
                    col = 'grey40', space = 0.65)

midpoints <- cwi.plot
text(x = midpoints[, 1],
     y = cwi.relative.austin + .07,
     labels = paste0(sprintf('%.0f', cwi.relative.austin * 100), '%'))
abline(h = 0)
abline(h = 1, lty = 2)
label <- 'Relative to Austin\n (100%)'
text(x = midpoints[6], y = 1.1, labels = label)

dev.off()


## Plot results with 95% CI

cwi.ci.plot.path <- paste0(src.dir, '/Plots/CWIBarplotCI.png')
png(file = cwi.ci.plot.path, width = 650, height = 350)
cwi.ci.plot <- barplot(cwi.relative.austin,
                    yaxt = 'n', names.arg = plot.names,
                    ylim = c(0, max(cwi.relative.austin.uci[1:5]) * 1.1),
                    col = 'grey40', space = 0.65)

midpoints <- cwi.ci.plot
text(x = midpoints[, 1] - 0.3,
     y = cwi.relative.austin + .07,
     labels = paste0(sprintf('%.0f', cwi.relative.austin * 100), '%'))
abline(h = 0)
abline(h = 1, lty = 2)
label <- 'Relative to Austin\n (100%)'
text(x = midpoints[6], y = 1.1, labels = label)

segments(midpoints, cwi.relative.austin.uci, midpoints, cwi.relative.austin.lci, lwd = 1.5)

arrows(midpoints, cwi.relative.austin.uci, midpoints, cwi.relative.austin.lci,
       lwd = 1.5, angle = 90, code = 3, length = 0.05)

dev.off()
