################################################################################
## Texas_Teacher_Comp_TEA.R                                                   ##
## Contact: ian@blincoe.xyz                                                   ##
## R version 3.3.1                                                            ##
## Description: Pull Texas teacher salary information and output dataset      ##
################################################################################

library(RCurl)

source.dir <- 'C:/Users/blincoeshirey/Documents/Ian/Analysis/Texas-Teacher-Comp'
working.dir <- 'C:/Users/blincoeshirey/Documents/Ian/Analysis/Texas-Teacher-Comp-WD'

setwd(working.dir)

DistrictClassification.path <- paste0(source.dir, '/Data/DistrictClassification.csv')
DistrictClassification <- read.csv(DistrictClassification.path, stringsAsFactors = FALSE)

RecapturePaid.path <- paste0(source.dir, '/Data/RecapturePaid.csv')
RecapturePaid <- read.csv(RecapturePaid.path, stringsAsFactors = FALSE)

aged.salary.zipfile.path <- paste0(source.dir, '/Data/A1607092.zip')
unzip(aged.salary.zipfile.path)

get.salary.data.from.file <- function(end.year) {
    salary.file.path <- paste0('A1607092_', end.year, '.csv')
    raw.dat <- read.csv(salary.file.path, stringsAsFactors = FALSE)
    raw.dat <- raw.dat[raw.dat$STAFF_GROUP== '01-PROFESSIONAL TEACHING STAFF', ]
    agg.dat <- aggregate(raw.dat[, c('FTE', 'BASEPAY')], by = list(DistrictID = raw.dat$DISTRICT, DistrictName = raw.dat$DISTNAME), FUN = sum)
    agg.dat$TeacherSalary <- agg.dat$BASEPAY
    agg.dat$TeacherFTE <- agg.dat$FTE

    out.dat <- agg.dat[ , c('DistrictID', 'DistrictName', 'TeacherSalary', 'TeacherFTE')]
    return(out.dat)
}

get.salary.data.from.web <- function(end.year) {
    url.stub <- 'https://rptsvr1.tea.texas.gov/cgi/sas/broker'
    year.param <- paste0('endyear=', end.year)
    url.params <- paste('_service=marykay', '_program=adhoc.addispatch.sas', 'major=pe',
                        'minor=b', 'linespg=60', 'charsln=120', 'selsumm=sd', 'format=C',
                        year.param,
                        sep = '&')
    url <- paste0(url.stub, '?', url.params)

    r <- getURL(url, ssl.verifypeer = FALSE)

    if (end.year %in% c('12', '13')) {
        skip.rows <- 2
    } else {
        skip.rows <- 5
    }

    raw.dat <- read.csv(text = r, skip = skip.rows, stringsAsFactors = FALSE)
    raw.dat <- raw.dat[raw.dat$Staff == 'TOTAL TEACHING STAFF', ]

    raw.dat$DistrictID <- raw.dat$District
    raw.dat$DistrictName <- raw.dat$District.Name
    raw.dat$TeacherSalary <- as.numeric(raw.dat$Total.Base.Pay)
    raw.dat$TeacherFTE <- as.numeric(raw.dat$FTE.Count)

    out.dat <- raw.dat[ , c('DistrictID', 'DistrictName', 'TeacherSalary', 'TeacherFTE')]
    return(out.dat)
}

web.SchoolYearlist <- c('2011-12', '2012-13', '2013-14', '2014-15', '2015-16')
SchoolYearList <- c('1993-94', '1994-95', '1995-96', '1996-97', '1997-98', '1998-99', '1999-00',
                    '2000-01', '2001-02', '2002-03', '2003-04', '2004-05', '2005-06', '2006-07',
                    '2007-08', '2008-09', '2009-10', '2010-11', '2011-12', '2012-13', '2013-14',
                    '2014-15', '2015-16')

for (SchoolYear in SchoolYearList) {
    end.year <- substring(SchoolYear, 6, 7)
    print(end.year)
    if (SchoolYear %in% web.SchoolYearlist) {
        dat <- get.salary.data.from.web(end.year)
    } else {
        dat <- get.salary.data.from.file(end.year)
    }

    dat$SchoolYear <- SchoolYear

    dat <- merge(dat, DistrictClassification, by = c('DistrictID', 'SchoolYear'))
    dat <- merge(dat, RecapturePaid, by = c('DistrictID', 'SchoolYear'), all.x = TRUE)
    dat$RecapturePaid <- ifelse(is.na(dat$RecapturePaid), 0, dat$RecapturePaid)

    out.dat <- dat[ , c('DistrictID', 'DistrictName', 'DistrictClassification', 'SchoolYear', 'TeacherSalary', 'TeacherFTE', 'RecapturePaid')]

    if (SchoolYear == '1993-94') {
        final.out.dat <- out.dat
    } else {
        final.out.dat <- rbind(final.out.dat, out.dat)
    }
}

final.out.csv.path <- paste0(source.dir, '/Data/TEADistrictSalaryRecapture.csv')
write.csv(final.out.dat, final.out.csv.path, row.names = FALSE)


