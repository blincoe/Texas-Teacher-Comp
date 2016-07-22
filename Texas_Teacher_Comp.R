library(RCurl)

url <- "https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=adhoc.addispatch.sas&major=pe&minor=b&linespg=60&charsln=120&selsumm=sd&format=C&endyear=16"
r <- getURL(url, ssl.verifypeer = FALSE)
raw.dat <- read.csv(text = r, skip = 5, stringsAsFactors = FALSE)
str(raw.dat)

dat <- raw.dat[raw.dat$Staff == "TOTAL TEACHING STAFF", c(1, 2, 3, 4, 7, 9)]
str(dat)

dat$Average.Base.Pay <- as.numeric(dat$Average.Base.Pay)
dat$FTE.Count <- as.numeric(dat$FTE.Count)


View(dat[dat$District.Name == "AUSTIN ISD", ])
View(dat)
hist(pmin(dat$FTE.Count, 1000))
plot(sort(dat$FTE.Count[dat$FTE.Count > 500]))
plot(dat$FTE.Count[dat$FTE.Count >= 1000], dat$Average.Base.Pay[dat$FTE.Count >= 1000])
sum(dat$FTE.Count >= 1000)
