################################################################################
## Texas_Teacher_Comp_Plots.R                                                 ##
## Contact: ian@blincoe.xyz                                                   ##
## R version 3.3.1                                                            ##
## Description:                                                               ##
################################################################################

























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













# Plot the distribution of teacher counts by district
tot.dat$FTE.Count.Bin <- cut(tot.dat$FTE.Count, breaks = c(0, 100, 1000, Inf), labels = c('Fewer than 100', '100 to 1000', 'More than 1000'))

bin.counts <- table(tot.dat$FTE.Count.Bin)

bin.counts.plot.path <- paste0(src.dir, '/Plots/binCountBarplot.png')
png(file = bin.counts.plot.path, width = 650, height = 350)
bin.counts.plot <- barplot(bin.counts, yaxt = 'n', ylim = c(0, max(bin.counts) * 1.1), col = 'grey40', space = 0.85)
midpoints <- bin.counts.plot
text(x = midpoints[, 1], y = bin.counts + 35, labels = bin.counts)
abline(h = 0)
dev.off()


# Plot the average salary and FTE count for the 'Major Urban' school districts
comp.salary.dat <- tot.dat[tot.dat$ClassDescription == 'MAJOR URBAN', c(5, 8, 9, 10)]
comp.salary.dat <- comp.salary.dat[order(-comp.salary.dat$FTE.Count), ]

comp.salary.top.five.mu.dat <- comp.salary.dat[1:5, c(1, 4)]
comp.salary.rest.mu.dat <- c('Other Major Urban',
                             sum(comp.salary.dat$Total.Base.Pay[6:nrow(comp.salary.dat)]) / sum(comp.salary.dat$FTE.Count[6:nrow(comp.salary.dat)]))
comp.salary.all.ms.dat <- c('All Major Suburban',
                           sum(tot.dat$Total.Base.Pay[tot.dat$ClassDescription == 'MAJOR SUBURBAN']) / sum(tot.dat$FTE.Count[tot.dat$ClassDescription == 'MAJOR SUBURBAN']))

comp.salary.dat <- rbind(comp.salary.top.five.mu.dat,
                         comp.salary.rest.mu.dat,
                         comp.salary.all.ms.dat,
                         rep(NA, 2))

comp.salary.dat$Average.Base.Pay <- as.numeric(comp.salary.dat$Average.Base.Pay)

statewide.average <- sum(tot.dat$Total.Base.Pay) / sum(tot.dat$FTE.Count)

avg.salary.plot.names <- c('Houston\n ISD', 'Dallas\n ISD', 'Northside\n ISD', 'Austin\n ISD', 'Ft. Worth\n ISD', 'Oth. Major\n Urban', 'All Major\n Suburban', '')

avg.salary.plot.path <- paste0(src.dir, '/Plots/AverageSalaryBarplot.png')
png(file = avg.salary.plot.path, width = 650, height = 350)
avg.salary.plot <- barplot(comp.salary.dat$Average.Base.Pay,
                           yaxt = 'n', names.arg = avg.salary.plot.names,
                           ylim = c(0, max(comp.salary.dat$Average.Base.Pay[!is.na(comp.salary.dat$Average.Base.Pay)]) * 1.1),
                           col = 'grey40', space = 0.65)

midpoints <- avg.salary.plot
text(x = midpoints[, 1],
     y = comp.salary.dat$Average.Base.Pay + 2000,
     labels = paste0(sprintf('$%.1f', comp.salary.dat$Average.Base.Pay / 1000), 'K'))
abline(h = 0)
abline(h = statewide.average, lty = 2)
statewide.average.label <- paste0('Statewide\n Average\n (',
                                  sprintf('$%.1f', statewide.average / 1000),
                                  'K)')
text(x = 12.6, y = statewide.average - 6000, labels = statewide.average.label)

dev.off()


