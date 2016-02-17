#source("~/swissinfo/_helpers/helpers.R")
library(swiTheme)
library(swiMap)
library(slopegraph)

############################################################################################
###   load vote data
############################################################################################

font <- "Open Sans"
# load results for the whole of Switzerland
voteCH <- read.csv("~/swissinfo/_helpers/cantonal/vote/genetic/concat_genetic.csv", row.names = 1, check.names = F)
colnames(voteCH) <- gsub("\\.", " ", colnames(voteCH))

# load the latest genetic vote results

vote2 <- read.csv("data/reproductive_medicine.csv", check.names = F)[,-3]
stopifnot(canton_namesStrict(rownames(voteCH), 'iso2') == vote2[,1] )

voteCH <- cbind(voteCH, dpi = round(vote2[,2], 1))

# TRANSFORM 2nd vote into % of no
voteCH[,2] <- 100 - voteCH[,2]


############################################################################################
###   load an indicator
############################################################################################
# indicator data
ifiles <- c("~/swissinfo/_helpers/cantonal/cantonalIndicators.csv")
indicators <- do.call(cbind, lapply(ifiles, function (csv) {
	ind <- read.csv(csv, row.names = 1)
	# ensure rownames (cantons) are ordered correctly
	stopifnot(identical(match(canton_CH[,1], rownames(ind)) , 1:26))
	ind
}))
# get only the population indicator
pop <- indicators[,which(colnames(indicators) == 'Population.Habitants.en.milliers'), drop = FALSE]


df <- voteCH


lineW <- pop[match(canton_namesStrict(rownames(df)), rownames(pop)),1]
lineW <- c(lineW,  max(lineW) + mean(lineW))
cols <- c(rep("black", nrow(voteCH)), swi_pal[9])

pdfswi_long("genetic_slopegraph.pdf")
par(family = font)
slopegraph(df, rescaleByColumn = F, cex.lab = 0.5, cex.num = 0.6, lab.sep = 0.25,
	offset.x = 0.08, offset.lab = 0.03, xlim = c(-1, ncol(voteCH) + 1.2),
	collapse.label = ", ", lwd =  scale(lineW, center = F), col.lines = cols, mai = c(0, 0, 0.6, 0),
	main = "")
dev.off()



par(family = font)
slopegraph(df, rescaleByColumn = F, cex.lab = 0.5, cex.num = 0.6, lab.sep = 0.25,
	offset.x = 0.045, offset.lab = 0.03, xlim = c(-1.2, ncol(votes) + 1.2),
	collapse.label = ", ", lwd =  scale(lineW, center = F) / 2, col.lines = cols, mai = c(0.01, 0, 0.9, 0))


par(family = font)
slopegraph(df[,2:4], rescaleByColumn = F, cex.lab = 0.5, cex.num = 0.6, lab.sep = 0.25,
	offset.x = 0.07, offset.lab = 0.03, xlim = c(-0.6, ncol(votes) + 0.8),
	collapse.label = ", ", lwd =  scale(lineW, center = F) / 2, col.lines = cols, mai = c(0.01, 0, 0.9, 0))


dev.off()


