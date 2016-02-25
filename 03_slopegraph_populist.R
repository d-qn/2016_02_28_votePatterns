library(swiTheme)
library(swiMap)
library(slopegraph)
library(dplyr)
library(htmltools)
library(svglite)

############################################################################################
###   load vote data
############################################################################################

languages <- c('fre', 'ger', 'ita', 'eng')

font <- "Open Sans"
# load results for the whole of Switzerland
voteCH <- read.csv("~/swissinfo/_helpers/cantonal/vote/populist/concatenate_popu.csv", row.names = 1, check.names = F)
colnames(voteCH) <- gsub("\\.", " ", colnames(voteCH))

# load the latest genetic vote results
vote2 <- read.csv("data/reproductive_medicine.csv", check.names = F)[,-3]
stopifnot(canton_namesStrict(rownames(voteCH), 'iso2') == vote2[,1] )

voteCH <- cbind(voteCH, dsi = round(vote2[,2], 1))

# # TRANSFORM 2nd vote into % of no
# voteCH[,2] <- 100 - voteCH[,2]

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

#df <- voteCH
### Drop some votes!
df <- voteCH %>% select(-`Contre la construction de minarets (29 11 2009)`, -`Contre l'immigration de masse (09 2 2014)`, -`Ecopop (30 11 2014)`)

lineW <- pop[match(canton_namesStrict(rownames(df)), rownames(pop)),1]
lineW <- c(lineW,  max(lineW) + mean(lineW))
cols <- c(rep("black", nrow(df)), swi_pal[9])

cant.iso2 <- canton_namesStrict(rownames(df))
cols <- ifelse(canton_namesStrict(cant.iso2, "isLatin") == "TRUE", "#366096", "#ab3d3f")

pdfswi_long("output/populist_slopegraph.pdf")
par(family = font)

sapply(languages, function(lang) {
  dd <- df
  rownames(dd) <- canton_namesStrict(cant.iso2, lang)
    slopegraph(round(dd), rescaleByColumn = F, cex.lab = 0.6, cex.num = 0.7, lab.sep = 0.25,
      offset.x = 0.05, offset.lab = 0.03, xlim = c(-0.4, ncol(df) + 1.4),
      collapse.label = ", ", lwd =  scale(lineW, center = F), col.lines = cols, mai = c(0, 0, 0.6, 0),
      main = "")
})

dev.off()


#
# svglite(file = "populist_slopegraph.svg", width = 10, height = 10 * 1.25)
# par(family = font)
# slopegraph(round(df), rescaleByColumn = F, cex.lab = 0.6, cex.num = 0.7, lab.sep = 0.25,
# 	offset.x = 0.038, offset.lab = 0.03, xlim = c(-1, ncol(df) + 1),
# 	collapse.label = ", ", lwd =  scale(lineW, center = F), col.lines = cols, mai = c(0, 0, 0.6, 0),
# 	main = "")
# dev.off()
