library(swiTheme)
library(swiMap)
library(swiRcharts)


###########################################################################################
###   SETTINGS
############################################################################################

# query data.frame data
qfiles <- c("data/reproductive_medicine.csv", "data/student_grants.csv", "data/inheritances_tax.csv", "data/radio_tv_act.csv")
## the votes data should have

# indicator data
ifiles <- c("~/swissinfo/_helpers/cantonal/cantonalIndicators.csv")

###########################################################################################
###   Load indicators & ballots
############################################################################################

#indicators
indicators <- do.call(cbind, lapply(ifiles, function (csv) {
  ind <- read.csv(csv, row.names = 1, check.names = F)
  # ensure rownames (cantons) are ordered correctly
  stopifnot(identical(match(canton_CH[,1], rownames(ind)) , 1:26))
  ind
}))

# ballots data
votes <- lapply(qfiles, function(qfile) {

  voteName <- gsub("(.*\\/|.csv)", "", qfile)
  cat("\n\n", voteName, "\n\n")
  votes <- read.csv(qfile, stringsAsFactors = F, row.names = 1)

  iorder <- match(canton_CH$iso2, rownames(votes))
  votes[iorder,]
})
names(votes) <- gsub("(.*\\/|.csv)", "", qfiles)


###########################################################################################
###   Plot bubble chart
############################################################################################

plotBubble <- function(name, series, x, y, z, x.name, y.name, z.name) {
  data.frame(x, y, z, name, )


}

plotBubble(name = rownames(votes[[1]]), y = indicators$`Population Etrangers en %`, z = votes[[1]][,2])




