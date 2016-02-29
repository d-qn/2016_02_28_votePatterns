library(swiTheme)
library(swiMap)
library(slopegraph)
library(dplyr)
library(htmltools)
library(svglite)

############################################################################################
###   load vote data
############################################################################################

languages <- c("rus")
withwholeCH <- T

font <- "Open Sans"

# translation
txt <-read.csv("input/Anti foreigners slopegraphs  - Sheet1.csv", row.names = 1, stringsAsFactors = F)

if(withwholeCH) {
  voteCH <- read.csv(file = "input/populistSlopegraph_data_addedCH.csv", row.names = 1, check.names = F)
} else {
  # load results for the whole of Switzerland
  voteCH <- read.csv("~/swissinfo/_helpers/cantonal/vote/populist/concatenate_popu.csv", row.names = 1, check.names = F)
  colnames(voteCH) <- gsub("\\.", " ", colnames(voteCH))
  
  # load the latest populist vote results
  vote2 <- read.csv("data/deportation.csv", check.names = F)[,-3]
  stopifnot(canton_namesStrict(rownames(voteCH), 'iso2') == vote2[,1] )
  
  # get the names of the last 2 votes
  library(XLConnect)
  # path to a data file used by SWI ballot visualisation
  infile <- 'data/foreigners ballot 29.02.2016.xlsx'
  wb <- loadWorkbook(infile)
  dn <- readWorksheet(wb, "config")
  dn <- dn[,grepl("^title", colnames(dn))]
  colnames(dn) <- gsub("^title\\.", "", colnames(dn))
  
  voteCH <- cbind(voteCH, dsi = vote2[,2])
  write.csv(voteCH, file = "input/populistSlopegraph_data.csv")
}

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


############################################################################################
###   Slopegraph: only the last 2 anti-immigrant ballots
############################################################################################

#df <- voteCH
### Drop some votes!
df2 <- voteCH
df <- voteCH %>% select(-`Contre la construction de minarets (29 11 2009)`, -`Contre l'immigration de masse (09 2 2014)`, -`Ecopop (30 11 2014)`)

lineW <- pop[match(canton_namesStrict(rownames(df)), rownames(pop)) ,1]
lineW <- ifelse(is.na(lineW), max(lineW, na.rm = T) + mean(lineW, na.rm = T), lineW)
#cols <- c(rep("black", nrow(df)), swi_pal[9])

cant.iso2 <- canton_namesStrict(rownames(df))
cols <- ifelse(canton_namesStrict(cant.iso2, "isLatin") == "TRUE", "#366096", "#ab3d3f")
cols <- ifelse(is.na(cols), "black", cols)

#pdfswi_long("output/populist_slopegraph.pdf")
#par(family = font)

sapply(languages, function(lang) {
  dd <- df
  colnames(dd) <- c(txt['criminels.vote', toupper(lang)], txt['oeuvre.vote', toupper(lang)])
  colnames(dd) <- sapply(strwrap(colnames(dd), width = 12, prefix = "\n", initial = "", simplify = F), paste, collapse = "")
  rownames(dd) <- c(canton_namesStrict(cant.iso2[which(!is.na(cant.iso2))], lang), as.character(txt['CH', toupper(lang)]))
  cairo_pdf(paste0("output/populist_slopegraph_", lang, ".pdf"),  width = 6, height = 6 * 1.25 , family = font)
  
    slopegraph(round(dd), rescaleByColumn = F, cex.lab = 0.6, cex.num = 0.7, lab.sep = 0.25,
      offset.x = 0.08, offset.lab = 0.03, xlim = c(-0.8, ncol(df) + 1.7),
      collapse.label = ", ", lwd =  scale(lineW, center = F), col.lines = cols, mai = c(0, 0, 0.6, 0),
      main = "")
  dev.off()
})



############################################################################################
###   Slopegraph: the 4 last anti-immigrant ballots
############################################################################################

ddd <- df2 %>% select( -`Ecopop (30 11 2014)`)
#pdfswi_long("output/allPopulist_slopegraph.pdf")

par(family = font)
#lang <- 'fre'
sapply(languages, function(lang) {
  
  colnames(ddd) <- txt[which(grepl("vote", rownames(txt))), toupper(lang)]
  rownames(ddd) <- c(canton_namesStrict(cant.iso2[which(!is.na(cant.iso2))], lang), as.character(txt['CH', toupper(lang)]))
  cairo_pdf(paste0("output/allPopulist_slopegraph_", lang, ".pdf"),  width = 6, height = 6 * 1.25 , family = font)
  
  slopegraph(
    round(ddd),
    rescaleByColumn = F,
    cex.lab = 0.6,
    cex.num = 0.6,
    lab.sep = 0.25,
    offset.x = 0.15,
    offset.lab = 0.08,
   # xlim = c(-4.5, ncol(df) + ),
    xlim = c(-4, ncol(df) + 5.5),
    collapse.label = ", ",
    lwd =  scale(lineW * 1.3, center = F),
    col.lines = cols,
    mai = c(0, 0, 0.6, 0),
    main = ""
  )
  dev.off()
})

