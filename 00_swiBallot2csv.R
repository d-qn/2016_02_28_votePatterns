library(XLConnect)

############################################################################################
###   SETTINGS
############################################################################################

# path to a data file used by SWI ballot visualisation
infile <- 'data/ballot-visualization 14.06.2015.xlsx'

############################################################################################
###   Load and reformat vote data
############################################################################################

wb <- loadWorkbook(infile)
sheets <- getSheets(wb)[getSheets(wb) != 'config']

invisible(lapply(sheets, function(sheet) {
	cat("\nProcess sheet:", sheet, "\n")

	dd <- readWorksheet(wb, sheet)

	if(nrow(dd) != 26 || ncol(dd) != 5) {
		stop("\nSheet:", sheet, " does not have the right structure!\n")
	}

	dd[,'bulletins'] <- dd[,'absolute.yes'] + dd[,'absolute.no']
	newColName <- paste(sheet, "% oui")
	dd[,newColName] <- (dd[,'absolute.yes'] / dd[,'bulletins']) * 100

	df <- dd[,c(newColName, 'bulletins')]
	row.names(df) <- dd[,'name']

	write.csv(df, file = paste("data/", sheet, ".csv", sep =""))
}))
