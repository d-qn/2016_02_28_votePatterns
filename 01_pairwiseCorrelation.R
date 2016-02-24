#source("~/swissinfo/_helpers/helpers.R")
library(swiTheme)
library(swiMap)
library(ggrepel)
library(svglite)

############################################################################################
###   SETTINGS
############################################################################################

svgoutput <- T
svg.height <- 15
svg.width <- svg.height * 1.3

# query data.frame data
qfiles <- c("data/reproductive_medicine.csv", "data/student_grants.csv", "data/inheritances_tax.csv", "data/radio_tv_act.csv")
## the votes data should have

# indicator data
ifiles <- c("~/swissinfo/_helpers/cantonal/cantonalIndicators.csv")

source <- "source: Office Fédéral de la Statistique | swissinfo.ch "
font <- "Open Sans"

############################################################################################
###   Load indicators
############################################################################################

indicators <- do.call(cbind, lapply(ifiles, function (csv) {
	ind <- read.csv(csv, row.names = 1, check.names = F)
	# ensure rownames (cantons) are ordered correctly
	stopifnot(identical(match(canton_CH[,1], rownames(ind)) , 1:26))
	ind
}))


############################################################################################
###   Load query data
############################################################################################


for (f in 1:length(qfiles)) {

	qfile <- qfiles[f]
	voteName <- gsub("(.*\\/|.csv)", "", qfile)
	cat("\n\n", voteName, "\n\n")

	votes <- read.csv(qfile, stringsAsFactors = F, row.names = 1)

	iorder <- match(canton_CH$iso2, rownames(votes))
	votes <- votes[iorder,]

	############################################################################################
	###  Compute
	############################################################################################

	# check
	if(any(rownames(indicators) != rownames(votes))) {
		stop("Problem with the query and indicators correspondance!")
	}

	# compute the correlation
	cortest <- sapply(1:ncol(indicators), function(j) {
		cor <- cor.test(votes[,1], indicators[,j])
		structure(c(cor$estimate, cor$p.value), names=c('cor', 'pvalue'))
	})

	# Plot all the pairwise scatterplots
	pdfswi_sq(paste0("output/", voteName, "_all.pdf"))
	sapply(1:ncol(indicators), function(j) {
		#browser()
		df <- cbind(votes, indicator = indicators[,j])
		nameVote.org <- colnames(df)[1]
		colnames(df)[1] <- 'vote'
		df$color <- cut(df$vote,  breaks = seq(0, 100, 10))

		sp <- ggplot(data = df) + geom_point(aes(x = vote, y = indicator, size = bulletins, color = color), alpha = 0.8) +
		# add veritcal line at 50%
		geom_vline( xintercept = 50, linetype = "dashed", color = "black", size = 0.5, alpha = 0.5) +
		ggtitle(paste((paste(c('correlation:', 'p-value:'), paste(format(cortest[,j], digits = 3)))), collapse="   ")) +
		# define bubble sizes, x and y labs
		scale_size_continuous(range = c(5,40)) + xlab(voteName) + ylab(colnames(indicators)[j]) +
		theme_swiYLines() + theme(legend.position="top") +
		# discrete color scale
		scale_colour_manual(values = rev(swi_dpal), limits = levels(df$color)) +
		# remove legend
		theme(legend.position = "none") +
		# add text over bubble
		geom_text(data = cbind(df, text = rownames(df)), aes(x = vote, y = indicator, label = text), size = rel(2), alpha = 0.9) +
		# expand axis
		scale_y_continuous(expand = c(0.1,0.1)) + scale_x_continuous(expand = c(0.1,0.1))

		print(sp)
	})
	dev.off()


	# plot only the highest p-values
	if(!svgoutput) pdfswi_sq(paste0("output/", voteName, "_signif.pdf"))

	idxs <- which(cortest[2,] <= 0.05)
	col <- idxs[order(cortest[2,idxs])]
	sapply(col, function(j) {

		df <- cbind(votes, indicator = indicators[,j])
		nameVote.org <- colnames(df)[1]
		colnames(df)[1] <- 'vote'
		df$color <- cut(df$vote,  breaks = seq(0, 100, 10))

		sp <- ggplot(data = df, aes(x = vote, y = indicator)) +
		# add veritcal line at 50%  -------- ???!!!!
		#geom_vline( x = 50, linetype = "dashed", color = "black", size = 0.5, alpha = 0.5) +
		# liner regression
		geom_smooth(method=lm, se=FALSE, alpha = 0.8, linetype = "dotted", color = "#663333", size = 0.5)  +
		# bubbles
		geom_point(aes(size = bulletins, color = color), alpha = 0.7) +
		ggtitle(paste((paste(c('correlation:', 'p-value:'), paste(format(cortest[,j], digits = 3)))), collapse="   ")) +
		# define bubble sizes, x and y labs
		scale_size_continuous(range = c(5,50)) + xlab(voteName) + ylab(colnames(indicators)[j]) +
		theme_swiYLines() + theme(legend.position="top") +
		# discrete color scale
		scale_colour_manual(values = rev(swi_dpal), limits = levels(df$color)) +
		# remove legend and add ticks
		theme(legend.position = "none") + scale_x_continuous(breaks=pretty_breaks(n=6)) +
		# add text over bubble ----------------------- ?!?!????????? Use repel if too much overlapping
		#geom_text(data = cbind(df, text = rownames(df)), aes(x = vote, y = indicator, label = text), size = rel(4), alpha = 0.7, family = font)
		geom_text_repel(data = cbind(df, text = rownames(df)), aes(x = vote, y = indicator, label = text), size = rel(4), alpha = 0.7, family = font)

		if(svgoutput) {
		  svglite(file = paste0("output/", voteName, "_signif_", j, "_a.svg"), height = svg.height, width = svg.width)
		}
		#print(sp)
		grid.arrange(sp,
		 bottom = textGrob(source, x = 0.99, y = 1, vjust = 0.1, hjust = 1,
		  gp = gpar(fontsize = 10, fontfamily = font, col = "#737373", alpha = 0.6))
		)
		if(svgoutput) {
		  dev.off()
		}

	})
	# dev.off()


	#### PLOT THE SAME COLORED BY ROSTIGRABEN
	sapply(col, function(j) {
		df <- cbind(votes, indicator = indicators[,j])
		nameVote.org <- colnames(df)[1]
		colnames(df)[1] <- 'vote'
		df$color <- canton_CH[match(rownames(df), canton_CH[,1]), 'isLatin']

		sp <- ggplot(data = df, aes(x = vote, y = indicator)) +
		# add veritcal line at 50% -------- ???
		# geom_vline( x = 50, linetype = "dashed", color = "black", size = 0.5, alpha = 0.5) +
		# liner regression
		geom_smooth(method=lm, se=FALSE, alpha = 0.8, linetype = "dotted", color = "#663333", size = 0.5)  +
		# bubbles
		geom_point(aes(size = bulletins, color = color), alpha = 0.7) +
		ggtitle(paste((paste(c('correlation:', 'p-value:'), paste(format(cortest[,j], digits = 3)))), collapse="   ")) +
		# define bubble sizes, x and y labs
		scale_size_continuous(range = c(5,50)) + xlab(voteName) + ylab(colnames(indicators)[j]) +
		theme_swiYLines() + theme(legend.position="top") +
		# discrete color scale
		scale_colour_manual(values = swi_rpal, limits = levels(df$color)) +
		# remove legend and add ticks
		theme(legend.position = "none") + scale_x_continuous(breaks=pretty_breaks(n=6)) +

		# add text over bubble --------- use repel if too much overlapping !?!?!
		#geom_text(data = cbind(df, text = rownames(df)), aes(x = vote, y = indicator, label = text), size = rel(4), alpha = 0.7, family = font)
		geom_text_repel(data = cbind(df, text = rownames(df)), aes(x = vote, y = indicator, label = text), size = rel(4), alpha = 0.7, family = font)


		if(svgoutput) {
		  svglite(file = paste0("output/", voteName, "_signif_", j, "_b.svg"), height = svg.height, width = svg.width)
		}
		#print(sp)
		grid.arrange(sp,
		  bottom = textGrob(source, x = 0.97, y = 1, vjust = 0.1, hjust = 1,
		    gp = gpar(fontsize = 10, fontfamily = font, col = "#737373", alpha = 0.6))
		)
		if(svgoutput) {
		  dev.off()
		}
	})
	dev.off()

}









