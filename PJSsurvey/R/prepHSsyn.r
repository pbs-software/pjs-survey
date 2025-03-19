## prepHSsyn----------------------------2022-05-17
##  Prepare Hecate Strait synoptic survey data.
## -----------------------------------------PJS|RH
prepHSsyn <- function(file, survey="HSsyn")
{
	surveyname = "Hecate Strait Synoptic"
	ttput(surveyname)
	ttput(survey)

	dat    = getLabels(file)
	ttget(stratum)
	label  = attributes(dat)$label ## keep temporarily just to be safe
#browser();return()

	#stratum = 1:4
	#names(stratum) = c("10-70m", "70-130m", "130-220m", "220-500m")
	#attr(stratum,"labelends") = getLabelends(labelname=names(stratum))
	#ttput(stratum)

	dat = doSynoptic(dat, survey)

	#for (i in stratum) {  ## why?
	#	z = !is.na(dat$stratum) & is.element(dat$stratum,i)
	#	dat$area[z] = switch(as.numeric(i), 5958, 3011, 2432, 1858)
	#}

	## Choose fields for data return
	if (!is.null(ttcall(synflds))){
		ttget(synflds)
		outflds = intersect(synflds, colnames(dat))
	} else {
		.flush.cat("All data fields will be returned.\n\tRun 'prepGFsurv' to get a standard subset of fields.\n\n")
		outflds = colnames(dat)
	}
	expr = expression(dat[,outflds])
	dat  = keepAtts(dat, expr, extras=list(outflds=outflds))
#browser();return()
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~prepHSsyn
