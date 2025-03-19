## prepQCSsyn---------------------------2022-05-17
##  Prepare QCS synoptic survey data
## -----------------------------------------PJS|RH
prepQCSsyn <- function(file, survey)
{
	ttget(gfbsurvey)
	if (all(gfbsurvey[[survey]]==1))
		surveyname = "QC Sound Synoptic"
	else if (all(gfbsurvey[[survey]]==21))
		surveyname = "GIG Historical"
	ttput(surveyname)
	ttput(survey)

	dat    = getLabels(file)
	ttget(stratum)
	label  = attributes(dat)$label ## keep temporarily just to be safe
#browser();return()

	#stratum = 1:8
	#names(stratum) = c("South:50-125m", "South:125-200m", "South:200-330m", "South:330-500m", "North:50-125m", "North:125-200m", "North:200-330m", "North:330-500m")
	#ttput(stratum)

	#delimit cr
	#lab save stratum using $stratumlabelfilename, replace

	dat = doSynoptic(dat, survey)

	if (all(gfbsurvey[[survey]]==21)) {
		dat$stratum = dat$stratum + 1  ## this won't work because strata are now characters
		if (dat$species=="396") {
			dat$area[dat$stratum==2] = 4148
			dat$area[dat$stratum==3] = 2200
			#notes drop area in 1
			#notes area: POP survey data: area values replaced with restratified POP values
		}
	}

	## Define depth strata
	#depstrat = 1:4
	#names(depstrat) = c("50-125m", "125-200m", "200-330m", "330-500m")
	#attr(depstrat,"labelends") = getLabelends(labelname=names(depstrat))
	#ttput(depstrat)

	#label = attributes(dat)$label
	#label[["depstrat"]] = list(decribe="Depth intervals")
	#label[["depstrat"]][["interval"]] = names(depstrat)
	#attr(dat,"label") = label

	## Populate dat with 'depstrat' -- note not terribly consistent with depth manipulation in 'doSynoptic'
	#lodep = as.numeric(attributes(depstrat)$labelends$low)
	#updep = as.numeric(attributes(depstrat)$labelends$upp)
	#dat$best_depth = coalesce(dat$seabird_depth, dat$sdepth, dat$edepth)  ## now in 'doSynoptic'
	#dat$depstrat = rep(NA, nrow(dat))
	#for (i in 1:length(depstrat)){
	#	ii = depstrat[i]
	#	zz = dat$best_depth>=lodep[i] & dat$best_depth<updep[i] & !is.na(dat$best_depth)
	#	dat$depstrat[zz] = ii
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
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~prepQCSsyn
