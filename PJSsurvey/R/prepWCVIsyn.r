## prepWCVIsyn-------------------------2022-05-17
##  Prepare west coast Vancouver Island synoptic survey data.
## -----------------------------------------PJS|RH
prepWCVIsyn <- function(file, survey="WCVIsyn")
{
	surveyname = "WCVI Synoptic"
	ttput(surveyname)
	ttput(survey)

	stratum = 1:4
	names(stratum) = c("50-125m", "125-200m", "200-330m", "330-500m")
	attr(stratum,"labelends") = getLabelends(labelname=names(stratum))
	ttput(stratum)

#browser();return()
	dat = doSynoptic(file, survey)

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
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~prepWCVIsyn
