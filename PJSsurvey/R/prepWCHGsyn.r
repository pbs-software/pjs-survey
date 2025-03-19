## prepWCHGsyn--------------------------2022-05-18
##  Prepare WCHG (formerly WCQCI) synoptic survey data
## -----------------------------------------PJS|RH
prepWCHGsyn <- function(file, survey="WCHGsyn")
{
	surveyname = "WCHG Synoptic"
	ttput(surveyname)
	ttput(survey)

	dat    = getLabels(file)
	ttget(stratum)
	label  = attributes(dat)$label ## keep temporarily just to be safe

	#stratumWCHG = 1:4
	#names(stratumWCHG) = c("180-330m", "330-500m", "500-800m", "800-1300m")
	#attr(stratumWCHG,"labelends") = getLabelends(labelname=names(stratumWCHG))
	#ttput(stratumWCHG)
   #
	#stratum2006 = 1:5
	#names(stratum2006) = c("150-200m", "200-330m", "330-500m", "500-800m", "800-1300m")
	#attr(stratum2006,"labelends") = getLabelends(labelname=names(stratum2006))
	#ttput(stratum2006)

	expr     = expression(dat[is.element(dat$year, 2006),])
	only2006 = keepAtts(dat, expr)
	#stratum  = stratum2006; ttput(stratum)
	only2006 = doSynoptic(only2006, survey)
	#ttget(stratum) ## doSynoptic adds a grouping code
	#nstrat   = attributes(stratum)$labelends$nstrat
	only2006 = restratify(only2006, strategy=4, "sdepth", "edepth")
	label[["usability"]] = attributes(only2006)$label$usability[c("describe","codes")]
	label[["usability"]][["only2006"]] = attributes(only2006)$label$usability[c("note", "Nrows", "nrows", "use")]
	only2006$stratum = as.numeric(only2006$stratum) - 1
	only2006 = only2006[only2006$stratum >= 1 & !is.na(only2006$stratum),]  ## shallowest stratum not repeated in subsequent years
	only2006$stratum = as.character(only2006$stratum)
#browser();return()

	expr    = expression(dat[!is.element(dat$year, c(2006, 2014)),])
	no2006  = keepAtts(dat, expr)
	#stratum = stratumWCHG; ttput(stratum)
	no2006  = doSynoptic(no2006, survey)
	#ttget(stratum) ## doSynoptic adds a grouping code
	nstrat  = attributes(stratum)$labelends$nstrat
	label[["usability"]][["no2006"]] = attributes(no2006)$label$usability[c("note", "Nrows", "nrows", "use")]
#browser();return()

	intflds  = intersect(colnames(only2006),colnames(no2006))
	dat      = rbind(only2006[,intflds], no2006[,intflds])
	attr(dat, "label") = label

	## Replace stratum areas for 2006 with those from later years
	.flush.cat("Replacing area values in 2006 with 2007+ values", "\n")
	areatab  = crossTab(dat[!is.element(dat$year, 2006),], c("year","group"), "area", mean, na.rm=TRUE)
	areavec  = apply(areatab, 2, function(x){ mean(x[x>0 & !is.na(x)]) } )
	avec2006 = c(399,1266,1090,927,2228); names(avec2006) = 125:129 ## from GFB C_Grouping
	areavec  =  c(areavec, avec2006)

	dat$area[is.element(dat$year, 2006)] = 0
	fix.area = (dat$area==0 | is.na(dat$area)) & !is.na(dat$group)
	dat$area[fix.area] = areavec[as.character(dat$group[fix.area])]
#browser();return()

	## Get rid of deepest stratum (4) -- not fished in 2006 & 2007
	.flush.cat("Removing shallowest stratum(==0: 15-200 m) & deepest stratum (==4: 800-1300 m) [not fished consistently]", "\n\n")
	expr = expression(dat[!is.na(dat$stratum) & !is.element(dat$stratum,c(0,4)),])
	dat  = keepAtts(dat, expr)

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
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~prepWCHGsyn
