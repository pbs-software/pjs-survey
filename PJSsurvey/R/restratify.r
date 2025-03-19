## restratify---------------------------2019-07-29
##  Restratify survey by depth
##  Pass the strategy number and the names of the variables with beginning and end tow depths
##  Arguments:
##  dat       -- survey data file
##  strategy  -- numeric code that chooses depths to use for restratification
##  dbegin    -- character field name for beginning depth of tow
##  dend      -- character field name for ending depth of tow
##  renamevar -- character name for new field
## -----------------------------------------PJS|RH
restratify <- function(dat, strategy, dbegin, dend, renamevar)
{
	## Definition of the values to pass for a restratification strategy:
	strategies = c("min(begin_end)", "max(begin_end)", "begin", "end", "mean(begin_end)")
	names(strategies) = 0:4

	if (strategy<0 || strategy>4) {
		stop("Passed strategy is out of range -- restratification exited w/o completion")
	} else {
		sstrategy = as.character(strategy)
		.flush.cat(paste0("Restratification subroutine using strategy: ", strategies[sstrategy]), "\n\n")
	}
	if (strategy==0)      dat$minmax = as.vector(apply(dat[,c(dbegin,dend),drop=FALSE], 1, min, na.rm=TRUE))
	else if (strategy==1) dat$minmax = as.vector(apply(dat[,c(dbegin,dend),drop=FALSE], 1, max, na.rm=TRUE))
	else if (strategy==2) dat$minmax = as.vector(dat[,dbegin])
	else if (strategy==3) dat$minmax = as.vector(dat[,dend])
	else if (strategy==4) dat$minmax = as.vector(apply(dat[,c(dbegin,dend),drop=FALSE], 1, mean, na.rm=TRUE))

	## Restratify based on strategy depth
	ttget(stratum)
	ugroup = .su(dat$group)
	zdep   = is.element(attributes(stratum)$gstrat,ugroup)
	ustrat  = attributes(stratum)$lstrat[as.character(ugroup)]

	lodep  = as.numeric(attributes(stratum)$labelends$low[zdep])
	updep  = as.numeric(attributes(stratum)$labelends$upp[zdep])
#browser();return()
	gcode  = ugroup #attributes(stratum)$grouping_code
	dat$group_old   = dat$group
	dat$group       = sapply(dat$minmax, function(mm){ zz = mm >= lodep & mm < updep; if(!any(zz)) NA else gcode[zz] },simplify=TRUE)
	dat$stratum_old = dat$stratum
	dat$stratum     = sapply(dat$minmax, function(mm){ zz = mm >= lodep & mm < updep; if(!any(zz)) NA else stratum[zdep][zz] },simplify=TRUE)
#browser();return()

	if(!missing(renamevar))
		colnames(dat)[grep("minmax", colnames(dat))] = renamevar

	no.strat = is.na(dat$stratum)
	if (any(no.strat))
		write.csv(dat[no.strat,c(c("fe_id", "year", "set", "major") , dbegin, dend, c("group_old", "group", "stratum_old", "stratum")),drop=FALSE], file=paste0(gsub(" ",".",ttcall(surveyname)),".restrat.drops.csv"), row.names=FALSE)
#browser();return()

#	if "$ifuse"~="" {
#		qui gen byte `tmpstrat'=1 $ifuse
#		di in ye _n "tagging stratum variable so that only tows which $ifuse are !=."
#		replace stratum=. if `tmpstrat'==.
#	}
#	di _n(2)
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~restratify
