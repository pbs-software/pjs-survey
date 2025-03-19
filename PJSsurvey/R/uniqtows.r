## uniqtows ----------------------------2019-07-24
##  Ensure unique tows occur for each survey,
##  collapse records as needed.
## -----------------------------------------PJS|RH
uniqtows <- function(dat)
{
	## check for more than 1 observation per tow:
	dat$tow = paste(dat$year, dat$set, sep="-")
	utows    = .su(dat$tow)
	if (length(utows) != nrow(dat)) {
		ntws = table(dat$tow)
		.flush.cat(paste0("WARNING!!  Up to ", max(ntws), " observations for some tows in this data set:"), "\n\n")
		dupe.tows = names(ntws)[ntws>1]
		dupe.recs = dat[is.element(dat$tow, dupe.tows),]
		print(dupe.recs[,c("year", "set", "date", "stratum", "sdepth", "edepth", "door", "distance", "weight"), drop=FALSE])
		list.recs = split(dupe.recs, dupe.recs$tow)
		poop = t(  ## transpose this shit (will be a matrix)
		sapply(list.recs, function(X) {
			doop = sapply(X,function(x){ length(.su(x))>1 })
			poop = X[1,,drop=FALSE]
			if (doop["weight"]) {
				weight = sum(X[,"weight"], na.rm=TRUE)
				poop[,"weight"] = weight
			}
			## could check for other duplicates and apply other functions
			return(poop)
		}) ) 

		expr1 = expression(dat[!is.element(dat$tow, dupe.tows),])
		good  = keepAtts(dat, expr1, extras=list(dupe.tows=dupe.tows))
		poop  = as.data.frame(poop)
		## https://stackoverflow.com/questions/27361081/r-assign-or-copy-column-classes-from-a-data-frame-to-another
		poop[] <- mapply(FUN = as, poop, sapply(good,class), SIMPLIFY = FALSE)
		.flush.cat("\nThese are revised observations for the duplicate tows in this data set:\n")
		print(poop[,c("year", "set", "date", "stratum", "sdepth", "edepth", "door", "distance", "weight"), drop=FALSE])

		expr2 = expression(rbind(dat, as.data.frame(poop)))
		dnew  = keepAtts(good, expr2, extras=list(poop=poop))

		expr3 = expression(dat[order(dat$tow),])
		dat   = keepAtts(dnew, expr3)
#browser();return()
	}
	expr = expression(dat[,setdiff(colnames(dat), "tow")])
	dat  = keepAtts(dat, expr)
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~uniqtows
