## doSynoptic --------------------------2023-01-26
##  Get the synoptic data for specified survey and
##  go through PJS machinations.
##  Can be applied to non-synoptic surveys.
## -----------------------------------------PJS|RH
doSynoptic <- function(dat, survey, logappendflag=TRUE)
{
	logfile = "surveyprepmessages.log"
	if (logappendflag) append=TRUE else FALSE
	
	#ppost1 = c("fe_id", "year", "set", "stratum", "lowbnd", "upbnd", "sdepth", "edepth", "seabird_depth", "using", "depth_problem_tows")
	#ppost2 = c("fe_id", "year", "set", "stratum", "lowdistance", "updistance", "distance", "distance_calc", "speed", "effort", "effort2", "using", "distance_problem_tows")
	uppdistance  = 2.4
	lowdistance  = 0.9

	ttget(stratum)
	vessels = getVessel(dat)

	## Subsetting in R loses attributes (very insane) so need to preserve and add back (sheesh)
	expr = expression (dat[,setdiff(colnames(dat), c("wing", "open"))])  ## drop trip wing open
	dat  = keepAtts(dat, expr)
	dat  = getUsability(dat)

	year    = .su(dat$year)
	nyear   = length(year)
	minyear = min(year)
	maxyear = max(year)

	ugroup  = .su(dat$grouping_code)
	ustrat  = attributes(stratum)$lstrat[as.character(ugroup)]
	Nstrat   = length(ustrat)
	colnames(dat)[grep("grouping_code", colnames(dat))] = "group"

	##Define 'group' based on 'stratum', which was originally 'grouping_code'
	dat$best_depth = coalesce(dat$sdepth, dat$edepth, dat$modal_depth, dat$seabird_depth)
	sgroup = stratum[ustrat]; names(sgroup) = names(ustrat)
	dat$stratum_group = rep(NA, nrow(dat))
	dat$stratum_group[!is.na(dat$group)] = sgroup[as.character(dat$group[!is.na(dat$group)])]

	dat$stratum_depth = rep(NA, nrow(dat))
	zdep = is.element(attributes(stratum)$gstrat,ugroup)
	for (i in 1:sum(zdep)) {
		z = dat$best_depth > as.numeric(attributes(stratum)$labelends$low[zdep][i]) & dat$best_depth <= as.numeric(attributes(stratum)$labelends$upp[zdep][i])
		dat$stratum_depth[z] = stratum[zdep][i]
	}
	dat$stratum = coalesce(dat$stratum_group, dat$stratum_depth)

	if (all(is.na(dat$door))) {
		.flush.cat(paste0(ttcall(surveyname), " has no doorspread values:\n\tusing ", ttcall(defaultdoorspread), " m as default value for all tows."), "\n")
		dat$door = ttcall(defaultdoorspread)
	}

	## Populate missing values of 'distance', 'door', and 'speed' with means
	## May eventually become a primary function if it's useful on a broader scale.
	## Added in loop to factor in vessels over the years (RH 220524)
	fixmiss <- function(dat, flds) {
		uvess = .su(dat$vessel)
		for (v in uvess) {
			zv = is.element(dat$vessel,v)
			vdat = dat[zv,]
			for (i in flds) {
				## If missing values, use mean by year and group #stratum (formerly 'grouping_code')
				ibad = vdat[,i]<=0 | is.na(vdat[,i])
				if (all(ibad)) next
				if (any(ibad)) {
					itab = crossTab(vdat, c("year","group"), i, function(x){if (all(is.na(x))) NA else mean(x,na.rm=TRUE)})
					if (all(is.na(itab))) next
					barf = try(diag(itab[as.character(vdat$year[ibad]), as.character(vdat$group[ibad]), drop=FALSE]), silent=TRUE)  ## (debug RH 230126)
					if(inherits( barf, "try-error" )) {
						imean = try(mean(itab, na.rm=TRUE))
						if (inherits(imean, "try-error")){
							browser(); return()
						}
					} else {
						imean = diag(itab[as.character(vdat$year[ibad]), as.character(vdat$group[ibad]), drop=FALSE])
					}
					vdat[,i][ibad] = imean
				} else next
				## If still missing values, use mean by year
				jbad = vdat[,i]<=0 | is.na(vdat[,i])
				if (all(jbad)) next
				if (any(jbad)) {
					jtab = crossTab(vdat, c("year"), i, function(x){if (all(is.na(x))) NA else mean(x,na.rm=TRUE)})
					vdat[,i][jbad] = jtab[as.character(vdat$year[jbad])]
				} else next
				## If still missing values, use overall mean
				kbad = vdat[,i]<=0 | is.na(vdat[,i])
				if (all(kbad)) next
				if (any(kbad)) {
					ktab =  mean(vdat[,i],na.rm=TRUE)
					vdat[,i][kbad] = ktab
				}
				dat[zv,i] = vdat[,i]  ## upload machinations for field i
			}
		}
		return(dat)
	}

	## Does not make sense to avergae distance and effort for non-standardised (synoptic) surveys
	if (survey %in% c("QCSsyn","WCVIsyn","WCHGsyn","HSsyn")) {  ## i.e. synoptic surveys
		dat = fixmiss(dat, c("door","distance","speed","effort"))
	} else {
		dat = fixmiss(dat, c("door","speed"))
	}

	## Check for inconsistencies in depth information
	## (only do this if the strata limits are consistent with what is in the file)
	#if (survey %in% c(2,3,9,10)) {  ## i.e. synoptic surveys
	#if (survey %in% c("QCSsyn","WCVIsyn","WCHGsyn","HSsyn")) {  ## i.e. synoptic surveys
	## Apply to all surveys:
		lodep = as.numeric(attributes(stratum)$labelends$low[zdep])
		updep = as.numeric(attributes(stratum)$labelends$upp[zdep])
		if (length(lodep)!=0 && length(updep)!=0) {
			names(lodep) = names(updep) = ugroup #stratum[zdep] #ustrat[!is.na(stratum)]
			dat$lodep    = lodep[as.character(dat$group)]
			dat$updep    = updep[as.character(dat$group)]
			seabird.dat  = dat[!is.na(dat$seabird_depth),]
			bad.depth    = seabird.dat$seabird_depth < seabird.dat$lodep | seabird.dat$seabird_depth > seabird.dat$updep
			if (any(bad.depth))
				write.csv(dat[bad.depth,c("fe_id", "year", "set", "stratum", "lodep", "updep", "sdepth", "edepth", "seabird_depth")], file=paste0(gsub(" ",".",ttcall(surveyname)),".depth.problems.csv"), row.names=FALSE)
		}
	#}

	## Calculate distance travelled
	dat$distance_calc = dat$speed * dat$effort2
	bad.dist = is.na(dat$distance_calc)
	dat$distance_calc[bad.dist] = dat$speed[bad.dist] * dat$effort[bad.dist]

	## Check for inconsistencies in distance information
	dat$lodist   = rep(lowdistance, nrow(dat))
	dat$updist   = rep(uppdistance, nrow(dat))
	bad.dist = (dat$distance_calc < dat$lodist | dat$distance_calc > dat$updist) & !is.na(dat$distance_calc)
	if (any(bad.dist))
		write.csv(dat[bad.dist,c("fe_id", "year", "set", "stratum", "lodist", "updist", "distance", "distance_calc", "speed", "effort", "effort2")], file=paste0(gsub(" ",".",ttcall(surveyname)),".distance.problems.csv"), row.names=FALSE)

	miss.dist = dat$distance<=0 | is.na(dat$distance)
	if (any(miss.dist)) {
		.flush.cat("Replacing missing distance travelled cells with calculated distance travelled","\n")
		pos.eff = dat$effort>0 & !is.na(dat$effort)
		pos.eff = rep(FALSE,length(pos.eff))  ## disable lm routine for now
		if (any(pos.eff)) {
			pdat    = dat[pos.eff,]
			lm.dist = lm(pdat$distance_calc ~ pdat$effort)  ## a bit tautological if lots of distances calculated as speed * effort above
			lm.dist = lm(pdat$distance ~ pdat$effort) 
			lm.coef = coefficients(lm.dist)
			zpred   = miss.dist & pos.eff
			dat$distance[zpred] = lm.coef[1] + lm.coef[2] * dat$effort[zpred]
		} else {
			dat$distance[miss.dist] = dat$distance_calc[miss.dist]
		}
	}
	## PJS averages speed over all fleets to calculate distance
	miss.speed = dat$speed<=0 | is.na(dat$speed)
	miss.dist  = dat$distance<=0 | is.na(dat$distance)
	if (any(miss.speed) && !all(miss.speed) && any(miss.dist)) {
		.flush.cat("Replacing missing speed with fleetwide average and populate missing distance (speed*effort)","\n")
		mean.speed  = mean(dat$speed[!miss.speed], na.rm=TRUE)
		dat$speed[miss.speed] = mean.speed
		best.effort = coalesce(dat$effort, dat$effort2)
		miss.effort = best.effort<=0 | is.na(best.effort)
		dist.effort   = miss.dist & !miss.effort
		dat$distance[dist.effort] = dat$speed[dist.effort] * dat$effort[dist.effort]
	}
	## Call this routine to ensure that there is only one observation per tow
	#if (survey %in% c(2,3,9,10))  ## i.e. synoptic surveys
	if (survey %in% c("QCSsyn","WCVIsyn","WCHGsyn","HSsyn"))  ## i.e. synoptic surveys
		dat = uniqtows(dat)

	dat$density = dat$weight / (dat$distance * dat$door/1000.)
	dat$density[is.na(dat$density)] = 0

	dat$best_depth = coalesce(dat$seabird_depth, dat$sdepth, dat$edepth)

	## Change a few field names
	colnames(dat)[grep("usability",colnames(dat))]      = "use"
	colnames(dat)[grep("dfo_stat",colnames(dat))]       = "dfo"
	colnames(dat)[grep("seabird_depth",colnames(dat))]  = "seabird"

	ttput(dat)
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~doSynoptic
