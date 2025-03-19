## prepNMFStri--------------------------2022-05-17
##  Prepare US National Marine Fisheries Service
##  triennial survey data from WCVI.
## -----------------------------------------PJS|RH
prepNMFStri <- function(file, survey="NMFStri")
{
	ici = lenv() ## RH 200716
	surveyname = "NMFS Triennial"
	ttput(surveyname)
	ttput(survey)

	stratum = 475:500 ## GFBio Grouping Code
	## Set strata to the original stratum codes from NMFS Triennial survey
	## Depth strata: 10-19 = shallow (55-183m), 20-29 = mid-depth (184-366m), 30-39 = deep (367-500m)
	## WCVI strata = 19, 29, 39
	stratum = c(10, 11, 12, 17, 18, 19, 27, 28, 29, 30, 31, 32, 37, 38, 39, 50, 51, 52, 121, 122, 123, 124, 131, 132, 133, 134)
	names(stratum) = 475:500 ## name using GFBio grouping codes
	ttput(stratum)

	dat   = doSynoptic(dat, survey) ## even though it's not synoptic, function has some standardising routines
	ttget(stratum)                   ## had attributes added in 'doSynoptic'
	label = attributes(dat)$label    ## add later as there is a lot of fiddly manipulation that follows

	note = list()
	note[["author"]] = "Paul J. Starr"
	note[["code_canada"]] = c(
		"cap drop canad",
		"gen byte canad=.",
		"replace canad=1 if substr(stratum,3,1)==\"N\"",
		"replace canad=. if substr(stratum,2,1)==\"7\"&canad~=.",
		"replace canad=1 if year==1980&(stratum==\"11\"|stratum==\"31\"|stratum==\"51\")",
		"replace canad=1 if year==1983&(stratum==\"12\"|stratum==\"32\"|stratum==\"52\")",
		"replace canad=1 if stratum==\"39\"",
		"replace canad=2 if canad==.",
		"lab def canad 2 \"US waters\" 1 \"Canadian waters\"",
		"lab val canad canad",
		"lab var canad \"US/Canada flag\"",
		"gen byte strat=real(substr(stratum,1,2))",
		"gen byte llen=length(stratum)",
		"gen byte portion=.",
		"replace portion=1 if llen==3&substr(stratum,3,1)==\"S\"",
		"replace portion=2 if llen==3&substr(stratum,3,1)==\"N\"",
		"cap label drop portion",
		"lab def portion 1 \"S\" 2 \"N\"",
		"lab val portion portion",
		"drop llen",
		"lab var portion \"N/S identifier\""
	)

	## Remove survey data prior to 1980
	dat = dat[dat$year>=1980 & !is.na(dat$year),]

	## Delineate Vancouver and Columbia INPFC regions (Columbia below 47.5 deg N)
	inVan     = dat$latitude > 47.5 & !is.na(dat$latitude)
	dat$INPFC = rep("",nrow(dat))
	dat$INPFC[inVan]  = "Vancouver"
	dat$INPFC[!inVan] = "Columbia"

	## GFBio has no information on tows in Canada vs USA other than strata 19, 29, and 39.
	## Use shapefile 'eez.bc' in package PBSdata to delimit tows based on their geographical coordinates
	data("eez.bc", package="PBSdata", envir=ici) ## RH 200716
	dat$EID = 1:nrow(dat)
	dat$X   = dat$longitude
	dat$Y   = dat$latitude
	dat     = as.EventData(dat, projection="LL")
	locdat  = findPolys(dat, eez.bc, includeBdry=1)
	inCan   = is.element(dat$EID, locdat$EID)

	dat$country         = rep("",nrow(dat))
	dat$country[inCan]  = "CAN"
	dat$country[!inCan] = "USA"

	## Define depth strata
	depstrat = 1:3
	names(depstrat) = c("55-183m", "184-366m", "367-500m")
	attr(depstrat,"labelends") = getLabelends(labelname=names(depstrat))
	ttput(depstrat)

	dat$depstrat = rep(NA, nrow(dat))
	lodep = as.numeric(attributes(depstrat)$labelends$low)
	updep = as.numeric(attributes(depstrat)$labelends$upp)
	## Depths in SQL extraction from GFBioSQL are in metres
	dat$depstrat = sapply(dat$best_depth, function(mm){ zz = mm >= lodep & mm < updep; if(!any(zz)) NA else depstrat[zz] },simplify=TRUE)

	## Individual tweaks
	dat$depstrat[is.element(dat$stratum,10:12) & is.element(dat$year,c(1980,1983))] = 1  ## comment: all are 1 already
	dat$depstrat[is.na(dat$depstrat) & is.element(dat$year,c(1980,1983))] = 2            ## comment: no NA depstrats in these years
	dat$depstrat[is.element(dat$stratum,17:19) & dat$year>=1989] = 1                     ## comment: 3 NAs changed to 1
	dat$depstrat[is.element(dat$stratum,20:29) & dat$year>=1989] = 2                     ## comment: all are 2 already
	dat$depstrat[dat$stratum>=30 & dat$year>=1989] = 3                                   ## comment: all are 3 already

	## Create strata for three indices (i) Canada Vancouver, (ii) USA Vancouver, and (iii) Total Vancouver
	dat$stratTT = dat$stratUS = dat$stratCA = NA
	dat$stratCA[inVan & inCan]   = dat$stratum[inVan & inCan]
	dat$stratUS[inVan & !inCan]  = dat$stratum[inVan & !inCan]
	dat$stratTT[inVan] = dat$stratum[inVan]

	## Why would you want to set strata to NA?
	## Notes suggest for biomass estimation (see PJS Table B.5 in RSR 2018 ResDoc)
	in17u = is.element(dat$stratUS, 17)    ## USA
	in18c = is.element(dat$stratCA, 18)    ## CAN
	in27u = is.element(dat$stratUS, 27)    ## USA
	in28c = is.element(dat$stratCA, 28)    ## CAN
	in30s = is.element(dat$stratTT, 37:39) ## ALL
#browser();return()
	dat$stratUS[in17u|in27u] = dat$stratTT[in17u|in27u] = NA
	dat$stratCA[in18c|in28c] = dat$stratTT[in18c|in28c] = NA
	dat$stratCA[in30s] = dat$stratUS[in30s] = dat$stratTT[in30s] = NA

	#dat$strat[is.element(dat$strat, c(17,18,27,28)) & is.element(dat$country,"USA")] = NA
	## PJS code for parsing strata on boundary where portion=1 is 'N' and portion=2 is 'S'
	#replace strat=. if (strat==17|strat==27)&portion==1	
	#replace strat=. if (strat==18|strat==28)&portion==2
	#merge 1:1 year vess haul using D:\Groundfish\2017\Redstripe\data\surveys\WCVITriennial\usadecod.dta, keepusing(usability) nogen
	#replace strat=. if usability==5 ## already handled by 'getUsability' in 'doSynoptic'

	## Need to estimate density (none provided)
	dat$weight[is.na(dat$weight)] = 0
	dat$number[is.na(dat$number)] = 0
	dat$cpue = dat$weight / dat$effort  ## kg/h
	dat$cpue[is.na(dat$cpue)] = 0
	#dat$door[is.na(dat$door)]   = 43  ## all door widths available (as MOUTH_OPENING_WIDTH)
	dat$speed[is.na(dat$speed)] = 5.1  ## no speeds available so set to a constant (using HS Ass)
	dat$distance[is.na(dat$distance)] = dat$speed * dat$effort  ## not really necessary because all distances are available
	dat$density = dat$weight / (dat$distance * dat$door/1000.)
	dat$density[is.na(dat$density)] = 0

	## Stratum definitions used in the 1980 and 1983 surveys were different than
	## those used in subsequent surveys (9166 km^2 / 7399 km^2) = 1.24
	scaleup = is.element(dat$year, c(1980,1983))
	## RH: Not sure that this is needed when stratum areas from earlier time periods have been estimated (see next code segment).
	#dat$density[scaleup] = dat$density[scaleup] * 1.24

	#Stratum areas (from PJS Survey Appendices in rockfish assessments where available and convex hulls if not)
	areaCA = c(0,8948,5317,0,159,8224,0,88,942,0,804,525,0,66,875,0,43,0)
	areaUS = c(3795,2422,0,1033,2123,363,125,787,270,725,1134,0,102,175,0,198,866,0)
	areaTT = c(3795,12948,5317,1033,2282,8587,125,875,1212,725,5481,525,102,241,875,198,7303,0)
	names(areaCA) = names(areaUS) = names(areaTT) = c(10,11,12,17,18,19,27,28,29,30,31,32,37,38,39,50,51,52)

	dat$areaTT = dat$areaUS = dat$areaCA = rep(NA,nrow(dat))
	zCA = inVan & inCan & is.element(dat$stratCA, names(areaCA)) & !is.na(dat$stratCA)
	dat$areaCA[zCA]  = areaCA[as.character(dat$stratCA[zCA])]
	zUS = inVan & !inCan & is.element(dat$stratUS, names(areaUS)) & !is.na(dat$stratUS)
	dat$areaUS[zUS] = areaUS[as.character(dat$stratUS[zUS])]
	zTT = inVan & is.element(dat$stratTT, names(areaTT)) & !is.na(dat$stratTT)
	dat$areaTT[zTT] = areaTT[as.character(dat$stratTT[zTT])]
#browser();return()


	note[["general"]] =c(
		"This is a file of the NMFS triennial survey data from 1980 to 2001.",
		"Use variable 'strat' instead of 'stratum' for biomass estimation." )
	note[["strat"]] = "Strata not used in biomass estimation have been set ==. (missing) -- RH use fields 'stratCA' or 'stratUS'"
	note[["depstrat"]] = "Depth identifier for all tows (not coded for biomass estimation)."
	note[["usability"]] =c(
		"Water hauls identified with usability==5 (removed from biomass estimation).",
		"See Chapter B.4.4 in Appendix B (surveys) of the Redstripe Rockfish stock assessment ResDoc for a discussion of this issue.",
		"See [InfoRecall\\Groundfish2017&2018\\Data preparation notes\\water hauls in the Triennial survey data] for how these tows were identified."
	)

	## Choose fields for data return
	if (!is.null(ttcall(triflds))){
		ttget(triflds)
		outflds = intersect(triflds, colnames(dat))
	} else {
		.flush.cat("All data fields will be returned.\n\tRun 'prepGFsurv' to get a standard subset of fields.\n\n")
		outflds = colnames(dat)
	}
	expr = expression(dat[,outflds])
	dat  = keepAtts(dat, expr, extras=list(outflds=outflds))

	attr(dat,"label") = label
	attr(dat,"note")  = note
#browser();return()
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~prepNMFStri
