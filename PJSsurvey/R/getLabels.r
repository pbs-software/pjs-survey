## getLabels----------------------------2022-05-19
##  Read in data file and get labels.
## -----------------------------------------PJS|RH
getLabels <- function(file)
{
	dat = read.csv(file) ## insheet using `file'
	## Process strata for GIG historical (because data have no grouping codes)
	if (grepl("SSID=21",file)) {
		dat = dat[dat$year<=1995 & !is.na(dat$year),]
		gcode = c(185, 186, 187) ## in GFBioSQL's GROUPING table
		lodep = c(120,183,218)
		updep = c(183,218,300)
		areas = c(2122,1199,1746)
		ndeps = length(lodep)
		GCloc = GC
		dat$GROUPING_CODE = dat$area = rep(NA, nrow(dat))  ## restratify all surveys
		for (i in 1:ndeps) {
			z = is.na(dat$GROUPING_CODE) & dat$begin_depth > lodep[i] & dat$begin_depth <= updep[i]
			dat$GROUPING_CODE[z] = gcode[i]
			dat$area[z] = areas[i]
			#GCloc = rbind(GCloc, c(gcode[i], paste0("H",i), round(lodep[i]), round(updep[i]), areas[i]))
		}
		#assign("GC", GCloc, envir=.GlobalEnv)
		dat = dat[!is.na(dat$GROUPING_CODE),]
	}
	
	## Get the strata and grouping codes directly without having to be specific in various other functions (RH 220518)
	ugroup = .su(dat$GROUPING_CODE)
	udeps  = GC[is.element(GC$group,ugroup),c("stratum","mindep","maxdep"),drop=FALSE]; rownames(udeps) = ugroup
	lstrat = apply(udeps,1,function(x){paste0(x[1],":",as.numeric(x[2]),"-",as.numeric(x[3]),"m")})
	ustrat = GC[is.element(GC$group,ugroup),"stratum"]; names(ustrat) = lstrat
	nstrat = length(ustrat)
	gstrat = as.numeric(names(lstrat)); names(gstrat) = lstrat
	stratum = ustrat
	attr(stratum,"gstrat") = gstrat
	attr(stratum,"lstrat") = lstrat
	attr(stratum,"labelends") = getLabelends(labelname=names(stratum))
	ttput(stratum)

	flds = colnames(dat)
	flds[grep("distance_travelled", flds)] = "distance" ## ren distance_travelled distance
	flds[grep("begin_depth", flds)]   = "sdepth"        ## ren begin_depth sdepth
	flds[grep("end_depth", flds)]   = "edepth"          ## ren end_depth edepth
	flds[grep("^time$", flds)]  = "effort"              ## ren time effort (retrieval - deployment)
	flds[grep("^time2$", flds)] = "effort2"             ## ren time2 effort2 (end bottom contact - start bottom contact)
	flds = tolower(flds)
	colnames(dat) = flds
	dat$species = getSpecies(dat$species)
	dat$month   = getMonth(dat$date)
#	run $stratumlabelfilename  ## what does this do?

	if (mean(dat$longitude,na.rm=TRUE) > 0)
		dat$longitude = -dat$longitude

	## Initialize the label list
	#label = list(survey=list(number=ttcall(survey), name=ttcall(surveyname), file=file))
	label = list(survey=list(code=ttcall(survey), name=ttcall(surveyname), file=file))

	label[["fields"]] = list()
	label[["fields"]][["effort"]]   = "[Begin_retrieval]-[End_deployment] (hrs)"
	label[["fields"]][["effort2"]]  = "[End_bottom contact]-[Begin_bottom contact] (hrs)"
	label[["fields"]][["door"]]     = "Doorspread (m)"
	label[["fields"]][["speed"]]    = "Speed (km/h)"
	label[["fields"]][["sdepth"]]   = "Depth at beginning of tow (m)"
	label[["fields"]][["edepth"]]   = "Depth at end of tow (m)"
	label[["fields"]][["distance"]] = "Distance travelled (km)"

	label[["reason"]]  = list(describe="Survey purpose")
	getData("REASON", "GFBioSQL")
	label[["reason"]][["codes"]] = PBSdat
	attr(dat, "label") = label
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~getLabels
