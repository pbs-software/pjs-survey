## prepGFsurv---------------------------2022-05-19
##  Prepare GF surveys using PJS codes.
## -----------------------------------------PJS|RH
prepGFsurv <- function(file, survey, strSpp, datemask, doorspread=61.6,
   shrimpyear=1975, savefile=TRUE, usevar=FALSE, attended=FALSE,
   spath="C:/Users/haighr/Files/Projects/R/Develop/PBStools/Authors/SQLcode")
{
	if (!file.exists("GC.rda")) {
		getData("SELECT G.GROUPING_CODE AS 'group', ISNULL(G.GROUPING_SPATIAL_ID,'') + ISNULL(G.GROUPING_DEPTH_ID,'') AS 'stratum', G.MIN_DEPTH_M AS 'mindep', G.MAX_DEPTH_M AS 'maxdep', G.AREA_KM2 AS 'area' FROM GROUPING G WHERE G.MIN_DEPTH_M IS NOT NULL AND G.MAX_DEPTH_M IS NOT NULL", dbName="GFBioSQL", type="SQLX")
		GC = PBSdat
		save("GC", file="GC.rda")
	} else {
		load("GC.rda", envir=.GlobalEnv)
	}
	ici = lenv() ## RH 200716
	#syntax, File(string) SUrvey(int) [DAtemask(string) DOorspread(real 61.6) SHrimpyear(int 1975)] /*

	if (missing(file))
		stop("Supply a CSV file name (e.g. 'SSID=16&species=435.csv') saved form a call to 'gfb_survey_data.sql'")
	if (missing(survey))
		stop("Supply a survey code.\n")
	#if (missing(survey))
	#	stop(paste0("Supply a PJS survey number from:\n",
	#"   1: old Hecate St Survey\n",
	#"      (note: this program does not prepare the recruited biomass estimate as done for ENL in 2006)\n",
	#"   2: QC Sound Synoptic\n",
	#"   3: WCVI Synoptic\n",
	#"   4: WCVI Shrimp\n",
	#"   5: NMFS Triennial\n",
	#"   6: Pcod Monitoring\n",
	#"   7: QCSound Shrimp\n",
	#"   8: historical GB Reed survey, now called GIG Historical\n", 
	#"      (includes 1984 Eastward Ho and 1990s Ocean Selector and Frosti -- use this for biomass ests)\n",
	#"   9: WCQCI Synoptic (added Nov 2008), now call WCHG (west coast Haida Gwaii)\n",
	#"  10: Hecate St synoptic survey (added July 2010)\n",
	#"  11: retrospective GIG (includes GB Reed and misc GIG surveys)\n",
	#"      (added Nov 2009 -- not for biomass)\n",
	#"  12: retrospective GIG\n",
	#"      (1995 Ocean Selector and Frosti only -- QC Snd synoptic stratification) (added Dec 2009)"
	#))
	#maxsurvey = 16
	#if (survey < 1 || survey > maxsurvey) {
	#	stop("You have selected an invalid survey code\nProgram ended")
	#}
	if (!missing(strSpp) && !grepl(strSpp,file))
		.flush.cat(paste0("WARNING -- string supplied by strSpp='", strSpp, "' does not appear in file name: '", file, "'."), "\n")
	if ( grepl("species=",file) ) {  ## overide strSpp if this condition is true
			strSpp = substring(file,regexpr("species=",file)+8, regexpr("species=",file)+10)  ## assume PJS standard naming
			.flush.cat(paste0("Argument 'strSpp' set to '", strSpp, "' based on input file name."), "\n\n")
	}
	if (missing(strSpp))
		stop("\n\n!!!!! String species neither supplied by user nor identified in file name.\n\tSupply species HART code to 'strSpp'\n\n")

	data(spn, package="PBSdata", envir=ici) ## RH 200716
	speciesname = spn[strSpp]
	ttput(speciesname)

	#gfbsurvey = c(2, 1, 4, 7, 79, 5, 6, 32, 16, 3, 21, 33); names(gfbsurvey) = 1:12
	gfbsurvey = list('QCSsyn'=1, 'HSass'=2, 'HSsyn'=3, 'WCVIsyn'=c(4,9), 'HSpac'=5, 'QCSshr'=6, 'WCVIshr'=7, 'WCHGsyn'=c(8,16),
		'WCVIlst'=11, 'IPHCll'=14, 'GIGhis'=21, 'HBLLn'=22, 'HBLLs'=36, 'SBFtrap'=42, 'SOGsyn'=45, 'NMFStri'=79)
	ttput(gfbsurvey)

	synflds = c("year", "month", "date", "vessel", "set", "fe_id", "group", "stratum", "major", "dfo", "area", "speed", "effort", "effort2", "distance", "distance_calc", "latitude", "longitude", "sdepth", "edepth", "seabird", "best_depth", "door", "reason", "use", "weight", "number", "density")
	ttput(synflds) ## for use in the various 'prep' survey functions

	assflds = c("year", "month", "date", "vessel", "trip", "set", "group", "stratum", "major", "dfo", "area", "speed", "effort", "distance", "distance_calc", "latitude", "longitude", "sdepth", "edepth", "seabird", "best_depth", "door", "reason", "use", "weight", "number", "cpue", "density")
	ttput(assflds) ## for use in the 'prepHSass' survey function

	triflds = c("year", "month", "date", "vessel", "trip", "set", "group", "stratCA", "stratUS", "stratTT", "areaCA", "areaUS", "areaTT", "speed", "effort", "distance", "latitude", "longitude", "best_depth", "door", "use", "weight", "number", "cpue", "density", "canada")
	ttput(triflds) ## for use in the 'prepNMFStri' survey function

	gigflds = c("year", "month", "date", "vessel", "set", "stratum", "depstrat", "group", "dfo", "area", "speed", "effort", "distance", "latitude", "longitude", "sdepth", "edepth", "door", "reason", "use", "weight", "number", "density")
	ttput(gigflds) ## for use in the 'prepGIGhis' survey function

	fm_to_m   = 1.8288; ttput(fm_to_m)   ## conversion from fathoms to metres
	kn_to_km  = 1.853248                 ## conversion from nautical miles to kilometres
	ft_to_m   = 0.3048                   ## feet to metres
	defaultdoorspread = doorspread; ttput(defaultdoorspread)
	shrimpdropyear    = shrimpyear
	defaultshrimpdoorspread = 29.6/1000  ## in km: to conform with Norm Olsen "best practice"
	#else survey=`survey'
	#drop _all
	#label drop _all

	#filename = as.character(substitute(file))
	if (!file.exists(file)) {
		getData("gfb_survey_data.sql",dbName="GFBioSQL",strSpp=strSpp, survserid=gfbsurvey[[survey]], path=spath)
		write.csv(PBSdat, file=file, row.names=FALSE)
	}

	if (missing(datemask)) datemask = "dmy"
	#else global datemask "`datemask'"
	if (usevar) useflag = 1
	else        useflag = 0
	if (attended) attendflag = ""
	else          attendflag = "notattended"

	#*di in ye "attended: `attended' global attendflag: $attendflag"  ## display?
	.flush.cat(paste0("attended: ", attended, "  attendflag: ", attendflag),"\n")
	
	fnsurv = survey #switch(survey,
		#"HSass",  "QCSsyn", "WCVIsyn", "WCVIshr", "NMFStri",
		#"PACmon", "QCSshr", "GIGhis",  "WCHGsyn", "HSsyn",
		#"GIGhis", "QCSsyn", "1996caledonian", "gbreedcvi", "WQCIhis",
		#"oceanselector")

	#mess = paste0("source(\"prep", fnsurv, ".r\"); dat = prep", fnsurv, "(file=\"", file, "\", survey=", survey, ")")
	mess = paste0("dat = prep", fnsurv, "(file=\"", file, "\", survey=\"", survey, "\")")
	eval(parse(text=mess))

	#qui compress ## qui = quietly
	if (savefile) {
		.flush.cat("Saving survey data file\n")
		mess = paste0(fnsurv, "=dat; save(\"", fnsurv, "\", file=\"", fnsurv, ".rda\")")
		eval(parse(text=mess))
	}
	else .flush.cat("Saving survey data file suppressed\n")
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~prepGFsurv
