# Taking cue from Roger Bivand's maptools:
.PJSsurvEnv <- new.env(FALSE, parent=globalenv())  # be sure to exportPattern("^\\.PJS") in NAMESPACE

.onAttach <- function(lib, pkg)
{
	pkg_info = utils::sessionInfo( package="PJSsurvey" )$otherPkgs$PJSsurvey
	if( is.character( pkg_info$Packaged ) )
		pkg_date <- strsplit( pkg_info$Packaged, " " )[[1]][1]
	else
		pkg_date  <- date()

	userguide_path <- system.file( "doc/PJSsurvey-UG.pdf", package = "PJSsurvey" )
	year <- substring(date(),nchar(date())-3,nchar(date()))

	packageStartupMessage("
-----------------------------------------------------------
PJS Survey", pkg_info$Version, " -- Copyright (C) 2007-",year," Paul J. Starr

Packaged on ", pkg_date, "
Pacific Biological Station, Nanaimo

All available PBS packages can be found at
https://github.com/pbs-software

Not a load of rubbish.
-----------------------------------------------------------

")
}
.onUnload <- function(libpath) {
	rm(.PJSsurvEnv)
}

# No Visible Bindings
# ===================
if(getRversion() >= "2.15.1") utils::globalVariables(names=c(
#	"dot",
	"addLabel","as.EventData","assflds",
	"bootci","booty",
#	"C",
	"dat","defaultdoorspread",
	"eez.bc","expandGraph","extra",
	"findPolys",
	"GC","gfbsurvey","gigflds",
#	"H",
#	"I",
#	"J", 
#	"K",
	"lucent",
#	"M",
#	"N",
#	"O",
	"PBSdat","pad0",
#	"Q",
#	"R",
	"speciesname","spn","stratum","survey","surveyname","synflds",
	"triflds",
	"unpackList"
#	"V",
#	"W",
#	"X",
#	"Y",
#	"Z"
	), package="PJSsurvey")

