## getVessel----------------------------2019-08-02
##  Get survey vessel codes.
## -----------------------------------------PJS|RH
getVessel <- function(dat)
{
	vessel = 1:20
	names(vessel) = c(
		"G. B. REED", 
		"SOUTHWARD HO",
		"EASTWARD HO",
		"OCEAN SELECTOR",
		"FROSTI",
		"VIKING STORM",
		"W. E. RICKER",
		"CHALLENGER",
		"DELIVERANCE",
		"OCEAN KING",
		"PACIFIC TRIDENT",
		"SHARLENE K",
		"SUNNFJORD",
		"CALEDONIAN",
		"BLUE WATERS",
		"SCOTIA BAY",
		"FREE ENTERPRISE NO 1",
		"NEMESIS",
		"NORDIC PEARL",
		"ARCTIC OCEAN"
	)
	label = attributes(dat)$label
	if (is.null(label)) label = list()

	if (!is.element("vessel",names(label))) {
		label[["vessel"]] = list()
		label[["vessel"]][["describe"]] = "PJS vessel codes"
		label[["vessel"]][["codes"]] = vessel
	}
	if ("vessel" %in% colnames(dat)) {
		vv = vessel[dat$vessel]
		if ( any(is.na(vv)) ) {
			vvdiff = setdiff(.su(dat$vessel),names(vessel))
			if (length(vvdiff)>0) {
				addvessel = max(vessel) + 1:length(vvdiff)
				names(addvessel) = vvdiff
				vessel = c(vessel,addvessel)
				vv = vessel[dat$vessel]
				label[["vessel"]][["codes"]] = vessel
				label[["vessel"]][["note"]] = paste0("Added vessels not in PJS list of 20 vessel codes: ", paste0(paste(addvessel,names(addvessel),sep=" = "),collapse="; "))
			}
			vv[is.na(vv)] = 0
		}
		dat$vessel = vv
	} else
		label[["vessel"]][["note"]] = paste0("Variable 'vessel' not present in this object; therefore, vessels, if any, were not converted to codes.")
	attr(dat, "label") = label
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~getVessel
