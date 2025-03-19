## getUsability-------------------------2019-08-02
##  Qualify data by usability code.
## -----------------------------------------PJS|RH
getUsability <- function(dat, use=c(0,1,2,6))
{
	Nrows = nrow(dat)
	if ("usability" %in% colnames(dat)) {
		useflag = TRUE
		nuse0 = is.na(dat$usability)
		dat$usability[nuse0] = 0
		dat  = dat[is.element(dat$usability, use),]
		nrows = nrow(dat)
	} else
		useflag = FALSE

	label = attributes(dat)$label
#browser();return()
	if (is.null(label)) label = list()
	if (!is.element("usability",names(label))) {
		label[["usability"]] = list()
		label[["usability"]][["describe"]] = "Records' usability for different purposes"
		getData("USABILITY", "GFBioSQL")
		label[["usability"]][["codes"]] = PBSdat
	}
	if (useflag) {
		label[["usability"]][["note"]] = paste0("Of the original ", Nrows, " records, ", sum(nuse0), " were converted to 0 from NA, and ", Nrows-nrows, " were removed after filtering.")
		label[["usability"]][["Nrows"]] = Nrows
		label[["usability"]][["nrows"]] = nrows
		label[["usability"]][["use"]] = use
	} else
		label[["usability"]][["note"]] = paste0("Variable 'usability' not present in this object; therefore,  all ", Nrows, " survey fishing events will be used for analysis without regard to their utility.")
	attr(dat, "label") = label
#browser();return()
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~getUsability
