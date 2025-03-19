## getMonth-----------------------------2019-07-30
##  Extract month from dates vector, where
##  type 1 = numeric month
##       2 = month abbreviation
##       3 = month name
##       4 = two-character string
## -----------------------------------------PJS|RH
getMonth <- function(dates, type=1)
{
	z = dates!="" & !is.na(dates)
	mos = rep(ifelse(type%in%c(1), NA, ""),length(dates))
	if (all(class(dates[z])=="character")) {
		strmos = substring(dates[z], 6, 7)
		nummos = as.numeric(strmos)
		mos[z] = switch(type,
			nummos,
			month.abb[nummos],
			month.name[nummos],
			strmos
		)
	} else
		stop("The 'date' field is not class 'character', revise function 'getMonth' to deal with date classes.")
#browser();return()
	#mos[z] = month.abb[as.numeric(strftime(as.Date(dates[z], "%d/%m/%Y"),"%m"))]
	return(mos)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~getMonth
