## keepAtts ----------------------------2019-07-23
##  Keep user's attributes after performing an
##  expression such as subsetting a file.
## ---------------------------------------------RH
keepAtts  <- function(dat, expr, batts=c("names","row.names","class"), extras=list())
{
	unpackList(extras)
	keepatts = attributes(dat)
	keepatts = keepatts[setdiff(names(keepatts), batts)]
	loco = lenv()
#browser();return()
	dat = eval(expr, envir=loco)
	attributes(dat) = c(attributes(dat), keepatts)
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~keepAtts
