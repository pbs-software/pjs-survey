## getLabelends-------------------------2019-07-23
##  Get depth ranges from stratum names (methinks)
## -----------------------------------------PJS|RH
getLabelends <- function (labelname)
{
	pos1 = pmax(regexpr(":",labelname),0)
	pos2 = regexpr("-",labelname)
	pos3 = regexpr("( ?f?)m$",labelname)
	lows = substring(labelname, pos1+1, pos2-1)
	upps = substring(labelname, pos2+1, pos3-1)
	nstrat = length(labelname)
#browser();return()
	return(list(low=lows, upp=upps, nstrat=nstrat))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~getLabelends
