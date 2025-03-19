## calcHBLL-----------------------------2022-07-21
##  Calculate swept-area index values for HBLL surveys.
##  Parallels PJS routine 'calcBiom'
## -----------------------------------------PJS|RH
calcHBLL <- function(dat, ssid, reps=0, seed=42, meth=1, xvar="year")
{
	##  GC  DESC                     AREA_KM2  SSID
	## 321  HBLL OUT North, 20 - 70 m    3329  22
	## 322  HBLL OUT North, 71 - 150 m   5485  22
	## 323  HBLL OUT North, 151 - 260 m  3705  22
	## 324  HBLL OUT South, 20 - 70 m    3969  36
	## 325  HBLL OUT South, 71 - 150 m   5499  36
	## 326  HBLL OUT South, 151 - 260 m  1957  36
	areas = c(3329,5485,3705,3969,5499,1957)
	names(areas) = 321:326

	## from IPHC routines
	## When sim!="parametric", 'statistic' must take at least two arguments.
	## The first argument passed will always be the original data.
	## The second will be a vector of indices, frequencies or weights which define the bootstrap sample.
	## I becomes a random index but what is controlling it?
	sweptArea <- function(x, I){
		xx = x[I,]
		xstra = split(x[I,], x[I,"GC"])
		out   = lapply(xstra, function(s){
			nobs = nrow(s)
			dens = mean(s$exp_dens, na.rm=TRUE)
			sdev = if (nobs<=1 || all(is.na(s$exp_dens))) 0 else sd(s$exp_dens, na.rm=TRUE)
			area = mean(s[,"str_area"], na.rm=TRUE)
			vari = (sdev^2 * area^2) / nobs
			vari[is.na(vari) | !is.finite(vari)] = 0
			biom = dens * area
			## summary stats
			bio = sum(biom)/1000.
			var = sum(vari)
			nn  = sum(nobs)
			return(data.frame(B=bio, V=var, N=nn))
			return(bio)
		})
		bvn = do.call("rbind", lapply(out, data.frame, stringsAsFactors=FALSE))
		BVN = apply(bvn,2,sum)
#browser();return()
		if (reps>0)
			return(BVN["B"])
		else {
			SE  = sqrt(BVN["V"])/1000.
			CV  = SE/BVN["B"]
			CV[is.na(CV) | !is.finite(CV)] = 0
			return(c(B=as.vector(BVN["B"]), V=as.vector(BVN["V"]), SE=as.vector(SE), CV=as.vector(CV), N=as.vector(BVN["N"])))
		}
	}
	dat = dat[is.element(dat$survey_series_id, ssid),]

	dat$GC = dat$grouping_code
	dat$str_area = areas[as.character(dat$GC)]
	dat$exp_biom = dat$exp_dens = rep(0,nrow(dat)) ## initialise zeroes
	zpos = dat$catch_count > 0 & !is.na(dat$catch_count)
	#dat$exp_dens[zpos] = (dat$density_ppkm2[zpos] / dat$catch_count[zpos]) * dat$expected_catch[zpos]
	dat$exp_dens[zpos] = dat$density_ppkm2[zpos] * (dat$expected_catch[zpos] / dat$catch_count[zpos])
	dat$exp_biom[zpos] = dat$exp_dens[zpos] * dat$str_area[zpos] / 1000.

	set.seed(seed)
	
	if (sum(dat$exp_biom)==0)
		stop(paste0("Survey series ", ssid, " has no catch for this species.\nProgram terminated"))

	strata = .su(dat$GC)
	nstrat = length(strata)
	nnst   = nstrat + 1
	xvars  = .su(dat[,xvar])
	nxvar  = length(xvars)

	if (reps==0)
		.flush.cat("Simple biomass calculation/no bootstrap","\n")
	else
		.flush.cat(paste0("Bootstrap biomass simulation: N reps=",reps), "\n")

	## initialize n-column matrix to hold number of tows in each year & 'GC'
	smat = array(0, dim=c(nxvar,nnst), dimnames=list(xvars,c(strata,"T")))
	stab = crossTab(dat, c(xvar,"GC"), xvar, countVec)
	smat[dimnames(stab)[[1]], dimnames(stab)[[2]]] = stab
	tvec = apply(stab,1,sum)
	smat[names(tvec),"T"] = tvec

	dat.need = dat[,c(xvar, "GC", "str_area", "exp_dens")]
	ntows = table(dat.need[,xvar])
	ptows = crossTab(dat.need, xvar, "exp_dens", countVec)
#browser();return()

	## Need to bootstrap annual indices separately when using boot::boot because strata assumes one population (as far as I can tell)
	booty = bootci = list()
	.flush.cat(paste0("Bootstrapping (", reps, " replicates):\n"))
	#xvars=c(2005,2007) ##testing
	for (i in xvars) {
		ii = as.character(i)
		.flush.cat(paste0(ii, ifelse(i==rev(xvars)[1], "\n\n", ", ")))
		idat = dat.need[is.element(dat.need[,xvar], i),]
		if (all(idat$exp_dens==0)) next
		if (meth==0)      idat$index = idat[,xvar]
		else if (meth==1) idat$index = paste(idat[,xvar], idat[,"GC"], sep="-")
		else stop ("No stratify method available for meth>1")
		idat$index = as.factor(idat$index)
		
		if (reps==0) {
			booty[[ii]] = t(as.matrix(sweptArea(idat)))
		} else {
			## https://stats.stackexchange.com/questions/242404/how-does-the-boot-package-in-r-handle-collecting-bootstrap-samples-if-strata-are
			## with strata specified
			booty[[ii]]  <- boot::boot(idat, sweptArea, R=reps, strata=idat$index)
#browser();return()
			bootci[[ii]] <- boot::boot.ci(booty[[ii]], type = "bca")
		}
	}
	if (reps==0) {
		analytic = do.call("rbind", lapply(booty, data.frame, stringsAsFactors=FALSE))
		return(list(analytic=analytic, extra=list(ntows=ntows, ptows=ptows)))
	} else {
		return(list(booty=booty, bootci=bootci, extra=list(ntows=ntows, ptows=ptows)))
	}
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~calcHBLL
