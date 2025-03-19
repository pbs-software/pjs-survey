## calcBiom-----------------------------2022-05-19
##  Calculate swept-area biomass estimate and 
##  bootstrap within strata depending on 'meth':
##  If meth==0, strata comprise 'xvar' (usually 'year') only.
##  If meth==1, strata comprise year and survey stratum.
## -----------------------------------------PJS|RH
calcBiom <- function(dat, reps=0, seed=42, meth=1, fix125flag, xvar="year", 
   areavar="area", stratumvar="stratum", towvar="set", postfilename)
{
	## syntax,  Density(string) Reps(int) SEed(int) Meth(int) FIX125flag(int) Xvar(string) /*
	## */ Areavar(string) STratumvar(string) Towvar(string) Postfilename(string) FILename(string)
	##  density units: kg/km^2;  area units: km^2 

	set.seed(seed)
	label = attributes(dat)$label
	## Use records with identifiable strata
	dat = dat[!is.na(dat[,stratumvar]),]
	
	## Check for missing areas (RH 220519)
	if (any(is.na(dat[,areavar]))){
		gcarea = GC$area; names(gcarea) = GC$group
		gcarea = gcarea[!is.na(gcarea)]
		noarea = is.na(dat[,areavar])
		dat[noarea,areavar] = gcarea[as.character(dat$group)][noarea]
		if (any(is.na(dat[,areavar]))) {  ## just remove them
			.flush.cat(paste0("Removing ", sum(is.na(dat[,areavar])), " record(s) with no group-area information.\n\n"))
			dat = dat[!noarea,]
			#stop (paste0("There are still missing areas (km^2) from '", substitute(dat), "'"))
		}
	}

	## Check for surveys with no catch data at all (it does happen!) 
	##   want to exit gracefully because we may be working on a batch situation
	if (sum(dat$density)==0)
		stop(paste0("Survey ", label$survey$number, " has no catch for this species.\nProgram terminated"))

	strata = .su(dat[,stratumvar])
	nstrat = length(strata)
	nnst   = nstrat + 1
	xvars  = .su(dat[,xvar])
	nxvar  = length(xvars)
	#egen byte `gg'=group(`xvar') ??

	if (reps==0)
		.flush.cat("Simple biomass calculation/no bootstrap","\n")
	else
		.flush.cat(paste0("Bootstrap biomass simulation: N reps=",reps), "\n")

	## initialize n-column matrix to hold number of tows in each year & `stratumvar'
	#smat = matrix(0, nrow=nxvar, ncol=nnst)
	smat = array(0, dim=c(nxvar,nnst), dimnames=list(xvars,c(strata,"T")))
	stab = crossTab(dat, c(xvar,stratumvar), xvar, countVec)
	smat[dimnames(stab)[[1]], dimnames(stab)[[2]]] = stab
	tvec = apply(stab,1,sum)
	smat[names(tvec),"T"] = tvec

	dat.need = dat[,c(xvar, stratumvar, areavar, "density")]
	ntows = table(dat.need[,xvar])
	ptows = crossTab(dat.need, xvar, "density", countVec)

	## from IPHC routines
	## When sim!="parametric", 'statistic' must take at least two arguments.
	## The first argument passed will always be the original data.
	## The second will be a vector of indices, frequencies or weights which define the bootstrap sample.
	## I becomes a random index but what is controlling it?
	sweptArea <- function(x, I){
		xx = x[I,]
		xstra = split(x[I,], x[I,stratumvar])
		out   = lapply(xstra, function(s){
			nobs = nrow(s)
			dens = mean(s$density, na.rm=TRUE)
			sdev = if (nobs<=1 || all(is.na(s$density))) 0 else sd(s$density, na.rm=TRUE)
			area = mean(s[,areavar], na.rm=TRUE)
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
		if (reps>0)
			return(BVN["B"])
		else {
			SE  = sqrt(BVN["V"])/1000.
			CV  = SE/BVN["B"]
			CV[is.na(CV) | !is.finite(CV)] = 0
			return(c(B=as.vector(BVN["B"]), V=as.vector(BVN["V"]), SE=as.vector(SE), CV=as.vector(CV), N=as.vector(BVN["N"])))
		}
	}
	## Need to bootstrap annual indices separately when using boot::boot because strata assumes one population (as far as I can tell)
	booty = bootci = list()
	.flush.cat(paste0("Bootstrapping (", reps, " replicates):\n"))
	#xvars=c(2005,2007) ##testing
	for (i in xvars) {
		ii = as.character(i)
		.flush.cat(paste0(ii, ifelse(i==rev(xvars)[1], "\n\n", ", ")))
		idat = dat.need[is.element(dat.need[,xvar], i),]
		if (all(idat$density==0)) next
		if (meth==0)      idat$index = idat[,xvar]
		else if (meth==1) idat$index = paste(idat[,xvar], idat[,stratumvar], sep="-")
		else stop ("No stratify method available for meth>1")
		idat$index = as.factor(idat$index)
		
		if (reps==0) {
			booty[[ii]] = t(as.matrix(sweptArea(idat)))
		} else {
			## https://stats.stackexchange.com/questions/242404/how-does-the-boot-package-in-r-handle-collecting-bootstrap-samples-if-strata-are
			## with strata specified
			booty[[ii]]  <- boot::boot(idat, sweptArea, R=reps, strata=idat$index)
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
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~calcBiom
