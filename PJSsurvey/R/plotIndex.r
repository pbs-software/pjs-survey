## plotIndex----------------------------2024-10-24
##  Plot survey index series after bootstrapping.
##  If type=="PJS", plot the series as mean with
##   bias-corrected percentile limits.
## If type=="RH", plot quantile boxes using 
##   0.05, 0.25, 0.5, 0.75, and 0.95 quantiles.
## Now using results from 'boot::boot' & 'boot::boot.ci' (RH 190821)
## -----------------------------------------PJS|RH
plotIndex <- function(bootbomb, analytic, type="PJS", surv=ttcall(surveyname),
   png=FALSE, pngres=400, PIN=c(8,7), lang=c("e","f"))
{
	createFdir(lang)
	unpackList(bootbomb)
	reps     = booty[[1]]$R
	years    = as.numeric(names(booty)); 
	nyrs     = length(years)
	yrs.surv = as.numeric(names(extra$ntows))
	nyrs.surv= length(yrs.surv)
	Bboot    = array(NA, dim=c(reps, nyrs), dimnames=list(rep=1:reps, year=years))
	index    = rep(NA,nyrs); names(index)=years
	yCL      = array(NA, dim=c(2, nyrs), dimnames=list(c("lower","upper"), year=years))
	for (i in names(booty)) {
		Bboot[,i] = booty[[i]]$t[,1]
		index[i]  = booty[[i]]$t0
		yCL[,i]   = bootci[[i]]$bca[4:5]
	}
	Qboot = as.list(rep(NA,length(min(years):max(years)))); names(Qboot)=min(years):max(years)
	qboot = lapply(as.data.frame(Bboot),function(x){x})
	Qboot[names(qboot)] = qboot
	
	## Make a table for output
	bbtab = data.frame(year=years, B_anal=index, B_boot=sapply(qboot,mean), B_lower=yCL["lower",], B_upper=yCL["upper",], CV_boot=sapply(qboot,function(x){sd(x)/mean(x)}))
	if (!missing(analytic))
		bbtab = data.frame(bbtab, CV_anal=analytic$analytic[,"CV"])
	write.csv(bbtab, file=paste0("bio.est.tab.", ttcall(survey),".csv"), row.names=FALSE)

	ylim  = c(0,max(yCL))
	ylim[1] = ylim[1]-(diff(ylim)*ifelse(png,0.02,0.01))
#browser();return()
	ayrs  = yrs.surv[1]:rev(yrs.surv)[1]; nayrs = length(ayrs)
	fout.e = paste0(gsub(" ","_",surv),"-",type)
	for (l in lang) {  ## could switch to other languages if available in 'linguaFranca'.
		changeLangOpts(L=l)
		#fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
		fout = switch(l, 'e' = paste0("./english/",fout.e), 'f' = paste0("./french/",fout.e) )
		if (png) {
			clearFiles(paste0(fout,".png"))
			png(filename=paste0(fout,".png"), units="in", res=pngres, width=PIN[1], height=PIN[2])
		}
		expandGraph(mar=c(3,3,0.5,0.5), oma=c(0,0,0,0))

		flab = paste0(c(surv, paste0(
			switch(l, 'e'="Mean +ve events: ", 'f'=eval(parse(text=deparse("moyen \u{00E9}v\u{00E9}nements positifs : "))) ),
			round(mean(extra$ptows)), "/", round(mean(extra$ntows))) ), collapse="\n")

		if (type=="PJS") {
			## estimate bias in std. norm deviates
			## https://influentialpoints.com/Training/bootstrap_confidence_intervals.htm#bias
			#yCL = sapply(1:ncol(Bboot), function(i,B,alpha=0.05){
			#	boot  = B[,i]; obs = index[i]; N = nrow(B)
			#	b     = qnorm((sum(boot > obs) + sum(boot==obs)/2) / N)
			#	alpha = 0.05 # 95% limits
			#	z     = qnorm(c(alpha/2, 1-alpha/2)) # Std. norm. limits
			#	p     = pnorm(z-2*b) ## bias-correct & convert to proportions
			#	iCL   = quantile(boot,p=p) # Bias-corrected percentile limits
			#	return(iCL)
			#}, B=Bboot)
			#colnames(yCL) = colnames(Bboot)
			#yCL  = apply(Bboot[-1,],2,quantile,c(0.025,0.975))  ## 95% confidence interval
			xseg = as.vector(sapply(split(years,years),function(x){c(x,x,NA)}))
			yseg = as.vector(rbind(yCL,rep(NA,length(years))))
			staple.width = 0.2
			xstp = as.vector(sapply(split(years,years),function(x,s){c(x-s,x+s,NA,x-s,x+s,NA)}, s=staple.width/2))
			ystp = as.vector(rbind(yCL[c(1,1),], rep(NA,length(years)), yCL[c(2,2),], rep(NA,length(years)) ))
			plot(years, index, type="n", xlim=extendrange(ayrs), ylim=ylim, xlab=linguaFranca("Year",l), ylab=linguaFranca("Relative biomass (t)",l), cex.axis=1.2, cex.lab=1.5)
			axis(1, at=seq(1900, 2100, ifelse(nayrs>30, 5, 1)), tcl=-0.2, labels=FALSE)
			axis(2, at=pretty(ylim, n=15), tcl=-0.2, labels=FALSE)
			lines(xseg,yseg,lwd=3,col="black")
			lines(xstp,ystp,lwd=3,col="black")
			points(years, index, pch=15, col="red", cex=1.2)
			if (length(setdiff(yrs.surv,years))>0){
				yrs.zero = setdiff(yrs.surv,years)
				#points(yrs.zero, rep(0,length(yrs.zero)), pch="\327", col="red", cex=1.5)
				points(yrs.zero, rep(0,length(yrs.zero)), pch=convUTF("\\u{00D7}"), col="red", cex=1.5)
			}
			text(yrs.surv, rep(par()$usr[3],nyrs.surv), paste0(extra$ptows,"/",extra$ntows), cex=0.8, col="blue", pos=3)
#browser();return()
			#text(years, rep(par()$usr[3],nyrs), extra$ptows, cex=0.8, col="blue", pos=3)
			#text(years, rep(par()$usr[3],nyrs), extra$ptows, cex=0.8, col="blue", adj=c(0.5,-0.2))
			if (sum(index[1:2]) > sum(rev(index)[1:2]))
				addLabel(0.95,0.95, txt=linguaFranca(flab,l), adj=c(1,1), cex=1.2, col="grey30")
			else
				addLabel(0.05,0.95, txt=linguaFranca(flab,l), adj=c(0,1), cex=1.2, col="grey30")
		} else {
			quantBox(Qboot, ylim=ylim, xlab=linguaFranca("Year",l), ylab=linguaFranca("Relative biomass (t)",l), boxwex=0.6, boxfill="aliceblue", whisklty=1, outpch=43, outcex=0.8, outcol="grey60", cex.lab=1.5)
			xpos  = 1:length(Qboot); names(xpos)=names(Qboot)
			Bmean = sapply(Qboot, mean, na.rm=TRUE)
			points(xpos, Bmean, pch=22, col=lucent("blue",1), bg=lucent("cyan",1), cex=1.5)
			points( xpos[as.character(years)], Bboot[1,], pch=15, col="blue", cex=0.8)
		}
		if (png) dev.off()
	}; eop()
	invisible(return(Qboot))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plotIndex
