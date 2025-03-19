## getSpecies---------------------------2019-07-17
##  Get species codes from data
##  (also checks for more than one species)
## -----------------------------------------PJS|RH
getSpecies <- function(species)
{
	#ici = lenv() ## RH 200716
	#tempfile = paste0(convSlashes(tempdir(),"unix"),"/tmpsurvey.txt")
	N = countVec(species)  ## count if species~=. 
	if (N>0) {
#		save `tmpsurvey' ## wtf?
		if (length(.su(species)) > 1)
			stop ("More than one species code in the file\n\texiting program...")
		#data("spn", package="PBSdata", envir=ici) ## RH 200716
		z = !is.na(species)
		species[z] = pad0(species[z], 3)
		#species[z] = spn[as.character(species[z])]
#		collapse (count) set if species~=., by(species)	
#		tab species
#		if r(r)>1 {
#			di in red "more than one species code in the file" _n "exit program" 
#			exit 998
#		}
#		else {
#			ren species sp
#			merge 1:1 sp using E:\ADO\Groundfish\species_name.dta
#			keep if _m==3
#			global speciesname=common_name[1]
#			global species=sp[1]
#		}
#		use `tmpsurvey', clear
#	} /* if r(N)>0 */
	}
	return(species)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~getSpecies
