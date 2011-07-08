

#' Prepares data for running RMark models
#' Prepares data by running process.data and make.design.data step for RMark.
#' Creates occasion-specific design data for groups and for occasions based on
#' platform used for re-sighting.
#' 
#' @export
#' @param ej.list list that results from running \code{\link{extract.Ej}}
#' @return \item{data.proc}{Processed data list for RMark} \item{ddl}{Design
#'   data list for RMark}
#' @author Jeff Laake
#' @examples
#' 
#' ej.list=extract.Ej()
#' ej.list=process.Ej.cjs(ej.list)
#' p=vector("list",2)
#' p[[1]]=list(formula=~late:Tag:time + time + patch1:Tag + patch2:Tag + patch3:Tag + scope:Tag + camera:Tag)
#' p[[2]]=list(formula=~late:Tag + time + patch1:Tag + patch2:Tag + patch3:Tag + scope:Tag + camera:Tag)
#' Phi=vector("list",1)
#' Phi[[1]]=list(formula=~early:trt+Brand:batch)
#' results=run.cjs.models(p,Phi,ej.list)
#' 
process.Ej.cjs <-function(ej.list)
{
#
# Create time interval vector with a unit interval of 7 days
#
    ti=as.numeric(diff(ej.list$times)/7)
#
#  Process data
#
	ej.proc=process.data(ej.list$data,model="CJS",groups=c("trt","tag","experiment","brand","batch","brander","anesthesiologist","sex","treatno"),time.intervals=ti,begin.time=0)
#
#  Create design data 
#
	ej.ddl=make.design.data(ej.proc,parameters=list(Phi=list(pim.type="time"),p=list(pim.type="time")))
#
# Add early late split on Phi and p
#
	ej.ddl$Phi$early=0
	ej.ddl$Phi$early[ej.ddl$Phi$Time<10]=1
	ej.ddl$p$late=1
	ej.ddl$p$late[ej.ddl$p$Time<11]=0

	ej.ddl$p$Brand=as.numeric(as.character(ej.ddl$p$brand))
	ej.ddl$p$Tag=1-ej.ddl$p$Brand
	ej.ddl$Phi$Brand=as.numeric(as.character(ej.ddl$Phi$brand))
	ej.ddl$Phi$Tag=1-ej.ddl$Phi$Brand
	
#
# Create occasion data frame to merge with the p design data
#
	times=cumsum(ti)

#	Need to change these for added occasions
	
	patch1=c(rep(1,5),rep(0,58))
	patch2=c(rep(0,5),rep(1,5),rep(0,53))
	patch3=c(rep(0,10),rep(1,5),rep(0,48))
	yp=c(rep(1,15),rep(0,48))

	camera=ej.list$platform[,1]
	vessel=ej.list$platform[,2]
	scope=ej.list$platform[,3]
	
	xcov=data.frame(patch1=patch1,patch2=patch2,patch3=patch3,yp=yp,camera=camera,vessel=vessel,scope=scope)
	if(ej.list$scenario==2)xcov=xcov[1:50,]
	if(ej.list$scenario==4)xcov=xcov[-c(54,55,57,59),]
	ej.ddl=merge_design.covariates(ej.ddl,"p",cbind(times=times,xcov))
	ej.ddl$p$extra=0
	ej.ddl$p$extra[ej.ddl$p$experiment==0]=1
	ej.ddl$Phi$threshold1=0
	ej.ddl$Phi$threshold1[ej.ddl$Phi$Time<4]=1
	ej.ddl$Phi$threshold2=0
	ej.ddl$Phi$threshold2[ej.ddl$Phi$Time>=4&ej.ddl$Phi$Time<8]=1
	ej.ddl$Phi$threshold3=0
	ej.ddl$Phi$threshold3[ej.ddl$Phi$Time>=4&ej.ddl$Phi$Time<6]=1
	ej.ddl$Phi$threshold4=0
	ej.ddl$Phi$threshold4[ej.ddl$Phi$Time>=6&ej.ddl$Phi$Time<8]=1
	ej.ddl$Phi$threshold5=0
	ej.ddl$Phi$threshold5[ej.ddl$Phi$Time>=8&ej.ddl$Phi$Time<10]=1
	
	return(list(data.proc=ej.proc,ddl=ej.ddl))
}

