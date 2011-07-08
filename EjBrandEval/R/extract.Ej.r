

#' Extract brand evaluation data from Access database
#' Extracts brand/tag and resight data from 2005 Rogue Reef cohort, creates
#' capture history using scenario specific criterion and formats for use in
#' RMark
#' 
#' Scenario=1 All data all times
#' 
#' Scenario=2 Time <= 9/29/2005
#' 
#' Scenario=3 All times, SiteName!="Cape Arago"
#' 
#' Scenario=4 Time <1/1/2006 | Time >=1/1/2006 & Region=="WA" | (Observer=="BG"
#' and Region=="BC")
#' 
#' @import RODBC
#' @export
#' @param file filename of Ej brand-resight database
#' @param dir path specification for directory containing database
#' @param scenario value of 1 to 4 for different data extract scenarios; see
#'   details
#' @return \item{ej}{dataframe containing capture histories and covariate data
#'   for analysis} \item{platform}{dataframe with platform covariates for each
#'   sampling occasion} \item{times}{vector of dates of sampling occasions}
#'   \item{scenario}{selected scenario value}
#' @author Jeff Laake
extract.Ej=function(file="Ej-Brand-Resight-Database.accdb",dir=NULL,scenario=1)
{
	if(is.null(dir))dir=file.path(installed.packages()[,"LibPath"]["EjBRandEval"],"EjBRandEval") 
	fdir=file.path(dir,file)
	connection=odbcConnectAccess2007(fdir)
#   Fetch brands and tagged (non-brands) and the resight data tables
	tblBrands=sqlFetch(connection,"tblBrands")
	tblResights=sqlFetch(connection,"tblResights")
	tblNonBrands=sqlFetch(connection,"tblNonBrands")
	tblResightsNonBrands=sqlFetch(connection,"tblResightsNonBrand")
#   Only use 2005 brand cohort when tags were also applied; drop unused factor levels	
    tblBrands=tblBrands[tblBrands$Year==2005,]
	tblBrands=droplevels(tblBrands)
#   Add a date field to resights
	tblResights$date=as.Date(paste(tblResights$Month,tblResights$Day,tblResights$Year,sep="/"),"%m/%d/%Y")
	tblResightsNonBrands$date=as.Date(paste(tblResightsNonBrands$Month,tblResightsNonBrands$Day,tblResightsNonBrands$Year,sep="/"),"%m/%d/%Y")
#   Limit resights based on scenario for scenarios 3 & 4
	brand_resights=tblResights
	tag_resights=tblResightsNonBrands
	if(scenario==3)
	{
		brand_resights=tblResights[tblResights$SiteName!="Cape Arago",]
		tag_resights=tblResightsNonBrands[tblResightsNonBrands$SiteName!="Cape Arago",]
	} else
      if(scenario==4)
	  {
		cut_date=as.Date("1/1/2006","%m/%d/%Y")
		brand_resights=tblResights[tblResights$date<=cut_date | 
						(tblResights$date>cut_date & tblResights$Region=="WA" | (tblResights$Region=="BC"& tblResights$Obs=="BG")),]
		tag_resights=tblResightsNonBrands[tblResightsNonBrands$date<=cut_date | (tblResightsNonBrands$date>cut_date & tblResightsNonBrands$Region=="WA" | (tblResightsNonBrands$Region=="BC"& tblResightsNonBrands$Obs=="BG")),]
	  }	
#   Merge brands and their resights
	brands=merge(tblBrands,brand_resights, by="Brand", all.x=TRUE)
	platform=table(brands$date,brands$Platform)[,-1]
	platform=ifelse(platform>0,1,0)
	colnames(platform)=c("camera","vessel","scope")
#   Merge tagged and their resights
	tags=merge(tblNonBrands,tag_resights,by.x="LTag",by.y="Brand",all.x=TRUE)
#   pool all dates and then create occasion partitions
	all.dates=unique(c(names(table(brands$date)),names(table(tags$date))))
	occasion.dates=c(as.Date(all.dates[1:52]),
			as.Date(c("10-8-2005","11-11-2005","12-11-2005","1-12-2006","2-12-2006","3-7-2006","3-31-2006","5-2-2006","5-29-2006",
			"6-15-2006","9-15-2006","6-15-2007","9-15-2007","6-15-2008","9-15-2008","6-15-2009","9-15-2009","6-15-2010","9-15-2010","6-15-2011"),"%m-%d-%Y"))
    brands$occasion=cut(brands$date,occasion.dates)
	tags$occasion=cut(tags$date,occasion.dates)
#   Next create capture histories for each id and exclude occasions 63,65,67,69,70,71
#   which are 15-Sept to 14-June intervals
	chmat=table(brands$Brand,brands$occasion)
	chmat=chmat[,-c(63,65,67,69,70,71)]
	platform=platform[-c(63,65,67,69,70,71),]
#   Also remove 50,51 which were low effort dates with a sighting on each from shore
	chmat=chmat[,-c(50,51)]	
	platform=platform[-c(50,51),]
	platform=rbind(platform[1:50,],matrix(0,ncol=3,nrow=13))
	rownames(platform)[51:63]=colnames(chmat)[51:63]
#   Limit occasions for scenario 2 to only include first 50 (<=9/29/2005)
#   IF scenario 4, remove 0 sighting occasions
	if(scenario==2) 
		chmat=chmat[,1:50]
	if(scenario==4)
		chmat=chmat[,-c(54,55,57,59)]
	chmat[chmat>1]=1
	bID=row.names(chmat)
	brands_ch=paste("1",apply(chmat,1,paste,collapse=""),sep="")
#   Now do same for tags
	chmat=table(tags$Brand,tags$occasion)
	chmat=chmat[,-c(63,65,67,69,70,71)]
	chmat=chmat[,-c(50,51)]	
	if(scenario==2) 
		chmat=chmat[,1:50]
	if(scenario==4)
		chmat=chmat[,-c(54,55,57,59)]
	chmat[chmat>1]=1
	tID=row.names(chmat)
	tags_ch=paste("1",apply(chmat,1,paste,collapse=""),sep="")
#   Create a data base for brands and tags by merging with initial records	
	brands=tblBrands[,names(tblBrands)[c(1,2,12,14,15,16,25:28,32,34)]]
	tags=tblNonBrands[,names(tblNonBrands)[c(1,2,12,14,15,16,25:28,32,34)]]
	brands_ch=data.frame(ID=bID,ch=brands_ch,trt="brand")
	tags_ch=data.frame(ID=tID,ch=tags_ch,trt="tag")
	brands_ch=merge(brands_ch,brands,by.x="ID",by.y="Brand")
	tags_ch=merge(tags_ch,tags,by.x="ID",by.y="Brand")
	brand_eval=rbind(tags_ch,brands_ch)
    brand_eval$ch=as.character(brand_eval$ch)
	brand_eval$Brander=factor(brand_eval$Brander)
	brand_eval$TimeMaskOn=as.POSIXct(strptime(paste("7/18/2005",as.character(brand_eval$TimeMaskOn)),"%m/%d/%Y %H:%M"))
	brand_eval$TimeMaskOn[is.na(brand_eval$TimeMaskOn)]=as.POSIXct(strptime(paste("7/18/2005 17:",formatC(2*(1:20),flag=0,width=2)),"%m/%d/%Y %H:%M"))
	brand_eval$TimeInduction=as.POSIXct(strptime(paste("7/18/2005",as.character(brand_eval$TimeInduction)),"%m/%d/%Y %H:%M"))
	brand_eval$TimeMaskOff=as.POSIXct(strptime(paste("7/18/2005",as.character(brand_eval$TimeMaskOff)),"%m/%d/%Y %H:%M"))
	brand_eval$TimeRelease=as.POSIXct(strptime(paste("7/18/2005",as.character(brand_eval$TimeRelease)),"%m/%d/%Y %H:%M"))   
	in_order=substr(brand_eval$ID,2,4)
	in_order[!is.na(brand_eval$Brander)]=substr(brand_eval$ID[!is.na(brand_eval$Brander)],1,3)
	in_order=as.numeric(in_order)
	brand_eval=brand_eval[order(in_order),]
	names(brand_eval)[names(brand_eval)=="Anethesiologist"]="anesthesiologist"
	brand_eval$batch=as.factor(c(rep(1,20),rep(2,20),rep(3,20),rep(4,20), rep(1,20),rep(2,20),rep(3,20),rep(4,20),rep(5,20)))
	Brand=rep(0,180)
	Brand[brand_eval$trt!="tag"]=1
	Tag=1-Brand
	brand_eval$brand=factor(Brand)
	brand_eval$tag=factor(Tag)
	brand_eval$experiment=0
	brand_eval$experiment[!is.na(brand_eval$TreatNo)]=1
	brand_eval$experiment=factor(brand_eval$experiment)
	brand_eval$TreatNo[is.na(brand_eval$TreatNo)]=5
	brand_eval$brander=as.character(brand_eval$Brander)
	brand_eval$brander[is.na(brand_eval$brander)]="None"
	brand_eval$brander=factor(brand_eval$brander)
	brand_eval$Brander=NULL
	names(brand_eval)[names(brand_eval)=="Sex"]="sex"
	brand_eval$treatno=factor(brand_eval$TreatNo)
	brand_eval$TreatNo=NULL
	brand_eval$brander=relevel(brand_eval$brander,"None")
	brand_eval$ci=brand_eval$Mass/brand_eval$Length
    times=as.Date(colnames(chmat))
	times=c(as.Date("7/18/2005","%m/%d/%Y"),times)
	if(scenario!=2)
	  if(scenario!=4)
         times[52:64]=times[52:64]+c(diff(times)[52:60]/2,rep(45,4))
      else
		 times[48:60]=times[48:60]+c(diff(times)[48:56]/2,rep(45,4))
	odbcClose(connection)
	return(list(ej=brand_eval,platform=platform,times=times,scenario=scenario))
}


