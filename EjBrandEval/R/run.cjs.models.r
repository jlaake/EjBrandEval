

#' Runs sets of CJS models for Ej data
#' For a given set of p and Phi models, create a model list of CJS models and
#' send to mark.wrapper to run each model and return results with model
#' selection table
#' 
#' 
#' @export
#' @param p_models list of p model specifications
#' @param Phi_models list of Phi model specifications
#' @param data list containing data.proc and ddl as created by
#'   \code{\link{process.Ej.cjs}}
#' @param ... Additional arguments to be passsed to \code{mark.wrapper} like
#'   run=FALSE, output=FALSE, invisible=FALSE
#' @return marklist of fitted models
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
run.cjs.models <-function(p_models,Phi_models,data,...)
{
	for(i in 1:length(p_models))assign(paste("p",i,sep="."),p_models[[i]])
	for(i in 1:length(Phi_models))assign(paste("Phi",i,sep="."),Phi_models[[i]])
	  cml=create.model.list("CJS")
	return(mark.wrapper(cml,data=data$data.proc,ddl=data$ddl,...))
}

