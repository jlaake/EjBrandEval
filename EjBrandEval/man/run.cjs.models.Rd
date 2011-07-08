\name{run.cjs.models}
\alias{run.cjs.models}
\title{Runs sets of CJS models for Ej data...}
\usage{run.cjs.models(p_models, Phi_models, data, ...)}

\description{
  Runs sets of CJS models for Ej data For a given set of p
  and Phi models, create a model list of CJS models and
  send to mark.wrapper to run each model and return results
  with model selection table
}
\value{marklist of fitted models}
\author{Jeff Laake}
\arguments{
  \item{p_models}{list of p model specifications}
  \item{Phi_models}{list of Phi model specifications}
  \item{data}{list containing data.proc and ddl as created by
\code{\link{process.Ej.cjs}}}
  \item{...}{Additional arguments to be passsed to \code{mark.wrapper} like
run=FALSE, output=FALSE, invisible=FALSE}
}
\examples{ej.list=extract.Ej()
ej.list=process.Ej.cjs(ej.list)
p=vector("list",2)
p[[1]]=list(formula=~late:Tag:time + time + patch1:Tag + patch2:Tag + patch3:Tag + scope:Tag + camera:Tag)
p[[2]]=list(formula=~late:Tag + time + patch1:Tag + patch2:Tag + patch3:Tag + scope:Tag + camera:Tag)
Phi=vector("list",1)
Phi[[1]]=list(formula=~early:trt+Brand:batch)
results=run.cjs.models(p,Phi,ej.list)}
