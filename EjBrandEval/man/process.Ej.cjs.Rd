\name{process.Ej.cjs}
\alias{process.Ej.cjs}
\title{Prepares data for running RMark models...}
\usage{process.Ej.cjs(ej.list)}

\description{
  Prepares data for running RMark models Prepares data by
  running process.data and make.design.data step for RMark.
  Creates occasion-specific design data for groups and for
  occasions based on platform used for re-sighting.
}
\value{\item{data.proc}{Processed data list for RMark} \item{ddl}{Design
data list for RMark}}
\author{Jeff Laake}
\arguments{
  \item{ej.list}{list that results from running \code{\link{extract.Ej}}}
}
\examples{ej.list=extract.Ej()
ej.list=process.Ej.cjs(ej.list)
p=vector("list",2)
p[[1]]=list(formula=~late:Tag:time + time + patch1:Tag + patch2:Tag + patch3:Tag + scope:Tag + camera:Tag)
p[[2]]=list(formula=~late:Tag + time + patch1:Tag + patch2:Tag + patch3:Tag + scope:Tag + camera:Tag)
Phi=vector("list",1)
Phi[[1]]=list(formula=~early:trt+Brand:batch)
results=run.cjs.models(p,Phi,ej.list)}
