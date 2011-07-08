\name{extract.Ej}
\alias{extract.Ej}
\title{Extract brand evaluation data from Access database...}
\usage{extract.Ej(file="Ej-Brand-Resight-Database.accdb",
    dir="C:/Users/JLaake/Workspace/EjBrandEval",
    scenario=1)}

\description{
  Extract brand evaluation data from Access database
  Extracts brand/tag and resight data from 2005 Rogue Reef
  cohort, creates capture history using scenario specific
  criterion and formats for use in RMark
}

\details{
  Scenario=1 All data all times

  Scenario=2 Time <= 9/29/2005

  Scenario=3 All times, SiteName!="Cape Arago"

  Scenario=4 Time <1/1/2006 | Time >=1/1/2006 &
  Region=="WA" | (Observer=="BG" and Region=="BC")
}
\value{\item{ej}{dataframe containing capture histories and covariate data
for analysis} \item{platform}{dataframe with platform covariates for each
sampling occasion} \item{times}{vector of dates of sampling occasions}
\item{scenario}{selected scenario value}}
\author{Jeff Laake}
\arguments{
  \item{file}{filename of Ej brand-resight database}
  \item{dir}{path specification for directory containing database}
  \item{scenario}{value of 1 to 4 for different data extract scenarios; see
details}
}
