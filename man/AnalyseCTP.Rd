% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AnalyseCTP.R
\name{AnalyseCTP}
\alias{AnalyseCTP}
\title{Analysing a Closed Testing Procedure}
\usage{
AnalyseCTP(ctp.struc, model, dataset, factor.name = NULL, test.name = "F", ...)
}
\arguments{
\item{ctp.struc}{Object generated by the function \code{\link{IntersectHypotheses}} (structure of CTP)}

\item{model}{model of the form response~treatment. If \code{testname="F"}, the model can be extended by covariates and other factors.
In the case
of a Logrank test the response must be imputed as \code{Surv(time,status)}.}

\item{dataset}{Dataframe, missing values in the response or treatment variable are not allowed!}

\item{factor.name}{Character string naming the factor whose levels are compared (treatment factor).
By default the first variable of the right-hand side of the model formula is used.}

\item{test.name}{One of the following strings \itemize{
    \item \code{"F"} - F-Test (ANOVA, default)
    \item \code{"kruskal"} -Kruskal-Wallis-Test
    \item \code{"chisq"} - Chi square test
    \item \code{"prob"} - Fisher's exact test for total number of observations <200 else Chi square test
    \item \code{"lgrank"} - Logrank-test 
    \item \code{"jonckheere"} - Jonckheere-Terpstra test of ordered alternatives
}}

\item{...}{Additional arguments for the chosen test.}
}
\value{
An object of old class(\code{ctp}), consisting of a list with:
  \itemize{
   \item{CTPparms} List with objects describing the CTP setup.
   \item{pvalues} Dataframe with all tested hypotheses, raw and adjusted p-values.
  }
}
\description{
Calculation of p-values of a closed testing procedure (CTP).
The function returns an object of \code{oldClass "ctp"; summary()} and \code{Display()} can be applied.
}
\details{
The hypothesis tree of the closed testing procedure must be created using \code{\link{IntersectHypotheses}}. For more details on the theory and the implementation as well
	for many examples, see the vignettes.
}
\note{
This procedure is constructed for testing differences and two-sided hypotheses,
	but not for equivalence tests. It is further based on independent samples from the population involved
	(i.e. on parallel group designs, but not on cross-over designs).
}
\examples{
		
	data(pasi)
	three.to.first <- IntersectHypotheses(list(1:2,c(1,3),c(1,4)))
	Display(three.to.first)
	pasi.ctp.F1 <- AnalyseCTP(three.to.first,pasi.ch~dose,pasi)
	summary(pasi.ctp.F1)
	Display(pasi.ctp.F1)
	
}
\seealso{
\code{\link{IntersectHypotheses}}, \code{\link{Display}}, \code{\link{summary.ctp.str}},  \code{\link{summary.ctp}},
	\code{\link{Adjust_raw}}
}
