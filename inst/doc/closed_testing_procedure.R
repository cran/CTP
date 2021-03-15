## ----setup, include = FALSE---------------------------------------------------
library(knitr);library(CTP)
knitr::opts_chunk$set(collapse = TRUE, out.width="100%"
											 ,fig.width=12,fig.height=8,dev.args = list(pointsize=25) )

## -----------------------------------------------------------------------------
library(CTP)
		Pairwise <- IntersectHypotheses(list(c(1,2), c(1,3),
		                                c(1,4), c(2,3), c(2,4), c(3,4)))
    Set24    <- TestingSet(Pairwise,"[24]")
    Set24

## -----------------------------------------------------------------------------
library(CTP)

data(pasi)
three.to.first <- IntersectHypotheses(list(1:2,c(1,3),c(1,4)))
Display(three.to.first,Type="s",arrow=TRUE)
pasi.ctp.F1    <- AnalyseCTP(three.to.first,pasi.ch~dose,pasi)

summary(pasi.ctp.F1)
Display(pasi.ctp.F1)

## -----------------------------------------------------------------------------
dose.steps4 <- IntersectHypotheses(list(1:2,2:3,3:4))
Display(dose.steps4,arr=TRUE)



pasi.ctp.F2 <- AnalyseCTP(dose.steps4,pasi.ch~dose,pasi)
summary(pasi.ctp.F2)
Display(pasi.ctp.F2)

## -----------------------------------------------------------------------------
pasi$Resp <- ifelse(pasi$pasi.ch > 50,1,0)

pasi.ctp_bin <-AnalyseCTP(three.to.first,Resp~dose,pasi,test.name="glm",family="binomial")
summary(pasi.ctp_bin)

## -----------------------------------------------------------------------------
pasi.ctp.K <- AnalyseCTP(dose.steps4,pasi.ch~dose,pasi, test="kruskal")
summary(pasi.ctp.K)
Display(pasi.ctp.K)

## ----warning=FALSE,message=FALSE----------------------------------------------
pasi.ctp.J1 <- AnalyseCTP(dose.steps4,pasi.ch~dose,pasi, test="jonckheere",alternative="increasing")
summary(pasi.ctp.J1)

## -----------------------------------------------------------------------------
two.to.first<- IntersectHypotheses(list(1:2,c(1,3)))

Display(two.to.first,Type="s",main="two vs control",arrow=TRUE)

#The two elementary hypotheses  are tested after comparing the three proportions globally.

data(colorectal)
colorectal.ctp <-AnalyseCTP(two.to.first,responder~dose,data=colorectal, test="prob")
summary(colorectal.ctp)
Display(colorectal.ctp,Type="t")

colorectal.chisq <-AnalyseCTP(two.to.first,responder~dose,data=colorectal, test="chisq")
summary(colorectal.chisq, digits=1)

## -----------------------------------------------------------------------------
library(survival)
		data(ovarian)
		
		print(survdiff(Surv(futime,fustat)~rx, data=ovarian))

## -----------------------------------------------------------------------------
ovarian$subgroups <- as.factor(10*ovarian$ecog.ps+ovarian$rx)
print(head(ovarian))

## -----------------------------------------------------------------------------
comb.sub  <- IntersectHypotheses(list(c(1,2),c(3,4)))
#Display(comb.sub)
ovar.ctp  <-AnalyseCTP(comb.sub,Surv(futime,fustat)~subgroups, ovarian, test="lgrank")
summary(ovar.ctp)
Display(ovar.ctp)

## -----------------------------------------------------------------------------
data(glucose)
glucose.ctp <- AnalyseCTP(three.to.first,GLUCOSE.CHANGE~GLUCOSE.BLA+DOSE,
                         data=glucose, factor.name="DOSE")
summary(glucose.ctp)
Display(glucose.ctp,Type="s")

## -----------------------------------------------------------------------------
G <- factor(rep(1:5,each=4)	)			
y <- rnorm(20)
Y <- data.frame(G,y)
				
xxx <- IntersectHypotheses(list(1:2,c(1,3),c(1,4),c(1,5),c(2,5),c(3,4)))
summary(xxx)
Display(xxx)

## -----------------------------------------------------------------------------
		Pairwise <- IntersectHypotheses(list(c(1,2), c(1,3), c(1,4), c(2,3), c(2,4), c(3,4)))
		Display(Pairwise)
		summary(Pairwise)
		
		# the vector of p-values calculated by another software
		# (Example from Prof. John M. Lachin, The Biostatistics Center Rockville MD)
		
		p.val <- c(
		  0.4374,
		  0.6485,
		  0.4103,
		  0.2203,
		  0.1302,
		  0.6725,
		  0.4704,
		  0.3173,
		  0.6762,
		  0.7112,
		  0.2866,
		  0.3362,
		  0.2871,
		  0.4633)

		result <- Adjust_raw(Pairwise, p.value=p.val)
		
		summary(result,digits=3)	
				
# details may be documented
		
		result <- Adjust_raw(Pairwise, p.value=p.val
		                     ,dataset.name="my Data", factor.name="Factor"
		                     ,factor.levels=c("A","B","C","D"), model=y~Factor
		                     ,test.name="my Test")
		
		summary(result,digits=3)

