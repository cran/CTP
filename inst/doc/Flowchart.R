## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
knitr::opts_chunk$set(collapse = TRUE, out.width="100%"
											 ,fig.width=12,fig.height=8,dev.args = list(pointsize=25),echo=FALSE )

## ---- warning=FALSE, message=FALSE--------------------------------------------
  library(Gmisc)
 library(grid)
  Line <- getOption("connectGrob",default=grid::gpar(col = "darkblue",fill="darkblue"))
  Arrow <- getOption("connectGrobArrow", default = grid::arrow(ends = "last", type ="closed",length=grid::unit(2,"mm")))  
  B <- list()
  B[[1]] <- boxGrob(label=expression("Hypothesis structure" %->% italic("InterSectHypotheses(...)")) , y=0.9, x=0.5,just = "left",
           txt_gp=gpar(cex=0.8,col="darkblue"),height=0.05)
  B[[2]] <- boxGrob(label=expression("Object of (old) class: " * italic("ctp.str")) , y=0.8, x=0.5,just = "left",
          txt_gp=gpar(cex=0.8,col="darkblue"),height=0.05)
  B[[3]] <- boxGrob(label=expression("Optional: " * italic("Display(obj)")) , y=0.7, x=0.5,just = "left",
          txt_gp=gpar(cex=0.8,col="darkblue"),height=0.05)
  B[[4]] <- boxGrob(label="p-values calculated or \"external?\" ", y=0.6, x=0.5,just = "left",
          txt_gp=gpar(cex=0.8,col="darkblue"),height=0.05)
  B[[5]] <- boxGrob(label=expression(italic("AnalyseCTP(...)")), y=0.5, x=0.3,just = "left",
          txt_gp=gpar(cex=0.8,col="darkblue"),height=0.05)
  B[[6]] <- boxGrob(label=expression(italic("Adjust_raw(...)")), y=0.5, x=0.7,just = "left",
          txt_gp=gpar(cex=0.8,col="darkblue"),height=0.05)
  B[[7]] <- boxGrob(label=expression("Object of (old) class: " * italic("ctp")) , y=0.4, x=0.5,just = "left",
          txt_gp=gpar(cex=0.8,col="darkblue"),height=0.05)
  B[[8]] <- boxGrob(label=expression(italic("summary(obj)")) , y=0.3, x=0.5,just = "left",
          txt_gp=gpar(cex=0.8,col="darkblue"),height=0.05)
  B[[9]] <- boxGrob(label=expression("Optional: " * italic("Display(obj)")) , y=0.2, x=0.5,just = "left",
          txt_gp=gpar(cex=0.8,col="darkblue"),height=0.05)

   for (i in 1:length(B)) print(B[[i]])
  
  connectGrob(B[[1]],B[[2]], arrow_obj = Arrow,lty_gp=Line)
  connectGrob(B[[2]],B[[3]], arrow_obj = Arrow,lty_gp=Line)
  connectGrob(B[[3]],B[[4]], arrow_obj = Arrow,lty_gp=Line)
  connectGrob(B[[4]],B[[5]], arrow_obj = Arrow,lty_gp=Line)
  connectGrob(B[[4]],B[[6]], arrow_obj = Arrow,lty_gp=Line)
  connectGrob(B[[5]],B[[7]], arrow_obj = Arrow,lty_gp=Line)
  connectGrob(B[[6]],B[[7]], arrow_obj = Arrow,lty_gp=Line)
  connectGrob(B[[7]],B[[8]], arrow_obj = Arrow,lty_gp=Line)
  connectGrob(B[[8]],B[[9]], arrow_obj = Arrow,lty_gp=Line)

