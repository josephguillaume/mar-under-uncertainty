if(!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

source("functions.R")
source("NPV.R")
ranges <- read.csv("cost_benefit_par_ranges.csv",stringsAsFactors=FALSE)

 ################################################################################


## Fig 1
## define scenarios to use
## LHS=name to display
## RHS=name passed to the NPV function
scens=c("Basin infiltration"="basin",
        "Surface Storage"="base",
        "ASR"="injection")
plotNPV(ranges,"pump.cost.dollar.per.ml",scens)

## Fig 2 is a diagram

## Fig 3
round(base=c(NPV("base"),basin=NPV("basin"),injection=NPV("injection"))/1000)

## Fig 4
plotNPV(ranges,"basin.capital.cost.per.ml.at.0.2.m.per.day",scens)

## Fig 5.1 and 5.2 are diagrams

## Table 1 Levelised costs
## FIXME row 2 basin, row 1 injection
table1=rbind(
  c(0,
    annualised.capital.cost(NPV("basin",state.var="capital.cost"), 0.07, 30)/200,
    annualised.capital.cost(NPV("injection",state.var="capital.cost",asr.treatment.capital.cost.per.ml=150), 0.07, 20)/200
  ),
  c(
    ##FIXME: make 0.6 the default
    NPV("base",state.var="ongoing.cost",capture.pump.cost.ratio.surface=0.6)/200,
    NPV("basin",state.var="ongoing.cost",capture.pump.cost.ratio.surface=0.6)/200,
    NPV("injection",state.var="ongoing.cost",capture.pump.cost.ratio.surface=0.6)/200
  ),
  c(
    NPV("base",state.var="cost",capture.pump.cost.ratio.surface=0.6)/200,
    NPV("basin",state.var="cost",capture.pump.cost.ratio.surface=0.6)/200,
    NPV("injection",state.var="cost",capture.pump.cost.ratio.surface=0.6,asr.treatment.capital.cost.per.ml=150)/200
  )
)
colnames(table1)<-c("base","basin","injection")
rownames(table1)<-c("capital.cost","ongoing.cost","total")
round(table1,1)

## Table 2 Irrigation benefits
table2 <- rbind(
  ## Initial volume taken from flooding river ML
  200,
  ## Useable volume (after losses) (ML)
  200*c(base=0.6,basin=0.95,injection=0.95),
  ## Gross value of crop ($/ML)
  ## Calculated on equal areas
  sum((NPV(state.var="gross.margin.per.ha")/sum(NPV(state.var="crop.water.requirement.ml.per.ha")))[-3]),
  ## Irrigation benefits
  200*c(base=0.6,basin=0.95,injection=0.95)* 
    sum((NPV(state.var="gross.margin.per.ha")/sum(NPV(state.var="crop.water.requirement.ml.per.ha")))[-3])
)
round(table2,1)

## Table 3

table3 <- cbind(univariate.breakeven(ranges,"base","basin")[,c("Variable","Modeled")],
                surface.basin=round(univariate.breakeven(ranges,"base","basin")[,"break."],2),
                surface.asr=round(univariate.breakeven(ranges,"base","injection")[,"break."],2),
                basin.asr=round(univariate.breakeven(ranges,"basin","injection")[,"break."],2)
                )
table3


## Table 4 point of greatest concern
temp.ranges <- ranges[c(1:6,10:14),]
pgc <- crossoverEquiconcern("base","basin",temp.ranges)
table4 <- cbind(temp.ranges[,c("Variable","Min","Max","Modeled")],
                pt.greatest.concern=round(pgc$values,2),
                change.from.best.guess=round(pgc$values-temp.ranges$Modeled,2)
                )
table4
