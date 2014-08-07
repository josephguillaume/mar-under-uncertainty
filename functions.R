## copied from library(cost.benefit.breakeven)
univariate.breakeven <- function (ranges, scen, baseline) {
  eqs <- sapply(ranges$Variable, function(v) {
    ranges0 <- subset(ranges, Variable == v)
    basin_base0 <- createFun(scen, baseline, ranges0)
    tryCatch(return(uniroot(basin_base0, interval = c(ranges0$Min, 
                                                      ranges0$Max))$root), error = function(e) return(NA))
  })
  data.frame(ranges, `break` = eqs)
}

## copied from library(cost.benefit.breakeven)
crossoverEquiconcern <-function (scen, baseline, ranges) {
  basin_base0 <- createFun(scen, baseline, ranges)
  dNPV = function(loc, bound) {
    x = ifelse(bound < ranges$Modeled, bound + loc * (ranges$Modeled - 
                                                        bound)/100, bound - loc * (bound - ranges$Modeled)/100)
    basin_base0(x)
  }
  bounds <- do.call(expand.grid, lapply(apply(ranges[, c("Min", 
                                                         "Max")], 1, as.list), unlist))
  locs = apply(bounds, 1, function(bound) {
    res = tryCatch(uniroot(dNPV, c(0, 100), bound = bound)$root, 
                   error = function(e) NA)
  })
  if (all(is.na(locs))) 
    stop("No crossover points found")
  w.max.loc = which.max(locs)
  loc = locs[w.max.loc]
  bound = as.numeric(bounds[w.max.loc, ])
  names(bound) <- ranges$Variable
  x = ifelse(bound < ranges$Modeled, bound + loc * (ranges$Modeled - 
                                                      bound)/100, bound - loc * (bound - ranges$Modeled)/100)
  names(x) <- ranges$Variable
  list(loc = loc, bound = bound, dNPV = dNPV(loc, bound = bound), 
       values = x)
}


## copied from library(cost.benefit.breakeven)
annualised.capital.cost <- function (C, r, t) {
  annuity.factor = (1 - 1/(1 + r)^t)/r
  C/annuity.factor
}

## copied from library(cost.benefit.breakeven)
getDefaultPars <- function (fn) {
  pars <- as.list(eval(formals(fn)))
  for (i in 1:nrow(ranges)) eval(parse(text = sprintf("pars$%s <- %f", 
                                                      ranges$Variable[i], ranges$Modeled[i])))
  pars
}

## copied from library(cost.benefit.breakeven)
createFun <- function (scen, baseline, ranges, fixed.vals = NULL, 
                       MODEL = get("NPV", envir = .GlobalEnv)){
  pars <- getDefaultPars(MODEL)
  for (v in ranges$Variable) {
    if (is.null(eval(parse(text = sprintf("pars$%s", v))))) 
      stop(sprintf("Parameter %s not recognised", v))
  }
  fixed.vals2 <- paste(sapply(1:length(fixed.vals), function(i) sprintf("pars$%s <- %s", 
                                                                        names(fixed.vals)[i], fixed.vals[[i]])), collapse = "\n    ")
  if (is.null(fixed.vals)) 
    fixed.vals2 <- ""
  settings <- paste(sapply(1:nrow(ranges), function(i) sprintf("pars$%s <- x[%d]", 
                                                               ranges[i, 1], i)), collapse = "\n    ")
  f <- eval(parse(text = sprintf("\n  function(x){\n    x<-as.numeric(x)\n    pars <- getDefaultPars(NPV)\n    %s\n    pars$scen='%s'\n    %s\n    if(any(!names(pars) %%in%% names(formals(NPV))))\n     warning(sprintf('Variables are given in ranges etc but not used: %%s',\n      paste(names(pars)[!names(pars) %%in%% names(formals(NPV))],\n       collapse=', '\n      )\n     ))\n    pars=pars[names(formals(NPV))]\n    s=do.call(NPV,pars)\n    pars$scen='%s'\n    b=do.call(NPV,pars)\n    diff=s-b\n    stopifnot(!is.na(diff))\n    diff\n  }", 
                                 fixed.vals2, scen, settings, baseline)))
  environment(f) = .GlobalEnv
  f
}


## ranges - data.frame with columns Variable,Modeled,Min,Max
## variable - one of ranges$Variable
## scens - vector of scenarios accepted by NPV as argument scen.
##   if named, names are used in plot
## NPV - function to evaluate
## text - if TRUE, return text instead of ggplot object
plotNPV<-function(ranges,variable,scens,NPV=get("NPV",.GlobalEnv),text=FALSE){
  if(is.null(names(scens))) names(scens) <- scens
  ranges0 <- subset(ranges, Variable == variable)
  ## Template of the ggplot command
  tpl="                    ggplot(data=data.frame(x=c(ranges0$Min,ranges0$Max)))+
          geom_vline(aes(xintercept={MODELED},linetype='Best guess',size='Best guess',colour='Best guess',show_guide=TRUE))+
          geom_vline(aes(xintercept=c({MIN},{MAX}),linetype='Limits',size='Limits',colour='Limits'))+
          geom_hline(aes(yintercept=0),colour='grey',size=1,linetype='solid')+
          scale_x_continuous(name=variable,limits=range(c(ranges0$Min,ranges0$Max)))+
          scale_y_continuous(name='NPV AU$1000s')+
          scale_linetype_manual(name='Lines',values=c('Limits'='solid','Best guess'='dashed',{LINETYPE}),
                    limits=c('Best guess','Limits',{SCENS}))+
          scale_size_manual(name='Lines',values=c('Limits'=0.5,'Best guess'=0.5,{SIZE}),
                    limits=c('Best guess','Limits',{SCENS}))+
          scale_colour_manual(name='Lines',values=c('Limits'='black','Best guess'='black',{COLOUR}),
                    limits=c('Best guess','Limits',{SCENS}))"
  ## Replace values
  tpl=gsub("{MODELED}",ranges0[,"Modeled"],tpl,fixed=TRUE)
  tpl=gsub("{MIN}",ranges0[,"Min"],tpl,fixed=TRUE)
  tpl=gsub("{MAX}",ranges0[,"Max"],tpl,fixed=TRUE)
  tpl=gsub("{LINETYPE}",paste(sprintf("'%s'='solid'",names(scens)),collapse=","),tpl,fixed=TRUE)
  tpl=gsub("{SIZE}",paste(sprintf("'%s'=1",names(scens)),collapse=","),tpl,fixed=TRUE)
  tpl=gsub("{COLOUR}",paste(sprintf("'%s'='%s'",names(scens),scales::hue_pal()(length(names(scens)))),collapse=","),tpl,fixed=TRUE)
  tpl=gsub("{SCENS}",paste(sprintf("'%s'",names(scens)),collapse=","),tpl,fixed=TRUE)
  ## Add functions to evaluate
  for(i in 1:length(scens)){
    scen.name=names(scens)[i]
    tpl=sprintf("%s+
          stat_function(fun=function(x){
               pars=getDefaultPars(NPV)
               pars$scen='%s'
               sapply(x,function(x2) {pars$%s=x2; do.call(NPV,pars)/1000})
},aes(linetype='%s',size='%s',colour='%s'))",tpl,scens[i],variable,scen.name,scen.name,scen.name)
  }
  if(text) return(tpl)
  eval(parse(text=tpl))
}