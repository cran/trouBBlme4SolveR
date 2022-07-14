
### lmer:::mkPfun(diag.only=FALSE, old=TRUE, prefix=NULL)
pfun <- function (g, e) 
{
  mm <- outer(e, e, paste, sep = ".")
  diag(mm) <- e
  mm <- mm[lower.tri(mm, diag = TRUE)]
  paste(g, mm, sep = ".")
}


# import lme4::factorize
### lmer:::mkBlist
### call mkBlist(findbars(model),model@frame, TRUE, reorder.vars = FALSE)
mkBlist <- function(x,frloc, drop.unused.levels=TRUE,
                    reorder.vars=FALSE) {
  frloc <- lme4::factorize(x,frloc)
  ## try to evaluate grouping factor within model frame ...
  ff0 <- replaceTerm(x[[3]], quote(`:`), quote(`%i%`))
  ff <- try(eval(substitute(makeFac(fac),
                            list(fac = ff0)),
                 frloc), silent = TRUE)
  if (inherits(ff, "try-error")) {
    stop("couldn't evaluate grouping factor ",
         deparse1(x[[3]])," within model frame:",
         "error =",
         c(ff),
         " Try adding grouping factor to data ",
         "frame explicitly if possible",call.=FALSE)
  }
  if (all(is.na(ff)))
    stop("Invalid grouping factor specification, ",
         deparse1(x[[3]]),call.=FALSE)
  ## NB: *also* silently drops <NA> levels - and mkReTrms() and hence
  ##     predict.merMod() have relied on that property  :
  if (drop.unused.levels) ff <- factor(ff, exclude=NA)
  nl <- length(levels(ff))
  list(ff = ff, nl = nl)
}

### lme4:::makeFac
makeFac <- function (x, char.only = FALSE) 
{
  if (!is.factor(x) && (!char.only || is.character(x))) 
    factor(x)
  else x
}


### lme4:::replaceTerm
replaceTerm <- function (term, target, repl) 
{
  if (identical(term, target)) 
    return(repl)
  if (!inForm(term, target)) 
    return(term)
  if (length(term) == 2) {
    return(substitute(OP(x), list(OP = replaceTerm(term[[1]], 
                                                   target, repl), x = replaceTerm(term[[2]], target, 
                                                                                  repl))))
  }
  return(substitute(OP(x, y), list(OP = replaceTerm(term[[1]], 
                                                    target, repl), x = replaceTerm(term[[2]], target, repl), 
                                   y = replaceTerm(term[[3]], target, repl))))
}

### lme4:::inForm
inForm <- function(form,value) {
    if (any(sapply(form,identical,value))) return(TRUE)
    if (all(sapply(form,length)==1)) return(FALSE)
    return(any(vapply(form,inForm,value,FUN.VALUE=logical(1))))
}

### lme4:::`%i%`
`%i%` <- function(f1, f2, fix.order = TRUE) {
  if (!is.factor(f1) || !is.factor(f2)) stop("both inputs must be factors")
  f12 <- paste(f1, f2, sep = ":")
  ## explicitly specifying levels is faster in any case ...
  u <- which(!duplicated(f12))
  if (!fix.order) return(factor(f12, levels = f12[u]))
  ## deal with order of factor levels
  levs_rank <- length(levels(f2))*as.numeric(f1[u])+as.numeric(f2[u])
  return(factor(f12, levels = (f12[u])[order(levs_rank)]))
}
