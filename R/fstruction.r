fstruction <- function(model, tol = 1e-4){
  arcmM <- getME(model,"theta")
  larcmM <- getME(model, "lower")
  rnames <- mapply(pfun, names(model@cnms), model@cnms, SIMPLIFY = FALSE)
  
  auxRE <- rep(TRUE, length(rnames))
  
  fbc <- findbars(formula(model))
  blist <- lapply(fbc, mkBlist, model@frame, TRUE, reorder.vars = FALSE)
  nl <- vapply(blist, `[[`, 0L, "nl")
  if (any(diff(nl) > 0)){
    ord <- rev(order(nl))
    fbc <- fbc[ord]
  }
  
  names(fbc) <- vapply(fbc, function(x) deparse1(x[[3]]), "")
  fbc <- vapply(fbc, deparse1, "")
  neither_0_nor_1_bool <- !grepl("(0\\s*\\+.*\\|)|(\\+\\s*0.*\\|)", fbc) & !grepl("(1\\s*\\+.*\\|)|(\\+\\s*1.*\\|)", fbc)
  
  fbc2 <- fbc
  fbc2[neither_0_nor_1_bool] <- paste("1", fbc[neither_0_nor_1_bool], sep = " + ")
  
  dstring <- ".~."
  betchar <- vector(mode = "character", length = 0)
  inchar <- vector(mode = "character", length = 0)
  i <- 1
  for(lind in seq_along(rnames)){
    attr(rnames[[lind]], "idstr") <- vector(length = length(rnames[[lind]]))
    attr(rnames[[lind]], "index") <- vector(mode = "numeric", length = length(rnames[[lind]]))
    for(vind in seq_along(rnames[[lind]])){
      attr(rnames[[lind]], "index")[vind] <- i
      if(larcmM[i] == 0 && arcmM[i] < tol){
        attr(rnames[[lind]], "idstr")[vind] <- TRUE
      }else{
        FALSE
      }
      i <- i + 1
    }
    if(any(attr(rnames[[lind]], "idstr"))){
      dstring <- paste0(dstring, " - (",fbc[lind],")")
      auxboolkeep <- larcmM[attr(rnames[[lind]], "index")] == 0 & !attr(rnames[[lind]], "idstr")
      auxboolforget <- larcmM[attr(rnames[[lind]], "index")] == 0 & attr(rnames[[lind]], "idstr")
      auxstrforget <- sapply(names(arcmM[attr(rnames[[lind]], "index")][which(auxboolforget)]), 
                             \(.) sub("\\(Intercept\\)", "1", sub(paste0(names(rnames)[lind], "\\."), "", .)), USE.NAMES = FALSE)
      betchar <- c(betchar, auxstrforget)
      inchar <- c(inchar, names(rnames)[lind])
      if(any(auxboolkeep)){
        
        # Path 1
        # auxstrforget <- paste(auxstrforget, collapse = "|")
        # dstring <- paste0(dstring, " + (", gsub("^\\s*\\+\\s*|\\s*\\+\\s*(?=\\s\\|)|\\+\\s*(?=\\+)", "", gsub(auxstrforget, "", fbc2[lind]), perl = TRUE),")")
        
        # Path 2
        auxstrkeep <- sapply(names(arcmM[attr(rnames[[lind]], "index")][which(auxboolkeep)]), 
                               \(.) sub("\\(Intercept\\)", "1", sub(paste0(names(rnames)[lind], "\\."), "", .)), USE.NAMES = FALSE)
        if(!"1" %in% auxstrkeep){
          auxstrkeep <- c("0", auxstrkeep)
        }
        auxstrkeep <- paste(auxstrkeep, collapse = " + ")
        dstring <- paste0(dstring, " + (", auxstrkeep, "|", names(rnames)[lind], ")")
      }else{
        auxRE[lind] <- FALSE
      }
    }
  }
  if(all(sapply(auxRE, isFALSE))){
    return(list(dstring = dstring))
  }
  
  betchar[betchar != "1"] <- paste(betchar[betchar != "1"], "slope", sep = " ")
  betchar[betchar == "1"] <- "intercept"
  
  return(list(dstring = dstring, betchar = betchar, inchar = inchar))
}
