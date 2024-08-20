dwmw <- function(lmmodel, boundary_check = TRUE, scale = FALSE, scale_info = TRUE, tol = 1e-04, max_message_iter = 7, pri_nAGQ = FALSE, max_nAGQ = 6, next_optimizer = "bobyqa", next_optCtrl = list(maxfun=2e5), when_next = max_message_iter - 1, verbose = FALSE) {

	self <- environment()
	self$messages <- "Initial model"
	lenmes <- oldlenmes <- 1
	i <- 0
	out_string <- c()
	nAGQ <- 1
	self$boolaux <- FALSE
	self$charaux <- NA_character_
	when_next <- min(when_next, max_message_iter - 1)

	while(any(!is.na(self$messages))){
		if(i > max_message_iter || nAGQ > max_nAGQ){
			stop(paste("Too many iterations!! to get the model", deparse1(formula(lmmodel)), "to converge. Check it!!"))
			return(NULL)
		}
		if(verbose && i > 0){
			cat("Iteration:", i, "\n",
			    "Try solving:\n", paste(self$messages, collapse = "\n"), "\n")
		}
		m2 <- withCallingHandlers(
					  expr = {
						  if (identical(self$messages, "Initial model")) {
							  self$messages <- NA_character_
							  if(grepl("^g*lmer\\(", deparse1(substitute(lmmodel)))){
								  m2 <- lmmodel
							  }else{
								  m2 <- update(lmmodel)
							  }
						  } else if(scale && (any(grepl("Rescale variables?", self$messages)) || any("Some predictor variables are on very different scales: consider rescaling" == self$messages))){
							  if(verbose){
								  cat("SCALING PARAMETERS\n")
							  }
							  self$messages <- NA_character_
							  self$boolaux <- TRUE
							  self$charaux <- paste(as.character(m2@call[["data"]]), "rescaled", sep = "-")
							  logOp <- sapply(m2@frame, is.numeric) & !(names(m2@frame) %in% c(vapply(findbars(formula(m2)), function(x) deparse1(x[[3]]), ""), as.character(formula(m2)[[2]])))
							  m2@frame[logOp] <- scale(self$m2@frame[logOp])
							  m2 <- update(self$m2, data = self$m2@frame)
							  out_string <- c(out_string, "Numeric predictors rescaled!!!\n")
						  } else if(any(grepl("Model failed to converge with max\\|grad\\|", self$messages)) && !(pri_nAGQ && any("Model is nearly unidentifiable: very large eigenvalue\n - Rescale variables?" == self$messages) && isa(self$m2, "glmerMod") && length(getME(self$m2,"theta")) == 1)) {
							  if(verbose){
								  cat("UPDATING MODEL START PARAMETERS\n")
							  }
							  self$messages <- NA_character_
							  self$ss <- getME(self$m2, c("theta", "fixef"))
							  if (i < when_next) {
								  m2 <- update(self$m2, start = self$ss)
							  } else {
								  if(verbose){
									  cat("UPDATING OPTIMIZER\n")
								  }
								  if(any(sapply(c("lmerMod", "lmerModLmerTest"), isa, x = self$m2))){
									  m2 <- update(self$m2, start = self$ss, control = lmerControl(optimizer = next_optimizer, optCtrl = next_optCtrl))
								  } else if(isa(self$m2, "glmerMod")){
									  m2 <- update(self$m2, start = self$ss, control = glmerControl(optimizer = next_optimizer, optCtrl = next_optCtrl))
								  }
							  }
						  } else if(any(grepl("boundary \\(singular\\) fit: see .+isSingular", self$messages))) {
							  if(verbose){
								  cat("REMOVING SINGULAR RANDOM EFFECTS\n")
							  }
							  self$messages <- NA_character_
							  if(boundary_check) {
								  dstring <- fstruction(self$m2, tol = tol)
								  if(length(dstring) == 1){
									  m2@call["start"] <- m2@call["REML"] <- m2@call["control"] <- NULL
									  if(any(sapply(c("lmerMod", "lmerModLmerTest"), isa, x = self$m2))){
										  m2@call[[1]] <- as.symbol("lm")
										  m2 <- update(self$m2, formula = dstring[["dstring"]])
									  } else if(isa(self$m2, "glmerMod")){
										  m2@call[[1]] <- as.symbol("glm")
										  m2 <- update(self$m2, formula = dstring[["dstring"]])
									  }
									  out_string <- c(out_string, "The default multilevel  model is singular since all the random-effects variances are zero. \nThen, we consider the next model after removing the random effects.\n\n")
								  }else{
									  m2 <- update(self$m2, formula = dstring[["dstring"]], start = NULL)
									  out_string <- c(out_string, paste0("The default multilevel  model is singular since ", paste0("the between-", dstring[["inchar"]], " variance for the ", dstring[["betchar"]], collapse = " and "),
													     ifelse(length(dstring[["betchar"]]) == 1," is","s are"),
													     " zero.\nThen, we consider the next model after removing", ifelse(length(dstring[["betchar"]]) == 1," this random effect"," these random effects"), ".\n\n"))
								  }
							  }
						  } else if(any("Model is nearly unidentifiable: very large eigenvalue\n - Rescale variables?" == self$messages) && isa(self$m2, "glmerMod") && length(getME(self$m2,"theta")) == 1) {
							  self$messages <- NA_character_
							  self$nAGQ <- self$nAGQ + 1
							  if(verbose){
								  cat("UPDATING nAGQ to", self$nAGQ,"\n")
							  }
							  self$max_message_iter <- self$max_message_iter + 1
							  m2 <- update(self$m2, nAGQ = self$nAGQ)
                                                          if(self$nAGQ == max_nAGQ) pri_nAGQ <- FALSE
						  } else if(identical("Some predictor variables are on very different scales: consider rescaling", self$messages)) {
							  self$messages <- NA_character_
							  if(scale_info){
								  out_string <- c(out_string, paste0("In the next model ", sub("^S(.*:) [a-z]+ ([a-z]+)$","s\\1 \\2 would be suitable", self$messages),".\n"))
							  }
						  }
						  m2
					  },
					  warning = function(w){
						  self$messages <- c(w$message, self$messages)
						  self$lenmes <- self$lenmes + 1
						  if (!is.null(findRestart('muffleWarning'))) {
							  invokeRestart('muffleWarning')
						  }
					  },
					  message = function(msg){
						  self$messages <- c(msg$message, self$messages)
						  self$lenmes <- self$lenmes + 1
						  if (!is.null(findRestart('muffleMessage'))) {
							  invokeRestart('muffleMessage')
						  }
					  }
		)
		i <- i + 1
		lenmes <- oldlenmes <- lenmes - oldlenmes
		self$messages <- self$messages[1:oldlenmes]
	}
	message(paste(out_string, collapse = ""))
	if(self$boolaux){
		if(isS4(m2)){
			m2@call[["data"]] <- as.symbol(self$charaux)
		}else{
			m2$call[["data"]] <- as.symbol(self$charaux)
		}
	}
	return(m2)
}

