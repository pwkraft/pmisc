#' Simulate expected values/first differences (replaces Zelig)
#' built: 2016-08-27, Patrick Kraft
#' @importFrom MASS mvrnorm
#' @importFrom sandwich vcovHC
#' @param models: list of model results (lm, glm, or vglm/tobit)
#' @param iv: data frame containing the values for comparison (only 2 rows, selected variables)
#' @param se: NULL, robust, newey; types of standard errors that should be used
#' @param nsim: number of simulations
#' @return data.frame: contains expected values, confidence intervals, variable names
#' @export
#'

sim <- function(models, iv, se=NULL, ci=c(0.025,0.975), nsim = 1000){

  ## prepare output object, convert input to model list
  out <- NULL
  if(class(models)[1] != "list") models <- list(models)

  for(i in 1:length(models)){
    ## simulate betas from sampling distribution
    if(is.null(se)){
      betas <- MASS::mvrnorm(nsim, coef(models[[i]]), sandwich::vcovHC(models[[i]]))
    } else if(se == "robust"){
      betas <- MASS::mvrnorm(nsim, coef(models[[i]]), vcov(models[[i]]))
    } else if(se == "newey"){
      betas <- MASS::mvrnorm(nsim, coef(models[[i]]), sandwich::NeweyWest(models[[i]]))
    }

    ## extract variable names
    vars <- names(coef(models[[i]]))
    int <- grep("[^)]:", vars)
    varsInt <- strsplit(vars[int], ":")

    ## generate matrix of covariates
    X <- matrix(1, nrow=length(vars), ncol=nrow(iv))
    X[vars %in% names(iv),] <- t(iv[vars[vars %in% names(iv)]])
    if(class(models[[i]])[1]=="lm"){
      means <- apply(models[[i]]$model[vars[-c(1,which(vars %in% names(iv)),int)]]
                     , 2, mean, na.rm=T)
    } else if(class(models[[i]])[1] == "glm"){
      means <- apply(models[[i]]$model[vars[-c(1,which(vars %in% names(iv)),int)]]
                     , 2, mean, na.rm=T)
    } else if(class(models[[i]])[1] == "vglm" & models[[i]]@family@vfamily == "tobit"){
      means <- apply(models[[i]]@x[,vars[-c(1,2,which(vars %in% names(iv)),int)]]
                     , 2, mean, na.rm=T)
    } else stop("Model type not supported")
    X[vars %in% names(means),] <- means

    ## calculate interaction effects
    if(length(varsInt)>0){
      for(j in 1:length(varsInt)){
        X[int[j],] <- apply(X[vars %in% varsInt[[j]],],2,prod)
      }
    }

    ## calculate expected values
    if(class(models[[i]])[1]=="lm"){
      evs <- betas %*% X
    } else if(class(models[[i]])[1] == "glm"){
      if(models[[i]]$family$link == "logit"){
        evs <- 1/(1+exp(-betas %*% X))
      } else if(models[[i]]$family$link == "probit"){
        evs <- pnorm(betas %*% X)
      } else stop("Model type not supported")
    } else if(class(models[[i]])[1] == "vglm" & models[[i]]@family@vfamily == "tobit"){
      ## IDEA: decompose effect of tobit in dP(Y>0) and dY|Y>0
      ## based on predicted values (rather than EVs)
      ## note that betas[,2] is log(Sigma) estimate
      ## CHECK CALCULATIONS!
      if(unique(models[[i]]@misc$Upper)!=Inf) stop("Upper limit not supported")
      if(unique(models[[i]]@misc$Lower)!=0) warning("Limit != 0 not testes yet!")
      loLim <- unique(models[[i]]@misc$Lower)[1,1]

      ## expected values for z>0
      evsTemp <- betas[,-2] %*% X[-2,]
      evs <- evsTemp + exp(betas[,2]) * dnorm(evsTemp/exp(betas[,2])) / pnorm(evsTemp/exp(betas[,2]))

      ## probability of z>0
      pvs <- array(dim = c(nsim,ncol(X),nsim))
      for(j in 1:nrow(pvs)){
        pvs[j,,] <- matrix(rnorm(nsim*ncol(X), mean = evsTemp[j,], sd = exp(betas[j,2]))
                           , ncol = nsim)
      }
      prob <- apply(pvs, 2, function(x) apply(x, 1, function(x) mean(x>loLim)))
    } else stop("Model type not supported")
    
    skip <- F

    if(nrow(iv)==2){
      ## calculate first differences
      evs <- evs[,2] - evs[,1]
      if(class(models[[i]])[1] == "vglm"){
        if(models[[i]]@family@vfamily == "tobit")
          prob <- prob[,2] - prob[,1]
      }
    } else if(nrow(iv)==4) {
      ## calculate difference-in-difference
      evs <- (evs[,2] - evs[,1]) - (evs[,4] - evs[,3])
      if(class(models[[i]])[1] == "vglm"){
        if(models[[i]]@family@vfamily == "tobit")
          prob <- (prob[,2] - prob[,1]) - (prob[,4] - prob[,3])
      }
    } else {
      ## compute predicted values for each step
      warning("Check number of scenarios - STILL TESTING")
      if(class(models[[i]])[1] != "vglm"){
        res <- data.frame(mean = apply(evs, 2, mean)
                          , cilo = apply(evs, 2, quantile, ci[1])
                          , cihi = apply(evs, 2, quantile, ci[2])
                          , dv = as.factor(colnames(models[[i]]$model)[1])
                          , iv = as.factor(paste(colnames(iv), collapse = "_"))
                          , ivval = iv[,1])
      } else if(models[[i]]@family@vfamily == "tobit"){
        res <- data.frame(mean = c(apply(prob, 2, mean), apply(evs, 2, mean))
                          , cilo = c(apply(prob, 2, quantile, ci[1]), apply(evs, 2, quantile, ci[1]))
                          , cihi = c(apply(prob, 2, quantile, ci[2]), apply(evs, 2, quantile, ci[2]))
                          , dv = as.factor(sub("(.*) \\~.*", "\\1", models[[i]]@call[2]))
                          , iv = as.factor(paste(colnames(iv), collapse = "_"))
                          , ivval = iv[,1]
                          , value = factor(rep(c("Probability P(y>0)","Expected Value E(y|y>0)"), each = nrow(iv))
                                           , levels = c("Probability P(y>0)","Expected Value E(y|y>0)")))
      } else stop("Check model type")
      out <- rbind(out, res)
      skip <- T
    }
    
    ## warning for Inf/-Inf in single iterations
    if(Inf %in% evs|-Inf %in% evs){
      warning(paste0("Inf/-Inf in ",length(evs[evs==Inf])+length(evs[evs==-Inf])," evs iteration(s)"))
      evs[evs==Inf|evs==-Inf] <- NA
    }
    
    if(!skip){
      ## generate output table
      if(class(models[[i]])[1] != "vglm"){
        res <- data.frame(mean = mean(evs)
                          , cilo = quantile(evs, ci[1])
                          , cihi = quantile(evs, ci[2])
                          , dv = as.factor(colnames(models[[i]]$model)[1])
                          , iv = as.factor(paste(colnames(iv), collapse = "_")))
      } else {
        res <- data.frame(mean = c(mean(prob, na.rm = T), mean(evs, na.rm = T))
                          , cilo = c(quantile(prob, ci[1], na.rm = T),quantile(evs, ci[1], na.rm = T))
                          , cihi = c(quantile(prob, ci[2], na.rm = T), quantile(evs, ci[2], na.rm = T))
                          , dv = as.factor(sub("(.*) \\~.*", "\\1", models[[i]]@call[2]))
                          , iv = as.factor(paste(colnames(iv), collapse = "_"))
                          , value = factor(c("Probability P(y>0)","Expected Value E(y|y>0)")
                                           , levels = c("Probability P(y>0)","Expected Value E(y|y>0)")))
      }
      out <- rbind(out, res)
    }
  }

  ## return output table
  rownames(out) <- NULL
  return(out)
}
