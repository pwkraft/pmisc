#' Simulate expected values/first differences (replaces Zelig)
#' built: 2016-08-27, Patrick Kraft
#' @param models: list of model results (lm, glm, or vglm/tobit)
#' @param iv: data frame containing the values for comparison (only 2 rows, selected variables)
#' @param robust: logical, should robust standard errors be used
#' @param nsim: number of simulations
#' @return data.frame: contains expected values, confidence intervals, variable names
#' @exportFrom MASS mvrnorm
#' 

sim <- function(models, iv, robust=F, ci=c(0.025,0.975), nsim = 1000){

  ## prepare output object, convert input to model list
  out <- NULL
  if(class(models)[1] != "list") models <- list(models)
  
  for(i in 1:length(models)){
    ## simulate betas from sampling distribution
    if(robust == T){
      betas <- mvrnorm(nsim, coef(models[[i]]), vcovHC(models[[i]]))
    } else {
      betas <- mvrnorm(nsim, coef(models[[i]]), vcov(models[[i]]))
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
      means <- apply(models[[i]]$data[vars[-c(1,which(vars %in% names(iv)),int)]]
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
      evs <- betas[,-2] %*% X[-2,]
      pvs <- array(dim = c(nsim,ncol(X),nsim))
      for(j in 1:nrow(pvs)){
        pvs[j,,] <- matrix(rnorm(nsim*ncol(X), mean = evs[j,], sd = exp(betas[j,2]))
                           , ncol = nsim)
      }
      test <- apply(pvs, 2, function(x) apply(x, 1, function(x) mean(x>0)))
      head(test)
      head(evs)
      plot(as.vector(test),as.vector(evs))
      
      pvs <- matrix(rnorm(length(betas)*100
                          , rep(as.vector(betas[,-2] %*% X[-2,]),each=100)
                          , ncol = ncol(X))
      pvs <- cbind(pvs
      evs <- apply(evsTemp, 2, function(x) x)
      
      unique(models[[i]]@misc$Lower)
    } else stop("Model type not supported")
    
    if(nrow(iv)==2){
      ## calculate first differences
      evs <- evs[,2] - evs[,1]
    } else if(nrow(iv)==4) {
      ## calculate difference-in-difference
      evs <- (evs[,2] - evs[,1]) - (evs[,4] - evs[,3])
    } else {
      warning("Check number of scenarios")
    }
    
    ## generate output table
    res <- data.frame(mean = mean(evs)
                      , cilo = quantile(evs, ci[1])
                      , cihi = quantile(evs, ci[2])
                      , dv = as.factor(colnames(models[[i]]$model)[1])
                      , iv = as.factor(paste(colnames(iv), collapse = "_")))
    out <- rbind(out, res)
  }
  
  ## return output table
  rownames(out) <- NULL
  return(out)
}