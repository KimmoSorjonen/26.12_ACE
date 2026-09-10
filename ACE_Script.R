
############################# BIENVENUE #############################

############# I SOLEMNLY SWEAR THAT I AM UP TO NO GOOD ##############

#################################
## Loading packages

library(lavaan)

#################################

n <- 159 ## sample size

rm <- matrix(c( ## correlations reported by Ye et al. (2026)
  
  1.000, 0.146, 0.297, 0.252,
  0.146, 1.000, 0.717, 0.410,
  0.297, 0.717, 1.000, 0.441,
  0.252, 0.410, 0.441, 1.000), nrow=4)

colnames(rm) <- rownames(rm) <- c("AC","AH","SH","SR") ## names of variables

#################################
## Alternative model

altmod <- "

## Loadings

CSE =~ (-1)*AC+AH+SH+SR

## Intercepts, set to zero

AC ~ 0*1
AH ~ 0*1
SH ~ 0*1
SR ~ 0*1

CSE ~ 0*1

## (Error) variances

AC ~~ AC
AH ~~ AH
SH ~~ SH
SR ~~ SR

CSE ~~ CSE

"

altmod2 <- paste(altmod,"\nAC~~AH") ## extended model

fit.alt <- lavaan(altmod, sample.cov=rm, ## fitting model to data 
            sample.nobs=n, sample.mean=rep(0,4))

fit.alt2 <- lavaan(altmod2, sample.cov=rm, ## fitting ext. model to data 
                  sample.nobs=n, sample.mean=rep(0,4))

summary(fit.alt, fit.measures=T, ci=T, standardized=T, rsq=T) ## the results

summary(fit.alt2, fit.measures=T, ci=T, standardized=T, rsq=T) ## the results


########################## MISCHIEF MANAGED #########################

############################# AU REVOIR #############################



