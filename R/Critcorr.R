#' Cointegration tests
#'
#' This function provides info about linear regression coefs between list components and executes various cointegration tests on it
#' @param listInput array of price arrays
#' @keywords cointegration
#' @export
#' @examples
#' Critcorr()

Critcorr = function(listInput){
  joinedInput = do.call(rbind,listInput)
  transposedInput = split(joinedInput,col(joinedInput))
  a = as.numeric(transposedInput[[1]])
  b = as.numeric(transposedInput[[2]])
  library(tseries)
  
  library(quantmod)
  
  m <- lm(a~b)
  
  kpsst1 = kpss.test(m$residuals, lshort = FALSE)
  kpsst2 = kpss.test(m$residuals, lshort = TRUE)
  adft1 = adf.test(m$residuals, alternative="stationary",0)
  adft2 = adf.test(m$residuals, alternative="stationary")
  ppt1 = pp.test(m$residuals, alternative="stationary",lshort = FALSE)
  ppt2 = pp.test(m$residuals, alternative="stationary",lshort = TRUE)
  
  cff = c(summary(m)$coefficients[1,1],
          summary(m)$coefficients[2,1])
  
  cr = c(cor(a,b),
         cor.test(a,b)$p.value)
  
  pvalpar = c(summary(m)$coefficients[1,4],
              summary(m)$coefficients[2,4])
  
  rsq = c(summary(m)$adj.r.squared)
  
  res = c(mean(m$residuals),
          var(m$residuals))
  
  kpss1 = c(kpsst1$p.value,
            kpsst1$parameter)
  
  kpss2 = c(kpsst2$p.value,
            kpsst2$parameter)
  
  adf1 = c(adft1$p.value,
           adft1$parameter)
  
  adf2 = c(adft2$p.value,
           adft2$parameter)
  
  pp1 = c(ppt1$p.value,
          ppt1$parameter)
  
  pp2 = c(ppt2$p.value,
          ppt2$parameter)
  
  stats <- list (cff, pvalpar, cr, rsq, 
                 res, kpss1, kpss2, adf1, adf2, pp1, pp2)
  
 # stats$name = 'stats'
  
  return(stats)
}