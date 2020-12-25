# install.packages("shinyWidgets")
# install.packages("dplyr")
# install.packages("EValue")

library(shiny)
library(plotly)
library(shinythemes)
library(shinyBS)
library(shinyalert)
library(bsplus)
library(shinydashboard)
library(shinyWidgets)


#library(EValue) #include confounded_meta and sens_plot below to test, will eventually be loaded into EValue package and can remove the functions below

# utils.R
# EValue.R
# meta-analysis.R
# effect_measures.R


library(MetaUtility)
# try to fix deployment problem
library(purrr)
library(plogr)
library(dplyr)
library(boot)

# keeps original error messages
options(shiny.sanitize.errors = FALSE)




############################ MAIN FUNCTIONS ############################ 


#' Sensitivity analysis for unmeasured confounding in meta-analyses
#' 
#' This function implements the sensitivity analyses of Mathur & VanderWeele (2020a, 2020b). It computes point estimates, standard errors, and confidence intervals
#' for (1) \code{Prop}, the proportion of studies with true causal effect sizes above or below a chosen threshold \code{q} as a function of the bias parameters;
#' (2) the minimum bias factor on the relative risk scale (\code{Tmin}) required to reduce to
#' less than \code{r} the proportion of studies with true causal effect sizes more extreme than
#' \code{q}; and (3) the counterpart to (2) in which bias is parameterized as the minimum
#' relative risk for both confounding associations (\code{Gmin}).
#' 
#' @param method \code{"calibrated"} or \code{"parametric"}. See Details. 
#' 
#' @param q True causal effect size chosen as the threshold for a meaningfully large effect.
#' 
#' @param r For \code{Tmin} and \code{Gmin}, value to which the proportion of meaningfully strong effect sizes is to be reduced.
#' 
#' @param tail \code{"above"} for the proportion of effects above \code{q}; \code{"below"} for
#' the proportion of effects below \code{q}. By default, is set to \code{"above"} if the pooled point estimate (\code{method = "parametric"}) or median of the calibrated estimates (\code{method = "calibrated"}) is above 1 on the relative risk scale and is set to \code{"below"} otherwise.
#' 
#' @param CI.level Confidence level as a proportion (e.g., 0.95).
#' 
#' @param give.CI Logical. If \code{TRUE}, confidence intervals are provided. Otherwise, only point estimates are provided.
#' 
#' @param R Number  of  bootstrap  iterates for confidence interval estimation. Only used if \code{method = "calibrated"} and \code{give.CI = TRUE}. 
#' 
#' @param muB Mean bias factor on the log scale across studies (greater than 0). When considering bias that is of homogeneous strength across studies (i.e., \code{method = "calibrated"} or \code{method = "parametric"} with \code{sigB = 0}), \code{muB} represents the log-bias factor in each study. If \code{muB} is not specified, then only \code{Tmin} and \code{Gmin} will be returned, not \code{Prop}. 
#' 
#' @param muB.toward.null Whether you want to consider bias that has on average shifted studies' point estimates away from the null (\code{FALSE}; the default) or that has on average shifted studies' point estimates toward the null (\code{TRUE}). See Details.
#' 
#' @param dat Dataframe containing studies' point estimates and variances. Only used if \code{method = "calibrated"}.
#' @param yi.name Name of variable in \code{dat} containing studies' point estimates on the log-relative risk scale. Only used if \code{method = "calibrated"}.
#' @param vi.name Name of variable in \code{dat} containing studies' variance estimates. Only used if \code{method = "calibrated"}.
#' 
#' @param sigB Standard deviation of log bias factor across studies. Only used if \code{method = "parametric"}.
#' 
#' @param yr Pooled point estimate (on log-relative risk scale) from confounded meta-analysis. Only used if \code{method = "parametric"}.
#' 
#' @param vyr Estimated variance of pooled point estimate from confounded meta-analysis. Only used if \code{method = "parametric"}.
#' 
#' @param t2 Estimated heterogeneity (\eqn{\tau^2}) from confounded meta-analysis. Only used if \code{method = "parametric"}.
#' 
#' @param vt2 Estimated variance of \eqn{\tau^2} from confounded meta-analysis. Only used if \code{method = "parametric"}.
#' 
#' @param \dots Additional arguments passed to \code{confounded_meta}.
#' 
#' @export
#' @details
#' ## Specifying the sensitivity parameters on the bias
#' By convention, the average log-bias factor, \code{muB}, is taken to be greater than 0 (Mathur & VanderWeele, 2020a; Ding & VanderWeele, 2017). Confounding can operate on average either away from or toward the null, a choice specified via \code{muB.toward.null}. The most common choice for sensitivity analysis is to consider bias that operates on average away from the null, which is \code{confounded_meta}'s default. In such an analysis, correcting for the bias involves shifting studies' estimates back toward the null by \code{muB} (i.e., if \code{yr > 0}, the estimates will be corrected downward; if \code{yr < 0}, they will be corrected upward). Alternatively, to consider bias that operates on average away from the null, you would still specify \code{muB > 0} but would also specify \code{muB.toward.null = TRUE}. For detailed guidance on choosing the sensitivity parameters \code{muB} and \code{sigB}, see Section 5 of Mathur & VanderWeele (2020a).
#' 
#' ## Specifying the threshold \code{q}
#' For detailed guidance on choosing the threshold \code{q}, see the Supplement of Mathur & VanderWeele (2020a).   
#' 
#' ## Specifying the estimation method
#' By default, \code{confounded_meta} performs estimation using a \strong{calibrated method} (Mathur & VanderWeele, 2020b) that extends work by Wang et al. (2019). This method makes no assumptions about the distribution of population effects and performs well in meta-analyses with as few as 10 studies, and performs well even when the proportion being estimated is close to 0 or 1. However, it only accommodates bias whose strength is the same in all studies (homogeneous bias). When using this method, the following arguments need to be specified:
#' \itemize{
#'  \item \code{q}
#'  \item \code{r} (if you want to estimate \code{Tmin} and \code{Gmin})
#'  \item \code{muB}
#'  \item \code{dat}
#'  \item \code{yi.name}
#'  \item \code{vi.name}
#' }
#' 
#' The \strong{parametric method} assumes that the population effects are approximately normal and that the number of studies is large. Parametric confidence intervals should only be used when the proportion estimate is between 0.15 and 0.85 (and \code{confounded_meta} will issue a warning otherwise). Unlike the calibrated method, the parametric method can accommodate bias that is heterogeneous across studies (specifically, bias that is log-normal across studies). When using this method, the following arguments need to be specified:
#' \itemize{
#' \item \code{q}
#' \item \code{r} (if you want to estimate \code{Tmin} and \code{Gmin})
#' \item \code{muB}
#' \item \code{sigB}
#' \item \code{yr}
#' \item \code{vyr} (if you want confidence intervals)
#' \item \code{t2}
#' \item \code{vt2} (if you want confidence intervals)
#' }
#' 
#' 
#' ## Effect size measures other than log-relative risks
#' If your meta-analysis uses effect sizes other than log-relative risks, you should first approximately convert them to log-relative risks, for example via [EValue::convert_measures()] and then pass the converted point estimates or meta-analysis estimates to \code{confounded_meta}. 
#' 
#' ## Interpreting \code{Tmin} and \code{Gmin}
#' \code{Tmin} is defined as the minimum bias factor on the relative risk scale, common to all meta-analyzed studies, that would be required to reduce to less than \code{r} the proportion of studies with true causal effect sizes stronger than the threshold \code{q}. \code{Gmin} is defined as the minimum confounding strength on the relative risk scale -- that is, the relative risk relating unmeasured confounder(s) to both the exposure and the outcome -- common to all meta-analyzed studies, that would be required to reduce to less than \code{r} the proportion of studies with true causal effect sizes stronger than the threshold \code{q}. \code{Gmin} is a one-to-one transformation of \code{Gmin} given by \eqn{Gmin = Tmin + \sqrt{Tmin * (Tmin - 1)} }. If the estimated proportion of meaningfully strong effect sizes is already less than \code{r} even without the introduction of any bias, \code{Tmin} and \code{Gmin} will be set to 1.
#' 
#' The direction of bias represented by \code{Tmin} and \code{Gmin} is dependent on the argument \code{tail}: when \code{tail = "above"}, these metrics consider bias that had operated to \emph{increase} studies' point estimates, and when \code{tail = "below"}, these metrics consider bias that had operated to \emph{decrease} studies' point estimates. Such bias could operate toward or away from the null depending on whether the pooled point estimate \code{yr} happens to fall above or below the null. As such, the direction of bias represented by \code{Tmin} and \code{Gmin} may or may not match that specified by the argument \code{muB.toward.null} (which is used only for estimation of \code{Prop}).
#' 
#' ## When these methods should be used
#' These methods perform well only in meta-analyses with at least 10 studies; we do not recommend reporting them in smaller meta-analyses. Additionally, it only makes sense to consider proportions of effects stronger than a threshold when the heterogeneity estimate \code{t2} is greater than 0. For meta-analyses with fewer than 10 studies or with a heterogeneity estimate of 0, you can simply report E-values for the point estimate via [EValue::evalue()] (VanderWeele & Ding, 2017; see Mathur & VanderWeele (2020a), Section 7.2 for interpretation in the meta-analysis context).
#'  
#' 
#' @keywords meta-analysis
#' @import
#' metafor
#' stats 
#' MetaUtility
#' boot
#' 
#' @references
#' Mathur MB & VanderWeele TJ (2020a). Sensitivity analysis for unmeasured confounding in meta-analyses. \emph{Journal of the American Statistical Association}.
#' 
#' Mathur MB & VanderWeele TJ (2020b). Robust metrics and sensitivity analyses for meta-analyses of heterogeneous effects. \emph{Epidemiology}.
#' 
#' Mathur MB & VanderWeele TJ (2019). New statistical metrics for meta-analyses of heterogeneous effects. \emph{Statistics in Medicine}.
#'
#' Ding P & VanderWeele TJ (2016). Sensitivity analysis without assumptions. \emph{Epidemiology}.
#' 
#' VanderWeele TJ & Ding P (2017). Introducing the E-value. \emph{Annals of Internal Medicine}.
#'
#' Wang C-C & Lee W-C (2019). A simple method to estimate prediction intervals and
#' predictive distributions: Summarizing meta-analyses
#' beyond means and confidence intervals. \emph{Research Synthesis Methods}.
#' @examples
#' 
#' ##### Using Calibrated Method #####
#' d = metafor::escalc(measure="RR", ai=tpos, bi=tneg,
#'                     ci=cpos, di=cneg, data=metafor::dat.bcg)
#' 
#' # obtaining all three estimators and inference
#' # number of bootstrap iterates
#' # should be larger in practice
#' R = 100
#' confounded_meta( method="calibrated",  # for both methods
#'                  q = log(0.90),
#'                  r = 0.20,
#'                  tail="below",
#'                  muB = log(1.5),
#'                  dat = d,
#'                  yi.name = "yi",
#'                  vi.name = "vi",
#'                  R = 100 )
#' 
#' # passing only arguments needed for prop point estimate
#' confounded_meta( method="calibrated",
#'                  q = log(0.90),
#'                  tail="below",
#'                  muB = log(1.5),
#'                  give.CI = FALSE,
#'                  dat = d,
#'                  yi.name = "yi",
#'                  vi.name = "vi" )
#' 
#' # passing only arguments needed for Tmin, Gmin point estimates
#' confounded_meta( method="calibrated",
#'                  q = log(0.90),
#'                  r = 0.10,
#'                  tail="below",
#'                  give.CI = FALSE,
#'                  dat = d,
#'                  yi.name = "yi",
#'                  vi.name = "vi" )
#' 
#' ##### Using Parametric Method #####
#' # fit random-effects meta-analysis
#' m = metafor::rma.uni(yi= d$yi,
#'                      vi=d$vi,
#'                      knha=TRUE,
#'                      measure="RR",
#'                      method="REML" )
#' 
#' yr = as.numeric(m$b)  # metafor returns on log scale
#' vyr = as.numeric(m$vb)
#' t2 = m$tau2
#' vt2 = m$se.tau2^2
#' 
#' # obtaining all three estimators and inference
#' # now the proportion considers heterogeneous bias
#' confounded_meta( method = "parametric",
#'                  q=log(0.90),
#'                  r=0.20,
#'                  tail = "below",
#'                  muB=log(1.5),
#'                  sigB=0.1,
#'                  yr=yr,
#'                  vyr=vyr,
#'                  t2=t2,
#'                  vt2=vt2,
#'                  CI.level=0.95 )
#' 
#' # passing only arguments needed for prop point estimate
#' confounded_meta( method = "parametric",
#'                  q=log(0.90),
#'                  tail = "below",
#'                  muB=log(1.5),
#'                  sigB = 0,
#'                  yr=yr,
#'                  t2=t2,
#'                  CI.level=0.95 )
#' 
#' # passing only arguments needed for Tmin, Gmin point estimates
#' confounded_meta( method = "parametric",
#'                  q=log(0.90),
#'                  r = 0.10,
#'                  tail = "below",
#'                  yr=yr,
#'                  t2=t2,
#'                  CI.level=0.95 )


confounded_meta = function( method="calibrated",  # for both methods
                            q,
                            r = NA,
                            tail = NA,
                            CI.level = 0.95,
                            give.CI = TRUE,
                            R = 1000,
                            
                            muB = NA,
                            muB.toward.null = FALSE,
                            
                            # only for calibrated
                            dat = NA,
                            yi.name = NA,
                            vi.name = NA,
                            
                            # only for parametric
                            sigB = NA,
                            yr = NA,
                            vyr = NA,
                            t2 = NA,
                            vt2 = NA,
                            
                            ...
                            
) {
    
    
    # # test only
    # method="calibrated"
    # q=median(d$calib)
    # tail = "above"
    # muB=0
    # r=0.1
    # q = 0.2
    # R = 250
    # CI.level = 0.95
    # 
    # give.CI=TRUE
    # dat = d
    # yi.name = "yi"
    # vi.name = "vyi"
    
    ##### Look for Additional Passed Arguments #####
    dots = list(...)
    
    # should warnings be simplified for use with the Shiny website?
    if ( "simplifyWarnings" %in% names(dots) ){
        simplifyWarnings =  dots$simplifyWarnings
    } else {
        simplifyWarnings = FALSE
    }
    
    
    ##### Check for Bad or Incomplete Input - Common to Parametric and Calibrated Methods #####
    if ( ! is.na(r) ) {
        if (r < 0 | r > 1) stop("r must be between 0 and 1")
    }
    
    if ( is.na(r) ){
        if ( simplifyWarnings == FALSE ) message("Cannot compute Tmin or Gmin without r. Returning only prop.")
        if ( simplifyWarnings == TRUE ) message("Cannot compute bias or confounding strength required to shift to r unless you provide r. Returning only the proportion.")
    }
    
    if ( !is.na(muB) & muB < 0 ) {
        if ( simplifyWarnings == FALSE ) stop("Must have muB > 0. Use the muB.toward.null argument instead if you want to consider bias away from the null. See Details.")
        
        if ( simplifyWarnings == TRUE ) stop("Must have muB > 0.")
        
    }
    
    
    ##### PARAMETRIC #####
    if (method=="parametric"){
        
        
        ##### Check for Bad Input #####
        if ( t2 < 0 ) stop("Heterogeneity cannot be negative")
        #if ( is.na(sigB) ) stop("Must provide sigB for parametric method")
        
        
        # the second condition is needed for Shiny app:
        #  if user deletes the input in box, then it's NA instead of NULL
        if ( !is.na(vyr) ) {
            if (vyr < 0) stop("Variance of point estimate cannot be negative")
        }
        
        if ( !is.na(vt2) ) {
            if (vt2 < 0) stop("Variance of heterogeneity cannot be negative")
        }
        
        if ( !is.na(sigB) ) {
            if ( t2 <= sigB^2 ) {
                if (simplifyWarnings == FALSE) stop("Must have t2 > sigB^2")
                
                # on the website, this can only happen if they set t2=0 but proportion due to confounding > 0
                if (simplifyWarnings == TRUE) stop("Cannot have a nonzero proportion of heterogeneity due to variation in confounding bias when there is no heterogeneity to begin with")
            }
            if ( sigB < 0 ) stop("Bias factor standard deviation cannot be negative")
        }
        
        
        
        ##### Messages When Not All Output Can Be Computed #####
        if ( is.na(vyr) | is.na(vt2) ) message("Cannot compute inference without variances of point estimate and of heterogeneity estimate. Returning only point estimates.")
        
        ##### Point Estimates: Causative Case #####
        # if tail isn't provided, assume user wants the more extreme one (away from the null)
        if ( is.na(tail) ) {
            tail = ifelse( yr > 0, "above", "below" )
            warning( paste( "Assuming you want tail =", tail, "because it wasn't specified") )
        }
        
        # bias-corrected mean
        # usual case: bias that went away from null, so correction shifts toward null
        if ( muB.toward.null == FALSE & yr > 0 ) yr.corr = yr - muB
        if ( muB.toward.null == FALSE & yr < 0 ) yr.corr = yr + muB
        # less-usual case: bias that went toward null, so correction shifts away from null
        if ( muB.toward.null == TRUE & yr > 0 ) yr.corr = yr + muB
        if ( muB.toward.null == TRUE & yr < 0 ) yr.corr = yr - muB
        
        
        if ( tail == "above" ) {
            
            # point estimate for Phat
            if ( !is.na(muB) & !is.na(sigB) ) {
                # point estimate for Phat
                Z = ( q - yr.corr ) / sqrt( t2 - sigB^2 )
                Phat = 1 - pnorm(Z) 
            } else {
                Phat = NA
            }
            
            # point estimates for Tmin, Gmin
            if ( !is.na(r) ) {
                
                # first check if any shifting is actually needed
                # current Phat with no bias
                Phat.naive = 1 - pnorm( (q - yr) / sqrt(t2) )
                
                if ( Phat.naive <= r ) {
                    Tmin = 1
                } else {
                    # min bias factor
                    # the max is there in case no bias is needed
                    # (i.e., the bias would be going in the other direction)
                    # (i.e., proportion of effects > q already < r without confounding)
                    Tmin = max( 1, exp( qnorm(1-r) * sqrt(t2) - q + yr ) )
                    
                    # alternative way of handling this issue:
                    # Tmin could be less than 1 if yr has to be shifted POSITIVELY
                    #  rather than negatively to achieve r
                    #  e.g., yr^c = log(0.5), q = log(1s.5), r = 0.75
                    # for consistency with calibrated output, take Tmin's inverse so it's always positive
                    #if ( Tmin < 1 ) Tmin = 1 / Tmin
                }
                
                # min confounding strength
                # suppress warnings to avoid warnings about NaN when term inside sqrt is negative
                Gmin = suppressWarnings( Tmin + sqrt( Tmin^2 - Tmin ) )
            }
            
            if ( is.na(r) ) {
                Tmin = Gmin = NA
            }
        } # end tail = "above"
        
        ##### Point Estimates: Preventive Case #####
        if ( tail == "below" ) {
            
            # point estimate for Phat
            if ( !is.na(muB) & !is.na(sigB) ) {
                Z = ( q - yr.corr ) / sqrt( t2 - sigB^2 )
                Phat = pnorm(Z) 
            } else {
                Phat = NA
            }
            
            # point estimates for Tmin, Gmin
            if ( !is.na(r) ) {
                
                # first check if any shifting is actually needed
                # current Phat with no bias
                Phat.naive = pnorm( (q - yr) / sqrt(t2) )
                
                if ( Phat.naive <= r ) {
                    Tmin = 1
                } else {
                    # the max is there in case no bias is needed
                    Tmin = max( 1, exp( q - yr - qnorm(r) * sqrt(t2) ) )
                    
                    # alternative way of handling this issue:
                    # # Tmin could be less than 1 if yr has to be shifted NEGATIVELY
                    # #  rather than positively to achieve r
                    # #  e.g., yr^c = log(1.5), q = log(0.5), r = 0.75
                    # # for consistency with calibrated output, take Tmin's inverse so it's always positive
                    # if ( Tmin < 1 ) Tmin = 1 / Tmin
                }
                
                # min confounding strength
                Gmin = suppressWarnings( Tmin + sqrt( Tmin^2 - Tmin ) )
            }
            
            if ( is.na(r) ) {
                Tmin = Gmin = NA
            }
            
        } # end tail = "below"
        
        ##### Delta Method Inference: P-Hat #####
        # do inference only if given needed SEs
        if ( !is.na(vyr) & !is.na(vt2) & !is.na(muB) & !is.na(sigB) ){
            
            # term in numerator depends tail
            num.term = ifelse( tail == "above", q + muB - yr, q - muB - yr )
            
            term1.1 = vyr / (t2 - sigB^2 )
            term1.2 = ( vt2 * (num.term)^2 ) / ( 4 * (t2 - sigB^2 )^3 )
            term1 = sqrt( term1.1 + term1.2 )
            
            Z = num.term / sqrt( t2 - sigB^2 )
            SE.Phat = term1 * dnorm(Z)
            
            # confidence interval
            tail.prob = ( 1 - CI.level ) / 2
            lo.Phat = max( 0, Phat + qnorm( tail.prob )*SE.Phat )
            hi.Phat = min( 1, Phat - qnorm( tail.prob )*SE.Phat )
            
            
            # warn if bootstrapping needed
            if ( Phat < 0.15 | Phat > 0.85 ) {
                if ( simplifyWarnings == FALSE) warning('Prop is close to 0 or 1. We recommend choosing method = \"calibrated\" or alternatively using bias-corrected and accelerated bootstrapping to estimate all inference in this case.')
                
                # no warning for the simplifyWarnings == TRUE case because website has its own version 
            }
            
        } else {
            SE.Phat = lo.Phat = hi.Phat = NA
        }
        
        ##### Delta Method Inference: Tmin and Gmin #####
        # do inference only if given needed SEs and r
        # last condition: if Tmin has been set to 1, give NAs for inference
        if ( !is.na(vyr) & !is.na(vt2) & !is.na(r) & Tmin != 1 ){
            
            ##### Tmin #####
            if ( tail == "above" ) {
                
                term = ( vt2 * qnorm(1-r)^2 ) / ( 4 * t2 )
                SE.T = exp( sqrt(t2) * qnorm(1-r) - q + yr ) * sqrt( vyr + term  )
                
            } else {
                term = ( vt2 * qnorm(r)^2 ) / ( 4 * t2 )
                SE.T = exp( q - yr - sqrt(t2) * qnorm(r) ) * sqrt( vyr + term  )
            }
            
            tail.prob = ( 1 - CI.level ) / 2
            lo.T = max( 1, Tmin + qnorm( tail.prob )*SE.T )  # bias factor can't be < 1
            hi.T = Tmin - qnorm( tail.prob )*SE.T  # but has no upper bound
            
            
            ##### Gmin #####
            SE.G = SE.T * ( 1 + ( 2*Tmin - 1 ) / ( 2 * sqrt( Tmin^2 - Tmin ) ) )
            
            lo.G = max( 1, Gmin + qnorm( tail.prob )*SE.G )  # confounding RR can't be < 1
            hi.G = Gmin - qnorm( tail.prob )*SE.G  # but has no upper bound
            
        } else {  # i.e., user didn't pass parameters needed for inference, or else Tmin = 1
            SE.T = SE.G = lo.T = lo.G = hi.T = hi.G = NA
        }
        
        
    } # closes parametric method
    
    ##### CALIBRATED #####
    if( method == "calibrated" ){
        
        # no need to catch bad input for this method
        
        # if tail isn't provided, assume user wants the more extreme one (away from the null)
        if ( is.na(tail) ) {
            calib = calib_ests( yi = dat[[yi.name]], 
                                sei = sqrt( dat[[vi.name]] ) )
            
            tail = ifelse( median(calib) > 0, "above", "below" )
            warning( paste( "Assuming you want tail =", tail, "because it wasn't specified") )
        }
        
        
        # initialize
        Phat = Tmin = Gmin = SE.Phat = SE.T = SE.G = lo.Phat = lo.T = lo.G = hi.Phat = hi.T = hi.G = NA
        
        
        ##### All Three Point Estimates #####
        Phat = Phat_causal( q = q, 
                            B = muB,
                            tail = tail,
                            muB.toward.null = muB.toward.null,
                            dat = dat,
                            yi.name = yi.name,
                            vi.name = vi.name )
        
        if ( !is.na(r) ) {
            Tmin = Tmin_causal(q = q,
                               r = r,
                               tail = tail,
                               dat = dat,
                               yi.name = yi.name,
                               vi.name = vi.name)
            
            
            Gmin = g(Tmin)
        }
        
        
        
        ##### All Three Confidence Intervals #####
        if ( give.CI == TRUE ) {
            
            # check for needed input
            # use length(dat) instead of is.na(dat) because latter will 
            if ( all(is.na(dat)) | is.na(yi.name) | is.na(vi.name) ) {
                stop("Must provide dat, yi.name, and vi.name to calculate confidence intervals with calibrated method")
            }
            
            Phat.CI.lims = Phat_CI_lims(.B = muB,
                                        R = R,
                                        q = q,
                                        tail = tail,
                                        dat = dat,
                                        muB.toward.null = muB.toward.null,
                                        yi.name = yi.name,
                                        vi.name = vi.name,
                                        CI.level = CI.level)
            
            lo.Phat = as.numeric( Phat.CI.lims[1] )
            hi.Phat = as.numeric( Phat.CI.lims[2] )
            SE.Phat = as.numeric( Phat.CI.lims[3] )
            
            if ( any( is.na( c(lo.Phat, hi.Phat, SE.Phat) ) ) ) {
                message("The confidence interval and/or standard error for the proportion were not estimable via bias-corrected and accelerated bootstrapping. You can try increasing the number of bootstrap iterates or choosing a less extreme threshold.")
            }
            
            Tmin.Gmin.CI.lims = Tmin_Gmin_CI_lims( R,
                                                   q,
                                                   r,
                                                   tail,
                                                   dat,
                                                   yi.name,
                                                   vi.name,
                                                   CI.level )
            
            lo.T = as.numeric( Tmin.Gmin.CI.lims["lo.T"] )
            hi.T = as.numeric( Tmin.Gmin.CI.lims["hi.T"] )
            SE.T = as.numeric( Tmin.Gmin.CI.lims["SE.T"] )
            lo.G = as.numeric( Tmin.Gmin.CI.lims["lo.G"] )
            hi.G = as.numeric( Tmin.Gmin.CI.lims["hi.G"] )
            SE.G = as.numeric( Tmin.Gmin.CI.lims["SE.G"] )
            
            # last condition is because we don't actually do bootstrapping if Tmin = 1
            if ( any( is.na( c(lo.T, hi.T, SE.T, lo.G, hi.G, SE.G) ) ) & ( !is.na(Tmin) & Tmin != 1 ) ) {
                message("The confidence interval and/or standard error for Tmin and Gmin were not estimable via bias-corrected and accelerated bootstrapping. You can try increasing the number of bootstrap iterates or choosing a less extreme threshold.")
            }
            
        }  # closes "if ( !is.na(r) )"
        
    } # closes calibrated method
    
    ##### Messages about Results #####
    if ( exists("Tmin") ) {
        if ( !is.na(Tmin) & Tmin == 1 ) {
            if (simplifyWarnings == FALSE) message("Prop is already less than or equal to r even with no confounding, so Tmin and Gmin are simply equal to 1. No confounding at all is required to make the specified shift.")
            
            # no warning for website because it produces its own
            #if (simplifyWarnings == TRUE) message("The proportion is already less than or equal to r even with no confounding, so the amount of bias and of confounding strength required to make the specified shift are simply equal to 1. No confounding at all is required to make the specified shift.")
        }
        
        if ( !is.na(Tmin) & muB.toward.null == TRUE ) {
            message("You chose to consider bias that has on average shifted studies' estimates toward the null, rather than away from the null. This specification was applied when estimating Prop. However, because Tmin and Gmin by definition consider the amount of bias required to reduce to less than r the proportion of studies with true causal effect sizes more extreme than q, that bias may be toward or away from the null as required to make the shift.")
        }
    }  
    
    
    ##### Return Results #####
    return( data.frame( Value = c("Prop", "Tmin", "Gmin"), 
                        Est = c( Phat, Tmin, Gmin ),
                        SE = c(SE.Phat, SE.T, SE.G),
                        CI.lo = c(lo.Phat, lo.T, lo.G), 
                        CI.hi = c(hi.Phat, hi.T, hi.G) ) )
    
} # closes confounded_meta function








#' Plots for sensitivity analyses
#'
#' Produces line plots (\code{type="line"}) showing the average bias factor across studies on the relative risk (RR) scale vs. the estimated proportion
#' of studies with true RRs above or below a chosen threshold \code{q}.
#' The plot secondarily includes a X-axis showing the minimum strength of confounding
#' to produce the given bias factor. The shaded region represents a pointwise confidence band.
#' Alternatively, produces distribution plots (\code{type="dist"}) for a specific bias factor showing the observed and 
#' true distributions of RRs with a red line marking exp(\code{q}).
#' @param method \code{"calibrated"} or \code{"parametric"}. See Details.
#' @param type \code{dist} for distribution plot; \code{line} for line plot (see Details)
#' @param q True causal effect size chosen as the threshold for a meaningfully large effect
#' @param CI.level Pointwise confidence level as a proportion (e.g., 0.95).
#' @param tail \code{"above"} for the proportion of effects above \code{q}; \code{"below"} for
#' the proportion of effects below \code{q}. By default, is set to \code{"above"} if the pooled point estimate (\code{method = "parametric"}) or median of the calibrated estimates (\code{method = "calibrated"}) is above 1 on the relative risk scale and is set to \code{"below"} otherwise.
#' @param muB.toward.null Whether you want to consider bias that has on average shifted studies' point estimates away from the null (\code{FALSE}; the default) or that has on average shifted studies' point estimates toward the null (\code{TRUE}). See Details.
#' 
#' @param give.CI Logical. If \code{TRUE}, a pointwise confidence intervals is plotted. 
#' @param Bmin Lower limit of lower X-axis on the log scale (only needed if \code{type = "line"}). 
#' @param Bmax Upper limit of lower X-axis on the log scale (only needed if \code{type = "line"})
#' @param breaks.x1 Breaks for lower X-axis (bias factor) on RR scale. (optional for \code{type = "line"}; not used for \code{type = "dist"}). 
#' @param breaks.x2 Breaks for upper X-axis (confounding strength) on RR scale (optional for \code{type = "line"}; not used for \code{type = "dist"})
#' 
#' 
#' @param muB Single mean bias factor on log scale (only needed if \code{type = "dist"})

#' @param sigB Standard deviation of log bias factor across studies (only used if \code{method = "parametric"})
#' @param yr Pooled point estimate (on log scale) from confounded meta-analysis (only used if \code{method = "parametric"})
#' @param vyr Estimated variance of pooled point estimate from confounded meta-analysis (only used if \code{method = "parametric"})
#' @param t2 Estimated heterogeneity (\eqn{\tau^2}) from confounded meta-analysis (only used if \code{method = "parametric"})
#' @param vt2 Estimated variance of \eqn{\tau^2} from confounded meta-analysis (only used if \code{method = "parametric"})

#' @param R  Number  of  bootstrap  iterates for confidence interval estimation. Only used if \code{method = "calibrated"} and \code{give.CI = TRUE}. 
#' @param dat Dataframe containing studies' point estimates and variances. Only used if \code{method = "calibrated"}.
#' @param yi.name Name of variable in \code{dat} containing studies' point estimates. Only used if \code{method = "calibrated"}.
#' @param vi.name Name of variable in \code{dat} containing studies' variance estimates. Only used if \code{method = "calibrated"}.
#'
#' @keywords meta-analysis confounding sensitivity
#' @details
#' This function calls \code{confounded_meta} to get the point estimate and confidence interval at each value of the bias factor. See \code{?confounded_meta} for details. 
#' 
#' Note that \code{Bmin} and \code{Bmax} are specified on the log scale for consistency with the \code{muB} argument and with the function \code{confounded_meta}, whereas \code{breaks.x1} and \code{breaks.x2} are specified on the relative risk scale to facilitate adjustments to the plot appearance. 
#' @export
#' @import
#' ggplot2
#' @importFrom dplyr %>% rowwise mutate rename
#' @references
#' Mathur MB & VanderWeele TJ (2020a). Sensitivity analysis for unmeasured confounding in meta-analyses. \emph{Journal of the American Statistical Association}.
#' 
#' Mathur MB & VanderWeele TJ (2020b). Robust metrics and sensitivity analyses for meta-analyses of heterogeneous effects. \emph{Epidemiology}.
#'
#' Wang C-C & Lee W-C (2019). A simple method to estimate prediction intervals and
#' predictive distributions: Summarizing meta-analyses
#' beyond means and confidence intervals. \emph{Research Synthesis Methods}.
#' @examples
#' 
#' ##### Example 1: Calibrated Line Plots #####
#' 
#' # simulated dataset with exponentially distributed 
#' #  population effects
#' # we will use the calibrated method to avoid normality assumption
#' data(toyMeta)
#' 
#' # without confidence band
#' sens_plot( method = "calibrated",
#'            type="line",
#'            q=log(.9),
#'            tail = "below",
#'            dat = toyMeta,
#'            yi.name = "est",
#'            vi.name = "var",
#'            give.CI = FALSE )
#' 
#' 
#' # # with confidence band and a different threshold, q
#' # # commented out because takes a while too run
#' # sens_plot( method = "calibrated",
#' #            type="line",
#' #            q=0,
#' #            tail = "below",
#' #            dat = toyMeta,
#' #            yi.name = "est",
#' #            vi.name = "var",
#' #            give.CI = TRUE,
#' #            R = 300 ) # should be higher in practice
#' 
#' 
#' ##### Example 2: Calibrated and Parametric Line Plots #####
#' 
#' # example dataset
#' d = metafor::escalc(measure="RR",
#'                     ai=tpos,
#'                     bi=tneg,
#'                     ci=cpos,
#'                     di=cneg,
#'                     data=metafor::dat.bcg)
#' 
#' # without confidence band
#' sens_plot( method = "calibrated",
#'            type="line",
#'            tail = "below",
#'            q=log(1.1),
#'            dat = d,
#'            yi.name = "yi",
#'            vi.name = "vi",
#'            give.CI = FALSE )
#' 
#' # # with confidence band
#' # # commented out because  it takes a while
#' # # this example gives bootstrap warnings because of its small sample size
#' # sens_plot( method = "calibrated",
#' #            type="line",
#' #            q=log(1.1),
#' #            R = 500,  # should be higher in practice (e.g., 1000)
#' #            dat = d,
#' #            yi.name = "yi",
#' #            vi.name = "vi",
#' #            give.CI = TRUE )
#' 
#' 
#' # now with heterogeneous bias across studies (via sigB) and with confidence band
#' sens_plot( method = "parametric",
#'            type="line",
#'            q=log(1.1),
#'            yr=log(1.3),
#'            vyr = .05,
#'            vt2 = .001,
#'            t2=0.4,
#'            sigB = 0.1,
#'            Bmin=0,
#'            Bmax=log(4) )
#' 
#' ##### Distribution Line Plot #####
#' 
#' # distribution plot: apparently causative
#' sens_plot( type="dist",
#'            q=log(1.1),
#'            muB=log(2),
#'            sigB = 0.1,
#'            yr=log(1.3),
#'            t2=0.4 )
#' 
#' # distribution plot: apparently preventive
#' sens_plot( type="dist",
#'            q=log(0.90),
#'            muB=log(1.5),
#'            sigB = 0.1,
#'            yr=log(0.7),
#'            t2=0.2 )

sens_plot = function(method="calibrated",
                     type,
                     q,
                     CI.level=0.95,
                     tail=NA,
                     muB.toward.null = FALSE,
                     give.CI=TRUE,
                     Bmin = 0,
                     Bmax = log(4),
                     breaks.x1=NA,
                     breaks.x2=NA,
                     
                     # for plot type "dist"
                     muB,
                     
                     # for type "line" and method "parametric"
                     sigB,
                     yr,
                     vyr=NA,
                     t2,
                     vt2=NA,
                     
                     
                     # for type "line" and method "calibrated"
                     R=1000,
                     dat = NA,
                     yi.name = NA,
                     vi.name = NA) {
    
    # # test only
    # method="calibrated"
    # type = "line"
    # q=median(d$calib)
    # tail = "above"
    # muB=0
    # r=0.1
    # q = 0.2
    # R = 250
    # CI.level = 0.95
    # 
    # give.CI=TRUE
    # dat = d
    # yi.name = "yi"
    # vi.name = "vi"
    # Bmin = 0
    # Bmax = log(5)
    # CI.level = 0.95
    # tail = "above"
    # breaks.x1 = NA
    # breaks.x2 = NA
    
    # method = "parametric"
    # type = "line"
    # q = log(1.1)
    # muB = log(2)
    # sigB = 0.1
    # yr = log(1.4)
    # vyr = 0.5
    # t2 = 0.3
    # vt2 = 0.02
    # r = 0.1
    # Bmin = 0
    # Bmax = log(5)
    # CI.level = 0.95
    # tail = "above"
    # breaks.x1 = NA
    # breaks.x2 = NA
    
    val = group = eB = phat = lo = hi = B = B.x = Phat = NULL
    
    ##### Distribution Plot ######
    if ( type=="dist" ) {
        
        # check for bad input
        if( is.na(muB) ) stop("For type='dist', must provide muB")
        
        if ( ( length(muB) > 1 ) | ( length(sigB) > 1 ) ) {
            stop( "For type='dist', muB and sigB must be length 1")
        }
        
        # simulate confounded distribution
        reps = 10000
        RR.c = exp( rnorm( n=reps, mean=yr, sd=sqrt(t2) ) )
        
        # simulate unconfounded distribution
        Mt = ifelse( yr > 0, yr - muB, yr + muB )
        RR.t = exp( rnorm( n=reps, mean=Mt, sd=sqrt(t2-sigB^2) ) )
        
        # get reasonable limits for X-axis
        x.min = min( quantile(RR.c, 0.01), quantile(RR.t, 0.01) )
        x.max = max( quantile(RR.c, 0.99), quantile(RR.t, 0.99) )
        
        temp = data.frame( group = rep( c( "Observed", "True" ), each = reps ), 
                           val = c( RR.c, RR.t ) )
        
        # cut the dataframe to match axis limits
        # avoids warnings from stat_density about non-finite values being removed
        temp = temp[ temp$val >= x.min & temp$val <= x.max, ]
        
        colors=c("black", "orange")
        
        p = ggplot2::ggplot( data = temp, aes(x=val, group=group ) ) +
            
            geom_density( aes( x=val, fill=group ), alpha=0.4 ) +
            theme_bw() +
            xlab("Study-specific relative risks") +
            ylab("") +
            guides(fill=guide_legend(title=" ")) +
            scale_fill_manual(values=colors) +
            geom_vline( xintercept = exp(q), lty=2, color="red" ) +
            scale_x_continuous( limits=c(x.min, x.max), breaks = seq( round(x.min), round(x.max), 0.5) ) +
            ggtitle("Observed and true relative risk distributions") 
        
        graphics::plot(p)
    }
    
    ##### Line Plot ######
    if ( type=="line" ) {
        
        
        # compute axis tick points for both X-axes
        if ( any( is.na(breaks.x1) ) ) breaks.x1 = seq( exp(Bmin), exp(Bmax), .5 )
        if ( any( is.na(breaks.x2) ) ) breaks.x2 = round( breaks.x1 + sqrt( breaks.x1^2 - breaks.x1 ), 2)
        
        
        if ( method=="parametric" ) {
            
            
            
            if ( is.na(tail) ) {
                tail = ifelse( yr > 0, "above", "below" )
                warning( paste( "Assuming you want tail =", tail, "because it wasn't specified") )
            }
            
            if ( is.na(vyr) | is.na(vt2) ) {
                message( "No confidence interval because vyr or vt2 is NULL")
            }
            
            # get mean bias factor values for a vector of B's from Bmin to Bmax
            t = data.frame( B = seq(Bmin, Bmax, .01), phat = NA, lo = NA, hi = NA )
            t$eB = exp(t$B)
            
            for ( i in 1:dim(t)[1] ) {
                # r is irrelevant here
                # suppress warnings about Phat being close to 0 or 1
                cm = suppressWarnings( suppressMessages( confounded_meta( method = method,
                                                                          q = q,
                                                                          r = NA,
                                                                          muB=t$B[i],
                                                                          sigB=sigB,
                                                                          yr=yr,
                                                                          vyr=vyr,
                                                                          t2=t2,
                                                                          vt2=vt2,
                                                                          CI.level=CI.level,
                                                                          tail=tail,
                                                                          muB.toward.null = muB.toward.null) ) )
                
                t$phat[i] = cm$Est[ cm$Value=="Prop" ]
                t$lo[i] = cm$CI.lo[ cm$Value=="Prop" ]
                t$hi[i] = cm$CI.hi[ cm$Value=="Prop" ]
            }
            
            
            p = ggplot2::ggplot( t, aes(x=eB,
                                        y=phat ) ) +
                theme_bw() +
                
                scale_y_continuous( limits=c(0,1),
                                    breaks=seq(0, 1, .1)) +
                
                scale_x_continuous(  breaks = breaks.x1,
                                     sec.axis = sec_axis( ~ g(.),  # confounding strength axis
                                                          name = "Minimum strength of both confounding RRs",
                                                          breaks = breaks.x2) ) +
                
                geom_line(lwd=1.2) +
                xlab("Hypothetical average bias factor across studies (RR scale)") +
                ylab( paste( ifelse( tail=="above",
                                     paste( "Estimated proportion of studies with true RR >", round( exp(q), 3 ) ),
                                     paste( "Estimated proportion of studies with true RR <", round( exp(q), 3 ) ) ) ) )
            
            # can't compute a CI if the bounds aren't there
            no.CI = any( is.na(t$lo) ) | any( is.na(t$hi) ) | (give.CI == FALSE)
            
            if ( no.CI ){
                graphics::plot(p)
            } else {
                graphics::plot( p + ggplot2::geom_ribbon( aes(ymin=lo, ymax=hi), alpha=0.15 ) )
                
                warning("Calculating parametric confidence intervals in the plot. For values of the proportion that are less than 0.15 or greater than 0.85, these confidence intervals may not perform well.")
            }
            
            
        } ## closes method=="parametric"
        
        
        if ( method == "calibrated" ) {
            
            # if tail isn't provided, assume user wants the more extreme one (away from the null)
            if ( is.na(tail) ) {
                calib = calib_ests( yi = dat[[yi.name]], 
                                    sei = sqrt( dat[[vi.name]] ) )
                
                tail = ifelse( median(calib) > 0, "above", "below" )
                warning( paste( "Assuming you want tail =", tail, "because it wasn't specified") )
            }
            
            res = data.frame( B = seq(Bmin, Bmax, .01) )
            
            # evaluate Phat causal at each value of B
            res = res %>% rowwise() %>%
                mutate( Phat = Phat_causal( q = q, 
                                            B = B,
                                            tail = tail,
                                            muB.toward.null = muB.toward.null,
                                            dat = dat,
                                            yi.name = yi.name,
                                            vi.name = vi.name ) ) 
            
            if ( give.CI == TRUE ) {
                # look at just the values of B at which Phat jumpss
                #  this will not exceed the number of point estimates in the meta-analysis
                # first entry should definitely be bootstrapped, so artificially set its diff to nonzero value
                diffs = c( 1, diff(res$Phat) )  
                res.short = res[ diffs != 0, ]
                
                
                # bootstrap a CI for each entry in res.short
                res.short = res.short %>% rowwise() %>%
                    mutate( Phat_CI_lims(.B = B,
                                         R = R,
                                         q = q,
                                         tail = tail,
                                         muB.toward.null = muB.toward.null,
                                         dat = dat,
                                         yi.name = yi.name,
                                         vi.name = vi.name,
                                         CI.level = CI.level)[1:2] )
                
                # merge this with the full-length res dataframe, merging by Phat itself
                res = merge( res, res.short, by = "Phat", all.x = TRUE )
                
                res = res %>% rename( B = B.x )
                
                
                ##### Warnings About Missing CIs Due to Boot Failures #####
                # if ALL CI limits are missing
                if ( all( is.na(res$lo) ) ) {
                    message( "None of the pointwise confidence intervals was estimable via bias-corrected and accelerated bootstrapping, so the confidence band on the plot is omitted. You can try increasing the number of bootstrap iterates or choosing a less extreme threshold." )
                    # avoid even trying to plot the CI if it's always NA to avoid geom_ribbon errors later
                    give.CI = FALSE
                }
                
                
                # outer "if" handles case in which AT LEAST ONE CI limit is NA because of boot failures
                if ( any( !is.na(res$lo) ) & any( !is.na(res$hi) ) ) {
                    
                    message( "Some of the pointwise confidence intervals were not estimable via bias-corrected and accelerated bootstrapping, so the confidence band on the plot may not be shown for some values of the bias factor. This usually happens at values with a proportion estimate close to 0 or 1. You can try increasing the number of bootstrap iterates or choosing a less extreme threshold." )
                    
                    if ( any( res$lo[ !is.na(res$lo) ] > res$Phat[ !is.na(res$lo) ] ) | any( res$hi[ !is.na(res$lo) ] < res$Phat[ !is.na(res$lo) ] ) ) {
                        
                        message( "Some of the pointwise confidence intervals do not contain the proportion estimate itself. This reflects instability in the bootstrapping process. See the other warnings for details." )
                        
                    }
                }
                
                
            }
            
            p = ggplot2::ggplot( data = res,
                                 aes( x = exp(B),
                                      y = Phat ) ) +
                theme_bw() +
                
                
                scale_y_continuous( limits=c(0,1),
                                    breaks=seq(0, 1, .1)) +
                scale_x_continuous(  #limits = c( min(breaks.x1), max(breaks.x1) ),  # this line causes an error with geom_line having "missing values"
                    breaks = breaks.x1,
                    sec.axis = sec_axis( ~ g(.),  # confounding strength axis
                                         name = "Minimum strength of both confounding RRs",
                                         breaks = breaks.x2)
                ) +
                geom_line(lwd=1.2) +
                
                xlab("Hypothetical bias factor in all studies (RR scale)") +
                ylab( paste( ifelse( tail=="above",
                                     paste( "Estimated proportion of studies with true RR >", round( exp(q), 3 ) ),
                                     paste( "Estimated proportion of studies with true RR <", round( exp(q), 3 ) ) ) ) )
            
            
            
            if ( give.CI == TRUE ) {
                p = p + geom_ribbon( aes(ymin=lo, ymax=hi), alpha=0.15, fill = "black" )
            }
            
            graphics::plot(p)
        }  # closes method == "calibrated"
        
    } ## closes type=="line"
} ## closes sens_plot function

############################ INTERNAL FUNCTIONS ############################ 

#' Proportion of studies with causal effects above or below q
#'
#' An internal function that estimates the proportion of studies with true effect sizes above or below \code{q} given the bias factor \code{B}. Users should call \code{confounded_meta} instead.
#' @import
#' boot 
#' @noRd
Phat_causal = function( q,
                        B,
                        tail,
                        muB.toward.null,
                        dat,
                        yi.name,
                        vi.name) {
    
    if ( ! yi.name %in% names(dat) ) stop("dat does not contain a column named yi.name")
    if ( ! vi.name %in% names(dat) ) stop("dat does not contain a column named vi.name")
    
    calib = MetaUtility::calib_ests( yi = dat[[yi.name]],
                                     sei = sqrt(dat[[vi.name]] ) )
    # confounding-adjusted calibrated estimates
    # bias that went away from null, so correction goes toward null
    if ( median(calib) > 0 & muB.toward.null == FALSE ) calib.t = calib - B
    if ( median(calib) < 0 & muB.toward.null == FALSE ) calib.t = calib + B
    # bias that went toward null, so correction goes away from null
    if ( median(calib) > 0 & muB.toward.null == TRUE ) calib.t = calib + B
    if ( median(calib) < 0 & muB.toward.null == TRUE ) calib.t = calib - B
    
    # confounding-adjusted Phat
    if ( tail == "above" ) Phat.t = mean( calib.t > q )
    if ( tail == "below" ) Phat.t = mean( calib.t < q )
    
    return(Phat.t)
}



#' Transformation from bias factor to confounding strength scale
#'
#' An internal function. 
#' @noRd
g = Vectorize( function(x) {
    # define transformation in a way that is monotonic over the effective range of B (>1)
    # to avoid ggplot errors in sens_plot
    # helper function for confounded_meta
    if ( is.na(x) ) return(NA)
    if (x < 1) return( x / 1e10 )
    x + sqrt( x^2 - x )
} )



#' Minimum common bias factor to reduce proportion of studies with causal effects above or below q t less than r
#'
#' An internal function that estimates; users should call \code{confounded_meta} instead.
#' @import
#' MetaUtility 
#' @noRd
Tmin_causal = function( q,
                        r,
                        tail,
                        
                        dat,
                        yi.name,
                        vi.name ) {
    
    # # test only
    # dat = d
    # calib.temp = MetaUtility::calib_ests(yi = d$yi,
    #                                      sei = sqrt(d$vyi))
    # q = quantile(calib.temp, 0.8)
    # r = 0.3
    # yi.name = "yi"
    # vi.name = "vyi"
    # tail = "above"
    
    
    # here, check if any shifting is actually needed
    # current Phat with no bias
    Phatc = Phat_causal(q = q,
                        B = 0,
                        tail = tail,
                        # this doesn't matter because there's no bias yet
                        muB.toward.null = FALSE,
                        dat = dat,
                        yi.name = yi.name,
                        vi.name = vi.name)
    if ( Phatc <= r ){
        return(1)
    }
    
    # evaluate the ECDF of the unshifted calib at those calib themselves
    #  to get the possible values that Phat can take
    #  this approach handles ties
    calib = sort( calib_ests( yi = dat[[yi.name]], sei = sqrt(dat[[vi.name]]) ) )
    Phat.options = unique( ecdf(calib)(calib) )
    # always possible to choose 0
    Phat.options = c(Phat.options, 0)
    
    # of Phats that are <= r, find the largest one (i.e., closest to r)
    Phat.target = max( Phat.options[ Phat.options <= r ] ) 
    
    
    # find calib.star, the calibrated estimate that needs to move to q
    # example for tail == "above":
    # calib.star is the largest calibrated estimate that needs to move to just
    #  BELOW q after shifting
    # k * Phat.target is the number of calibrated estimates that should remain
    #  ABOVE q after shifting
    k = length(calib)
    if ( tail == "above" ) calib.star = calib[ k - (k * Phat.target) ]
    if ( tail == "below" ) calib.star = calib[ (k * Phat.target) + 1 ]
    
    # pick the bias factor that shifts calib.star to q
    #  and then add a tiny bit (0.001) to shift calib.star to just
    # below or above q
    # if multiple calibrated estimates are exactly equal to calib.star, 
    #  all of these will be shifted just below q (if tail == "above")
    #
    # because we're taking Tmin to be the (exp) ABOLSUTE difference between 
    #  the calib estimate that needs to move to q and q itself, Tmin
    #  will automatically be the bias in whatever direction is needed to
    #  make the shift
    ( Tmin = exp( abs(calib.star - q) + 0.001 ) )
    
    return(as.numeric(Tmin))
}


#' CI for proportion of studies with causal effects above or below q
#'
#' An internal function that estimates a CI for the proportion of studies with true effect sizes above or below \code{q} given the bias factor \code{B}. Users should call \code{confounded_meta} instead.
#' @import
#' boot 
#' @noRd
Phat_CI_lims = function(.B,
                        R,
                        q,
                        tail,
                        muB.toward.null,
                        dat,
                        yi.name,
                        vi.name,
                        CI.level) {
    
    tryCatch({
        boot.res = suppressWarnings( boot( data = dat,
                                           parallel = "multicore",
                                           R = R, 
                                           statistic = function(original, indices) {
                                               
                                               # draw bootstrap sample
                                               b = original[indices,]
                                               
                                               Phatb = suppressWarnings( Phat_causal( q = q, 
                                                                                      B = .B,
                                                                                      tail = tail,
                                                                                      muB.toward.null = muB.toward.null,
                                                                                      dat = b,
                                                                                      yi.name = yi.name,
                                                                                      vi.name = vi.name) )
                                               return(Phatb)
                                           } ) )
        
        bootCIs = boot.ci(boot.res,
                          type="bca",
                          conf = CI.level )
        
        lo = bootCIs$bca[4]
        hi = bootCIs$bca[5]
        se = sd(boot.res$t)
        
        # avoid issues with creating df below
        if ( is.null(lo) ) lo = NA
        if ( is.null(hi) ) hi = NA
        
    }, error = function(err) {
        lo <<- NA
        hi <<- NA
        se <<- NA
    })
    
    # return as data frame to play well with rowwise() and mutate()
    return( data.frame( lo, hi, se ) )
}



#' CI for Tmin and Gmin
#'
#' An internal function that estimates a CI for Tmin and Gmin. Users should call \code{confounded_meta} instead.
#' @import
#' boot 
#' @noRd
Tmin_Gmin_CI_lims = function(
    R,
    q,
    r,
    tail,
    dat,
    yi.name,
    vi.name,
    CI.level) {
    
    tryCatch({
        boot.res = suppressWarnings( boot( data = dat,
                                           parallel = "multicore",
                                           R = R, 
                                           statistic = function(original, indices) {
                                               
                                               # draw bootstrap sample
                                               b = original[indices,]
                                               
                                               Tminb = Tmin_causal(q = q,
                                                                   r = r,
                                                                   tail = tail,
                                                                   dat = b,
                                                                   yi.name = yi.name,
                                                                   vi.name = vi.name)
                                               return(Tminb)
                                           } ) )
        
        
        bootCIs.Tmin = boot.ci(boot.res,
                               type="bca",
                               conf = CI.level )
        
        lo.T = max(1, bootCIs.Tmin$bca[4])  # bias factor can't be < 1
        hi.T = bootCIs.Tmin$bca[5]  # but has no upper bound
        SE.T = sd(boot.res$t)
        
        # avoid issues with creating df below and with g() transformation
        if ( is.null(lo.T) ) lo.T = NA
        if ( is.null(hi.T) ) hi.T = NA
        
        
        ##### Gmin #####
        lo.G = max( 1, g(lo.T) )  # confounding RR can't be < 1
        hi.G = g(hi.T)  # but has no upper bound
        SE.G = sd( g(boot.res$t) )
        
        
        # avoid issues with creating df below
        if ( is.null(lo.G) ) lo.G = NA
        if ( is.null(hi.G) ) hi.G = NA
        
    }, error = function(err) {
        lo.T <<- NA
        hi.T <<- NA
        
        lo.G <<- NA
        hi.G <<- NA
        
        SE.T <<- NA
        SE.G <<- NA
    })
    
    # return as data frame to play well with rowwise() and mutate()
    return( data.frame( lo.T, hi.T, SE.T, lo.G, hi.G, SE.G ) )
}



############################ EXAMPLE DATASETS ############################ 

#' An example meta-analysis
#'
#' A simple simulated meta-analysis of 50 studies with exponentially distributed population effects.
#'
#' @docType data
#' @keywords datasets
#' @details
#' The variables are as follows:
#' \itemize{
#'   \item \code{est} Point estimate on the log-relative risk scale.
#'   \item \code{var} Variance of the log-relative risk.
#' }
"toyMeta"



#' A meta-analysis on soy intake and breast cancer risk (Trock et al., 2006)
#'
#' A meta-analysis of observational studies (12 case-control and six cohort or nested case-control) on the association of soy-food intake with breast cancer risk. Data are from Trock et al.'s (2006) Table 1. This dataset was used as the applied example in Mathur & VanderWeele (2020a). 
#'
#' @docType data
#' @keywords datasets
#' @references 
#' Trock BJ, Hilakivi-Clarke L, Clark R (2006). Meta-analysis of soy intake and breast cancer risk. \emph{Journal of the National Cancer Institute}.
#' 
#' Mathur MB & VanderWeele TJ (2020a). Sensitivity analysis for unmeasured confounding in meta-analyses. \emph{Journal of the American Statistical Association}.
#' @details
#' The variables are as follows:
#' \itemize{
#'\item \code{author} Last name of the study's first author.
#'   \item \code{est} Point estimate on the log-relative risk or log-odds ratio scale.
#'   \item \code{var} Variance of the log-relative risk or log-odds ratio.
#' }
"soyMeta"





#' An example dataset
#'
#' An example dataset from Hsu and Small (Biometrics, 2013). 
#'
#' @docType data
#' @keywords datasets
"lead"


#' Compute E-value for a linear regression coefficient estimate
#'
#' Returns a data frame containing point estimates, the lower confidence limit,
#' and the upper confidence limit on the risk ratio scale (through an
#' approximate conversion) as well as E-values for the point estimate and the
#' confidence interval limit closer to the null.
#' @param est The linear regression coefficient estimate (standardized or
#'   unstandardized)
#' @param se The standard error of the point estimate
#' @param delta The contrast of interest in the exposure
#' @param sd The standard deviation of the outcome (or residual standard
#'   deviation); see Details
#' @param true The true standardized mean difference to which to shift the
#'   observed point estimate. Typically set to 0 to consider a null true effect.
#' @param ... Arguments passed to other methods.
#' @export
#' @keywords e-value
#' @details This function is for linear regression with a continuous exposure
#' and outcome. Regarding the continuous exposure, the choice of \code{delta}
#' defines essentially a dichotomization in the exposure between hypothetical
#' groups of subjects with exposures equal to an arbitrary value \emph{c} versus
#' to another hypothetical group with exposures equal to \emph{c} +
#' \code{delta}. Regarding the continuous outcome, the function uses the
#' effect-size conversions in Chinn (2000) and VanderWeele (2017) to
#' approximately convert the mean difference between these exposure "groups" to
#' the odds ratio that would arise from dichotomizing the continuous outcome.
#'
#' For example, if resulting E-value is 2, this means that unmeasured
#' confounder(s) would need to double the probability of a subject's having
#' exposure equal to \emph{c} + \code{delta} instead of \emph{c}, and would also
#' need to double the probability of being high versus low on the outcome, in
#' which the cutoff for "high" versus "low" is arbitrary subject to some
#' distributional assumptions (Chinn, 2000).
#'
#' A true standardized mean difference for linear regression would use \code{sd}
#' = SD(Y | X, C), where Y is the outcome, X is the exposure of interest, and C
#' are any adjusted covariates. See Examples for how to extract this from
#' \code{lm}. A conservative approximation would instead use \code{sd} = SD(Y).
#' Regardless, the reported E-value for the confidence interval treats \code{sd}
#' as known, not estimated.
#' @references Chinn, S (2000). A simple method for converting an odds ratio to
#' effect size for use in meta-analysis. \emph{Statistics in Medicine}, 19(22),
#' 3127-3131.
#'
#' VanderWeele, TJ (2017). On a square-root transformation of the odds ratio for
#' a common outcome. \emph{Epidemiology}, 28(6), e58.
#' @examples
#' # first standardizing conservatively by SD(Y)
#' data(lead)
#' ols = lm(age ~ income, data = lead)
#'
#' # for a 1-unit increase in income
#' evalues.OLS(est = ols$coefficients[2],
#'             se = summary(ols)$coefficients['income', 'Std. Error'],
#'             sd = sd(lead$age))
#'
#' # for a 0.5-unit increase in income
#' evalues.OLS(est = ols$coefficients[2],
#'             se = summary(ols)$coefficients['income', 'Std. Error'],
#'             sd = sd(lead$age),
#'             delta = 0.5)
#'
#' # now use residual SD to avoid conservatism
#' # here makes very little difference because income and age are
#' # not highly correlated
#' evalues.OLS(est = ols$coefficients[2],
#'             se = summary(ols)$coefficients['income', 'Std. Error'],
#'             sd = summary(ols)$sigma)

evalues.OLS = function( est, se = NA, sd, delta = 1, true = 0, ... ) {
    
    if ( !is.na( se ) ) {
        if ( se < 0 ) stop( "Standard error cannot be negative" )
    }
    
    if ( delta < 0 ) {
        delta = -delta
        wrapmessage( "Recoding delta to be positive" )
    }
    
    if ( !inherits(est, "OLS") ) est = OLS( est, sd = sd )
    if ( !inherits(se, "OLS") ) se = OLS( se, sd = attr(est, "sd") )
    if ( !inherits(true, "MD") ) true = MD( true )
    
    # rescale to reflect a contrast of size delta
    est = toMD( est, delta = delta )
    se = toMD( se, delta = delta )
    
    return( evalues.MD( est = est, se = se, true = true ) )
}


#' Compute E-value for a difference of means and its confidence interval limits
#'
#' Returns a data frame containing point estimates, the lower confidence limit,
#' and the upper confidence limit on the risk ratio scale (through an
#' approximate conversion) as well as E-values for the point estimate and the
#' confidence interval limit closer to the null.
#' @param est The point estimate as a standardized difference (i.e., Cohen's d)
#' @param se The standard error of the point estimate
#' @param true The true standardized mean difference to which to shift the
#'   observed point estimate. Typically set to 0 to consider a null true effect.
#' @param ... Arguments passed to other methods.
#' @export
#' @keywords e-value
#' @details 
#' Regarding the continuous outcome, the function uses the effect-size conversions in Chinn (2000)
#' and VanderWeele (2017) to approximately convert the mean difference between the exposed versus unexposed groups
#' to the odds ratio that would arise from dichotomizing the continuous outcome.
#' 
#' For example, if resulting E-value is 2, this means that unmeasured confounder(s) would need to double
#' the probability of a subject's being exposed versus not being exposed, and would also need to
#' double the probability of being high versus low on the outcome, in which the cutoff for "high" versus
#' "low" is arbitrary subject to some distributional assumptions (Chinn, 2000). 
#' @references 
#' Chinn, S (2000). A simple method for converting an odds ratio to effect size for use in meta-analysis. \emph{Statistics in Medicine}, 19(22), 3127-3131.
#'
#' VanderWeele, TJ (2017). On a square-root transformation of the odds ratio for a common outcome. \emph{Epidemiology}, 28(6), e58.
#' @examples
#' # compute E-value if Cohen's d = 0.5 with SE = 0.25
#' evalues.MD(.5, .25)

evalues.MD = function( est, se = NA, true = 0, ... ) {
    
    if ( !is.na( se ) ) {
        if ( se < 0 ) stop( "Standard error cannot be negative" )
    }
    
    if ( !inherits(est, "MD") ) est = MD(est)
    if ( !inherits(true, "MD") ) true = MD(true)
    
    lo = NA
    hi = NA
    if ( !is.na(se) ) {
        lo = exp( 0.91 * est - 1.78 * se )
        hi = exp( 0.91 * est + 1.78 * se )
        #lo =  exp( log( est ) - 1.96 * log( MDtoRR( se ) )) # ( est converted )
        #hi =  exp( log( est ) + 1.96 * log( MDtoRR( se ) ))
    }
    
    if ( !is.na(lo) ) lo = RR(lo)
    if ( !is.na(hi) ) hi = RR(hi)
    est = toRR(est)
    true = toRR(true)
    
    return( evalues.RR( est = est, lo = lo, hi = hi, true = true ) )
}



#' Compute E-value for a hazard ratio and its confidence interval limits
#'
#' Returns a data frame containing point estimates, the lower confidence limit,
#' and the upper confidence limit on the risk ratio scale (through an
#' approximate conversion if needed when outcome is common ) as well as E-values
#' for the point estimate and the confidence interval limit closer to the null.
#' @param est The point estimate
#' @param lo The lower limit of the confidence interval
#' @param hi The upper limit of the confidence interval
#' @param rare 1 if outcome is rare (<15 percent at end of follow-up); 0 if
#'   outcome is not rare (>15 percent at end of follow-up)
#' @param true The true HR to which to shift the observed point estimate.
#'   Typically set to 1 to consider a null true effect.
#' @param ... Arguments passed to other methods.
#' @export
#' @keywords e-value
#' @examples
#' # compute E-value for HR = 0.56 with CI: [0.46, 0.69]
#' # for a common outcome
#' evalues.HR(0.56, 0.46, 0.69, rare = FALSE)


evalues.HR = function( est, lo = NA, hi = NA, rare = NA, true = 1, ... ) {
    
    # sanity checks
    if ( est < 0 ) stop( "HR cannot be negative" )
    
    if ( is.na(rare) ) rare = NULL # for compatibility w/ HR constructor
    
    if ( !inherits(est, "HR") ) est = HR( est, rare = rare )
    if ( !is.na(lo) && !inherits(lo, "HR") ) lo = HR( lo, rare = attr(est, "rare") )
    if ( !is.na(hi) && !inherits(hi, "HR") ) hi = HR( hi, rare = attr(est, "rare") )
    if ( !inherits(true, "HR") ) true = HR( true, rare = attr(est, "rare") )
    
    est = toRR(est)
    if ( !is.na(lo) ) lo = toRR(lo)
    if ( !is.na(hi) ) hi = toRR(hi)
    true = toRR(true)
    
    return( evalues.RR( est = est, lo = lo, hi = hi, true = true ) )
}



#' Compute E-value for an odds ratio and its confidence interval limits
#'
#' Returns a data frame containing point estimates, the lower confidence limit,
#' and the upper confidence limit on the risk ratio scale (through an
#' approximate conversion if needed when outcome is common) as well as E-values
#' for the point estimate and the confidence interval limit closer to the null.
#' @param est The point estimate
#' @param lo The lower limit of the confidence interval
#' @param hi The upper limit of the confidence interval
#' @param rare 1 if outcome is rare (<15 percent at end of follow-up); 0 if
#'   outcome is not rare (>15 percent at end of follow-up)
#' @param true The true OR to which to shift the observed point estimate.
#'   Typically set to 1 to consider a null true effect.
#' @param ... Arguments passed to other methods.
#' @keywords e-value
#'
#' @export
#' @examples
#' # compute E-values for OR = 0.86 with CI: [0.75, 0.99]
#' # for a common outcome
#' evalues.OR(0.86, 0.75, 0.99, rare = FALSE)
#'
#' ## Example 2
#' ## Hsu and Small (2013 Biometrics) Data
#' ## sensitivity analysis after log-linear or logistic regression
#'
#' head(lead)
#'
#' ## log linear model -- obtain the conditional risk ratio
#' lead.loglinear = glm(lead ~ ., family = binomial(link = "log"),
#'                          data = lead[,-1])
#' est = summary(lead.loglinear)$coef["smoking", c(1, 2)]
#'
#' RR       = exp(est[1])
#' lowerRR  = exp(est[1] - 1.96*est[2])
#' upperRR  = exp(est[1] + 1.96*est[2])
#' evalues.RR(RR, lowerRR, upperRR)
#'
#' ## logistic regression -- obtain the conditional odds ratio
#' lead.logistic = glm(lead ~ ., family = binomial(link = "logit"),
#'                         data = lead[,-1])
#' est = summary(lead.logistic)$coef["smoking", c(1, 2)]
#'
#' OR       = exp(est[1])
#' lowerOR  = exp(est[1] - 1.96*est[2])
#' upperOR  = exp(est[1] + 1.96*est[2])
#' evalues.OR(OR, lowerOR, upperOR, rare=FALSE)


evalues.OR = function( est, lo = NA, hi = NA, rare = NA, true = 1, ... ) {
    
    # sanity checks
    if ( est < 0 ) stop( "OR cannot be negative" )
    
    if ( is.na(rare) ) rare = NULL # for compatibility w/ OR constructor
    
    if ( !inherits(est, "OR") ) est = OR( est, rare = rare )
    if ( !is.na(lo) && !inherits(lo, "OR") ) lo = OR( lo, rare = attr(est, "rare") )
    if ( !is.na(hi) && !inherits(hi, "OR") ) hi = OR( hi, rare = attr(est, "rare") )
    if ( !inherits(true, "OR") ) true = OR( true, rare = attr(est, "rare"))
    
    est = toRR(est)
    if ( !is.na(lo) ) lo = toRR(lo)
    if ( !is.na(hi) ) hi = toRR(hi)
    true = toRR(true)
    
    return( evalues.RR( est = est, lo = lo, hi = hi, true = true ) )
}



#' Compute E-value for a risk ratio or rate ratio and its confidence interval
#' limits
#'
#' Returns a data frame containing point estimates, the lower confidence limit,
#' and the upper confidence limit for the risk ratio (as provided by the user)
#' as well as E-values for the point estimate and the confidence interval limit
#' closer to the null.
#' @param est The point estimate
#' @param lo The lower limit of the confidence interval
#' @param hi The upper limit of the confidence interval
#' @param true The true RR to which to shift the observed point estimate.
#'   Typically set to 1 to consider a null true effect.
#' @param ... Arguments passed to other methods.
#' @keywords e-value
#' @export
#' @examples
#' # compute E-value for leukemia example in VanderWeele and Ding (2017)
#' evalues.RR(0.80, 0.71, 0.91)
#'
#' # you can also pass just the point estimate
#' evalues.RR(0.80)
#'
#' # demonstrate symmetry of E-value
#' # this apparently causative association has same E-value as the above
#' evalues.RR(1 / 0.80)

evalues.RR = function( est, lo = NA, hi = NA, true = 1, ... ) {
    
    # organize user's values
    values = c( est, lo, hi )
    
    # sanity checks
    if ( est < 0 ) stop( "RR cannot be negative" )
    if ( true < 0 ) stop( "True value is impossible" )
    
    # warn user if using non-null true value
    if ( true != 1 ) wrapmessage(c("You are calculating a \"non-null\" E-value,",
                                   "i.e., an E-value for the minimum amount of unmeasured",
                                   "confounding needed to move the estimate and confidence",
                                   "interval to your specified true value rather than to",
                                   "the null value."))
    
    # check if CI crosses null
    null.CI = NA
    if ( est > true & !is.na( lo ) ) {
        null.CI = ( lo < true )
    }
    
    if ( est < true & !is.na( hi ) ) {
        null.CI = ( hi > true )
    }
    
    
    # sanity checks for CI
    if ( !is.na( lo ) & !is.na( hi ) ) {
        # check if lo < hi
        if ( lo > hi ) stop( "Lower confidence limit should be less than upper confidence limit" )
    }
    
    if ( !is.na( lo ) & est < lo ) stop( "Point estimate should be inside confidence interval" )
    if ( !is.na( hi ) & est > hi ) stop( "Point estimate should be inside confidence interval" )
    
    # compute E-values
    E = sapply( values, FUN = function(x) threshold( x, true = true ) )
    
    
    # clean up CI reporting
    # if CI crosses null, set its E-value to 1
    if ( !is.na(null.CI) & null.CI == TRUE ){
        E[ 2:3 ] = 1
        wrapmessage("Confidence interval crosses the true value, so its E-value is 1.") 
    }
    
    # if user provides either CI limit...
    if ( !is.na(lo) | !is.na(hi) ) {
        # ...then only report E-value for CI limit closer to null
        if ( est > true ) E[3] = NA
        if ( est < true ) E[2] = NA
        if ( est == true ) {
            E[2] = 1
            E[3] = NA
        }
    }
    
    result = rbind(values, E)
    
    rownames(result) = c("RR", "E-values")
    colnames(result) = c("point", "lower", "upper")
    class(result) = c("evalue", "matrix")
    
    result
}


#'Estimate risk ratio and compute CI limits from two-by-two table
#'
#'Given counts in a two-by-two table, computes risk ratio and confidence
#'interval limits.
#'@param n11 Number exposed (X=1) and diseased (D=1)
#'@param n10 Number exposed (X=1) and not diseased (D=0)
#'@param n01 Number unexposed (X=0) and diseased (D=1)
#'@param n00 Number unexposed (X=0) and not diseased (D=0)
#'@param alpha Alpha level associated with confidence interval
#'@export
#'@import stats
#' @examples
#' # Hammond and Holl (1958 JAMA) Data
#' # Two by Two Table
#' #          Lung Cancer    No Lung Cancer
#'# Smoker    397            78557
#'# Nonsmoker 51             108778
#'
#'twoXtwoRR(397, 78557, 51, 108778)

twoXtwoRR = function( n11, n10, n01, n00, alpha = 0.05 ){
    
    p1     = n11/( n11 + n10 )
    p0     = n01/( n01 + n00 )
    RR     = p1/p0
    logRR  = log( RR )
    
    selogRR  = sqrt( 1/n11 - 1/( n11+n10 ) + 1/n01 - 1/( n01+n00 ) )
    q.alpha  = qnorm( 1 - alpha/2 )
    
    upperRR  = exp( logRR + q.alpha*selogRR )
    lowerRR  = exp( logRR - q.alpha*selogRR )
    
    res         = c( RR, upperRR, lowerRR )
    names(res)  = c( "point", "upper", "lower" )
    
    return(res) 
}




#'Compute E-value for single value of risk ratio
#'
#'Computes E-value for a single value of the risk ratio. Users should typically
#'call the relevant \code{evalues.XX()} function rather than this internal
#'function.
#'@param x The risk ratio
#'@param true The true RR to which to shift the observed point estimate.
#'  Typically set to 1 to consider a null true effect.
#'@export
#'@keywords internal
#'
#' @examples
#' ## Example 1
#' ## Hammond and Holl (1958 JAMA) Data
#' ## Two by Two Table
#' #          Lung Cancer    No Lung Cancer
#'# Smoker    397            78557
#'# Nonsmoker 51             108778
#'
#' # first get RR and CI bounds
#' twoXtwoRR(397, 78557, 51, 108778)
#'
#' # then compute E-values
#' evalues.RR(10.729780, 8.017457, 14.359688)


threshold = function( x, true = 1 ) {
    
    if ( is.na(x) ) return(NA)
    
    if( x < 0 ){
        warning("The risk ratio must be non-negative.")
    }  
    
    if( x <= 1 ){
        x = 1 / x
        true = 1 / true
    }
    
    # standard case: causal effect is toward null
    if ( true <= x ) return( ( x + sqrt( x * ( x - true ) ) ) / true )
    
    # causal effect is away from null
    else if ( true > x ) {
        # ratio that is > 1
        rat = true / x 
        return( rat + sqrt( rat * ( rat - 1 ) ) )
    }
    
}







#' Compute E-value for a population-standardized risk difference and its
#' confidence interval limits
#'
#' Returns E-values for the point estimate and the lower confidence interval
#' limit for a positive risk difference. If the risk difference is negative, the
#' exposure coding should be first be reversed to yield a positive risk
#' difference.
#' @param n11 Number of exposed, diseased individuals
#' @param n10 Number of exposed, non-diseased individuals
#' @param n01 Number of unexposed, diseased individuals
#' @param n00 Number of unexposed, non-diseased individuals
#' @param true True value of risk difference to which to shift the point
#'   estimate. Usually set to 0 to consider the null.
#' @param alpha Alpha level
#' @param grid Spacing for grid search of E-value
#' @param ... Arguments passed to other methods.
#' @keywords e-value
#'
#' @export
#' @export evalues.RD
#' @import stats graphics
#' @examples
#'
#' ## example 1
#' ## Hammond and Holl (1958 JAMA) Data
#' ## Two by Two Table
#' ##          Lung Cancer    No Lung Cancer
#' ##Smoker    397            78557
#' ##Nonsmoker 51             108778
#'
#' # E-value to shift observed risk difference to 0
#' evalues.RD(397, 78557, 51, 108778)
#'
#' # E-values to shift observed risk difference to other null values
#' evalues.RD(397, 78557, 51, 108778, true = 0.001)


evalues.RD = function( n11, n10, n01, n00,  
                       true = 0, alpha = 0.05, grid = 0.0001, ... ) {
    
    # sanity check
    if ( any( c(n11, n10, n01, n00) < 0 ) ) stop("Negative cell counts are impossible.")
    
    # sample sizes
    N = n10 + n11 + n01 + n00
    N1 = n10 + n11  # total X=1
    N0 = n00 + n01  # total X=0
    
    # compute f = P(X = 1)
    f = N1 / N
    
    # P(D = 1 | X = 1)
    p1  = n11 / N1
    
    # P(D = 1 | X = 0)
    p0  = n01 / N0
    
    if( p1 < p0 ) stop("RD < 0; please relabel the exposure such that the risk difference > 0.")
    
    
    # standard errors
    se.p1 = sqrt( p1 * ( 1-p1 ) / N1 )
    se.p0 = sqrt( p0 * ( 1-p0 ) / N0 )
    
    # back to Peng's code
    s2.f   = f*( 1-f )/N
    s2.p1  = se.p1^2
    s2.p0  = se.p0^2
    diff   = p0*( 1-f ) - p1*f
    
    # bias factor and E-value for point estimate
    est.BF = ( sqrt( ( true + diff )^2 + 4 * p1 * p0 * f * ( 1-f )  ) - ( true + diff ) ) / ( 2 * p0 * f )
    est.Evalue    = threshold(est.BF)   
    if( p1 - p0 <= true ) stop("For risk difference, true value must be less than or equal to point estimate.")
    
    # compute lower CI limit
    Zalpha        = qnorm( 1-alpha/2 )  # critical value
    lowerCI       = p1 - p0 - Zalpha*sqrt( s2.p1 + s2.p0 )
    
    # check if CI contains null
    if ( lowerCI <= true ) {
        
        # warning( "Lower CI limit of RD is smaller than or equal to true value." )
        return( list( est.Evalue = est.Evalue, lower.Evalue = 1 ) )
        
    } else {
        # find E-value for lower CI limit
        # we know it's less than or equal to E-value for point estimate
        BF.search = seq( 1, est.BF, grid )
        
        # population-standardized risk difference
        RD.search = p1 - p0 * BF.search
        f.search  = f + ( 1-f )/BF.search
        
        # using equation for RD^true on pg 376, compute the lower CI limit for these parameters
        # RD.search * f.search is exactly the RHS of the inequality for RD^true ( population )
        Low.search = RD.search * f.search -
            Zalpha * sqrt( ( s2.p1 + s2.p0 * BF.search^2 ) * f.search^2 +
                               RD.search^2 * ( 1 - 1 / BF.search )^2 * s2.f )
        
        # get the first value for BF_u such that the CI limit hits the true value
        Low.ind    = ( Low.search <= true )
        Low.no     = which( Low.ind==1 )[1]
        lower.Evalue = threshold( BF.search[Low.no] )
        
        
        return(list(est.Evalue   = est.Evalue,
                    lower.Evalue = lower.Evalue))
    }
    
}



#' Plot bias factor as function of confounding relative risks
#'
#' Plots the bias factor required to explain away a provided relative risk.
#' @param RR The relative risk
#' @param xmax Upper limit of x-axis.
#' @export
#' @keywords e-value
#' @examples
#' # recreate the plot in VanderWeele and Ding (2017)
#' bias_plot(RR=3.9, xmax=20)

bias_plot = function( RR, xmax ) {
    
    x = seq( 0, xmax, 0.01 )
    
    # MM: reverse RR if it's preventive
    if ( RR < 1 ) RR = 1/RR
    
    plot( x, x, lty = 2, col = "white", type = "l", xaxs = "i", yaxs = "i", xaxt="n", yaxt = "n",
          xlab = expression( RR[EU] ), ylab = expression( RR[UD] ),
          xlim = c( 0,xmax ),
          main = "" )
    
    x = seq( RR, xmax, 0.01 )
    
    y    = RR*( RR-1 )/( x-RR )+RR
    
    lines( x, y, type = "l" )
    
    
    high = RR + sqrt( RR*( RR-1 ) )
    
    
    points( high, high, pch = 19 )
    
    label5 = seq( 5, 40, by = 5 )
    axis( 1, label5, label5, cex.axis = 1 )
    axis( 2, label5, label5, cex.axis = 1 )
    
    g = round( RR + sqrt( RR * ( RR - 1 ) ), 2 )
    label = paste( "( ", g, ", ", g, " )", sep="" )
    
    text( high + 3, high + 1, label )
    
    legend( "bottomleft", expression(
        RR[EU]*RR[UD]/( RR[EU]+RR[UD]-1 )==RR
    ), 
    lty = 1:2,
    bty = "n" )
    
}


#' @export
evalue.RR = function( est, lo = NA, hi = NA, se = NA, delta = NA, true = 1, ... ){
    evalues.RR(est = est, lo = lo, hi = hi, true = true, ...)
}

#' @export
evalue.OR = function(est, lo = NA, hi = NA, se = NA, delta = NA, true = 1, ...){
    evalues.OR(est = est, lo = lo, hi = hi, true = true, ...)
}

#' @export
evalue.HR = function(est, lo = NA, hi = NA, se = NA, delta = NA,true = 1, ...){
    evalues.HR(est = est, lo = lo, hi = hi, true = true, ...)
}

#' @export
evalue.OLS = function(est, lo = NA, hi = NA, se = NA, delta = 1, true = 0, ...){
    evalues.OLS(est, se = se, delta = delta, true = true, ...)
}

#' @export
evalue.MD = function(est, lo = NA, hi = NA, se = NA, delta = NA, true = 0, ...){
    evalues.MD(est, se = se, true = true, ...)
}


#' @export
evalue.default <- function(est, ...) {
    
    if (is.null(measure) && !inherits(est, "estimate")) stop("Effect measure must be specified")
    
    measure <- class(est)[1]
    
    evalues_func = switch(measure,
                          "HR" = evalues.HR,
                          "OR" = evalues.OR,
                          "RR" = evalues.RR,
                          "RD" = evalues.RD,
                          "OLS" = evalues.OLS,
                          "MD" = evalues.MD)
    
    evalues_func(est, ...)
}

#' Compute an E-value for unmeasured confounding
#'
#' Returns a data frame containing point estimates, the lower confidence limit,
#' and the upper confidence limit on the risk ratio scale (possibly through an
#' approximate conversion) as well as E-values for the point estimate and the
#' confidence interval limit closer to the null.
#' @param est The effect estimate that was observed but which is suspected to be
#'   biased. A number of class "estimate" (constructed with [RR()], [OR()],
#'   [HR()], [OLS()], or [MD()]; for E-values for risk differences, 
#'   see [evalues.RD()]).
#' @param lo Optional. Lower bound of the confidence interval. If not an object
#'   of class "estimate", assumed to be on the same scale as `est`.
#' @param hi Optional. Upper bound of the confidence interval. If not an object
#'   of class "estimate", assumed to be on the same scale as `est`.
#' @param true A number to which to shift the observed estimate to. Defaults to
#'   1 for ratio measures ([RR()], [OR()], [HR()]) and 0 for additive measures
#'   ([OLS()], [MD()]).
#' @param se The standard error of the point estimate, for `est` of class "OLS"
#' @param delta The contrast of interest in the exposure, for `est` of class "OLS"
#' @param ... Arguments passed to other methods.
#' @export
#' @details An E-value for unmeasured confounding is minimum strength of
#'   association, on the risk ratio scale, that an unmeasured confounder would
#'   need to have with both the treatment and the outcome to fully explain away
#'   a specific treatment–outcome association, conditional on the measured
#'   covariates.
#'
#'   The estimate is converted appropriately before the E-value is calculated.
#'   See [conversion functions][convert_measures] for more details. The point
#'   estimate and confidence limits after conversion are returned, as is the
#'   E-value for the point estimate and the confidence limit closest to the
#'   proposed "true" value (by default, the null value.)
#'
#'   For an [OLS()] estimate, the E-value is for linear regression with a
#'   continuous exposure and outcome. Regarding the continuous exposure, the
#'   choice of \code{delta} defines essentially a dichotomization in the
#'   exposure between hypothetical groups of subjects with exposures equal to an
#'   arbitrary value \emph{c} versus to another hypothetical group with
#'   exposures equal to \emph{c} + \code{delta}.
#'
#'   For example, if resulting E-value is 2, this means that unmeasured
#'   confounder(s) would need to double the probability of a subject's having
#'   exposure equal to \emph{c} + \code{delta} instead of \emph{c}, and would
#'   also need to double the probability of being high versus low on the
#'   outcome, in which the cutoff for "high" versus "low" is arbitrary subject
#'   to some distributional assumptions (Chinn, 2000).
#'   
#' @keywords e-value
#' @export
#' @examples
#' # compute E-value for leukemia example in VanderWeele and Ding (2017)
#' evalue(RR(0.80), 0.71, 0.91)
#'
#' # you can also pass just the point estimate
#' # and return just the E-value for the point estimate with summary()
#' summary(evalue(RR(0.80)))
#'
#' # demonstrate symmetry of E-value
#' # this apparently causative association has same E-value as the above
#' summary(evalue(RR(1 / 0.80)))
#' 
#' # E-value for a non-null true value
#' summary(evalue(RR(2), true = 1.5))
#' 
#' ## Hsu and Small (2013 Biometrics) Data
#' ## sensitivity analysis after log-linear or logistic regression
#' head(lead)
#'
#' ## log linear model -- obtain the conditional risk ratio
#' lead.loglinear = glm(lead ~ ., family = binomial(link = "log"),
#'                          data = lead[,-1])
#' est_se = summary(lead.loglinear)$coef["smoking", c(1, 2)]
#'
#' est      = RR(exp(est_se[1]))
#' lowerRR  = exp(est_se[1] - 1.96*est_se[2])
#' upperRR  = exp(est_se[1] + 1.96*est_se[2])
#' evalue(est, lowerRR, upperRR)
#'
#' ## logistic regression -- obtain the conditional odds ratio
#' lead.logistic = glm(lead ~ ., family = binomial(link = "logit"),
#'                         data = lead[,-1])
#' est_se = summary(lead.logistic)$coef["smoking", c(1, 2)]
#'
#' est      = OR(exp(est_se[1]), rare = FALSE)
#' lowerOR  = exp(est_se[1] - 1.96*est_se[2])
#' upperOR  = exp(est_se[1] + 1.96*est_se[2])
#' evalue(est, lowerOR, upperOR)
#' 
#' ## linear regression
#' # standardizing conservatively by SD(Y)
#' ols = lm(age ~ income, data = lead)
#' est = OLS(ols$coefficients[2], sd = sd(lead$age))
#'
#' # for a 1-unit increase in income 
#' evalue(est = est, 
#'        se = summary(ols)$coefficients['income', 'Std. Error'])
#' 
#' # for a 0.5-unit increase in income
#' evalue(est = est,
#'        se = summary(ols)$coefficients['income', 'Std. Error'],
#'        delta = 0.5)
#'
#' # E-value for Cohen's d = 0.5 with SE = 0.25
#' evalue(est = MD(.5), se = .25)
#' 
#' # compute E-value for HR = 0.56 with CI: [0.46, 0.69]
#' # for a common outcome
#' evalue(HR(0.56, rare = FALSE), lo = 0.46, hi = 0.69)
#' # for a rare outcome
#' evalue(HR(0.56, rare = TRUE), lo = 0.46, hi = 0.69)

evalue = function( est, lo = NA, hi = NA, se = NA, delta = 1, true = c(0, 1), ... ) {
    UseMethod( "evalue")
}

#' @export
summary.evalue = function( object, ... ) {
    if ( !inherits(object, "evalue")) stop('Argument must be of class "evalue"')
    object[2,1]
}

#' @export
print.evalue = function( x, ... ) {
    class(x) <- "matrix" # to suppress attr printing
    print.default(x)
}





#' Internal function used to fit roots of a polynomial made up of the product of
#' bias factors.
#'
#' @param x A number. The variable to solve for.
#' @param y A number. The observed risk ratio.
#' @param n A number. Degree of polynomial in the numerator.
#' @param d A number. Degree of polynomial in the denominator.
#' @return Returns the value of the expression. Used for root solving. At the
#'   function's roots, will return 0.
#' @keywords internal

deg_func <- function(x, y, n, d) {
    (x^n) / ((2 * x - 1)^d) - y
}


#' Internal function used to calculate arbitrary bias factors.
#'
#' @param rr1 A number. A risk ratio that is a component of a bias factors
#' @param rr2 A number. The other risk ratio that is a component of a bias
#'   factors
#' @return Returns the value of the expression. Used for calculating bias
#'   factors.
#' @keywords internal

bf_func <- function(rr1, rr2) {
    (rr1 * rr2) / (rr1 + rr2 - 1)
}

#' Nicely wrap a message
#' @noRd
wrapmessage <- function(mess, width = 0.9 * getOption("width")) {
    message(paste(strwrap(paste(mess, collapse = " "), width = width), collapse = "\n"))
}


#' Declare an effect measure
#'
#' @description These functions allow the user to declare that an estimate is a
#'   certain type of effect measure: risk ratio (`RR`), odds ratio (`OR`),
#'   hazard ratio (`HR`), risk difference (`RD`), linear regression coefficient
#'   (`OLS`), or mean standardized difference (`MD`).
#' @name effect_measures
#' @param est The effect estimate (numeric).
#' @param rare Logical. Whether the outcome is sufficiently rare for use of risk
#'   ratio approximates; if not, approximate conversions are used. Used only for
#'   [HR()] and [OR()]; see Details.
#' @param sd The standard deviation of the outcome (or residual standard
#'   deviation). Used only for [OLS()]; see Details.
#' @return An object of classes "estimate" and the measure of interest,
#'   containing the effect estimate and any other attributes to be used in
#'   future calculations.
#' @details The [conversion functions][convert_measures] use these objects to
#'   convert between effect measures when necessary to calculate E-values. Read
#'   more about the conversions in Table 2 of VanderWeele TJ, Ding P.
#'   *Sensitivity Analysis in Observational Research: Introducing the E-Value.*
#'   Annals of Internal Medicine. 2017;167(4):268–75.
#'
#'   See also VanderWeele TJ. *Optimal approximate conversions of odds ratios
#'   and hazard ratios to risk ratios.* Biometrics. 2019 Jan 6;(September
#'   2018):1–7.
#'
#'   For [OLS()], `sd` must be specified. A true standardized mean difference
#'   for linear regression would use \code{sd} = SD( Y | X, C ), where Y is the
#'   outcome, X is the exposure of interest, and C are any adjusted covariates.
#'   See Examples for how to extract this from \code{lm}. A conservative
#'   approximation would instead use \code{sd} = SD( Y ). Regardless, the
#'   reported E-value for the confidence interval treats \code{sd} as known, not
#'   estimated.
#' @examples
#' # Both odds ratios are 3, but will be treated differently in E-value calculations
#' # depending on whether rare outcome assumption is reasonable
#' OR(3, rare = FALSE)
#' OR(3, rare = TRUE)
#' evalue(OR(3, rare = FALSE))
#' evalue(OR(3, rare = TRUE))
#' attributes(OR(3, rare = FALSE))
#' 
#' # If an estimate was constructed via conversion from another effect measure,
#' # we can see the history of a conversion using the summary() function
#' summary(toRR(OR(3, rare = FALSE)))
#' summary(toRR(OLS(3, sd = 1)))
#' 
#' # Estimating sd for an OLS estimate
#' # first standardizing conservatively by SD(Y)
#' data(lead)
#' ols = lm(age ~ income, data = lead)
#' est = ols$coefficients[2]
#' sd = sd(lead$age)
#' summary(evalue(OLS(est, sd)))
#' # now use residual SD to avoid conservatism
#' # here makes very little difference because income and age are
#' # not highly correlated
#' sd = summary(ols)$sigma
#' summary(evalue(OLS(est, sd)))


#' @rdname effect_measures
#' @export
RR <- function(est) {
    class(est) <- c("RR", "estimate")
    est
}

#' @rdname effect_measures
#' @export
OR <- function(est, rare) {
    class(est) <- c("OR", "estimate")
    attr(est, "rare") <- rare
    est
}

#' @rdname effect_measures
#' @export
HR <- function(est, rare) {
    class(est) <- c("HR", "estimate")
    attr(est, "rare") <- rare
    est
}

#' @rdname effect_measures
#' @export
RD <- function(est) {
    class(est) <- c("RD", "estimate")
    est
}

#' @rdname effect_measures
#' @export
OLS <- function(est, sd) {
    class(est) <- c("OLS", "estimate")
    attr(est, "sd") <- sd
    est
}

#' @rdname effect_measures
#' @export
MD <- function(est) {
    class(est) <- c("MD", "estimate")
    est
}

#' @export
print.estimate <- function(x, ...) {
    attr(x, "sd") <- NULL
    attr(x, "rare") <- NULL
    attr(x, "history") <- NULL
    class(x) <- "numeric"
    print.default(x, ...)
}

#' @export
summary.estimate <- function(object, ...) {
    if (is.null(attr(object, "history"))) return(cat(class(object)[1], "=", object))
    history <- attr(object, "history")
    cat(class(object)[1], "=", object,
        "\nThis is an approximate conversion of the original", 
        history[1,1], "estimate =", history[1,2])
}

#' Convert an effect measure
#'
#' @description These helper functions are mostly used internally to convert
#'   [effect measures][effect_measures] for the calculation of E-values. The
#'   approximate conversion of odds and hazard ratios to risk ratios depends on
#'   whether the rare outcome assumption is made.
#' @name convert_measures
#' @param est The effect estimate; constructed with one of [RR()], [OR()], [HR()],
#'   [MD()], [OLS()].
#' @param rare When converting a [OR()] or [HR()] estimate, a logical indicating
#'   whether the outcome is sufficiently rare to approximate a risk ratio.
#' @param delta When converting an [OLS()] estimate, the contrast of interest 
#'   in the exposure. Defaults to 1 (a 1-unit contrast in the exposure).
#' @param ... Arguments passed to other methods.
#' @return An object of class "estimate" and the desired effect measure. Also
#'   includes as an attribute its conversion history.
#' @details Uses the conversions listed in Table 2 of VanderWeele TJ, Ding P.
#'   *Sensitivity Analysis in Observational Research: Introducing the E-Value.*
#'   Annals of Internal Medicine. 2017;167(4):268–75.
#'
#'   See references.
#'
#'   Regarding the continuous outcome, the function uses the effect-size
#'   conversions in Chinn (2000) and VanderWeele (2017) to approximately convert
#'   the mean difference between these exposure "groups" to the odds ratio that
#'   would arise from dichotomizing the continuous outcome.
#'
#' @references Chinn, S (2000). A simple method for converting an odds ratio to
#' effect size for use in meta-analysis. \emph{Statistics in Medicine}, 19(22),
#' 3127-3131.
#'
#' VanderWeele, TJ (2017). On a square-root transformation of the odds ratio for
#' a common outcome. \emph{Epidemiology}, 28(6), e58.
#'
#' VanderWeele TJ (2020). *Optimal approximate conversions of odds ratios and
#' hazard ratios to risk ratios.* Biometrics.
#' @examples
#' # Both odds ratios are 3, but will be treated differently
#' # depending on whether rare outcome assumption is reasonable
#' OR(3, rare = FALSE)
#' OR(3, rare = TRUE)
#' toRR(OR(3, rare = FALSE))
#' toRR(OR(3, rare = TRUE))
#' attributes(toRR(toMD(OLS(3, sd = 1.2), delta = 1)))
#' @rdname convert_measures
#' @export
toRR <- function(est, rare, delta = 1, ...) {
    UseMethod("toRR", est)
}
#' @rdname convert_measures
#' @export
toMD <- function(est, delta = 1, ...) {
    UseMethod("toMD", est)
}



#' @export
toMD.OLS <- function(est, delta = 1, ... ) {
    sd_attr <- attr(est, "sd")
    
    if (is.null(sd_attr)) 
        stop("Must specify the outcome standard deviation. Use argument sd = in the OLS() function")
    
    MD <- est * delta / sd_attr
    class(MD) <- c("MD", "estimate")
    attr(MD, "history") <- rbind(attr(est, "history"), c("OLS", est))
    MD
}

#' @export
toRR.MD <- function(est, ... ) {
    RR <- exp( 0.91 * est )
    class(RR) <- c("RR", "estimate")
    attr(RR, "history") <- rbind(attr(est, "history"), c("MD", est))
    RR
}

#' @export
toRR.OLS <- function(est, rare = NULL, delta = 1, ... ) {
    toRR(toMD(est, delta = delta))
}

#' @export
toRR.HR <- function(est, rare, ... ) {
    rare_attr <- attr(est, "rare")
    
    if (is.null(rare_attr)) 
        stop("Must specify whether the rare outcome assumption can be made. Use argument rare = in the HR() function.")
    
    if (rare_attr) RR <- est else {
        RR <- ( 1 - 0.5^sqrt( est ) ) / ( 1 - 0.5^sqrt( 1 / est ) )
    }
    class(RR) <- c("RR", "estimate")
    attr(RR, "history") <- rbind(attr(est, "history"), c("HR", est))
    RR
}

#' @export
toRR.OR <- function(est, rare, ... ) {
    rare_attr <- attr(est, "rare")
    
    if (is.null(rare_attr)) 
        stop("Must specify whether the rare outcome assumption can be made. Use argument rare = in the OR() function.")
    
    if (rare_attr) RR <- est else RR <- sqrt(est)
    class(RR) <- c("RR", "estimate")
    attr(RR, "history") <- rbind(attr(est, "history"), c("OR", est))
    RR
}


#' @export
toRR.default <- function(est, ... ) {
    stop("RR conversion is currently available only for estimates of class \"OR\", \"HR\", \"MD\", and \"OLS\"")
}


#' @export
toMD.default <- function(est, ...) {
    stop("MD conversion is currently available only for estimates of class \"OLS\"")
}






