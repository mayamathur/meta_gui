# install.packages("shinyWidgets")
# install.packages("dplyr")
# install.packages("EValue")

library(shiny)
#library(EValue) #include confounded_meta and sens_plot below to test, will eventually be loaded into EValue package and can remove the functions below
library(plotly)
library(shinythemes)
library(shinyBS)
library(shinyalert)
library(bsplus)
library(shinydashboard)
library(shinyWidgets)
library(MetaUtility)

# try to fix deployment problem
library(purrr)
library(plogr)
library(dplyr)
library(boot)

# keeps original error messages
options(shiny.sanitize.errors = FALSE)




############ temp only: use version of fns not yet in package


############################ MAIN FUNCTIONS ############################

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
                            vt2 = NA
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
    
    
    ##### Check for Bad or Incomplete Input - Common to Parametric and Calibrated Methods #####
    if ( ! is.na(r) ) {
        if (r < 0 | r > 1) stop("r must be between 0 and 1")
    }
    
    if ( is.na(r) ) message("Cannot compute Tmin or Gmin without r. Returning only prop.")
    
    if ( !is.na(muB) & muB < 0 ) {
        stop("Must have muB > 0. Use the muB.toward.null argument instead if you want to consider bias away from the null. See Details.")
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
            if ( t2 <= sigB^2 ) stop("Must have t2 > sigB^2")
            if ( sigB < 0 ) stop("Bias factor standard deviation cannot be negative")
        }
        
        
        
        ##### Messages When Not All Output Can Be Computed #####
        if ( is.na(vyr) | is.na(vt2) ) message("Cannot compute inference without vyr and vt2. Returning only point estimates.")
        
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
            if ( Phat < 0.15 | Phat > 0.85 ) warning('Prop is close to 0 or 1. We recommend choosing method = \"calibrated\" or alternatively using bias-corrected and accelerated bootstrapping to estimate all inference in this case.')
            
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
                                        yi.name = yi.name,
                                        vi.name = vi.name,
                                        CI.level = CI.level)
            
            lo.Phat = as.numeric( Phat.CI.lims[1] )
            hi.Phat = as.numeric( Phat.CI.lims[2] )
            SE.Phat = as.numeric( Phat.CI.lims[3] )
            
            if ( any( is.na( c(lo.Phat, hi.Phat, SE.Phat) ) ) ) {
                message("The confidence interval and/or standard error for the proportion were not estimable via bias-corrected and accelerated bootstrapping. You can try increasing R.")
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
            
            
            if ( any( is.na( c(lo.T, hi.T, SE.T, lo.G, hi.G, SE.G) ) ) ) {
                message("The confidence interval and/or standard error for Tmin and Gmin were not estimable via bias-corrected and accelerated bootstrapping. You can try increasing R.")
            }
            
        }  # closes "if ( !is.na(r) )"
        
    } # closes calibrated method
    
    ##### Messages about Results #####
    if ( exists("Tmin") ) {
        if ( !is.na(Tmin) & Tmin == 1 ) {
            message("Prop is already less than or equal to r even with no confounding, so Tmin and Gmin are simply equal to 1. No confounding at all is required to make the specified shift.")
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





sens_plot = function(method="calibrated",
                     type,
                     q,
                     CI.level=0.95,
                     tail=NA,
                     muB.toward.null = FALSE,
                     give.CI=TRUE,
                     Bmin = log(1),
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
    # Bmin = log(1)
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
    # Bmin = log(1)
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
                tail = ifelse( yr > log(1), "above", "below" )
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

                warning("Calculating parametric confidence intervals in the plot. For values of Phat that are less than 0.15 or greater than 0.85, these confidence intervals may not perform well.")
            }


        } ## closes method=="parametric"


        if ( method == "calibrated" ) {

            # if tail isn't provided, assume user wants the more extreme one (away from the null)
            if ( is.na(tail) ) {
                calib = calib_ests( yi = dat[[yi.name]],
                                    sei = sqrt( dat[[vi.name]] ) )

                tail = ifelse( median(calib) > log(1), "above", "below" )
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
                    message( "None of the pointwise confidence intervals was estimable via bias-corrected and accelerated bootstrapping, so the confidence band on the plot is omitted. You can try increasing R." )
                    # avoid even trying to plot the CI if it's always NA to avoid geom_ribbon errors later
                    give.CI = FALSE
                }


                # outer "if" handles case in which AT LEAST ONE CI limit is NA because of boot failures
                if ( any( !is.na(res$lo) ) & any( !is.na(res$hi) ) ) {

                    message( "Some of the pointwise confidence intervals were not estimable via bias-corrected and accelerated bootstrapping, so the confidence band on the plot may not be shown for some values of the bias factor. This usually happens at values with a proportion estimate close to 0 or 1. Otherwise, you can try increasing R." )

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



g = Vectorize( function(x) {
    # define transformation in a way that is monotonic over the effective range of B (>1)
    # to avoid ggplot errors in sens_plot
    # helper function for confounded_meta
    if ( is.na(x) ) return(NA)
    if (x < 1) return( x / 1e10 )
    x + sqrt( x^2 - x )
} )



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



####### FROM EVALUE.R


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



evalue.RR = function( est, lo = NA, hi = NA, se = NA, delta = NA, true = 1, ... ){
    evalues.RR(est = est, lo = lo, hi = hi, true = true, ...)
}


evalue.OR = function(est, lo = NA, hi = NA, se = NA, delta = NA, true = 1, ...){
    evalues.OR(est = est, lo = lo, hi = hi, true = true, ...)
}


evalue.HR = function(est, lo = NA, hi = NA, se = NA, delta = NA,true = 1, ...){
    evalues.HR(est = est, lo = lo, hi = hi, true = true, ...)
}


evalue.OLS = function(est, lo = NA, hi = NA, se = NA, delta = 1, true = 0, ...){
    evalues.OLS(est, se = se, delta = delta, true = true, ...)
}


evalue.MD = function(est, lo = NA, hi = NA, se = NA, delta = NA, true = 0, ...){
    evalues.MD(est, se = se, true = true, ...)
}



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


evalue = function( est, lo = NA, hi = NA, se = NA, delta = 1, true = c(0, 1), ... ) {
    UseMethod( "evalue")
}


summary.evalue = function( object, ... ) {
    if ( !inherits(object, "evalue")) stop('Argument must be of class "evalue"')
    object[2,1]
}


print.evalue = function( x, ... ) {
    class(x) <- "matrix" # to suppress attr printing
    print.default(x)
}









