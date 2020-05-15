source("startup.R")

function(input, output, session) {

    ##### For Tab Panel 2 ##### 
    output$text1 = renderText({

        yr.2 = input$yr.2
        t2.2 = input$t2.2
        q.2 = input$q.2
        vyr.2 = input$vyr.2
        vt2.2 = input$vt2.2
        muB.2 = input$muB.2
        sigB.2 = input$sigB.2
        r.2 = input$r.2
        
        cm = confounded_meta(q=q.2, r = r.2, muB = muB.2, sigB = sigB.2, yr = yr.2, vyr = vyr.2,
                t2 = t2.2, vt2 = vt2.2, CI.level = 0.95)
        
        p = round( cm$Est[ cm$Value=="Prop" ], 3 )
        p.lo = round( cm$CI.lo[ cm$Value=="Prop" ], 3 )
        p.hi = round( cm$CI.hi[ cm$Value=="Prop" ], 3 )
    
        
        ##### Create String for UI ##### 
        string.p = paste( p, " (95% CI: ", p.lo, ", ", p.hi, ")", sep="" )
        return( string.p )
        
    })
    

    ##### For Tab Panel 2 ##### 
    output$text2 = renderText({
        
        yr.2 = input$yr.2
        t2.2 = input$t2.2
        q.2 = input$q.2
        vyr.2 = input$vyr.2
        vt2.2 = input$vt2.2
        muB.2 = input$muB.2
        sigB.2 = input$sigB.2
        r.2 = input$r.2
        
        
        cm = confounded_meta(q=q.2, r = r.2, muB = muB.2, sigB = sigB.2, yr =yr.2, vyr = vyr.2,
                              t2 = t2.2, vt2 = vt2.2, CI.level = 0.95)
        
        Tmin = round( cm$Est[ cm$Value=="Tmin" ], 3 )
        Tmin.lo = round( cm$CI.lo[ cm$Value=="Tmin" ], 3 )
        Tmin.hi = round( cm$CI.hi[ cm$Value=="Tmin" ], 3 )
        

        ##### Create String for UI ##### 
        string.Tmin = paste( Tmin, " (95% CI: ", Tmin.lo, ", ", Tmin.hi, ")", sep="" )
        return( string.Tmin )
        
    })
    
    ##### For Tab Panel 2 ##### 
    output$text3 = renderText({
        
        yr.2 = input$yr.2
        t2.2 = input$t2.2
        q.2 = input$q.2
        vyr.2 = input$vyr.2
        vt2.2 = input$vt2.2
        muB.2 = input$muB.2
        sigB.2 = input$sigB.2
        r.2 = input$r.2
        
        
        cm = confounded_meta(q=q.2, r = r.2, muB = muB.2, sigB = sigB.2, yr =yr.2, vyr = vyr.2,
                              t2 = t2.2, vt2 = vt2.2, CI.level = 0.95)
        
        Gmin = round( cm$Est[ cm$Value=="Gmin" ], 3 )
        Gmin.lo = round( cm$CI.lo[ cm$Value=="Gmin" ], 3 )
        Gmin.hi = round( cm$CI.hi[ cm$Value=="Gmin" ], 3 )
        
        
        ##### Create String for UI ##### 
        string.Gmin = paste( Gmin, " (95% CI: ", Gmin.lo, ", ", Gmin.hi, ")", sep="" )
        return( string.Gmin )
        
    })
    
    
  ##### For Tab Panel 1 #####   
  output$plot1 <- renderPlot({

      yr = input$yr
      t2 = input$t2
      q = input$q
      vyr = input$vyr
      vt2 = input$vt2
      sigB = input$sigB
      Bmin = input$Bmin
      Bmax = input$Bmax
      
      sens_plot( type="line", q=q, yr=yr, vyr=vyr, t2=t2, vt2=vt2,
                 Bmin=Bmin, Bmax=Bmax, sigB=sigB )       
  })

  ##### For Tab Panel 2 ##### 
  output$plot2 <- renderPlot({
      
     # observeEvent( input$make.plot, {
          yr.2 = input$yr.2
          t2.2 = input$t2.2
          q.2 = input$q.2
          vyr.2 = input$vyr.2
          vt2.2 = input$vt2.2
          muB.2 = input$muB.2
          sigB.2 = input$sigB.2
          r.2 = input$r.2
          
          
          sens_plot( type="dist", q=q.2, yr=yr.2, vyr=vyr.2, t2=t2.2, vt2=vt2.2,
                     muB=muB.2, sigB=sigB.2 ) 
         
          
   #   } )
      
  })
  
  
}





