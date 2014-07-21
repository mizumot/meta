library(shiny)
library(shinyAce)
library(meta)
library(metafor)
library(MAd)
library(MAc)



shinyServer(function(input, output) {
    
    options(warn=-1)
    
    
    
    
    data <- reactive({
        
        dat <- read.csv(text=input$text, sep="\t")
        
        
        if (input$type == "mdms") {
            
            dat <- escalc(measure="SMD", n1i=N1, n2i=N2,
            m1i=M1, m2i=M2,
            sd1i=SD1, sd2i=SD2,
            data=dat, append=TRUE)
            
            dat$ES <- round(dat$yi, 3)
            dat$yi <- NULL
            dat$SV <- round(dat$vi, 3) # SV=sampling variances
            dat$vi <- NULL
            
            cat("ES = Effect size [Hedges's g], SV = Sampling variance [sqrt(SV) = Std err]", "\n", "\n")
            
            print(dat)
        }
        
        
        else if (input$type == "mdes") {
            
            # variance weighting
            dat$SV <- round((((dat$N1+dat$N2)/(dat$N1*dat$N2))+((dat$ES*dat$ES)/(2*(dat$N1+dat$N2)))),4)
            
            cat("ES = Effect size [Hedges's g], SV = Sampling variance [sqrt(SV) = Std err]", "\n", "\n")
            
            print(dat)
        }
        
        
        else if (input$type == "cor") {
            
            dat <- escalc(measure="ZCOR", ni=n, ri=r, data=dat, append=TRUE)
            dat$FZ <- round(dat$yi,4)
            dat$yi <- NULL
            dat$SV <- round(dat$vi, 4) # SV=sampling variances
            dat$vi <- NULL
            
            cat("FZ = Fisher's Z, SV = Sampling variance [sqrt(SV) = Std err]", "\n", "\n")
            
            print(dat)
            
        }
        
        
        
    })
    
    
    
    
    
    fe <- reactive({
        
        if (input$type == "mdms") {
            
            dat <- read.csv(text=input$text, sep="\t")
            
            dat <- escalc(measure="SMD", n1i=N1, n2i=N2,
            m1i=M1, m2i=M2,
            sd1i=SD1, sd2i=SD2,
            data=dat, append=TRUE)
            
            dat$ES <- round(dat$yi, 3)
            dat$yi <- NULL
            dat$SV <- round(dat$vi, 3) # SV=sampling variances
            dat$vi <- NULL
            
            FE.res <- rma(ES, SV, method="FE", data=dat, slab=paste(Study))
            
            cat("The FE model is a description of the K studies (Kovalchik, 2013).","\n")
            cat("---","\n")
            
            FE.res
        }
        
        
        else if (input$type == "mdes") {
            
            # variance weighting
            dat <- read.csv(text=input$text, sep="\t")
            
            dat$SV <- round((((dat$N1+dat$N2)/(dat$N1*dat$N2))+((dat$ES*dat$ES)/(2*(dat$N1+dat$N2)))),4)
            
            FE.res <- rma(ES, SV, method="FE", data=dat, slab=paste(Study))
            
            cat("The FE model is a description of the K studies (Kovalchik, 2013).","\n",
            "---","\n")
            
            FE.res
            
        }
        
        
        else if (input$type == "cor") {
            
            dat <- read.csv(text=input$text, sep="\t")
            
            metacor(dat$r, dat$n)
            
        }
    })
    
    
    
    
    
    re <- reactive({
        
        dat <- read.csv(text=input$text, sep="\t")
        
        
        if (input$type == "mdms") {
            
            dat <- escalc(measure="SMD", n1i=N1, n2i=N2,
            m1i=M1, m2i=M2,
            sd1i=SD1, sd2i=SD2,
            data=dat, append=TRUE)
            
            dat$ES <- round(dat$yi, 3)
            dat$yi <- NULL
            dat$SV <- round(dat$vi, 3) # SV=sampling variances
            dat$vi <- NULL
            
            RE.res <- rma(ES, SV, method="REML", data=dat, slab=paste(Study))
            
            cat("The RE model regards the K studies as a sample of","\n")
            cat(" a larger universe of studies (Kovalchik, 2013).","\n")
            cat("---","\n")
            
            RE.res
        }
        
        
        else if (input$type == "mdes") {
            
            # variance weighting
            dat$SV <- round((((dat$N1+dat$N2)/(dat$N1*dat$N2))+((dat$ES*dat$ES)/(2*(dat$N1+dat$N2)))),4)
            
            RE.res <- rma(ES, SV, method="REML", data=dat, slab=paste(Study))
            
            cat("The RE model regards the K studies as a sample of","\n")
            cat(" a larger universe of studies (Kovalchik, 2013).","\n")
            cat("---","\n")
            
            RE.res
            
        }
        
        
        else if (input$type == "cor") {
            
            cat("Both FE and RE model results are reported above.","\n","\n")
            
            cat("---","\n")
            
            cat("The FE model is a description of the K studies.","\n")
            cat("The RE model regards the K studies as a sample of","\n")
            cat(" a larger universe of studies (Kovalchik, 2013).","\n")
            
        }
    })
    
    
    
    
    
    makefePlot <- function(){
        
        dat <- read.csv(text=input$text, sep="\t")
        
        if (input$type == "mdms") {
            
            dat <- escalc(measure="SMD", n1i=N1, n2i=N2,
            m1i=M1, m2i=M2,
            sd1i=SD1, sd2i=SD2,
            data=dat, append=TRUE)
            
            dat$ES <- round(dat$yi, 3)
            dat$yi <- NULL
            dat$SV <- round(dat$vi, 3) # SV=sampling variances
            dat$vi <- NULL
            
            FE.res <- rma(ES, SV, method="FE", data=dat, slab=paste(Study))
            
            forest(FE.res)
            
        }
        
        
        else if (input$type == "mdes") {
            
            dat$SV <- round((((dat$N1+dat$N2)/(dat$N1*dat$N2))+((dat$ES*dat$ES)/(2*(dat$N1+dat$N2)))),4)
            FE.res <- rma(ES, SV, method="FE", data=dat, slab=paste(Study))
            forest(FE.res)
            
        }
        
        
        else if (input$type == "cor") {
            
            dat <- escalc(measure="ZCOR", ni=n, ri=r, data=dat, append=TRUE)
            dat$FZ <- round(dat$yi,4)
            dat$yi <- NULL
            dat$SV <- round(dat$vi, 4) # SV=sampling variances
            dat$vi <- NULL
            
            res.fixed <- rma(FZ, SV, data=dat, method = "FE", slab=paste(Study))
            forest(res.fixed, transf=transf.ztor)
            
        }
    }
    
    
    output$fePlot <- renderPlot({
        print(makefePlot())
    })
    
    
    
    
    
    makerePlot <- function(){
        
        dat <- read.csv(text=input$text, sep="\t")
        
        if (input$type == "mdms") {
            
            dat <- escalc(measure="SMD", n1i=N1, n2i=N2,
            m1i=M1, m2i=M2,
            sd1i=SD1, sd2i=SD2,
            data=dat, append=TRUE)
            
            dat$ES <- round(dat$yi, 3)
            dat$yi <- NULL
            dat$SV <- round(dat$vi, 3) # SV=sampling variances
            dat$vi <- NULL
            
            RE.res <- rma(ES, SV, method="REML", data=dat, slab=paste(Study))
            
            forest(RE.res)
            
        }
        
        
        else if (input$type == "mdes") {
            
            dat$SV <- round((((dat$N1+dat$N2)/(dat$N1*dat$N2))+((dat$ES*dat$ES)/(2*(dat$N1+dat$N2)))),4)
            
            RE.res <- rma(ES, SV, method="REML", data=dat, slab=paste(Study))
            
            forest(RE.res)
            
        }
        
        
        else if (input$type == "cor") {
            
            dat <- escalc(measure="ZCOR", ni=n, ri=r, data=dat, append=TRUE)
            dat$FZ <- round(dat$yi,4)
            dat$yi <- NULL
            dat$SV <- round(dat$vi, 4) # SV=sampling variances
            dat$vi <- NULL
            
            res.random <- rma(FZ, SV, data=dat, slab=paste(Study))
            forest(res.random, transf=transf.ztor)
            
        }
    }
    
    
    output$rePlot <- renderPlot({
        print(makerePlot())
    })
    
    
    
    
    
    makeFunFixPlot <- function(){
        
        dat <- read.csv(text=input$text, sep="\t")
        
        if (input$type == "mdms") {
            
            dat <- escalc(measure="SMD", n1i=N1, n2i=N2,
            m1i=M1, m2i=M2,
            sd1i=SD1, sd2i=SD2,
            data=dat, append=TRUE)
            
            dat$ES <- round(dat$yi, 3)
            dat$yi <- NULL
            dat$SV <- round(dat$vi, 3) # SV=sampling variances
            dat$vi <- NULL
            
            FE.res <- rma(ES, SV, method="FE", data=dat, slab=paste(Study))
            
            metafor::funnel(trimfill(FE.res))
            
        }
        
        
        else if (input$type == "mdes") {
            
            dat$SV <- round((((dat$N1+dat$N2)/(dat$N1*dat$N2))+((dat$ES*dat$ES)/(2*(dat$N1+dat$N2)))),4)
            
            FE.res <- rma(ES, SV, method="FE", data=dat, slab=paste(Study))
            
            metafor::funnel(trimfill(FE.res))
            
        }
        
        
        else if (input$type == "cor") {
            
            dat <- escalc(measure="ZCOR", ni=n, ri=r, data=dat, append=TRUE)
            dat$FZ <- round(dat$yi,4)
            dat$yi <- NULL
            dat$SV <- round(dat$vi, 4) # SV=sampling variances
            dat$vi <- NULL
            
            res.fixed <- rma(FZ, SV, data=dat, method = "FE")
            
            metafor::funnel(trimfill(res.fixed))
            
        }
    }
    
    
    output$FunFixPlot <- renderPlot({
        print(makeFunFixPlot())
    })
    
    
    
    
    
    makeFunRandPlot <- function(){
        
        dat <- read.csv(text=input$text, sep="\t")
        
        if (input$type == "mdms") {
            
            dat <- escalc(measure="SMD", n1i=N1, n2i=N2,
            m1i=M1, m2i=M2,
            sd1i=SD1, sd2i=SD2,
            data=dat, append=TRUE)
            
            dat$ES <- round(dat$yi, 3)
            dat$yi <- NULL
            dat$SV <- round(dat$vi, 3) # SV=sampling variances
            dat$vi <- NULL
            
            RE.res <- rma(ES, SV, method="REML", data=dat, slab=paste(Study))
            
            metafor::funnel(trimfill(RE.res))
            
        }
        
        
        else if (input$type == "mdes") {
            
            dat$SV <- round((((dat$N1+dat$N2)/(dat$N1*dat$N2))+((dat$ES*dat$ES)/(2*(dat$N1+dat$N2)))),4)
            
            RE.res <- rma(ES, SV, method="REML", data=dat, slab=paste(Study))
            
            metafor::funnel(trimfill(RE.res))
            
        }
        
        
        else if (input$type == "cor") {
            
            dat <- escalc(measure="ZCOR", ni=n, ri=r, data=dat, append=TRUE)
            dat$FZ <- round(dat$yi,4)
            dat$yi <- NULL
            dat$SV <- round(dat$vi, 4) # SV=sampling variances
            dat$vi <- NULL
            
            res.random <- rma(FZ, SV, data=dat)
            metafor::funnel(trimfill(res.random))
            
        }
    }
    
    
    output$FunRandPlot <- renderPlot({
        print(makeFunRandPlot())
    })
    
    
    
    
    asy <- reactive({
        
        dat <- read.csv(text=input$text, sep="\t")
        
        
        if (input$type == "mdms") {
            
            dat <- escalc(measure="SMD", n1i=N1, n2i=N2,
            m1i=M1, m2i=M2,
            sd1i=SD1, sd2i=SD2,
            data=dat, append=TRUE)
            
            dat$ES <- round(dat$yi, 3)
            dat$yi <- NULL
            dat$SV <- round(dat$vi, 3) # SV=sampling variances
            dat$vi <- NULL
            
            RE.res <- rma(ES, SV, method="REML", data=dat, slab=paste(Study))
            
            regt <- regtest(RE.res, model="lm")
            value <- fsn(y = RE.res$yi, v = RE.res$vi)
            
            return(list('No publication bias if p > .05 (Nonsignificant)' = regt,
            'File drawer analysis' = value))
        }
        
        
        else if (input$type == "mdes") {
            
            dat$SV <- round((((dat$N1+dat$N2)/(dat$N1*dat$N2))+((dat$ES*dat$ES)/(2*(dat$N1+dat$N2)))),4)
            
            RE.res <- rma(ES, SV, method="REML", data=dat, slab=paste(Study))
            
            regt <- regtest(RE.res, model="lm")
            value <- fsn(y = RE.res$yi, v = RE.res$vi)
            
            return(list('No publication bias if p > .05 (Nonsignificant)' = regt,
            'File drawer analysis' = value))
            
        }
        
        
        else if (input$type == "cor") {
            
            dat <- escalc(measure="ZCOR", ni=n, ri=r, data=dat, append=TRUE)
            dat$FZ <- round(dat$yi,4)
            dat$yi <- NULL
            dat$SV <- round(dat$vi, 4) # SV=sampling variances
            dat$vi <- NULL
            
            res.random <- rma(FZ, SV, data=dat)
            
            regt <- regtest(res.random, model="lm")
            value <- fsn(y = RE.res$yi, v = RE.res$vi)
            
            return(list('No publication bias if p > .05 (Nonsignificant)' = regt,
            'File drawer analysis' = value))
        }
    })
    
    
    
    
    
    modAnalysis <- reactive({
        
        dat <- read.csv(text=input$text, sep="\t")
        
        
        if (input$moderator == 1) {
            
            
            if (input$type == "mdms") {
                
                dat <- escalc(measure="SMD", n1i=N1, n2i=N2,
                m1i=M1, m2i=M2,
                sd1i=SD1, sd2i=SD2,
                data=dat, append=TRUE)
                
                dat$ES <- round(dat$yi, 3)
                dat$yi <- NULL
                dat$SV <- round(dat$vi, 3) # SV=sampling variances
                dat$vi <- NULL
                
                fixed <- MAd::macat(ES, SV, mod = Moderator, data=dat, method= "fixed")
                random <- MAd::macat(ES, SV, mod = Moderator, data=dat, method= "random")
                
                cat("---", "\n", "Fixed effects model:", "\n")
                print(fixed)
                
                cat("\n", "\n", "---", "\n", "Random effects model:", "\n")
                print(random)
                
            }
            
            
            else if (input$type == "mdes") {
                
                dat$SV <- round((((dat$N1+dat$N2)/(dat$N1*dat$N2))+((dat$ES*dat$ES)/(2*(dat$N1+dat$N2)))),4)
                
                fixed <- MAd::macat(ES, SV, mod = Moderator, data=dat, method= "fixed")
                random <- MAd::macat(ES, SV, mod = Moderator, data=dat, method= "random")
                
                cat("---", "\n", "Fixed effects model:", "\n")
                print(fixed)
                
                cat("\n", "\n", "---", "\n", "Random effects model:", "\n")
                print(random)
                
            }
            
            
            else if (input$type == "cor") {
                
                dat <- escalc(measure="ZCOR", ni=n, ri=r, data=dat, append=TRUE)
                dat$FZ <- round(dat$yi,4)
                dat$yi <- NULL
                dat$SV <- round(dat$vi, 4) # SV=sampling variances
                dat$vi <- NULL
                
                
                # Fixed effects
                fixed <- MAc::macat(FZ, SV, mod = Moderator, data=dat, ztor = TRUE, method= "fixed")
                z.fixed <- MAc::macat(FZ, SV, mod = Moderator, data=dat, method= "fixed") # Accurate z and p
                
                # Random effects
                random <- MAc::macat(FZ, SV, mod = Moderator, data=dat, ztor = TRUE, method= "random")
                z.random <- MAc::macat(FZ, SV, mod = Moderator, data=dat, method= "random")
                
                
                cat("---", "\n", "Fixed effects model:", "\n")
                print(fixed)
                
                cat("\n", "Accurate z and p values:", "\n")
                print(z.fixed$Model[8:9])
                
                
                cat("\n", "\n", "---", "\n", "Random effects model:", "\n")
                print(random)
                
                cat("\n", "Accurate z and p values:", "\n")
                print(z.random$Model[8:9])
                
                
            }
            
        } else {
            
            cat("No moderator (subgroup) analysis is conducted.","\n")
            
        }
        
    })
    
    
    
    
    
    
    
    
    
    
    info <- reactive({
        info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")# バージョン情報
        info2 <- paste("It was executed on ", date(), ".", sep = "")# 実行日時
        cat(sprintf(info1), "\n")
        cat(sprintf(info2), "\n")
    })
    
    
    
    
    
    
    output$info.out <- renderPrint({
        info()
    })
    
    
    
    
    
    output$data.out <- renderPrint({
        data()
    })
    
    output$fe.out <- renderPrint({
        fe()
    })
    
    output$re.out <- renderPrint({
        re()
    })
    
    output$asy.out <- renderPrint({
        asy()
    })
    
    output$modAnalysis.out <- renderPrint({
        modAnalysis()
    })
    
})
