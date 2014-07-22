library(shiny)
library(shinyAce)
library(meta)
library(metafor)
library(MAd)
library(MAc)



shinyServer(function(input, output) {
    
    options(warn=-1)
    
    
# First calculation to be used later
    W.data <- reactive({
        
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
            dat$W  <- round(1/dat$SV, 3)
            
            list(dat = dat) # To be used later
        }
        
        
        else if (input$type == "mdes") {
            
            df <- (dat$N1 + dat$N2) - 2
            j <- 1 - (3/(4 * df - 1))
            g <- j * dat$d
            dat$ES <- round(g, 3)
            
            dat$SV <- round((((dat$N1+dat$N2)/(dat$N1*dat$N2))+((dat$ES*dat$ES)/(2*(dat$N1+dat$N2)))),3)
            
            dat$W  <- round(1/dat$SV, 3)


            list(dat = dat) # To be used later
        }
        
        
        else if (input$type == "cor") {
            
            dat <- read.csv(text=input$text, sep="\t")
            
            dat <- escalc(measure="ZCOR", ni=N, ri=r, data=dat, append=TRUE)
            dat$FZ <- round(dat$yi,3)
            dat$yi <- NULL
            dat$SV <- round(dat$vi, 3) # SV=sampling variances
            dat$vi <- NULL

            list(dat = dat) # To be used later
            
        }
    })
    
    
    


# Fixed effects model to be used later
    FE.est <- reactive({
        
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
            
            list(FE.res = FE.res) # To be used later
        }
        
        else if (input$type == "mdes") {
            
            df <- (dat$N1 + dat$N2) - 2
            j <- 1 - (3/(4 * df - 1))
            g <- j * dat$d
            dat$ES <- round(g, 3)
            
            dat$SV <- round((((dat$N1+dat$N2)/(dat$N1*dat$N2))+((dat$ES*dat$ES)/(2*(dat$N1+dat$N2)))),3)
            
            FE.res <- rma(ES, SV, method="FE", data=dat, slab=paste(Study))
            
            list(FE.res = FE.res) # To be used later
        }
        
        
        else if (input$type == "cor") {
            
            dat <- escalc(measure="ZCOR", ni=N, ri=r, data=dat, append=TRUE)
            dat$FZ <- round(dat$yi,4)
            dat$yi <- NULL
            dat$SV <- round(dat$vi, 4) # SV=sampling variances
            dat$vi <- NULL
            
            FE.res <- rma(FZ, SV, data=dat, method = "FE", slab=paste(Study))
            
            list(FE.res = FE.res) # To be used later
        }
    })
    
    
    
    
    
# Random effects model to be used later
    RE.est  <- reactive({
        
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
            
            list(RE.res = RE.res) # To be used later
        }
        
        
        else if (input$type == "mdes") {
            
            df <- (dat$N1 + dat$N2) - 2
            j <- 1 - (3/(4 * df - 1))
            g <- j * dat$d
            dat$ES <- round(g, 3)
            
            dat$SV <- round((((dat$N1+dat$N2)/(dat$N1*dat$N2))+((dat$ES*dat$ES)/(2*(dat$N1+dat$N2)))),3)
            
            RE.res <- rma(ES, SV, method="REML", data=dat, slab=paste(Study))
            
            list(RE.res = RE.res) # To be used later
        }
        
        
        else if (input$type == "cor") {
            
            dat <- escalc(measure="ZCOR", ni=N, ri=r, data=dat, append=TRUE)
            dat$FZ <- round(dat$yi,4)
            dat$yi <- NULL
            dat$SV <- round(dat$vi, 4) # SV=sampling variances
            dat$vi <- NULL
            
            RE.res <- rma(FZ, SV, data=dat, method = "REML", slab=paste(Study))

            list(RE.res = RE.res) # To be used later
            
        }
    })
    
    
    
    

################################################
# Displaying the first calculation
################################################

    data <- reactive({
    
        dat <- read.csv(text=input$text, sep="\t")
    
    
        if (input$type == "mdms") {
        
            dat <- W.data()$dat
        
            cat("\n","ES = Effect size [Hedges's g]", "\n",
                "SV = Sampling variance [sqrt(SV) = Std err]", "\n",
                " W = Inverse variance weight [1/SV]", "\n", "\n")
                cat("---","\n")
        
            print(dat)
        }
    
    
        else if (input$type == "mdes") {
        
            dat <- W.data()$dat
        
            cat("\n","ES = Effect size [Hedges's g]", "\n",
                "SV = Sampling variance [sqrt(SV) = Std err]", "\n",
                " W = Inverse variance weight [1/SV]", "\n", "\n")
                cat("---","\n")
        
            print(dat)
        }
    
    
        else if (input$type == "cor") {
        
            dat <- W.data()$dat
        
            cat("\n","FZ = Fisher's Z", "\n",
                "SV = Sampling variance [sqrt(SV) = Std err]", "\n", "\n")
                cat("---","\n")
        
            print(dat)
            
        }
    })





################################################
# FE & RE model result
################################################

    fe <- reactive({
        
        dat <- read.csv(text=input$text, sep="\t")


        if (input$type == "mdms") {
            
            FE.res <- FE.est()$FE.res
            
            cat("The FE model is a description of the K studies (Kovalchik, 2013).","\n")
            cat("---","\n")
            
            FE.res
        }
        
        
        else if (input$type == "mdes") {
            
            FE.res <- FE.est()$FE.res
            
            cat("The FE model is a description of the K studies (Kovalchik, 2013).","\n",
            "---","\n")
            
            FE.res
            
        }
        
        
        else if (input$type == "cor") { # Using different function here.
            
            dat <- read.csv(text=input$text, sep="\t")

            FE.res <- metacor(dat$r, dat$N)
            
            FE.res
        }
    })
    
    
    
    
    
    re <- reactive({
        
        if (input$type == "mdms") {
            
            RE.res <- RE.est()$RE.res
            
            cat("The RE model regards the K studies as a sample of","\n")
            cat(" a larger universe of studies (Kovalchik, 2013).","\n")
            cat("---","\n")
            
            RE.res
        }
        
        
        else if (input$type == "mdes") {
            
            RE.res <- RE.est()$RE.res

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
    
    
    


################################################
# Forest plot
################################################
    
    makefePlot <- function(){
        
        if (input$type == "mdms") {
            
            FE.res <- FE.est()$FE.res
            
            forest(FE.res)
        }
        
        
        else if (input$type == "mdes") {
            
            FE.res <- FE.est()$FE.res

            forest(FE.res)
        }
        
        
        else if (input$type == "cor") {
            
            FE.res <- FE.est()$FE.res

            forest(FE.res, transf=transf.ztor)
            
        }
    }
    
    
    output$fePlot <- renderPlot({
        print(makefePlot())
    })
    
    
    
    
    
    makerePlot <- function(){
        
        if (input$type == "mdms") {
            
            RE.res <- RE.est()$RE.res

            forest(RE.res)
            
        }
        
        
        else if (input$type == "mdes") {
            
            RE.res <- RE.est()$RE.res
            
            forest(RE.res)
            
        }
        
        
        else if (input$type == "cor") {
            
            RE.res <- RE.est()$RE.res

            forest(RE.res, transf=transf.ztor)
            
        }
    }
    
    
    output$rePlot <- renderPlot({
        print(makerePlot())
    })
    
    
    
    
    
################################################
# Funnel plot
################################################

    makeFunFixPlot <- function(){
        
        if (input$type == "mdms") {
            
            FE.res <- FE.est()$FE.res
            
            metafor::funnel(trimfill(FE.res))
            
        }
        
        
        else if (input$type == "mdes") {
            
            FE.res <- FE.est()$FE.res
            
            metafor::funnel(trimfill(FE.res))
            
        }
        
        
        else if (input$type == "cor") {
            
            FE.res <- FE.est()$FE.res
            
            metafor::funnel(trimfill(FE.res))
            
        }
    }
    
    
    output$FunFixPlot <- renderPlot({
        print(makeFunFixPlot())
    })
    
    
    
    
    
    makeFunRandPlot <- function(){
        
        if (input$type == "mdms") {
            
            RE.res <- RE.est()$RE.res
            
            metafor::funnel(trimfill(RE.res))
            
        }
        
        
        else if (input$type == "mdes") {
            
            RE.res <- RE.est()$RE.res

            metafor::funnel(trimfill(RE.res))
            
        }
        
        
        else if (input$type == "cor") {
            
            RE.res <- RE.est()$RE.res

            metafor::funnel(trimfill(RE.res))
            
        }
    }
    
    
    output$FunRandPlot <- renderPlot({
        print(makeFunRandPlot())
    })
    
    
    


################################################
# Test of asymmetry & Fail-safe N
################################################

    asy <- reactive({
        
        dat <- read.csv(text=input$text, sep="\t")
        
        
        if (input$type == "mdms") {
            
            RE.res <- RE.est()$RE.res

            regt <- regtest(RE.res, model="lm")
            value <- fsn(y = RE.res$yi, v = RE.res$vi)
            
            return(list('No publication bias if p > .05 (Nonsignificant)' = regt,
            'File drawer analysis' = value))
        }
        
        
        else if (input$type == "mdes") {
            
            RE.res <- RE.est()$RE.res
            
            regt <- regtest(RE.res, model="lm")
            value <- fsn(y = RE.res$yi, v = RE.res$vi)
            
            return(list('No publication bias if p > .05 (Nonsignificant)' = regt,
            'File drawer analysis' = value))
            
        }
        
        
        else if (input$type == "cor") {
            
            RE.res <- RE.est()$RE.res
            
            regt <- regtest(RE.res, model="lm")
            value <- fsn(y = RE.res$yi, v = RE.res$vi)
            
            return(list('No publication bias if p > .05 (Nonsignificant)' = regt,
            'File drawer analysis' = value))
        }
    })
    
    
    
    

################################################
# Moderator analysis
################################################

    modAnalysis <- reactive({
        
        if (input$moderator == 1) {
            
            
            if (input$type == "mdms") {
                
                dat <- W.data()$dat
                
                fixed <- MAd::macat(ES, SV, mod = Moderator, data=dat, method= "fixed")
                random <- MAd::macat(ES, SV, mod = Moderator, data=dat, method= "random")
                
                cat("---", "\n", "Fixed effects model:", "\n")
                print(fixed)
                
                cat("\n", "\n", "---", "\n", "Random effects model:", "\n")
                print(random)
                
            }
            
            
            else if (input$type == "mdes") {
                
                dat <- W.data()$dat

                fixed <- MAd::macat(ES, SV, mod = Moderator, data=dat, method= "fixed")
                random <- MAd::macat(ES, SV, mod = Moderator, data=dat, method= "random")
                
                cat("---", "\n", "Fixed effects model:", "\n")
                print(fixed)
                
                cat("\n", "\n", "---", "\n", "Random effects model:", "\n")
                print(random)
                
            }
            
            
            else if (input$type == "cor") {
                
                dat <- W.data()$dat
                dat$var.z <- var_z(dat$N) # 正確な値を計算するために追加
                
                # Fixed effects
                fixed <- MAc::macat(FZ, var.z, mod = Moderator, data=dat, ztor = TRUE, method= "fixed")
                z.fixed <- MAc::macat(FZ, var.z, mod = Moderator, data=dat, ztor = FALSE, method= "fixed") # Accurate z and p
                
                # Random effects
                random <- MAc::macat(FZ, var.z, mod = Moderator, data=dat, ztor = TRUE, method= "random")
                z.random <- MAc::macat(FZ, var.z, mod = Moderator, data=dat, ztor = FALSE, method= "random") # Accurate z and p
                
                
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
    
    
    
    
    
################################################
# R session info
################################################

    info <- reactive({
        info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")# バージョン情報
        info2 <- paste("It was executed on ", date(), ".", sep = "")# 実行日時
        cat(sprintf(info1), "\n")
        cat(sprintf(info2), "\n")
    })
    
    
    
    
    
################################################
# server.R and ui.R connection
################################################

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
