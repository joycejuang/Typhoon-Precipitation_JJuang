read_tydata_trans <- function(nn, NoMissing=T, trans, CWBpath=CWBpath){
  
  ## 1. nn: select the typhoon number you want to read.
  ## 2. NoMissing: whether delete unavailable data, if yes then NoMissing=T (True).
  ## 3. trans: if trans = "log10", use log10 to translate data and calculate log10(ensemble members)'s mean, var, sd
  ##           if trans = "unit_cm", change data's unit from 'mm' to 'cm' (use /10)
  ## 4. CWBpath: CWB typhoon data's path
  
  setwd(CWBpath)
  
  ty_name <- c(sprintf("TY%02d", seq(1, 12)), "Others")  ## numbers of typhoon files name
  alltimes <- c(7, 4, 7, 12, 11, 12, 15, 11, 12, 15, 15, 13, 3)  ## each typhoon's ensemble members total forecast time
  
  mydata <- list() 
  nn <- nn  ## which typhoon you want to read
  
  for (i in nn) {
    for (j in 1:alltimes[i]){
      
      # ------ read data ------
      # produce eg. "TY01_01" data.frame
      ttt <- sprintf("%s_%02d", ty_name[i], j)
      tt2 <- paste("'./00-24_rds/", ttt, ".rds'", sep="")
      eval(parse(text=paste(ttt, " <- readRDS(file=", tt2, ")", sep="")))
      eval(parse(text=paste(ttt, "$site <- seq(1:15429)", sep="")))
      
      if (NoMissing!=T){
        
        # ------ fuction output original data only, include unavailable data ------
        eval(parse(text = paste0("mydata$", ttt, " <- ", ttt)))
        
      } else {
        
        # ------ data processing ------
        # produce eg. "PTY01_01" data.frame
        
        # ------ delete unavailable data (V1=-1) ------ 
        ptt <- paste0("P", ttt)
        eval(parse(text=sprintf("%s <- %s[which(%s[,1]!=-1),]", ptt, ttt, ttt)))
        ## PTY10_01 <- TY10_01[which(TY10_01[,1]!=-1),]  ### id didn't change after delete V1=-1 ##
        
        # ------ "no translate" data col.23-25 ------
        {
          # ----------------------------------------------------------------#
          # col.1: observation / col.2-21: emsemble members / col.22: site  #
          # col.23-25: ens members' mean, variance, standard deviation      #
          # ----------------------------------------------------------------#
          
          eval(parse(text=sprintf("%s$ens_mm <- apply(%s[,2:21], 1, mean)", ptt, ptt)))
          ### PTY10_01$ens_mm <- apply(PTY10_01[,2:21], 1, mean)   ### add variable mean
          eval(parse(text=sprintf("%s$ens_vv <- apply(%s[,2:21], 1, var)", ptt, ptt)))
          ### PTY10_01$ens_vv <- apply(PTY10_01[,2:21], 1, var)  ### add variable variance: var(x)
          eval(parse(text=sprintf("%s$ens_ss <- sqrt(%s$ens_vv)", ptt, ptt)))
          ### PTY10_01$ens_ss <- sqrt(PTY10_01$vv) ### add variable
        }
        
        # ------ add "log10" data, col.26-49 -------
        {
          # ----------------------------------------------------------------#
          # col.26: log10(obs) / col.27-46: log10(ens members)              #
          # col.47-49: log10(ens members)'s mean, variance, sd              #
          # ----------------------------------------------------------------#
          
          if (trans=="log10"){
            # [ Gamma data (obs + ens members) translate to Normal by log10() ]
            for (l in 1:21){
              eval(parse(text=sprintf("%s[,25+l] <- ifelse(%s[,l]==0, log10(%s[,l]+0.001), log10(%s[,l]))", ptt, ptt, ptt, ptt)))
              ### PTY10_01[,25+(1:21)] <- ifelse(PTY10_01[,1:21]==0, log10(PTY10_01[,1:21]+0.001), log10(PTY10_01[,1:21]))   ### log observation
            }
            eval(parse(text=sprintf("%s$log10ens_mm <- apply(%s[,27:46], 1, mean)", ptt, ptt)))
            ### PTY10_01$mm <- apply(PTY10_01[,27:46], 1, mean)   ### add variable mean
            eval(parse(text=sprintf("%s$log10ens_vv <- apply(%s[,27:46], 1, var)", ptt, ptt)))
            ### PTY10_01$vv <- apply(PTY10_01[,27:46], 1, var)  ### add variable variance: var(x)
            eval(parse(text=sprintf("%s$log10ens_ss <- sqrt(%s$log10ens_vv)", ptt, ptt)))
            ### PTY10_01$ss <- sqrt(PTY10_01$vv) ### add variable
          }
        }
        
        # ------ add "unit_cm" data, col.26-49 -------
        {
          # ----------------------------------------------------------------#
          # col.26: unit='cm' observation                                   #
          # col.27-46: unit='cm' ensemble members                           #
          # col.47-49: unit='cm' ens members' mean, variance, sd            #
          # ----------------------------------------------------------------#
          
          if (trans=="unit_cm"){
            ### obs and ensemble member's unit change to 'cm' (only col.1 - col.21) ###
            for (l in 1:21){
              eval(parse(text=sprintf("%s[,25+l] <- %s[,l]/10", ptt, ptt)))
              ### PTY10_01[,(26:46)] <- PTY10_01[,(1:21)]/10
            }
            eval(parse(text=sprintf("%s$CMens_mm <- apply(%s[,27:46], 1, mean)", ptt, ptt)))
            ### PTY10_01$ens_mm <- apply(PTY10_01[,2:21], 1, mean)   ### add variable mean
            eval(parse(text=sprintf("%s$CMens_vv <- apply(%s[,27:46], 1, var)", ptt, ptt)))
            ### PTY10_01$ens_vv <- apply(PTY10_01[,2:21], 1, var)  ### add variable variance: var(x)
            eval(parse(text=sprintf("%s$CMens_ss <- sqrt(%s$CMens_vv)", ptt, ptt)))
            ### PTY10_01$ens_ss <- sqrt(PTY10_01$ens_vv) ### add variable
          }
        }
        
        # ------ function output processed data ------
        eval(parse(text = sprintf("mydata$%s <- %s", ptt, ptt)))
        
      }
    }
  }
  
  return(mydata)
  
}
