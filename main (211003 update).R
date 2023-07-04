# rm(list=ls())

# ------ packages ------
# install.packages("scoringRules", "stringr")
library(scoringRules)
library(stringr)

### before running, there are two parts (below here) need to set up first ###

# ------ file direction [set.1] ------
{
  # code's path & output saving path
  mainpath <- "/Users/joycejuang/Documents/®ð¶H/neural network/typhoon neural network postprocessing"
  # grids' information data path:
  gridpath <- "/Users/joycejuang/Documents/®ð¶H/rainmap/Map_Region_Data 2"
  # CWB typhoon data's path: 
  CWBpath <- "/Users/joycejuang/Documents/CWB_Data_Discussion"
}

# ----- main setting [set.2] ------
{
  ## modeling's training data sample size:
  TDSS <- 3
  ## input the proportion of training data weight
  TDweight <- c(1,1,1)
  # check: expect to be "TRUE"
  length(TDweight) == TDSS
  ## which typhoon you want to predict, input typhoon's number
  run_typhoon_num <- c(10,12)
  ## each typhoon's ensemble members total forecast time
  alltimes <- c(7, 4, 7, 12, 11, 12, 15, 11, 12, 15, 15, 13, 3)
  ## choose neural network construction:
  modelname <- c("FCN", "NN2N", "NN4N", "NN3H4N")[1]
}

### set up completed! ###

# ------ start!! for loop (for typhoon number you have chosen 'run_typhoon_num') ------
{
  sink("Rlog.txt")  ## record. If there is any 'NAN' output, will stop and be recorded here
  
  for (nn in run_typhoon_num){
    
    # ------ subroutine ------
    {
      setwd(mainpath)
      
      source("read_tydata_trans.R")  # read typhoon data
      source("regiondata.R")  # read site(grid) information
      source("choosedata_onesite.R")  # choose specific site(grid)'s typhoon data
      
      source("SoftPlus.R")  # activation function
      
      source('traincode.R')  # all model train function collection
      source("predictcode.R")  # all model predict function collection
    }
    
    # ------- read data ------
    print(nn)
    times <- alltimes[nn]  ## typhoon's ensemble members total forecast time
    Alldata <- read_tydata_trans(nn=nn, NoMissing=T, trans="unit_cm", CWBpath=CWBpath)
    
    # ------ modeling and predicting ------
    timestart <- Sys.time()
    
    ## setup unequal weight part
    weightlist <- (TDweight/sum(TDweight))*3
    ifelse(length(unique(TDweight))==1, (UEname=""), (UEname=sprintf("UE%s", str_c(TDweight, collapse = ""))))
    
    ## add new file for saving output
    setwd(mainpath)
    dir.create(sprintf("%s", tolower(modelname)))  # [path 00] model file, e.g. ".../fcn/..."
    dir.create(sprintf("%s/predict_TY%02d%s", tolower(modelname), nn, ifelse(length(unique(TDweight))==1, UEname, paste0("_", UEname))))  # [path 01] each grids output seperate, e.g. ".../fcn/predict_TY10/..."
    dir.create(sprintf("%s/predict2_TY%02d%s", tolower(modelname), nn, ifelse(length(unique(TDweight))==1, UEname, paste0("_", UEname))))  # [path 02] combine all grids output, e.g. ".../fcn/predict2_TY10/..."
    
    ## saving output file's path
    txtsave <- sprintf("%s/%s/predict_TY%02d%s", mainpath, tolower(modelname), nn, ifelse(length(unique(TDweight))==1, UEname, paste0("_", UEname)))  # [path 01]
    txtsave2 <- sprintf("%s/%s/predict2_TY%02d%s", mainpath, tolower(modelname), nn, ifelse(length(unique(TDweight))==1, UEname, paste0("_", UEname)))  # [path 02]
    
    ## require site's number
    site <- eval(parse(text=sprintf("Alldata$PTY%02d_01[,22]", nn)))
    # will be like:  site <- Alldata$PTY10_01[,22]
    
    ## every grids and forecast time modeling and predicting seperately
    allsitecrps <- matrix(NA, length(site), 6)  # for combining allsite output, just for observe roughly
    for (s in site){
      print(paste0("s=", s))  # grids
      data <- choosedata_onesite(s, nn, 1:times, translated=T)  # choose specific grid's data
      allpredict <- matrix(NA, times-TDSS, 4)  # for saving specific grid's all output
      for (t in 1:(times-TDSS)){
        print(paste0("t=", t))  # predict time
        train <- t:as.integer(t+TDSS-1)  # training data row
        test <- t+TDSS  # testing data row
        
        ## modeling
        eval(parse(text=sprintf("(precipitation.%s <- train.%s(x=1:2, y=3, traindata=data[train,], testdata=data[test,], maxit=2000, random.seed=1234, unequalweight=weightlist))", tolower(modelname), tolower(modelname))))
        # will be like:  (precipitation.fcn <- train.fcn(x=1:2, y=3, traindata=data[train,], testdata=data[test,], maxit=2000, random.seed=1234, unequalweight=weightlist))
        
        ## predicting
        eval(parse(text=sprintf("(precipitation.predict <- predict.%s(precipitation.%s, data[test, 1:2]))", tolower(modelname), tolower(modelname))))
        # will be like:  (precipitation.predict <- predict.fcn(precipitation.fcn, data[test, 1:2]))
        
        testcrps <- crps_norm(matrix(as.numeric(data[test, 3]),ncol=1), mean=precipitation.predict[,1], sd=precipitation.predict[,2])
        allpredict[t, ] <- cbind(matrix(as.numeric(data[test, 3]),ncol=1), precipitation.predict, testcrps)
      }
      allpredict <- cbind(allpredict, matrix(crps_sample(as.numeric(matrix(data[(TDSS+1):times,3])), 
                                                         dat=matrix(as.numeric(data[(TDSS+1):times,4:23]), ncol=20)), times-TDSS, 1),
                          matrix(as.numeric(data[(TDSS+1):times,1:2]),ncol=2), 
                          matrix(as.numeric(data[(TDSS+1):times,4:23]),ncol=20))
      colnames(allpredict) <- c("obs", sprintf(paste0(tolower(modelname), UEname, "_%s"), c("mean", "sd", "crps")), 
                                sprintf("raw_%s", c("crps", "mean", "sd")), 1:20)
      saveRDS(allpredict, file = sprintf("%s/allpredict%05d.rds", txtsave, s))
      allsitecrps[match(s, site), ] <- cbind(s, sum(allpredict[,4]), mean(allpredict[,4]),
                                             sum(allpredict[,5]), mean(allpredict[,5]), 
                                             (mean(allpredict[,4])<=mean(allpredict[,5])))
    }
    colnames(allsitecrps) <- c("site", sprintf(paste0(tolower(modelname), UEname, "_crps_%s"), c("sum", "mean")), 
                               sprintf("raw_crps_%s", c("sum", "mean")), paste0(tolower(modelname), UEname, "_better"))
    saveRDS(allsitecrps, file = paste0(txtsave2, "/allsitecrps.rds"))
    timeend<-Sys.time()
    (runningtime<-timeend-timestart)
    
    # ------ summary data I (simply, roughly) -------
    {
      ## read data (allsitecrps)
      allsitecrps <- readRDS(paste0(txtsave2, "/allsitecrps.rds"))
      
      ## combine crps diff (allsitecrps), just for observe roughly
      allsitecrps <- cbind(allsitecrps, allsitecrps[,3]-allsitecrps[,5])
      colnames(allsitecrps)[7] <- c("crps_diff")
      
      ## save data (allsitecrps)
      write.csv(as.data.frame(allsitecrps), paste0(txtsave2, "/allsitecrps.csv"), row.names=F)
      # ------------------------------------ #
      
      ## combine the grids' information data (allsitecrps_region) ##
      ## include region data (allsitecrps_region)
      setwd(gridpath)
      regiondata <- regiondata(offshoreislands_as_sea = F, Sea_remain = T)  # get grids' data information
      names(regiondata)[9] <- "site"; names(regiondata)[10] <- "region_code"
      allsitecrps_region <- merge(x=allsitecrps, y=regiondata[,5:11], by="site", all.x=TRUE)
      write.csv(allsitecrps_region, paste0(txtsave2, "/allsitecrps_region.csv"), row.names=F)
      
      ## translate unit 'cm' to 'mm', and save data
      allsitecrps_region_mm <- allsitecrps_region
      allsitecrps_region_mm[,c(2:5,7)] <- allsitecrps_region_mm[,c(2:5,7)]*10
      write.csv(allsitecrps_region_mm, paste0(txtsave2, "/allsitecrps_region_mm.csv"), row.names=F)
    }
    
    # ------ summary data II (for each predict time and each site's output summary) ------
    {
      ## read data
      allsitecrps_region_mm <- read.csv(sprintf("%s/allsitecrps_region_mm.csv", txtsave2))
      
      for (i in 1:(times-TDSS)){
        ## produce each predict time's output summary data
        predict <- matrix(NA, length(site), 27)
        for (j in site){
          predict[match(j, site), ] <- readRDS(sprintf("%s/allpredict%05d.rds", txtsave, j))[i,]
        }
        predict <- cbind(site, predict)
        colnames(predict) <- c("site", "obs", sprintf(paste0(tolower(modelname), UEname, "_%s"), c("mean", "sd", "crps")),
                               sprintf("raw_%s", c("crps", "mean", "sd")), 1:20)
        write.csv(as.data.frame(predict), file = sprintf("%s/predict%02d_region.csv", txtsave2, i), row.names = F)
        
        ## combine region data
        eval(parse(text=sprintf("predict%02d <- cbind.data.frame(allsitecrps_region_mm[,8:13], predict%02d", i, i)))
        
        ## translate unit 'cm' to 'mm', and save data
        eval(parse(text=sprintf("predict%02d[,c(8:34)] <- predict%02d[,c(8:34)]*10", i, i)))
        eval(parse(text=sprintf("write.csv(predict%02d, '%s/predict%02d_region_mm.csv', row.names=F)", i, txtsave2, i)))
      }
    }
  }
  sink()
}

# ------ done!! ------
