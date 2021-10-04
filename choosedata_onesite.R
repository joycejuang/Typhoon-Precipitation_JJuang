choosedata_onesite <- function(site, nn, FCSTtime, translated=T){

  ## note1: need to run function 'read_tydata2_cm' and set NoMissing=T to get the typhoon's data first.
  ## note2: will get 1.ensemble members' mean, 2.ensemble members' sd, 3.observation, 4.ensemble members (20)

  ## 1. site: means 'grid', which grids you choose (single answer).
  ## 2. nn: which tyhoon you want to predict, enter typhoon number.
  ## 3. FCSTtime: which forecast time's data you want to choose.
  ## 4. translated: whether translated data in function 'read_tydata2_cm' (which trans="log10" or "unit_cm"), 
  ##                if yes then translated=T (True). cause their column will be different.

  data <- c()
  for (i in nn){
    for (j in FCSTtime){
      if (translated!=T){
        tt1 <- paste0("PTY", sprintf("%02d", i), "_", sprintf("%02d", j))
        eval(parse(text=paste0(tt1, " <- Alldata$", tt1)))
        data <- rbind(data, matrix(as.numeric(c(eval(parse(text=paste0(tt1, "[", tt1, "$site==", site, ",23]"))),           ## mean
                                     eval(parse(text=paste0(tt1, "[", tt1, "$site==", site, ",25]"))),           ## sd
                                     eval(parse(text=paste0(tt1, "[", tt1, "$site==", site, ",1]"))),            ## obs
                                     eval(parse(text=paste0(tt1, "[", tt1, "$site==", site, ",2:21]"))))), 1, 23)) 
      } else {
        tt1 <- paste0("PTY", sprintf("%02d", i), "_", sprintf("%02d", j))
        eval(parse(text=paste0(tt1, " <- Alldata$", tt1)))
        data <- rbind(data, matrix(as.numeric(c(eval(parse(text=paste0(tt1, "[", tt1, "$site==", site, ",47]"))),           ## mean
                                     eval(parse(text=paste0(tt1, "[", tt1, "$site==", site, ",49]"))),           ## sd
                                     eval(parse(text=paste0(tt1, "[", tt1, "$site==", site, ",26]"))),           ## obs
                                     eval(parse(text=paste0(tt1, "[", tt1, "$site==", site, ",27:46]"))))), 1, 23))
      }
    }
  }
  
  return(data)
  
}
