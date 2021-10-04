# [.txt trans to .rds]

# [ All data ] save data as .rds ----------
rm(list=ls())
setwd("D:/CWB_Data_Discussion")

dir.create("00-24_rds")  ## create files

first_category_name <- list.files("00-24_txt")  ## 抓下指定資料夾中的檔案名稱
dir <- paste("./00-24_txt/", first_category_name, sep="")  ## 完整路徑(合併路徑與檔案名稱)

ty_all_num <- c(paste("TY", sprintf("%02d", seq(1, 12)), sep=""), "Others")  ## 幾個颱風
alltimes <- c(7, 4, 7, 12, 11, 12, 15, 11, 12, 15, 15, 13, 3)  ## 各颱風的紀錄次數

for (i in 1:13){
  for (j in 1:alltimes[i]){
    ttt <- paste(ty_all_num[i], "_", sprintf("%02d", j), sep="")
    tt2 <- paste("./00-24_rds/", ttt, ".rds", sep="")
    eval(parse(text=paste(ttt, " = read.table(dir[", ifelse(i==1, j, sum(alltimes[1:i-1])+j), "])", sep="")))
    eval(parse(text=paste("saveRDS(", ttt, ", '", tt2, "')", sep="")))
  }
}

