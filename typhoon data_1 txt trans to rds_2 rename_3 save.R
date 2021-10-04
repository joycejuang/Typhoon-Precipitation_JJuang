# [.txt trans to .rds]

# [ All data ] save data as .rds ----------
rm(list=ls())
setwd("D:/CWB_Data_Discussion")

dir.create("00-24_rds")  ## create files

first_category_name <- list.files("00-24_txt")  ## ��U���w��Ƨ������ɮצW��
dir <- paste("./00-24_txt/", first_category_name, sep="")  ## ������|(�X�ָ��|�P�ɮצW��)

ty_all_num <- c(paste("TY", sprintf("%02d", seq(1, 12)), sep=""), "Others")  ## �X�ӻ䭷
alltimes <- c(7, 4, 7, 12, 11, 12, 15, 11, 12, 15, 15, 13, 3)  ## �U�䭷����������

for (i in 1:13){
  for (j in 1:alltimes[i]){
    ttt <- paste(ty_all_num[i], "_", sprintf("%02d", j), sep="")
    tt2 <- paste("./00-24_rds/", ttt, ".rds", sep="")
    eval(parse(text=paste(ttt, " = read.table(dir[", ifelse(i==1, j, sum(alltimes[1:i-1])+j), "])", sep="")))
    eval(parse(text=paste("saveRDS(", ttt, ", '", tt2, "')", sep="")))
  }
}
