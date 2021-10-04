regiondata <- function(offshoreislands_as_sea = T, Sea_remain = T){
  
  # ------------------------------------------------------------------------- #
  # 1. offshoreislands_as_sea:
  #   �N����k���b�q���ϰ쪺�~�q���I�������v (�s��:99)�A�����]�t���������� /
  #   consider the offshore island grids which were originally classified as 
  #   the island area as the ocean area (No. 99), but excluding 'Penghu', 'Jinmen', 'Matsu'
  # 2. Sea_remain: 
  #   �O�s���v��� / keep the ocean area's data
  # ------------------------------------------------------------------------- #
  
  input_data <- read.table("data_location.txt",header=F)
  grid <- input_data[,4]+(input_data[,3]-1)*139
  data <- cbind(input_data,grid)
  
  QPESUMS <- read.table("Table_QPESUMS_6a.txt",header=F)
  
  Index <- which(QPESUMS[,1] %in% data[,5] & QPESUMS[,2] %in% data[,6])
  testing <- QPESUMS[Index,]
  
  if(offshoreislands_as_sea == T){ testing[c(6579,9770,9771,9782,9783,9909,9910,10048,10049),3] <- "99" }
  
  Map_Infor <- as.data.frame(cbind(data,testing[,3]))
  
  Map_Infor$region <- NA
  Map_Infor[which(Map_Infor[,10]=="23" | Map_Infor[,10]=="24" | Map_Infor[,10]=="25"), 11] <- "OI"
  ## Offshore Islands �~�q(�]�t����������)
  Map_Infor[which(Map_Infor[,10]=="111"), 11] <- "N"  ## �_
  Map_Infor[which(Map_Infor[,10]=="222"), 11] <- "M"  ## ��
  Map_Infor[which(Map_Infor[,10]=="333"), 11] <- "S"  ## �n
  Map_Infor[which(Map_Infor[,10]=="444"), 11] <- "EN"  ## �F�_
  Map_Infor[which(Map_Infor[,10]=="555"), 11] <- "E"  ## �F
  Map_Infor[which(Map_Infor[,10]=="666"), 11] <- "ES"  ## �F�n
  Map_Infor[which(Map_Infor[,10]=="99"), 11] <- "Sea"  ## ���v
  
  ifelse(Sea_remain == T,
         Final_data <- Map_Infor,
         Final_data <- Map_Infor[which(Map_Infor[,7]!="S"),])

  return(Final_data)
}