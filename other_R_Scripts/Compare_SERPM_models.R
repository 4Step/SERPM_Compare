# Compare SERPM models (27th Ave and Flagler St):
library(dplyr)
library(tidyr)


dir      <- c("c:\\SERPM7_27AveModel", "c:\\SERPM7_FlaglerModel")
input    <- c("input\\IN-2014T\\ctramp")
output   <- c("output\\Out-2014T_25PercentSample\\ctramp")
infiles  <- c("maz_data.csv","hhFile.csv", "personFile.csv")
outfiles <- c("householdData_1.csv","personData_1.csv")
report   <- "C:\\projects\\SERPM_Compare"

getColSumDiff <- function(df1, df2){
  df_sum1 <- df1 %>% select(which(sapply(., is.numeric))) %>% colSums()
  df_sum2 <- df2 %>% select(which(sapply(., is.numeric))) %>% colSums()
  df_diff <- df_sum1 - df_sum2
  return(df_diff)
}

# Compare input files
for (i in 1:length(infiles)){
    fileName1 <- paste0(dir[1],"\\",input,"\\",infiles[i]) 
    fileName2 <- paste0(dir[2],"\\",input,"\\",infiles[i]) 
    df1 <- read.csv(fileName1)
    df2 <- read.csv(fileName2)  
    x <- getColSumDiff(df1,df2)
    write.csv(x,paste0(report,"\\compare_",infiles[i]), row.names = TRUE)
}


