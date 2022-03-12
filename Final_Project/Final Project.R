
library(tidyverse)
library(lubridate)
library(DataExplorer)
library(data.table)
library(plyr)
library(readr)
library(fs)
library(ggplot2)
install.packages("glmer")
library(glmer)
library(lme4)
install.packages("lattice")                       
library(lattice) 

# Read all files first. 
col_names  <-  c("key_resp.keys","key_resp.rt","participant","session", "date", "condition",
                 "CorrResp_Train", "Part_Resp_Train", "Code_Train", "CorrResp_Test", "Part_Resp_Test", "Code_Test")
Individual_Files <-list.files()
AllFiles<-Individual_Files
df <- read_csv(AllFiles, col_names = col_names, skip =7)


#Delete the unnecessary columns. 

##An easy way to delete columns. 
df_final<-subset (df, select = -c (key_resp.keys,key_resp.rt, session,date))

## writing functions to remove columns. 
Column_Remove <- function(s1){
    df<- subset (df, select = -c (key_resp.keys,key_resp.rt, session,date))
  return(df)
}

df_final_function <- Column_Remove(df)


## Using for loops to run a series of regression analyses. 
pred <- c("Code_Test", "Code_Train")
for (p in pred) {
  f <- as.formula(paste("Code_Test ~ ", p))
  res <- lm(f, data = df_final)
  print(p)
  print(summary(res))
}


## Box plot.
AO <- subset(df_final, condition == "AO") 
AV <- subset(df_final, condition == "AV")
MO <- subset(df_final, condition == "MO")

## Box plot for training. 
AO_mean_train <- mean(AO$Code_Train)
AV_mean_train <- mean(AV$Code_Train)
MO_mean_train <- mean (MO$Code_Train)
df_mean <- data.frame(AO_mean_train, AV_mean_train,MO_mean_train)
plot_train<- ggplot(data=df_final, mapping=aes(x=condition, y=Code_Train)) + 
  stat_summary(fun.data=mean_sdl, geom="bar",
              fill=c("blue","red", "orange"))+
              ylim(0,1)+
              labs(title="Percent Correct During Training")
              plot_train+ theme_bw()

## Box plot for test. 
AO_mean_test <- mean(AO$Code_Test)
AV_mean_test <- mean(AV$Code_Test)
MO_mean_test <- mean (MO$Code_Test)
df_mean <- data.frame(AO_mean_test, AV_mean_test,MO_mean_test)
plot_test <- ggplot(data=df_final, mapping=aes(x=condition, y=Code_Test)) + 
  stat_summary(fun.data=mean_sdl, geom="bar",
               fill=c("blue","red", "orange"))+
               ylim(0,1)+
               labs(title="Percent Correct During Test")
               plot_test + theme_bw()
