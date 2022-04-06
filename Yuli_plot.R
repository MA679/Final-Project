library(tidyverse)
library(R.matlab)
library(fastICA)
library(zoo)
library(ggpubr)

tmp_behavior=readMat("data/Opp_Sex/608102_412/Day_1/Trial_002_0/binned_behavior.mat")$binned.behavior
tmp_zscore=readMat("data/Opp_Sex/608102_412/Day_1/Trial_002_0/binned_zscore.mat")$binned.zscore
behavior<-data.frame(tmp_behavior)
zscore<-data.frame(tmp_zscore)
df<-cbind(zscore,t(x1))
rownames(df) <- NULL
df %>% select(X1:X47) 

tmp_func<-function(x){
  re=paste0(x,'/',list.files(x))
  return(re)
}

get_dir<-function(){
  cur_list<-paste0('/',list.files(paste0(getwd(),'/Data/Opp_Sex')))
  dir=paste0(getwd(),'/Data/Opp_Sex')
  tmp1<-list.dirs(dir)
  tmp2=grep('Trial', tmp1, value = T)
  re=sapply(tmp2,tmp_func)
  return(re)
}

get_box<-function(tmp_behavior,tmp_zscore,title){
  behavior<-tmp_behavior
  zscore<-tmp_zscore
  set.seed(1)
  re=fastICA(zscore, 1)
  df_new<-cbind(re$S,t(behavior)) %>% data.frame()
  rownames(df_new) <- NULL
  colnames(df_new)<-c('X1','B1','B2')
  p<-df_new %>% mutate(behavior=as.factor(paste0(as.character(B1),as.character(B2)))) %>% ggplot()+
    geom_boxplot(aes(x=behavior,y=X1),outlier.shape = NA)+
    scale_x_discrete(labels = c("no touch", "female","male"))
  return(p)
}


read_file<-function(path){
  
  
}

#coord_cartesian(ylim = c(quantile(df_new$X1,0.01), quantile(df_new$X1,0.99)))+

get_box(tmp_behavior,tmp_zscore)

dd<-get_dir()
for (i in dd){
  print(i)
}
dd
getwd()
behav_409=readMat("Data/Opp_Sex/608034_409/Day_1/Trial_002_0/binned_behavior.mat")$binned.behavior
zscore_409=readMat("data/Opp_Sex/608034_409/Day_1/Trial_002_0/binned_zscore.mat")$binned.zscore

behav_412=readMat("Data/Opp_Sex/608102_412/Day_1/Trial_002_0/binned_behavior.mat")$binned.behavior
zscore_412=readMat("Data/Opp_Sex/608102_412/Day_1/Trial_002_0/binned_zscore.mat")$binned.zscore

behav_414=readMat("Data/Opp_Sex/608102_414/Day_2/Trial_002_0/binned_behavior.mat")$binned.behavior
zscore_414=readMat("Data/Opp_Sex/608102_414/Day_2/Trial_002_0/binned_zscore.mat")$binned.zscore

behav_416=readMat("Data/Opp_Sex/608103_416/Day_2/Trial_002_0/binned_behavior.mat")$binned.behavior
zscore_416=readMat("Data/Opp_Sex/608103_416/Day_2/Trial_002_0/binned_zscore.mat")$binned.zscore

behav_417=readMat("Data/Opp_Sex/608103_417/Day_2/Trial_002_0/binned_behavior.mat")$binned.behavior
zscore_417=readMat("Data/Opp_Sex/608103_417/Day_2/Trial_002_0/binned_zscore.mat")$binned.zscore

behav_418=readMat("Data/Opp_Sex/608103_418/Day_2/Trial_002_0/binned_behavior.mat")$binned.behavior
zscore_418=readMat("Data/Opp_Sex/608103_418/Day_2/Trial_002_0/binned_zscore.mat")$binned.zscore

plot409<-get_box(behav_409,zscore_409)+coord_cartesian(ylim = c(-3,3))+labs(title='Opp_sex 409: Boxplot of the first ICA')+theme(plot.title = element_text(hjust = 0.5))
plot412<-get_box(behav_412,zscore_412)+coord_cartesian(ylim = c(-3,3))+labs(title='Opp_sex 412: Boxplot of the first ICA')+theme(plot.title = element_text(hjust = 0.5))

plot414<-get_box(behav_414,zscore_414)+coord_cartesian(ylim = c(-3,3))+labs(title='Opp_sex 414: Boxplot of the first ICA')+theme(plot.title = element_text(hjust = 0.5))
plot416<-get_box(behav_416,zscore_416)+coord_cartesian(ylim = c(-3,3))+labs(title='Opp_sex 416: Boxplot of the first ICA')+theme(plot.title = element_text(hjust = 0.5))

plot417<-get_box(behav_417,zscore_417)+coord_cartesian(ylim = c(-3,3))+labs(title='Opp_sex 417: Boxplot of the first ICA')+theme(plot.title = element_text(hjust = 0.5))
plot418<-get_box(behav_418,zscore_418)+coord_cartesian(ylim = c(-3,3))+labs(title='Opp_sex 418: Boxplot of the first ICA')+theme(plot.title = element_text(hjust = 0.5))


ggarrange(plot409,plot412,plot414,plot416,plot417,plot418,ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom")
