library(tidyverse)
library(R.matlab)
library(fastICA)
library(zoo)

tmp=readMat("data/binned_zscore.mat")
file<-list.files(paste0(getwd(),'/data'))
tmp$binned.zscore %>% dim()
tmp$binned.zscore %>% view()

tmp_behavior=readMat("data/Opp_Sex/608102_412/Day_1/Trial_002_0/binned_behavior.mat")$binned.behavior
tmp_zscore=readMat("data/Opp_Sex/608102_412/Day_1/Trial_002_0/binned_zscore.mat")$binned.zscore
behavior<-data.frame(tmp_behavior)
zscore<-data.frame(tmp_zscore)
df<-cbind(zscore,t(x1))
rownames(df) <- NULL
df %>% select(X1:X47) 

set.seed(1)
re=fastICA(df %>% select(X1:X47), 2)
df_new<-cbind(re$S,t(x1)) %>% data.frame()
rownames(df_new) <- NULL
colnames(df_new)<-c('X1','X2','B1','B2')
df_new$Time=seq(1,dim(df_new)[1])
df_new %>% ggplot() + geom_line(aes(x=Time,y=X1),col='red',alpha=0.6)+geom_line(aes(x=Time,y=X2),alpha=0.6) +geom_point(aes(x=Time,y=B1*3),col='green',size=0.1)+geom_point(aes(x=Time,y=B2*-3),col='blue',size=0.1)+theme_bw()
df_new %>% ggplot() + geom_line(aes(x=Time,y=X2),col='red') +geom_point(aes(x=Time,y=B1*3),col='green',size=0.1)+geom_point(aes(x=Time,y=B2*-3),col='blue',size=0.1)+theme_bw()

#rollapply(df_new, width=63, function(x) cor(x[,1],x[,3]), by.column=FALSE)


df_new %>% mutate(behavior=as.factor(paste0(as.character(B1),as.character(B2)))) %>% ggplot()+
  geom_boxplot(aes(x=behavior,y=X1),outlier.shape = NA)+
  coord_cartesian(ylim = c(-2, 2))+
  scale_x_discrete(labels = c("no touch", "female","male"))


# 
# 
# pr.out<-prcomp(df,scale = TRUE)
# pr.var <- pr.out$sdev^2
# (pr.var / sum (pr.var)) %>% cumsum()
# 
# df_b<-df_new %>% select(B1,B2)
# df_b$B=0
# df_b[(df_b$B1==1 & df_b$B2==0),3]=1
# df_b[(df_b$B1==0 & df_b$B2==1),3]=2
# df_b[(df_b$B1==0 & df_b$B2==0),3]=3
# df_b[(df_b$B1==1 & df_b$B2==1),4]=4
# df_b %>% group_by(as.factor(B1),as.factor(B2)) %>% summarise(c=count(B))
# df_tmp<-df_b %>% count(B1,B2) %>% mutate(behavior=paste0(as.character(B1),as.character(B2)),prop= n/sum(n))
# df_tmp %>% ggplot()+geom_histogram()
# 
# 
# df_b %>% mutate(behavior=paste0(as.character(B1),as.character(B2)))

