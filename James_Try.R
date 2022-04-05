library(R.matlab)
library(tidyverse)
library(dplyr)
library(ggplot2)

##Mouse 412
Mouse_412_behavior=readMat("Data/Opp_Sex/608102_412/Day_1/Trial_002_0/binned_behavior.mat")
M412_b <- as.data.frame(t(Mouse_412_behavior$binned.behavior))
M412_b <- M412_b %>% mutate(Act =
                     case_when(V1 == 1 ~ "A", 
                               V2 == 1 ~ "B",
                               V1 == V2 ~ "C")
)
#View(M412_b)
table(M412_b$Act)

Mouse_412_zscore=readMat("Data/Opp_Sex/608102_412/Day_1/Trial_002_0/binned_zscore.mat")
M412_z <- as.data.frame(Mouse_412_zscore$binned.zscore)
M412_z$avg <- rowMeans(M412_z, na.rm=TRUE)
M412_z$sum <- rowSums(M412_z, na.rm = TRUE)
#View(M412_z)



total412 <- cbind(M412_b, M412_z)
value_412 <- total412[,c(1,2,50,51)]
value_412$time <- c(1:6300)
#View(value_412)

#Time Series EDA for 412
ggplot(data = value_412)+
  geom_line(mapping = aes(x = time, y = avg))+
  geom_point(mapping = aes(x = time, y = V1), col = 'Red', size = 0.01)+
  geom_point(mapping = aes(x = time, y = V2), col = 'Blue', size = 0.01)

##Mouse 417
Mouse_417_behavior=readMat("Data/Opp_Sex/608103_417/Day_2/Trial_002_0/binned_behavior.mat")
M417_b <- as.data.frame(t(Mouse_417_behavior$binned.behavior))
#View(M417_b)

Mouse_417_zscore=readMat("Data/Opp_Sex/608103_417/Day_2/Trial_002_0/binned_zscore.mat")
M417_z <- as.data.frame(Mouse_417_zscore$binned.zscore)
M417_z$avg <- rowMeans(M417_z, na.rm=TRUE)
M417_z$sum <- rowSums(M417_z, na.rm = TRUE)
#View(M417_z)

total417 <- cbind(M417_b, M417_z)
value_417 <- total417[,c(1,2,43,44)]
value_417$time <- c(1:6858)
#View(value_417)

#Time Series for 417
ggplot(data = value_417)+
  geom_line(mapping = aes(x = time, y = avg))+
  geom_point(mapping = aes(x = time, y = V1), col = 'Red', size = 0.01)+
  geom_point(mapping = aes(x = time, y = V2), col = 'Blue', size = 0.01)

##mouse 416
Mouse_416_behavior=readMat("Data/Opp_Sex/608103_416/Day_2/Trial_002_0/binned_behavior.mat")
M416_b <- as.data.frame(t(Mouse_416_behavior$binned.behavior))
M416_b <- M416_b %>% mutate(Act =
                              case_when(V1 == 1 ~ "male", 
                                        V2 == 1 ~ "female",
                                        V1 == V2 ~ "no touch")
)
#View(M416_b)

Mouse_416_zscore=readMat("Data/Opp_Sex/608103_416/Day_2/Trial_002_0/binned_zscore.mat")
M416_z <- as.data.frame(Mouse_416_zscore$binned.zscore)
M416_z$avg <- rowMeans(M416_z, na.rm=TRUE)
M416_z$sum <- rowSums(M416_z, na.rm = TRUE)
#View(M416_z)

##Mouse 409
Mouse_409_behavior=readMat("Data/Opp_Sex/608034_409/Day_1/Trial_002_0/binned_behavior.mat")
M409_b <- as.data.frame(t(Mouse_409_behavior$binned.behavior))
M409_b <- M409_b %>% mutate(Act =
                              case_when(V1 == 1 ~ "male", 
                                        V2 == 1 ~ "female",
                                        V1 == V2 ~ "no touch")
)
#View(M409_b)

Mouse_409_zscore=readMat("Data/Opp_Sex/608034_409/Day_1/Trial_002_0/binned_zscore.mat")
M409_z <- as.data.frame(Mouse_409_zscore$binned.zscore)
M409_z$avg <- rowMeans(M409_z, na.rm=TRUE)
M409_z$sum <- rowSums(M409_z, na.rm = TRUE)
#View(M409_z)



