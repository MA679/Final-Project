library(R.matlab)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(glmnet)


##Mouse 412
Mouse_412_behavior=readMat("Data/Opp_Sex/608102_412/Day_1/Trial_002_0/binned_behavior.mat")
M412_b <- as.data.frame(t(Mouse_412_behavior$binned.behavior))
M412_b <- M412_b %>% mutate(Act =
                     case_when(V1 == 1 ~ "male", 
                               V2 == 1 ~ "female",
                               V1 == V2 ~ "no touch")
)
#View(M412_b)
table(M412_b$Act)

Mouse_412_zscore=readMat("Data/Opp_Sex/608102_412/Day_1/Trial_002_0/binned_zscore.mat")
M412_z <- as.data.frame(Mouse_412_zscore$binned.zscore)
M412_z$avg <- rowMeans(M412_z, na.rm=TRUE)
M412_z$sum <- rowSums(M412_z, na.rm = TRUE)
#View(M412_z)

total412 <- cbind(M412_b, M412_z)
value_412 <- total412[,c(3,51,52)]
value_412$time <- c(1:6300)
#View(value_412)

#Time Series EDA for 412
ggplot(data = value_412)+
  geom_line(mapping = aes(x = time, y = avg))+
  geom_point(mapping = aes(x = time, y = V1), size = 0.01)+
  geom_point(mapping = aes(x = time, y = V2), col = 'Blue', size = 0.01)

Mouse_412 <- ggplot(data = value_412)+
  #geom_line(mapping = aes(x = time, y = avg), col = 'Grey')+
  geom_point(mapping = aes(x = time, y = avg, col = Act, shape = Act),size = 0.5)
  

#---------------------------------------------------------------------------

##Mouse 417
Mouse_417_behavior=readMat("Data/Opp_Sex/608103_417/Day_2/Trial_002_0/binned_behavior.mat")
M417_b <- as.data.frame(t(Mouse_417_behavior$binned.behavior))
M417_b <- M417_b %>% mutate(Act =
                              case_when(V1 == 1 ~ "male", 
                                        V2 == 1 ~ "female",
                                        V1 == V2 ~ "no touch")
)
#View(M417_b)

Mouse_417_zscore=readMat("Data/Opp_Sex/608103_417/Day_2/Trial_002_0/binned_zscore.mat")
M417_z <- as.data.frame(Mouse_417_zscore$binned.zscore)
M417_z$avg <- rowMeans(M417_z, na.rm=TRUE)
M417_z$sum <- rowSums(M417_z, na.rm = TRUE)
#View(M417_z)

total417 <- cbind(M417_b, M417_z)
value_417 <- total417[,c(3,44,45)]
value_417$time <- c(1:6858)
#View(value_417)

#Time Series for 417
Mouse_417 <- ggplot(data = value_417)+
  #geom_line(mapping = aes(x = time, y = avg), col = 'Grey')+
  geom_point(mapping = aes(x = time, y = avg, col = Act), size = 0.01)



#---------------------------------------------------------------------------------------

##mouse 416
Mouse_416_behavior=readMat("Data/Opp_Sex/608103_416/Day_2/Trial_002_0/binned_behavior.mat")
M416_a <- as.data.frame(t(Mouse_416_behavior$binned.behavior))
M416_b <- M416_b %>% mutate(Act =
                              case_when(V1 == 1 ~ 1, 
                                        V2 == 1 ~ 2,
                                        V1 == V2 ~ 3)
)
#View(M416_b)

Mouse_416_zscore=readMat("Data/Opp_Sex/608103_416/Day_2/Trial_002_0/binned_zscore.mat")
M416_z <- as.data.frame(Mouse_416_zscore$binned.zscore)
M416_z$avg <- rowMeans(M416_z, na.rm=TRUE)
M416_z$sum <- rowSums(M416_z, na.rm = TRUE)
#View(M416_z)

total416 <- cbind(M416_b, M416_z)
value_416 <- total416[,c(3,17,18)]
value_416$time <- c(1:6301)
#View(value_416)

total416_a <- cbind(M416_a, M416_z)
value_416_a <- total416_a[,c(1,2,16,17)]

#Time Series for 416
Mouse_416 <- ggplot(data = value_416)+
  #geom_line(mapping = aes(x = time, y = avg), col = 'Grey')+
  geom_point(mapping = aes(x = time, y = avg, col = Act), size = 0.01)

#-----------------------------------------------------------------------------------------

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

total409 <- cbind(M409_b, M409_z)
value_409 <- total409[,c(3,132,133)]
value_409$time <- c(1:6301)
#View(value_409)

#Time Series for 409
Mouse_409 <- ggplot(data = value_409)+
  #geom_line(mapping = aes(x = time, y = avg), col = 'Grey')+
  geom_point(mapping = aes(x = time, y = avg, col = Act), size = 0.01)

#------------------------------------------------------------------------------------------------
##Mouse 414
Mouse_414_behavior=readMat("Data/Opp_Sex/608102_414/Day_2/Trial_002_0/binned_behavior.mat")
M414_a <- as.data.frame(t(Mouse_414_behavior$binned.behavior))
M414_b <- M414_b %>% mutate(Act =
                              case_when(V1 == 1 ~ "male", 
                                        V2 == 1 ~ "female",
                                        V1 == V2 ~ "no touch")
)
#View(M414_b)

Mouse_414_zscore=readMat("Data/Opp_Sex/608102_414/Day_2/Trial_002_0/binned_zscore.mat")
M414_z <- as.data.frame(Mouse_414_zscore$binned.zscore)
M414_z$avg <- rowMeans(M414_z, na.rm=TRUE)
M414_z$sum <- rowSums(M414_z, na.rm = TRUE)
#View(M414_z)

total414 <- cbind(M414_b, M414_z)
value_414 <- total414[,c(3,39,40)]
value_414$time <- c(1:6301)
#View(value_414)

#Time Series for 414
Mouse_414 <- ggplot(data = value_414)+
  #geom_line(mapping = aes(x = time, y = avg), col = 'Grey')+
  geom_point(mapping = aes(x = time, y = avg, col = Act), size = 0.01)

#---------------------------------------------------------------------------------------

##Mouse 418
Mouse_418_behavior=readMat("Data/Opp_Sex/608103_418/Day_2/Trial_002_0/binned_behavior.mat")
M418_b <- as.data.frame(t(Mouse_418_behavior$binned.behavior))
M418_b <- M418_b %>% mutate(Act =
                              case_when(V1 == 1 ~ "male", 
                                        V2 == 1 ~ "female",
                                        V1 == V2 ~ "no touch")
)
#View(M418_b)

Mouse_418_zscore=readMat("Data/Opp_Sex/608103_418/Day_2/Trial_002_0/binned_zscore.mat")
M418_z <- as.data.frame(Mouse_418_zscore$binned.zscore)
M418_z$avg <- rowMeans(M418_z, na.rm=TRUE)
M418_z$sum <- rowSums(M418_z, na.rm = TRUE)
#View(M418_z)

total418 <- cbind(M418_b, M418_z)
value_418 <- total418[,c(3,38,39)]
value_418$time <- c(1:6301)
#View(value_418)

#Time Series for 418
Mouse_418 <- ggplot(data = value_418)+
  #geom_line(mapping = aes(x = time, y = avg), col = 'Grey')+
  geom_point(mapping = aes(x = time, y = avg, col = Act), size = 0.01)

#-----------------------------------------------------------------------------------------

#all the graphs: mouse(409, 412, 414, 416, 417, 418)

all <- ggarrange(Mouse_409,Mouse_412,Mouse_414,Mouse_416,Mouse_417,Mouse_418,
                 ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom")
all 


ggarrange(c, t, ncol = 2, nrow = 1,
          common.legend = TRUE, legend="bottom")


#-----------------------------------------------------------------------------------

#ridge-regression

lambda_values <- 10^seq(2, -2, by = -.1)
M412_b <- M412_b %>% mutate(ACT =
                              case_when(V1 == 1 ~ 1, 
                                        V2 == 1 ~ 2,
                                        V1 == V2 ~ 3)
)
fit_412 <- glmnet(total412[,4:50], total412$ACT, alpha = 0, lambda = lambda_values)
summary(fit_412)

ridge_412 <- cv.glmnet(as.matrix(total412[,5:51]), total412$ACT, alpha = 0)
ridge_412$lambda.min

sort(coef(glmnet(total412[,5:51], total412$ACT, alpha = 0, lambda = ridge_412$lambda.min)))

#-------------------------------------------------------------------------------

library(foreign)
library(nnet)
library(reshape2)

fit416_log_m <- glm(V1 ~ avg, data = value_416_a, family = 'binomial')
fit416_log_f <- glm(V2 ~ avg, data = value_416_a, family = 'binomial')
#View(value_416_a)

#Visualization of mouse 416 for V1
summary(fit416_log_m)
summary(fit416_log_m)$coefficients
exp(coefficients(fit416_log_m)[2])

predict(fit416_log_m, newdata = list(avg = c(0.01, 1, 5)), type = "response")

avgs = seq(min(value_416_a$avg), max(value_416_a$avg), 0.05)
probs = predict(fit416_log_m, 
                newdata = data.frame(avg = avgs), 
                type = "response", 
                se.fit = TRUE)

pm = probs$fit
pu = probs$fit + probs$se.fit * 1.96 # 95% confidence interval
pl = probs$fit - probs$se.fit * 1.96 # 95% confidence interval

plot(value_416_a$avg, 
     value_416_a$V1, 
     pch = 16, 
     cex = 1, 
     ylab = "Interact with male", 
     xlab = "Avg z.score")

grid()

polygon(c(rev(avgs),avgs), c(rev(pl),pu),
        col = "grey90", border = NA)

lines(avgs, pm, lwd = 2)
lines(avgs, pu, lwd = 2, col = "red")
lines(avgs, pl, lwd = 2, col = "red")

abline(h=0.1, lty=2)
abline(h=0.5, lty=2)
abline(h=0.9, lty=2)

#Visualization of mouse 416 for V2

summary(fit416_log_f)
summary(fit416_log_f)$coefficients
exp(coefficients(fit416_log_f)[2])

predict(fit416_log_f, newdata = list(avg = c(-1, 0.05, 5)), type = "response")

avgs_f = seq(min(value_416_a$avg), max(value_416_a$avg), 0.05)
probs_f = predict(fit416_log_f, 
                newdata = data.frame(avg = avgs_f), 
                type = "response", 
                se.fit = TRUE)

pm_f = probs_f$fit
pu_f = probs_f$fit + probs$se.fit * 1.96 # 95% confidence interval
pl_f = probs_f$fit - probs$se.fit * 1.96 # 95% confidence interval

plot(value_416_a$avg, 
     value_416_a$V2, 
     pch = 16, 
     cex = 1, 
     ylab = "Interact with female", 
     xlab = "Avg z.score")

grid()

polygon(c(rev(avgs_f),avgs_f), c(rev(pl_f),pu_f),
        col = "grey90", border = NA)

lines(avgs, pm, lwd = 2, col = "black")
lines(avgs, pu, lwd = 2, col = "blue")
lines(avgs, pl, lwd = 2, col = "blue")

lines(avgs, pm_f, lwd = 2)
lines(avgs, pu_f, lwd = 2, col = "red")
lines(avgs, pl_f, lwd = 2, col = "red")

abline(h=0.1, lty=2)
abline(h=0.5, lty=2)
abline(h=0.9, lty=2)

#------------------------------------------------------------------------------
#Visualize for mouse 414
total414_a <- cbind(M414_a, M414_z)
value_414_a <- total414[,c(1,2,38,39)]

fit414_log_m <- glm(V1 ~ avg, data = value_414_a, family = 'binomial')
fit414_log_f <- glm(V2 ~ avg, data = value_414_a, family = 'binomial')
#View(value_414_a)

#Visualization of mouse 414 for V1
summary(fit414_log_m)
summary(fit414_log_m)$coefficients
exp(coefficients(fit414_log_m)[2])

predict(fit414_log_m, newdata = list(avg = c(0.01, 1, 5)), type = "response")

avgs_414 = seq(min(value_414_a$avg), max(value_414_a$avg), 0.05)
probs_414 = predict(fit414_log_m, 
                newdata = data.frame(avg = avgs_414), 
                type = "response", 
                se.fit = TRUE)

pm_414m = probs_414$fit
pu_414m = probs_414$fit + probs_414$se.fit * 1.96 # 95% confidence interval
pl_414m = probs_414$fit - probs_414$se.fit * 1.96 # 95% confidence interval

plot(value_414_a$avg, 
     value_414_a$V1, 
     pch = 16, 
     cex = 1, 
     ylab = "Interact with male", 
     xlab = "Avg z.score")

grid()

polygon(c(rev(avgs_414),avgs_414), c(rev(pl_414),pu_414),
        col = "grey90", border = NA)

lines(avgs_414, pm_414m, lwd = 2)
lines(avgs_414, pu_414m, lwd = 2, col = "red")
lines(avgs_414, pl_414m, lwd = 2, col = "red")

#Visualization of mouse 414 for V2


predict(fit414_log_f, newdata = list(avg = c(-1, 0.05, 5)), type = "response")

avgs_414f = seq(min(value_414_a$avg), max(value_414_a$avg), 0.05)
probs_414f = predict(fit414_log_f, 
                  newdata = data.frame(avg = avgs_414f), 
                  type = "response", 
                  se.fit = TRUE)

pm_414f = probs_414f$fit
pu_414f = probs_414f$fit + probs_414f$se.fit * 1.96 # 95% confidence interval
pl_414f = probs_414f$fit - probs_414f$se.fit * 1.96 # 95% confidence interval

plot(value_414_a$avg, 
     value_414_a$V2, 
     pch = 16, 
     cex = 1, 
     ylab = "Interact with female", 
     xlab = "Avg z.score")

grid()

polygon(c(rev(avgs_414f),avgs_414f), c(rev(pl_414f),pu_414f),
        col = "grey90", border = NA)

lines(avgs_414, pm_414m, lwd = 2, col = "black")
lines(avgs_414, pu_414m, lwd = 2, col = "blue")
lines(avgs_414, pl_414m, lwd = 2, col = "blue")

lines(avgs_414f, pm_414f, lwd = 2)
lines(avgs_414f, pu_414f, lwd = 2, col = "red")
lines(avgs_414f, pl_414f, lwd = 2, col = "red")

abline(h=0.1, lty=2)
abline(h=0.5, lty=2)
abline(h=0.9, lty=2)

#-------------------------------------------------------------------------------
library(RcppRoll)


#Sum every ten rows
value_414_b <- value_414_a[,1:3]
sum_rows_414 <- value_414_b %>% 
  group_by(gr=gl(n()/10,10)) %>% 
  mutate(Sum=sum(avg)) %>%
  slice(10)

#roll sum every ten rolls
roll_sum_414 <- value_414_b %>%
  mutate(roll_sum = roll_sum(avg, 10, align = "right", fill = NA))   

#View(roll_sum_414)





