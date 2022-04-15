library(R.matlab)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)



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
# ggplot(data = value_412)+
#   geom_line(mapping = aes(x = time, y = avg))+
#   geom_point(mapping = aes(x = time, y = V1), size = 0.01)+
#   geom_point(mapping = aes(x = time, y = V2), col = 'Blue', size = 0.01)

Mouse_412 <- ggplot(data = value_412)+
  #geom_line(mapping = aes(x = time, y = avg), col = 'Grey')+
  geom_point(mapping = aes(x = time, y = avg, col = Act, shape = Act),size = 0.5)+
  labs(title = "Opp_sex 412: Average score of cells vs Time in behavior")+
  theme(plot.title = element_text(hjust = 0.5))
Mouse_412 + scale_y_continuous(limits = c(0.0,2.0))

 
table(value_412$Act)

# pie_chart
info412 <- c(2467,1727,2106)
names <- c("female","male","no touch")
colors <- c("pink","lightblue","yellow")
piepercent412 <- paste(round(100*info412/sum(info412)),"%")

pie412 <- pie(info412, labels = piepercent412, main = "Opp_Sex 412", col = colors)


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
p417 <- ggplot(data = value_417)+
  #geom_line(mapping = aes(x = time, y = avg), col = 'Grey')+
  geom_point(mapping = aes(x = time, y = avg, col = Act), size = 0.01)+
  labs(title = "Opp_sex 417: Average score of cells vs Time in behavior")+
  theme(plot.title = element_text(hjust = 0.5))
Mouse_417 <- p417 + scale_y_continuous(limits = c(0.0,2.0))


table(value_417$Act)

# pie_chart
info417 <- c(2615,1779,2464)
names <- c("female","male","no touch")
colors <- c("pink","lightblue","yellow")
piepercent417 <- paste(round(100*info417/sum(info417)),"%")

pie417 <- pie(info417, labels = piepercent417, main = "Opp_Sex 417", col = colors)


#---------------------------------------------------------------------------------------

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

total416 <- cbind(M416_b, M416_z)
value_416 <- total416[,c(3,17,18)]
value_416$time <- c(1:6301)
#View(value_416)

#Time Series for 416
p416 <- ggplot(data = value_416)+
  #geom_line(mapping = aes(x = time, y = avg), col = 'Grey')+
  geom_point(mapping = aes(x = time, y = avg, col = Act), size = 0.01)+
  labs(title = "Opp_sex 416: Average score of cells vs Time in behavior")+
  theme(plot.title = element_text(hjust = 0.5))

Mouse_416 <- p416 + scale_y_continuous(limits = c(0.0,2.0))

table(value_416$Act)

# pie_chart
info416 <- c(2370,1334,2597)
names <- c("female","male","no touch")
colors <- c("pink","lightblue","yellow")
piepercent416 <- paste(round(100*info416/sum(info416)),"%")

pie416 <- pie(info416, labels = piepercent416, main = "Opp_Sex 416", col = colors)

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
p409 <- ggplot(data = value_409)+
  #geom_line(mapping = aes(x = time, y = avg), col = 'Grey')+
  geom_point(mapping = aes(x = time, y = avg, col = Act), size = 0.01)+
  labs(title = "Opp_sex 409: Average score of cells vs Time in behavior")+
  theme(plot.title = element_text(hjust = 0.5))
Mouse_409 <- p409 + scale_y_continuous(limits = c(0.0,2.0))

table(value_409$Act)

# pie_chart
info409 <- c(1329,2110,2862)
names <- c("female","male","no touch")
colors <- c("pink","lightblue","yellow")
piepercent409 <- paste(round(100*info409/sum(info409)),"%")

pie409 <- pie(info409, labels = piepercent409, main = "Opp_Sex 409", col = colors)


#------------------------------------------------------------------------------------------------
##Mouse 414
Mouse_414_behavior=readMat("Data/Opp_Sex/608102_414/Day_2/Trial_002_0/binned_behavior.mat")
M414_b <- as.data.frame(t(Mouse_414_behavior$binned.behavior))
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
p414 <- ggplot(data = value_414)+
  #geom_line(mapping = aes(x = time, y = avg), col = 'Grey')+
  geom_point(mapping = aes(x = time, y = avg, col = Act), size = 0.01)+
  labs(title = "Opp_sex 414: Average score of cells vs Time in behavior")+
  theme(plot.title = element_text(hjust = 0.5))
Mouse_414 <- p414 + scale_y_continuous(limits = c(0.0,2.0))

ggsave('Opp_sex414_average_time_sex.png', Mouse_414, height = 6, width = 10)

table(value_414$Act)

# pie_chart
info414 <- c(1834,3104,1363)
names <- c("female","male","no touch")
colors <- c("pink","lightblue","yellow")
piepercent414 <- paste(round(100*info414/sum(info414)),"%")

pie414 <- pie(info414, labels = piepercent414, main = "Opp_Sex 414", col = colors)


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
p418 <- ggplot(data = value_418)+
  #geom_line(mapping = aes(x = time, y = avg), col = 'Grey')+
  geom_point(mapping = aes(x = time, y = avg, col = Act), size = 0.01)+
  labs(title = "Opp_sex 418: Average score of cells vs Time in behavior")+
  theme(plot.title = element_text(hjust = 0.5))
Mouse_418 <- p418 + scale_y_continuous(limits = c(0.0,2.0))

table(value_418$Act)

# pie_chart
info418 <- c(2249,1801,2251)
names <- c("female","male","no touch")
colors <- c("pink","lightblue","yellow")
piepercent418 <- paste(round(100*info418/sum(info418)),"%")

pie418 <- pie(info418, labels = piepercent418, main = "Opp_Sex 418", col = colors)
legend("topright",names, cex = 0.8, fill = colors)

#-----------------------------------------------------------------------------------------

#all the graphs: mouse(409, 412, 414, 416, 417, 418)

all_point <- ggarrange(Mouse_409,Mouse_412,Mouse_414,Mouse_416,Mouse_417,Mouse_418,
                 ncol = 1, nrow = 6, common.legend = TRUE, legend = "bottom")
all_point

ggsave('average_time_sex.png', all_point, height = 6, width = 10)
 
par(mfrow = c(2,3))
pie(info409, labels = piepercent409, main = "Opp_Sex 409", col = colors)
pie(info412, labels = piepercent412, main = "Opp_Sex 412", col = colors)
pie(info414, labels = piepercent414, main = "Opp_Sex 414", col = colors)
pie(info416, labels = piepercent416, main = "Opp_Sex 416", col = colors)
pie(info417, labels = piepercent417, main = "Opp_Sex 417", col = colors)
legend(-2.5,-1,names, cex = 0.8, fill = colors,horiz=T,box.lty = 0, xpd=T)
pie(info418, labels = piepercent418, main = "Opp_Sex 418", col = colors)


#----------------------------------------------------------------------------

# Preliminary Analysis
## separate male, female and no touch.
### male
value_409_male <- filter(value_409, Act == "male")
p_409_male <- ggplot(data = value_409_male) + geom_point(mapping = aes(x = time, y = avg), color = "lightblue",size = 0.01)+
  labs(title = "Opp_sex 409: Average score of cells vs Time in male")+
  theme(plot.title = element_text(hjust = 0.5))

p_409_male_y <- p_409_male + scale_y_continuous(limits = c(0.0,2.0))


##

value_412_male <- filter(value_412, Act == "male")
p_412_male <- ggplot(data = value_412_male) + geom_point(mapping = aes(x = time, y = avg), color = "lightblue",size = 0.01)+
  labs(title = "Opp_sex 416: Average score of cells vs Time in male")+
  theme(plot.title = element_text(hjust = 0.5))

p_412_male_y <- p_412_male + scale_y_continuous(limits = c(0.0,2.0))


##
value_414_male <- filter(value_414, Act == "male")
p_414_male <- ggplot(data = value_414_male) + geom_point(mapping = aes(x = time, y = avg), color = "lightblue",size = 0.01)+
  labs(title = "Opp_sex 414: Average score of cells vs Time in male ")+
  theme(plot.title = element_text(hjust = 0.5))

p_414_male_y <- p_414_male + scale_y_continuous(limits = c(0.0,2.0))



##
value_416_male <- filter(value_416, Act == "male")
p_416_male <- ggplot(data = value_416_male) + geom_point(mapping = aes(x = time, y = avg), color = "lightblue",size = 0.01)+
  labs(title = "Opp_sex 416: Average score of cells vs Time in male")+
  theme(plot.title = element_text(hjust = 0.5))

p_416_male_y <- p_416_male + scale_y_continuous(limits = c(0.0,2.0))


##
value_417_male <- filter(value_417, Act == "male")
p_417_male <- ggplot(data = value_417_male) + geom_point(mapping = aes(x = time, y = avg), color = "lightblue",size = 0.01)+
  labs(title = "Opp_sex 417: Average score of cells vs Time in male")+
  theme(plot.title = element_text(hjust = 0.5))

p_417_male_y <- p_417_male + scale_y_continuous(limits = c(0.0,2.0))


## 
value_418_male <- filter(value_416, Act == "male")
p_418_male <- ggplot(data = value_418_male) + geom_point(mapping = aes(x = time, y = avg), color = "lightblue",size = 0.01)+
  labs(title = "Opp_sex 418: Average score of cells vs Time in male")+
  theme(plot.title = element_text(hjust = 0.5))

p_418_male_y <- p_418_male + scale_y_continuous(limits = c(0.0,2.0))


p_male <- ggarrange(p_409_male_y,p_412_male_y,p_414_male_y,p_416_male_y,p_417_male_y,p_418_male_y,
                    ncol = 1, nrow = 6, legend = "bottom")
p_male

ggsave("average_male.png")


### female
value_409_female <- filter(value_409, Act == "female")
p_409_female <- ggplot(data = value_409_female) + geom_point(mapping = aes(x = time, y = avg), color = "pink",size = 0.01)+
  labs(title = "Opp_sex 409: Average score of cells vs Time in female")+
  theme(plot.title = element_text(hjust = 0.5))

p_409_female_y <- p_409_female + scale_y_continuous(limits = c(0.0,2.0))


##
value_412_female <- filter(value_412, Act == "female")
p_412_female <- ggplot(data = value_412_female) + geom_point(mapping = aes(x = time, y = avg), color = "pink",size = 0.01)+
  labs(title = "Opp_sex 412: Average score of cells vs Time in female")+
  theme(plot.title = element_text(hjust = 0.5))

p_412_female_y <- p_412_female + scale_y_continuous(limits = c(0.0,2.0))


##
value_414_female <- filter(value_414, Act == "female")
p_414_female <- ggplot(data = value_414_female) + geom_point(mapping = aes(x = time, y = avg), color = "pink",size = 0.01)+
  labs(title = "Opp_sex 414: Average score of cells vs Time in female")+
  theme(plot.title = element_text(hjust = 0.5))

p_414_female_y <- p_414_female + scale_y_continuous(limits = c(0.0,2.0))


##
value_416_female <- filter(value_416, Act == "female")
p_416_female <- ggplot(data = value_416_female) + geom_point(mapping = aes(x = time, y = avg), color = "pink",size = 0.01)+
  labs(title = "Opp_sex 416: Average score of cells vs Time in female")+
  theme(plot.title = element_text(hjust = 0.5))

p_416_female_y <- p_416_female + scale_y_continuous(limits = c(0.0,2.0))


##
value_417_female <- filter(value_417, Act == "female")
p_417_female <- ggplot(data = value_417_female) + geom_point(mapping = aes(x = time, y = avg), color = "pink",size = 0.01)+
  labs(title = "Opp_sex 417: Average score of cells vs Time in female")+
  theme(plot.title = element_text(hjust = 0.5))

p_417_female_y <- p_417_female + scale_y_continuous(limits = c(0.0,2.0))


##
value_418_female <- filter(value_418, Act == "female")
p_418_female <- ggplot(data = value_418_female) + geom_point(mapping = aes(x = time, y = avg), color = "pink",size = 0.01)+
  labs(title = "Opp_sex 418: Average score of cells vs Time in female")+
  theme(plot.title = element_text(hjust = 0.5))

p_418_female_y <- p_418_female + scale_y_continuous(limits = c(0.0,2.0))


p_female <- ggarrange(p_409_female_y,p_412_female_y,p_414_female_y,p_416_female_y,p_417_female_y,p_418_female_y,
                      ncol = 1, nrow = 6, legend = "bottom")
p_female

ggsave("average_female.png")


### no touch
value_409_nt <- filter(value_409, Act == "no touch")
p_409_nt <- ggplot(data = value_409_nt) + geom_point(mapping = aes(x = time, y = avg), color = "purple",size = 0.01)+
  labs(title = "Opp_sex 409: Average score of cells vs Time without touch")+
  theme(plot.title = element_text(hjust = 0.5))

p_409_nt_y <- p_409_nt + scale_y_continuous(limits = c(0.0,2.0))

##
value_412_nt <- filter(value_412, Act == "no touch")
p_412_nt <- ggplot(data = value_412_nt) + geom_point(mapping = aes(x = time, y = avg), color = "purple",size = 0.01)+
  labs(title = "Opp_sex 412: Average score of cells vs Time without touch")+
  theme(plot.title = element_text(hjust = 0.5))

p_412_nt_y <- p_412_nt + scale_y_continuous(limits = c(0.0,2.0))


##
value_414_nt <- filter(value_414, Act == "no touch")
p_414_nt <- ggplot(data = value_414_nt) + geom_point(mapping = aes(x = time, y = avg), color = "purple",size = 0.01)+
  labs(title = "Opp_sex 414: Average score of cells vs Time without touch")+
  theme(plot.title = element_text(hjust = 0.5))

p_414_nt_y <- p_414_nt + scale_y_continuous(limits = c(0.0,2.0))


##
value_416_nt <- filter(value_416, Act == "no touch")
p_416_nt <- ggplot(data = value_416_nt) + geom_point(mapping = aes(x = time, y = avg), color = "purple",size = 0.01)+
  labs(title = "Opp_sex 416: Average score of cells vs Time without touch")+
  theme(plot.title = element_text(hjust = 0.5))

p_416_nt_y <- p_416_nt + scale_y_continuous(limits = c(0.0,2.0))

##
value_417_nt <- filter(value_417, Act == "no touch")
p_417_nt <- ggplot(data = value_417_nt) + geom_point(mapping = aes(x = time, y = avg), color = "purple",size = 0.01)+
  labs(title = "Opp_sex 417: Average score of cells vs Time without touch")+
  theme(plot.title = element_text(hjust = 0.5))

p_417_nt_y <- p_417_nt + scale_y_continuous(limits = c(0.0,2.0))

##
value_418_nt <- filter(value_418, Act == "no touch")
p_418_nt <- ggplot(data = value_418_nt) + geom_point(mapping = aes(x = time, y = avg), color = "purple",size = 0.01)+
  labs(title = "Opp_sex 418: Average score of cells vs Time without touch")+
  theme(plot.title = element_text(hjust = 0.5))

p_418_nt_y <- p_418_nt + scale_y_continuous(limits = c(0.0,2.0))

p_nt <- ggarrange(p_409_nt_y,p_412_nt_y,p_414_nt_y,p_416_nt_y,p_417_nt_y,p_418_nt_y,
                              ncol = 1, nrow = 6, legend = "bottom")
p_nt

ggsave("average_nt.png")


#------------------------------------------------------------------------------
p409 <- ggplot(data = value_409)+
  geom_point(mapping = aes(x = time, y = avg), size = 0.3)+
  facet_wrap(~Act)+
  labs(title = "Opp_sex 409")
 
  

p412 <- ggplot(data = value_412)+
  geom_point(mapping = aes(x = time, y = avg), size = 0.3)+
  facet_wrap(~Act)+
  labs(title = "Opp_sex 412")

p414 <- ggplot(data = value_414)+
  geom_point(mapping = aes(x = time, y = avg), size = 0.3)+
  facet_wrap(~Act)+
  labs(title = "Opp_sex 414")

p416 <- ggplot(data = value_416)+
  geom_point(mapping = aes(x = time, y = avg), size = 0.3)+
  facet_wrap(~Act)+
  labs(title = "Opp_sex 416")

p417 <- ggplot(data = value_417)+
  geom_point(mapping = aes(x = time, y = avg), size = 0.3)+
  facet_wrap(~Act)+
  labs(title = "Opp_sex 417")

p418 <- ggplot(data = value_418)+
  geom_point(mapping = aes(x = time, y = avg), size = 0.3)+
  facet_wrap(~Act)+
  labs(title = "Opp_sex 418")

p_male_female <- ggarrange(p409,p412,p414,p416,p417,p418, ncol = 1, nrow = 6, legend = "bottom")

ggsave("p_male_female.png")

#------------------------------------------------------------------------------
source("James_Try.R")
##Preliminary Analysis
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
     pch = 10, 
     cex = 0.5, 
     ylab = "Interact with female", 
     xlab = "Avg z.score",
     main = "The probability of the Mouse416 interact result ")

grid()

polygon(c(rev(avgs_f),avgs_f), c(rev(pl_f),pu_f),
        col = "grey90", border = NA)

lines(avgs, pm, lwd = 2, col = "black")
lines(avgs, pu, lwd = 2, col = "lightblue")
lines(avgs, pl, lwd = 2, col = "lightblue")

lines(avgs, pm_f, lwd = 2)
lines(avgs, pu_f, lwd = 2, col = "pink")
lines(avgs, pl_f, lwd = 2, col = "pink")

legend("topright", legend = c("male","female"), col = c("lightblue","pink"),border = "grey",
       lty = 2, lwd = 2, bty = "n",box.lty = 1, box.lwd = 1,cex = 1,text.font = 2 )
# abline(h=0.1, lty=2)
# abline(h=0.5, lty=2)
# abline(h=0.9, lty=2)



