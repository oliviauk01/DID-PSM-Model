## Provide effective strategies for mobile app platform owners 
## and app developers after a popular app has been released on a platform
setwd("/Users/olivia/Desktop/IMC/462 MM/Assignment")
kakao<- read.csv("kakao.csv", head=TRUE)

## INSTALL LIBRARY
install.packages("lmtest")
library(lmtest)
lrtest(trm)


## 2 decimals
options(digits=3)
###
str(kakao)
summary(kakao)
table(kakao$tg)


## Variable Transformation
kakao$t_kakao <- kakao$t_kakao_talk + kakao$t_kakao_story + kakao$t_kakao_game
kakao$week <- as.factor(kakao$week)
kakao$tg <- as.factor(kakao$tg)
kakao$ii <- as.factor(kakao$ii)
kakao$age <- as.factor(kakao$age)
kakao$gender <- as.factor(kakao$gender)
kakao$education <- as.factor(kakao$education)


## Sub Datasets
pre.kakao <- subset(kakao, week == 1)
pre.kakao
pre.kakao$t_kakao_game
post.kakao <- subset(kakao, week == 2)
post.kakao

## Instal package
install.packages("MatchIt")
library(MatchIt)

## 1 One-to-one, Without, 0.05
NN1.out = matchit(tg ~ as.factor(age)+as.factor(education)+as.factor(gender)+
                    t_kakao_talk+t_kakao_story+t_non_kakao, 
                  data=pre.kakao, method="nearest",ratio=1, caliper= 0.05)
summary(NN1.out)
plot(NN1.out, type = "hist")
match1 = get_matches(NN1.out)
match1
id.match1 = match1["panel_id"]
final.match1 = merge(kakao, id.match1, by = "panel_id")
str(final.match1)

match1.p = pdata.frame(final.match1, index=c("panel_id","week"))




## 2 One-to-one, Without, 0.1
NN2.out = matchit(tg ~ as.factor(age)+as.factor(education)+as.factor(gender)+
                    t_kakao_talk+t_kakao_story+t_non_kakao, 
                  data=pre.kakao, method="nearest",ratio=1, caliper= 0.1)
summary(NN2.out)
plot(NN2.out, type = "hist")
match2 = get_matches(NN2.out)
match2
id.match2 = match2["panel_id"]
final.match2 = merge(kakao, id.match2, by = "panel_id")
str(final.match2)

match2.p = pdata.frame(final.match2, index=c("panel_id","week"))




## 3 One-to-one, Without, 0.2
NN3.out = matchit(tg ~ as.factor(age)+as.factor(education)+as.factor(gender)+
                    t_kakao_talk+t_kakao_story+t_non_kakao, 
                  data=pre.kakao, method="nearest",ratio=1, caliper= 0.2)
summary(NN3.out)
plot(NN3.out, type = "hist")
match3 = get_matches(NN3.out)
match3
id.match3 = match3["panel_id"]
final.match3 = merge(kakao, id.match3, by = "panel_id")
str(final.match3)

match3.p = pdata.frame(final.match3, index=c("panel_id","week"))



## 4 One-to-one, With, 0.05
NN4.out = matchit(tg ~ as.factor(age)+as.factor(education)+as.factor(gender)+
                    t_kakao_talk+t_kakao_story+t_non_kakao, 
                  data=pre.kakao, method="nearest",ratio=1, replace = TRUE, caliper= 0.05)
summary(NN4.out)
plot(NN4.out, type = "hist")
match4 = match.data(NN4.out)
match4
id.match4 = match4["panel_id"]
final.match4 = merge(kakao, id.match4, by = "panel_id")
str(final.match4)

match4.p = pdata.frame(final.match4, index=c("panel_id","week"))




## 5 One-to-one, With, 0.1
NN5.out = matchit(tg ~ as.factor(age)+as.factor(education)+as.factor(gender)+
                    t_kakao_talk+t_kakao_story+t_non_kakao, 
                  data=pre.kakao, method="nearest",ratio=1, replace = TRUE, caliper= 0.1)
summary(NN5.out)
plot(NN5.out, type = "hist")
match5 = match.data(NN5.out)
match5
id.match5 = match5["panel_id"]
final.match5 = merge(kakao, id.match5, by = "panel_id")
str(final.match5)

match5.p = pdata.frame(final.match5, index=c("panel_id","week"))




## 6 One-to-one, With, 0.2
NN6.out = matchit(tg ~ as.factor(age)+as.factor(education)+as.factor(gender)+
                    t_kakao_talk+t_kakao_story+t_non_kakao, 
                  data=pre.kakao, method="nearest",ratio=1, replace = TRUE, caliper= 0.2)
summary(NN6.out)
plot(NN6.out, type = "hist")
match6 = match.data(NN6.out)
match6
id.match6 = match6["panel_id"]
final.match6 = merge(kakao, id.match6, by = "panel_id")
str(final.match6)

match6.p = pdata.frame(final.match6, index=c("panel_id","week"))




## 7 Two-to-one, Without, 0.05
NN7.out = matchit(tg ~ as.factor(age)+as.factor(education)+as.factor(gender)+
                    t_kakao_talk+t_kakao_story+t_non_kakao, 
                  data=pre.kakao, method="nearest",ratio=2, caliper= 0.05)
summary(NN7.out)
plot(NN7.out, type = "hist")
match7 = get_matches(NN7.out)
match7
id.match7 = match7["panel_id"]
final.match7 = merge(kakao, id.match7, by = "panel_id")
str(final.match7)

match7.p = pdata.frame(final.match7, index=c("panel_id","week"))



## 8 Two-to-one, Without, 0.1
NN8.out = matchit(tg ~ as.factor(age)+as.factor(education)+as.factor(gender)+
                    t_kakao_talk+t_kakao_story+t_non_kakao, 
                  data=pre.kakao, method="nearest",ratio=2, caliper= 0.1)
summary(NN8.out)
plot(NN8.out, type = "hist")
match8 = get_matches(NN8.out)
match8
id.match8 = match8["panel_id"]
final.match8 = merge(kakao, id.match8, by = "panel_id")
str(final.match8)

match8.p = pdata.frame(final.match8, index=c("panel_id","week"))



## 9 Two-to-one, Without 0.2
NN9.out = matchit(tg ~ as.factor(age)+as.factor(education)+as.factor(gender)+
                    t_kakao_talk+t_kakao_story+t_non_kakao, 
                  data=pre.kakao, method="nearest",ratio=2, caliper= 0.2)
summary(NN9.out)
plot(NN9.out, type = "hist")
match9 = get_matches(NN9.out)
match9
id.match9 = match9["panel_id"]
final.match9 = merge(kakao, id.match9, by = "panel_id")
str(final.match9)

match9.p = pdata.frame(final.match9, index=c("panel_id","week"))





## 10 Two-to-one, With, 0.05
NN10.out = matchit(tg ~ as.factor(age)+as.factor(education)+as.factor(gender)+
                    t_kakao_talk+t_kakao_story+t_non_kakao, 
                  data=pre.kakao, method="nearest",ratio=2,replace = TRUE, caliper= 0.05)
summary(NN10.out)
plot(NN10.out, type = "hist")
match10 = match.data(NN10.out)
match10
id.match10 = match10["panel_id"]
final.match10 = merge(kakao, id.match10, by = "panel_id")
str(final.match10)

match10.p = pdata.frame(final.match10, index=c("panel_id","week"))





 ##DO WE NEED TO ESTIMATE THE SIGNIFICANCE?
summary(lm(kakao$t_kakao ~ t_anipang + ii, data = final.match1))
## SALLI'S CODE
NN1.out = matchit(tg ~ as.factor(age)+as.factor(income)+as.factor(education)+as.factor(gender)+
                    t_kakao_talk+t_kakao_story+t_non_kakao_game+t_non_kakao_story+t_non_kakao_talk+t_non_kakao, 
                  data = kakao_new, method = "nearest")

### Generate Panel Data Frame
install.packages("plm")
library(plm)



### RQ 1 ###
## RQ1 - match 1
# Dummy Variable Regression
## summary(lm(lwage ~ as.factor(nr) + as.factor(year)*educ + married + union, data=wagepan.p))

summary(lm(t_kakao ~ as.factor(panel_id) + as.factor(week) +ii+t_kakao_talk+t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game+t_non_kakao, data=match1.p))

## SALLI'S CODE
summary(lm(formula = t_kakao_game ~ as.factor(panel_id) + as.factor(week) + as.factor(ii) +
             t_kakao_talk + t_kakao_story + t_anipang +
             t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game + t_non_kakao, data = final.dat1))

# Fixed Effects Estimation
summary(plm(t_kakao ~ as.factor(week)+ii+t_kakao_talk+t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game+t_non_kakao, data=match1.p, model="within"))


## RQ1 - match 2
# Dummy Variable Regression
summary(lm(t_kakao ~ as.factor(panel_id) + as.factor(week) +ii+t_kakao_talk+t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game+t_non_kakao, data=match2.p))

# Fixed Effects Estimation
summary(plm(t_kakao ~ as.factor(week)+ii+t_kakao_talk+ t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game+t_non_kakao, data=match2.p, model="within"))



## RQ1 - match 4
summary(lm(t_kakao ~ as.factor(panel_id) + as.factor(week) +ii+t_kakao_talk+t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game+t_non_kakao, data=match4.p))

# Fixed Effects Estimation
summary(plm(t_kakao ~ as.factor(week)+ii+t_kakao_talk+t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game+t_non_kakao, data=match4.p, model="within"))



## RQ1 - match 5
summary(lm(t_kakao ~ as.factor(panel_id) + as.factor(week) +ii+t_kakao_talk+t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game+t_non_kakao, data=match5.p))

# Fixed Effects Estimation
summary(plm(t_kakao ~ as.factor(week)+ii+t_kakao_talk+t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game+t_non_kakao, data=match5.p, model="within"))


## RQ1 - match 6
summary(lm(t_kakao ~ as.factor(panel_id) + as.factor(week) +ii+t_kakao_talk+t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game+t_non_kakao, data=match6.p))

# Fixed Effects Estimation
summary(plm(t_kakao ~ as.factor(week)+ii+t_kakao_talk+t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game+t_non_kakao, data=match6.p, model="within"))








## RQ 2 ### -match1
# Dummy Variable Regression
summary(lm(t_kakao_talk ~ as.factor(panel_id) + as.factor(week) +ii+t_kakao_story+t_kakao_game+t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game+t_non_kakao, data=match1.p))

# Fixed Effects Estimation
summary(plm(t_kakao_talk ~ as.factor(week)+ii+t_kakao_story+t_kakao_game+t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game+t_non_kakao, data=match1.p, model="within"))


## RQ 2 -match4
# Dummy Variable Regression
summary(lm(t_kakao_talk ~ as.factor(panel_id) + as.factor(week) +ii+t_kakao_story+t_kakao_game+t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game+t_non_kakao, data=match4.p))

# Fixed Effects Estimation
summary(plm(t_kakao_talk ~ as.factor(week)+ii+t_kakao_story+t_kakao_game+t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game+t_non_kakao, data=match4.p, model="within"))


## RQ 2 -match5
# Dummy Variable Regression
summary(lm(t_kakao_talk ~ as.factor(panel_id) + as.factor(week) +ii+t_kakao_story+t_kakao_game+t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game+t_non_kakao, data=match5.p))

# Fixed Effects Estimation
summary(plm(t_kakao_talk ~ as.factor(week)+ii+t_kakao_story+t_kakao_game+t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game+t_non_kakao, data=match5.p, model="within"))

## RQ 2 -match6
# Dummy Variable Regression
summary(lm(t_kakao_talk ~ as.factor(panel_id) + as.factor(week) +ii+t_kakao_story+t_kakao_game+t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game+t_non_kakao, data=match6.p))

# Fixed Effects Estimation
summary(plm(t_kakao_talk ~ as.factor(week)+ii+t_kakao_story+t_kakao_game+t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game+t_non_kakao, data=match6.p, model="within"))



## RQ 3 ### -match1
# Dummy Variable Regression
summary(lm(t_kakao_story ~ as.factor(panel_id) + as.factor(week) +ii+t_kakao_talk + t_kakao_game+t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game + t_non_kakao, data=match1.p))

# Fixed Effects Estimation
summary(plm(t_kakao_story ~ as.factor(week)+ii+ t_kakao_talk + t_kakao_game+t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game + t_non_kakao, data=match1.p, model="within"))





## RQ 3 ### -match4
# Dummy Variable Regression
summary(lm(t_kakao_story ~ as.factor(panel_id) + as.factor(week) +ii+t_kakao_talk + t_kakao_game+t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game + t_non_kakao, data=match4.p))

# Fixed Effects Estimation
summary(plm(t_kakao_story ~ as.factor(week)+ii+ t_kakao_talk + t_kakao_game+t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game + t_non_kakao, data=match4.p, model="within"))




## RQ 3 ### -match5
# Dummy Variable Regression
summary(lm(t_kakao_story ~ as.factor(panel_id) + as.factor(week) +ii+t_kakao_talk + t_kakao_game+t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game + t_non_kakao, data=match5.p))

# Fixed Effects Estimation
summary(plm(t_kakao_story ~ as.factor(week)+ii+ t_kakao_talk + t_kakao_game+t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game + t_non_kakao, data=match5.p, model="within"))



## RQ 3 ### -match6
# Dummy Variable Regression
summary(lm(t_kakao_story ~ as.factor(panel_id) + as.factor(week) +ii+t_kakao_talk + t_kakao_game+t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game + t_non_kakao, data=match6.p))

# Fixed Effects Estimation
summary(plm(t_kakao_story ~ as.factor(week)+ii+ t_kakao_talk + t_kakao_game+t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game + t_non_kakao, data=match6.p, model="within"))


## RQ 4 ### -match1
# Dummy Variable Regression
summary(lm(t_kakao_game ~ as.factor(panel_id) + as.factor(week) +ii+t_kakao_talk + t_kakao_story +t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game + t_non_kakao, data=match1.p))

# Fixed Effects Estimation
summary(plm(t_kakao_game ~ as.factor(week) +ii+t_kakao_talk + t_kakao_story+t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game + t_non_kakao, data=match1.p, model="within"))




## RQ 4 ### -match4
# Dummy Variable Regression
summary(lm(t_kakao_game ~ as.factor(panel_id) + as.factor(week) +ii+t_kakao_talk + t_kakao_story +t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game + t_non_kakao, data=match4.p))

# Fixed Effects Estimation
summary(plm(t_kakao_game ~ as.factor(week) +ii+t_kakao_talk + t_kakao_story+t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game + t_non_kakao, data=match4.p, model="within"))


## RQ 4 ### -match5
# Dummy Variable Regression
summary(lm(t_kakao_game ~ as.factor(panel_id) + as.factor(week) +ii+t_kakao_talk + t_kakao_story +t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game + t_non_kakao, data=match5.p))

# Fixed Effects Estimation
summary(plm(t_kakao_game ~ as.factor(week) +ii+t_kakao_talk + t_kakao_story+t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game + t_non_kakao, data=match5.p, model="within"))


## RQ 4 ### -match6
# Dummy Variable Regression
summary(lm(t_kakao_game ~ as.factor(panel_id) + as.factor(week) +ii+t_kakao_talk + t_kakao_story +t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game + t_non_kakao, data=match6.p))

# Fixed Effects Estimation
summary(plm(t_kakao_game ~ as.factor(week) +ii+t_kakao_talk + t_kakao_story+t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game + t_non_kakao, data=match6.p, model="within"))




summary(plm(lwage ~ as.factor(year)*educ + married + union, data=wagepan.p, model="within"))






### DESCREPTIVE STATISTICS
### SQL
# install.packages("tidyverse")
library(tidyverse)


# install.packages("corrr")
library(corrr)

head(kakao)
str(kakao)
mean(kakao$income)
hist(kakao$income)
mean(kakao$age)
hist(kakao$age)
table(kakao$age)
table(kakao$education)
table(kakao$gender)

## kakao platform
mean(kakao$t_kakao)
mean(pre.kakao$t_kakao)
mean(post.kakao$t_kakao)

## TALK
mean(kakao$t_kakao_talk)
mean(pre.kakao$t_kakao_talk)
mean(post.kakao$t_kakao_talk)

## STORY
mean(kakao$t_kakao_story)
mean(pre.kakao$t_kakao_story)
mean(post.kakao$t_kakao_story)

## GAME
mean(kakao$t_kakao_game)
mean(pre.kakao$t_kakao_game)
mean(post.kakao$t_kakao_game)



## Non-kakao platform
mean(kakao$t_non_kakao)
mean(pre.kakao$t_non_kakao)
mean(post.kakao$t_non_kakao)

## NON-KAKAO TALK
mean(kakao$t_non_kakao_talk)
mean(pre.kakao$t_non_kakao_talk)
mean(post.kakao$t_non_kakao_talk)

## STORY
mean(kakao$t_non_kakao_story)
mean(pre.kakao$t_non_kakao_story)
mean(post.kakao$t_non_kakao_story)

## GAME
mean(kakao$t_non_kakao_game)
mean(pre.kakao$t_non_kakao_game)
mean(post.kakao$t_non_kakao_game)



## POST-KAKAO

### KAKAO
post.inapp <- data.frame("Anipang" = post.kakao$t_anipang, "kakao_talk" = post.kakao$t_kakao_talk,"kakao_story" = post.kakao$t_kakao_story,"kakao_game" = post.kakao$t_kakao_game)
plot(post.inapp)
cor(post.inapp)

head(post.kakao)
str(post.kakao)

post.kakao$adoptor <- subset(post.kakao, ii==1)
cor(post.kakao[,c(4,9,11,12)], adopter)
pre.kakao <- subset(kakao, week == 1)
table(kakao$ii)

table(kakao$t_anipang)
### NON-KAKAO
Nonpost.inapp <- data.frame("Anipang" = post.kakao$t_anipang, "Non-kakao_talk" = post.kakao$t_non_kakao_talk,"Non-kakao_story" = post.kakao$t_non_kakao_story,"Non-kakao_game" = post.kakao$t_non_kakao_game)
plot(Nonpost.inapp)
cor(Nonpost.inapp)


install.packages("psych")
library(psych)



# Correlations
corr_post <- cor(post.inapp)

corr_post

### GGPLOT
library(ggplot2)
library(reshape2)

melt_post <- melt(corr_post)

head(melt_post)


##2
upper_post <- corr_post

# Make upper triangular matrix by setting NA to lower triangular part:
upper_post[lower.tri(upper_post )] <- NA

upper_post
up_m_post <- melt(upper_post, na.rm = TRUE)

head(up_m_post)

ggplot(data = up_m_post, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(midpoint = 0.5, mid ="grey70", limits = c(-1, +1)) +
  labs(x = "", y = "", fill = "Correlation \n Measure") +
  theme(plot.title = element_text(hjust = 0.5, colour = "blue"), 
        axis.title.x = element_text(face="bold", colour="darkgreen", size = 12),
        axis.title.y = element_text(face="bold", colour="darkgreen", size = 12),
        legend.title = element_text(face="bold", colour="brown", size = 10)) +
  geom_text(aes(x = Var1, y = Var2, label = round(value, 2)), color = "black", 
            fontface = "bold", size = 5) +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )

ggsave('myplot.png', bg='transparent')


#### NON-KAKAO
corr_postnon <- cor(Nonpost.inapp)
melt_postnon <- melt(corr_postnon)
head(melt_postnon)
upper_postnon <- corr_postnon

# Make upper triangular matrix by setting NA to lower triangular part:
upper_postnon[lower.tri(upper_postnon )] <- NA
up_m_postnon <- melt(upper_postnon, na.rm = TRUE)
head(up_m_postnon)

ggplot(data = up_m_postnon, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(midpoint = 0.5, mid ="grey70", limits = c(-1, +1)) +
  labs(x = "", y = "", fill = "Correlation \n Measure") +
  theme(plot.title = element_text(hjust = 0.5, colour = "blue"), 
        axis.title.x = element_text(face="bold", colour="darkgreen", size = 12),
        axis.title.y = element_text(face="bold", colour="darkgreen", size = 12),
        legend.title = element_text(face="bold", colour="brown", size = 10)) +
  geom_text(aes(x = Var1, y = Var2, label = round(value, 3)), color = "black", 
            fontface = "bold", size = 5)+
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )



ggsave('nonkakao.png', bg='transparent')




colnames(cor) <-  c("Anipang", "kakao_talk", "kakao_story", "kakao_game")

rownames(M) <- c("alpha", "beta", NA, "$a[0]", "$ a[beta]")
vif(post.inapp)
out <- data.frame(kakao$t_anipang,kakao$t_non_kakao_talk, kakao$t_non_kakao_story, kakao$t_non_kakao_game,kakao$t_non_kakao)
plot(out)
cor(out)
cor(kakao$t_kakao_talk,kakao$week)
plot(kakao)

kakao$week <- as.factor(kakao$week)
str(kakao$week)

summary(lm(t_kakao_talk+ t_kakao_game+t_kakao_story+t_anipang~ week, data=kakao))



## t_anipang   v.s. t_kakao_talk
CrossTable(kakao$t_anipang, kakao$inapp)
talktable <- table(kakao$t_anipang,kakao$t_kakao_talk)
prop.table(talktable, 1)



# Brand Category Frequency
table(data$genre_new)
hist(data$genre_new)

# Days of Week Frequency
table(data$day)

# Average Clip Duration
mean(data$clip_duration)




## genre_new v.s. Gender
CrossTable(pre_roll$gender, pre_roll$genre)
genretable <- table(pre_roll$gender, pre_roll$genre)
prop.table(genretable, 1)

## ad_brand_cat v.s. Gender
CrossTable(pre_roll$gender, pre_roll$ad_brand_cat)
brandcat_table <- table(pre_roll$gender, pre_roll$ad_brand_cat)
prop.table(brandcat_table, 1)

table(pre_roll$ad_brand_cat)
