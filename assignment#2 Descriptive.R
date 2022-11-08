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

