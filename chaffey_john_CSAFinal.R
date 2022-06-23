setwd("C:/Users/chaff/Desktop/Boring Adult Stuff/School Work/UC Irvine/Winter Quarter/Customer and Social Analytics/final")
library("tidyverse")
library(purrr)


highnote <-  read.csv("HighNote Data.csv") 
highnote2 <- read.csv("HighNote Data.csv") 
highnote3 <- read.csv("HighNote Data.csv")


#Summary statistics:
library(psych)
#Overall summary statistics
describe(highnote[-1])

#this generates summ stats for adopters
adopter_summs <- describe(filter(highnote, adopter == 1)[-1])

adopter_summs <- cbind("Variables" = rownames(adopter_summs), adopter_summs)
rownames(adopter_summs) <- 1:nrow(adopter_summs)
colnames(adopter_summs)
adopter_summs


#this generates summ stats for non-adopters
non_adopter_summs <- describe(filter(highnote, adopter == 0)[-1])
non_adopter_summs <- cbind("Variables" = rownames(non_adopter_summs), non_adopter_summs)
rownames(non_adopter_summs) <- 1:nrow(non_adopter_summs)
colnames(non_adopter_summs)
non_adopter_summs

#mean of the data for adopter column
mean_stats <- highnote %>% group_by(adopter) %>% summarise_all(mean)

library(ggplot2)
library(gridExtra)

#Demographics
highnote$adopter = factor(highnote$adopter, labels = c("non-adopter","adopter"))

#age histogram
ggplot(highnote, aes(x = age) + geom_histogram(aes(highnote$adopter), binwidth = 1, color = "black") + labs(title="Age Distribution", fill="")) 


#age
ggplot(highnote,aes(x=age,group=adopter,fill=adopter))+geom_histogram(position="identity", binwidth=0.5)+theme_bw() + labs(title="Age Distribution", fill="") 

#age density
ggplot(highnote, aes(x = age))+ geom_density(aes(fill=adopter), alpha=0.6)+ labs(title="Age Density", fill="") 


#gender bar
#install.packages("sjPlot")
library(sjPlot)
sjPlot::plot_xtab(highnote$adopter, factor(highnote$male, labels = c("female","male")), margin = "row", bar.pos = "stack")
sjPlot::plot_xtab(highnote$adopter, factor(highnote$good_country, labels = c("no","yes")), margin = "row", bar.pos = "stack")


####Peer influence

#Friend_cnt vs Subscriber_friend_cnt
ggplot(highnote, aes(x = friend_cnt, y = subscriber_friend_cnt))+ geom_point(aes(color = adopter), size = 1, alpha = 0.8)+geom_smooth(aes(color = adopter), se = F)+labs(title="Friend_cnt vs Subscriber_friend_cnt", color = " ")


#friend_cnt
friend_cnt<-highnote %>% group_by(adopter)%>% summarise(friend_cnt=mean(friend_cnt))

ggplot(friend_cnt,aes(x = adopter,y=friend_cnt)) + geom_bar(stat="identity",position=position_identity(), fill="navyblue")+theme_classic()+ labs(title="Average friend count")


#friend age
ggplot(highnote,aes(x=avg_friend_age,group=adopter,fill=adopter))+geom_histogram(position="identity",binwidth=0.5)+theme_classic()


#subscriber friend count
subscriber_friend_cnt<-highnote %>% group_by(adopter)%>% summarise(subscriber_friend_cnt=mean(subscriber_friend_cnt))

ggplot(subscriber_friend_cnt,aes(x = adopter,y=subscriber_friend_cnt)) + geom_bar(stat="identity",position=position_identity(), fill="navyblue")+theme_classic()


#avg friend male
avg_friend_male<- highnote %>% group_by(adopter) %>% summarise(avg_friend_male=mean(avg_friend_male))

ggplot(avg_friend_male,aes(x = adopter,y=avg_friend_male)) + geom_bar(stat="identity",position=position_identity(), fill="navyblue")+theme_classic()+labs(title="Average of avg friend male")

#subscription rate
highnote = highnote %>% mutate(subscription_rate = subscriber_friend_cnt/friend_cnt)
adopter_subscription_rate = mean(highnote$subscription_rate[highnote$adopter == "adopter"])
#0.05
nonadopter_subscription_rate = mean(highnote$subscription_rate[highnote$adopter == "non-adopter"])
#0.021

ggplot(highnote, aes(x = adopter, y = subscription_rate)) + geom_bar(stat = "summary", fun.y = "mean", width = 0.3) + labs(title="mean subscription rate")

#overall correlation between the age and friend age
cor(highnote$age,highnote$avg_friend_age)
#0.69



#### User engagement #####################################################

#songs listened
g=songsListened<- highnote %>% group_by(adopter)%>% summarise(songsListened=mean(songsListened))
ggplot(songsListened,aes(x = adopter,y=songsListened)) +geom_bar(stat="identity",position=position_identity(), fill="black")+ labs(title="Average # Songs Listened")


#loved tracks
h=lovedTracks<- highnote %>%  group_by(adopter)%>%  summarise(lovedTracks=mean(lovedTracks))
ggplot(lovedTracks,aes(x = adopter,y=lovedTracks)) +  geom_bar(stat="identity",position=position_identity(), fill="brown")+ labs(title="Average # Loved Songs")


#posts
i=posts<-highnote %>% group_by(adopter)%>%summarise(posts=mean(posts))
ggplot(posts,aes(x = adopter,y=posts)) +geom_bar(stat="identity",position=position_identity(), fill="yellow", color = "black")+ labs(title="Average # of Posts")


#playlists
j=playlists<-highnote %>% group_by(adopter)%>%summarise(playlists=mean(playlists))
ggplot(playlists,aes(x = adopter,y=playlists)) + geom_bar(stat="identity",position=position_identity(), fill="orange", color = "purple")+ labs(title="Average # of Playlists")


#shouts
k=shouts<-highnote %>%group_by(adopter)%>%summarise(shouts=mean(shouts))
ggplot(shouts,aes(x = adopter,y=shouts)) + geom_bar(stat="identity",position=position_identity(), fill="pink", color = "black")+labs(title="Average # of Shouts")


#tenure
l=tenure<- highnote %>% group_by(adopter)%>%summarise(tenure=mean(tenure))
ggplot(tenure,aes(x = adopter,y=tenure)) + geom_bar(stat="identity",position=position_identity(), fill="navyblue")+theme_classic()


##Propensity Score Matching (PSM) Question 3 #######################################################################

#create the treatment variable
highnote3$Treatment <- ifelse(highnote3$subscriber_friend_cnt >= 1,1,0)
#checking the proportions
highnote3 %>%  group_by(Treatment) %>%  summarise(count = n(), proportion_ad = mean(adopter))

#Proportions
table(highnote3$Treatment,highnote3$adopter)
sjPlot::plot_xtab(highnote3$adopter, factor(highnote3$Treatment, labels = c("no","yes")), margin = "row", bar.pos = "stack")

#checking for significance
t.test(highnote3$adopter ~ highnote3$Treatment)


HN_cov <- c('age','male','friend_cnt','avg_friend_age','avg_friend_male','friend_country_cnt','songsListened','lovedTracks','playlists','posts','shouts','good_country', 'tenure')

means <- highnote3 %>% group_by(Treatment) %>% select(one_of(HN_cov)) %>% summarise_all(mean)

#do the t test

lapply(highnote3[,c('age','male' , 'friend_cnt' , 'avg_friend_male' ,'avg_friend_age', 'friend_country_cnt'  , 'songsListened' , 'lovedTracks' , 'posts' , 'playlists' ,'shouts' , 'tenure' ,'good_country')], function(i) t.test(i ~ highnote3$Treatment))

#Propensity Score Estimation
logit_ps <- glm(Treatment ~ age +male+ friend_cnt + songsListened + playlists + posts + shouts + lovedTracks + good_country + avg_friend_age + avg_friend_male + friend_country_cnt + tenure, family = binomial(), data = highnote3)

summary(logit_ps)


prp_df <- data.frame(pr_score = predict(logit_ps, type = "response"),has_subscriber_friends = logit_ps$model$Treatment)
head(prp_df)


#region of common support
labs <- paste("Actual treatment type :", c("having subscriber friends", "No subscriber friends"))
prp_df %>%  mutate(has_subscriber_friends = ifelse(has_subscriber_friends == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~has_subscriber_friends) +
  xlab("Probability of having subscriber friends") +
  theme_bw()

#install.packages("Hmisc")
library(Hmisc)

highnote3$prp_score <- prp_df$pr_score
highnote3$has_subscriber_friends <- prp_df$has_subscriber_friends
histbackback(split(highnote3$prp_score, highnote3$has_subscriber_friends))


#running a matching algorithm
library(MatchIt)
matching <- matchit(Treatment ~ log(age) +log(1+male)+ log(friend_cnt) + log(1+songsListened) + log(1+playlists) + log(1+posts) + log(1+shouts) + log(1+lovedTracks) + log(1+good_country) + log(avg_friend_age) + log(1+avg_friend_male) + log(1+friend_country_cnt) + log(1+tenure), method = "nearest", data = highnote3, caliper=0.08)
summary(matching)

matched_data <- match.data(matching)


#Distribution of propensity score #########################################
###########WARNING THIS KEEPS BREAKING RUNTIME FOR SOME REASON############################################

#commented out so a batch run does not autocrash
#plot(matching,	type	= "jitter") + geom_point(aes(shape = 4), fill = "red") 


####################################################
plot(matching, type="hist")


#install.packages("RItools")
library(RItools)
xBalance(Treatment ~ age + male+friend_cnt + songsListened + playlists + posts + shouts + lovedTracks + good_country + avg_friend_age + avg_friend_male + friend_country_cnt + tenure,	data	= matched_data,	report	= c("chisquare.test"))
#p value 0.091
#t-test
t.test(	matched_data$adopter,matched_data$Treatment)

#distribution plot
sjPlot::plot_xtab(matched_data$adopter, factor(matched_data$Treatment, labels = c("no","yes")), margin = "row", bar.pos = "stack")


#Effects of Treatment
#Plotting the propensity scores in a similar pattern as before, so that we can see a significant difference.
histbackback(split(matched_data$distance, matched_data$has_subscriber_friends))

#T-Tests
matched_cov <- c('age','male','friend_cnt',"songsListened","playlists","posts",'shouts','lovedTracks','good_country','avg_friend_age','avg_friend_male','friend_country_cnt', 'tenure')

match_means <- matched_data %>% group_by(Treatment) %>% select(one_of(matched_cov)) %>% summarise_all(mean)

lapply(matched_data[,c('age','male','friend_cnt',"songsListened","playlists","posts",'shouts','lovedTracks', 'good_country','avg_friend_age','avg_friend_male','friend_country_cnt', 'tenure')], function(i) t.test(i ~ matched_data$Treatment,paired=TRUE))


#Logistic Regression on matched data
#logit_reg1 <- glm(adopter ~ age +male+ friend_cnt + songsListened + playlists + posts + shouts + lovedTracks + good_country + avg_friend_age + subscriber_friend_cnt + avg_friend_male + friend_country_cnt + tenure, family = binomial(), data = matched_data)
#summary(logit_reg1)


#remove variables that are insignificant THIS model is better
log_reg2 <- glm(adopter ~ male +  songsListened + playlists + lovedTracks + good_country + avg_friend_age + subscriber_friend_cnt + tenure, family = binomial(), data = matched_data)
summary(log_reg2)

#getting odds ratios
exp(coef(log_reg2))

