sense<-read.csv("sense.csv")

table(sense$removed, sense$eaten, sense$bait) #sample sizes/ percentages

levels(sense$bait)<-c("control", "substituted", "treatment") #rename bait type

#plot
library(ggplot2)
labels<-c(look="Visual", smell="Odour",  taste="Taste")
ggplot(sense, aes(x=bait, fill = factor(eaten))) +  
  geom_bar(position = "fill") + 
  facet_grid(~ removed, labeller=labeller(removed = labels)) + 
  theme_classic()+
  scale_fill_manual(values=c("grey", "black"), labels=c("Uneaten", "Eaten"), guide = guide_legend(title = "Responce"))+
  labs(x = "Bait type", y= "Bait Responce")

#analyse
model<-glm(eaten~removed*bait+sex+origin+time, data=sense, family=binomial)
summary(model)

origin<-glm(eaten~removed*bait+sex+time, data=sense, family=binomial)
anova(model,origin, test="Chisq")

interaction<-glm(eaten~removed+bait+sex+origin+time, data=sense, family=binomial)
anova(interaction, model, test="Chisq")


bait<-glm(eaten~removed+sex+origin+time, data=sense, family=binomial)
anova(bait,model, test="Chisq")

removed<- glm(eaten~bait+sex+origin+time, data=sense, family=binomial)
anova(removed,model, test="Chisq")

sex<-glm(eaten~removed*bait+origin+time, data=sense, family=binomial)
anova(sex,model, test="Chisq")

time<-glm(eaten~removed*bait+origin+sex, data=sense, family=binomial)
anova(time,model, test="Chisq")

# probably not the way to go- not a repeated measures
# library(lme4)
# model<-glmer(eaten~removed*bait+time+sex+origin+(1|arks), data=sence, family=binomial)
# summary(model)
# origin<-glmer(eaten~removed+time+sex+(1|arks), data=swap, family=binomial)
# anova(origin, model)
