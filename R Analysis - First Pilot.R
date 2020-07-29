
# load the data and necessary libraries
library(readxl)
library(ggplot2)
library(GGally)
library(reshape2)
library(lme4)
library(compiler)
library(parallel)
#library(boot)
library(lattice)



############# data cleaning and data visualization ############# 


# change following line in your laptop
pilot_data <- read_excel("Grammaticality Judgement Task R Data Frame.xlsx")
View(pilot_data)
pilot_data<-as.data.frame(pilot_data)

head(pilot_data)

# create a function that will identify subjects more accurately (not needed in pilot)
# take the last 3 characters of a vector
# substrRight <- function(x, n){
#   substr(x, nchar(x)-n+1, nchar(x))
# }




# identify unique subjects (not needed in pilot)
# rsvp_actual <- as.data.frame(rsvp[-which(is.na(rsvp$Prompt.Condition)),])
# rsvp_actual$Subject_Unique <- as.factor(paste0(rsvp_actual$Subject,substrRight(as.character(rsvp_actual$ExperimentName),3)))
# 
# typeof(rsvp_actual)
# rsvp_actual<-as.data.frame(rsvp_actual)
# View(rsvp_actual)
# 
# dim(rsvp_actual)


# investigate the variables
# binary response
table(pilot_data$Subject)
table(pilot_data$Response)

# categorical predictor
table(pilot_data$SynType)
table(pilot_data$Family)
table(pilot_data$Item)
#table(pilot_data$ynbias)



# continuous predictor
# hist(pilot_data$Prompt.RT)
# hist(pilot_data$SentenceLength)


# random intercept
unique(pilot_data$Family)
unique(pilot_data$Subject)




# graph our continuous predictor variables
# ggpairs(rsvp_actual[, c("Prompt.RT", "SentenceLength")])


# ggplot(rsvp_actual, aes(x = Prompt.Condition, y = Prompt.RT)) +
#   stat_sum(aes(size = ..n.., group = 1)) +
#   scale_size_area(max_size=5)



# ggplot(rsvp_actual, aes(x = TrialPattern, y = Prompt.RT)) +
#   stat_sum(aes(size = ..n.., group = 1)) +
#   scale_size_area(max_size=5)



# tmp <- melt(pilot_data[, c("SynType","Response")], id.vars="SynType")
# ggplot(tmp, aes(x = SynType, y = value)) +
#   geom_jitter(alpha = .1) +
#   geom_violin(alpha = .75) +
#   facet_grid(variable ~ .) +
#   scale_y_sqrt()



# tmp <- melt(rsvp_actual[, c("Prompt.Condition", "SentenceLength")], id.vars="Prompt.Condition")
# ggplot(tmp, aes(x = Prompt.Condition, y = value)) +
#   geom_jitter(alpha = .1) +
#   geom_violin(alpha = .75) +
#   facet_grid(variable ~ .) +
#   scale_y_sqrt()







# tmp <- melt(rsvp_actual[, c("Prompt.ACC", "Prompt.RT", "SentenceLength")],
#             id.vars="Prompt.ACC")
# ggplot(tmp, aes(factor(Prompt.ACC), y = value, fill=factor(Prompt.ACC))) +
#   geom_boxplot() +
#   facet_wrap(~variable, scales="free_y")




# tmp1 <- melt(rsvp_actual[, c("Prompt.RESP", "Prompt.RT", "SentenceLength")],
#             id.vars="Prompt.RESP")
# ggplot(tmp1, aes(factor(Prompt.RESP), y = value, fill=factor(Prompt.RESP))) +
#   geom_boxplot() +
#   facet_wrap(~variable, scales="free_y")






############# fit a GLM with random effects for item and subject############# 

# estimate the model and store results in m
m1 <- glmer(as.factor(Response) ~ SynType + (1|Family) + (1|Subject), 
            data=pilot_data, nAGQ=0, 
            family="binomial", na.action = "na.omit")

# examine the model output
summary(m1)
BIC(m1)

# view random effects
res <- ranef(m1)

# these should resemble normal distributions
hist(unlist(res$Family))
hist(unlist(res$Subject))

mnull <- glm(as.factor(Response) ~ 1, 
             data=pilot_data, 
             family="binomial", na.action = "na.omit")
BIC(mnull)

m3 <- glmer(as.factor(Response) ~ 
              SynType*Family + (1|Subject), 
            data=pilot_data, nAGQ=0, 
            family="binomial", na.action = "na.omit")
summary(m3)
c("null"=BIC(mnull), "full"=BIC(m1), 
  "noPattern"=BIC(m3))

barplot(table(pilot_data$Response, pilot_data$SynType))



# accuracy by response time
# Response by SynType
m4 <- glmer(as.factor(Response) ~ SynType + (1|Subject) + (1|Family),
            data=pilot_data, nAGQ=0,
            family="binomial", na.action = "na.omit")
summary(m4)
BIC(m4)



# yes/no bias
# rsvp_actual$ynbias <- rsvp_actual$WordLength=="F"
# 
# ynmodel <- glm(rsvp_actual$Prompt.ACC ~ rsvp_actual$ynbias,
#                family="binomial")
# summary(ynmodel)


# accuracy by response time
# m4 <- glmer(as.factor(Prompt.ACC) ~ Prompt.RT + 
#               Prompt.Condition + (1|Trials) + (1|Subject_Unique), 
#             data=rsvp_actual, nAGQ=0, 
#             family="binomial", na.action = "na.omit")
# summary(m4)
# BIC(m4)


# m7 <- glmer(as.factor(Prompt.ACC) ~ Prompt.RT + SentenceLength + 
#               Prompt.Condition + (1|Trials) + (1|Subject_Unique), 
#             data=rsvp_actual, nAGQ=0, 
#             family="binomial", na.action = "na.omit")
# summary(m7)
# BIC(m7)



# yes/no by response time
# consider subset called rsvp_posRT where RT is positive
# rsvp_posRT <- rsvp_actual[rsvp_actual$Prompt.RT>0,]
# m5 <- glmer(as.factor(Prompt.RESP) ~ Prompt.RT + 
#               Prompt.Condition + (1|Trials) + (1|Subject_Unique), 
#             data=rsvp_posRT, nAGQ=0, 
#             family="binomial", na.action = "na.omit")
# summary(m5)
# BIC(m5)

# we add SentenceLength in model
# m6 <- glmer(as.factor(Prompt.RESP) ~ Prompt.RT + SentenceLength +
#               Prompt.Condition + (1|Trials) + (1|Subject_Unique), 
#             data=rsvp_posRT, nAGQ=0, 
#             family="binomial", na.action = "na.omit")
# summary(m6)
# BIC(m6)




############# data analysis and statistical inference ############# 

# print the mod results without correlations among fixed effects
print(m3, corr = FALSE)
se <- sqrt(diag(vcov(m3)))

# table of estimates with 95% CI
(tab <- cbind(Est = fixef(m3), LL = fixef(m3) - 1.96 * se, UL = fixef(m3) + 1.96 *
                se))


# odds ratios
exp(tab)



# # temporary data
# tmpdat <- rsvp_posRT[, c("Prompt.RT","Prompt.Condition",
#                           "Trials","Subject_Unique")]
# 
# summary(rsvp_posRT$Prompt.RT)
#


jvalues <- with(rsvp_posRT, seq(from = min(Prompt.RT), to = max(Prompt.RT), length.out = 100))

# calculate predicted probabilities and store in a list
pp <- lapply(jvalues, function(j) {
  tmpdat$Prompt.RT <- j
  predict(m5, newdata = tmpdat, type = "response")
})


# get the means with lower and upper quartiles
plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))

# add in values and convert to data frame
plotdat <- as.data.frame(cbind(plotdat, jvalues))

# better names and show the first few rows
colnames(plotdat) <- c("PredictedProbability", "Lower", "Upper", "Prompt.RT")
head(plotdat)


# plot average marginal predicted probabilities
ggplot(plotdat, aes(x = Prompt.RT, y = PredictedProbability)) + 
  geom_line() +
  ylim(c(0, 1))


ggplot(plotdat, aes(x = Prompt.RT, y = PredictedProbability)) + 
  geom_linerange(aes(ymin = Lower,ymax = Upper)) + 
  geom_line() + 
  ylim(c(0, 1))






# calculate predicted probabilities and store in a list
# rsvp_posRT$Prompt.Condition<-as.factor(rsvp_posRT$Prompt.Condition)
# 
# biprobs <- lapply(levels(rsvp_posRT$Prompt.Condition), function(stage) {
#   tmpdat$Prompt.Condition[] <- stage
#   lapply(jvalues, function(j) {
#     tmpdat$Prompt.RT<- j
#     predict(m5, newdata = tmpdat, type = "response")
#   })
# })
# 
# # get means and quartiles for all jvalues for each level 
# plotdat2 <- lapply(biprobs, function(X) {
#   temp <- t(sapply(X, function(x) {
#     c(M=mean(x), quantile(x, c(.25, .75)))
#   }))
#   temp <- as.data.frame(cbind(temp, jvalues))
#   colnames(temp) <- c("PredictedProbability", "Lower", "Upper", "Prompt.RT")
#   return(temp)
# })
# 
# # collapse to one data frame
# plotdat2 <- do.call(rbind, plotdat2)
# 
# # add Prompt.Condition
# plotdat2$Prompt.Condition <- factor(rep(levels(rsvp_actual$Prompt.Condition), each = length(jvalues)))

# show first few rows
# head(plotdat2)
# 
# 
# ggplot(plotdat2, aes(x = Prompt.RT, y = PredictedProbability)) +
#   geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Prompt.Condition), alpha = .15) +
#   geom_line(aes(colour = Prompt.Condition), size = 2) +
#   ylim(c(0, 1)) + facet_wrap(~  Prompt.Condition)
# 
# 
# 
# ggplot(data.frame(Probs = biprobs[[1]][[100]]), aes(Probs)) + geom_histogram() +
#   scale_x_sqrt(breaks = c(0.01, 0.1, 0.25, 0.5, 0.75))
# 
# ggplot(data.frame(Probs = biprobs[[2]][[100]]), aes(Probs)) + geom_histogram() +
#   scale_x_sqrt(breaks = c(0.01, 0.1, 0.25, 0.5, 0.75))
# 
# 
# 

# lattice::dotplot(ranef(m5, which = "Trials", condVar = TRUE), scales = list(y = list(alternating = 0)))
# 
# lattice::dotplot(ranef(m5, which = "Subject_Unique", condVar = TRUE))
# 

# Plan:  Do McNemar's Tet between S/G and SG/GS (wrt "bad"), then between S and G (wrt "weird"), 
# and then between SG and GS (wrt "weird")


# McNemar's test between G and S SynTypes
y1=pilot_data$Response[which(pilot_data$SynType=="G")]

y2=pilot_data$Response[which(pilot_data$SynType=="S")]

y1
y2

length(y1)
length(y2)

table(y1, y2)

mcnemar.test(y1,y2)


# #Here, the p-value indicates a significant result at the 5% level. 
# p-value = 1.383e-05
# #Thus, we reject the null hypothesis and can conclude 
# #that the two groups are different.
# 


y5=pilot_data$Response[which(pilot_data$SynType=="GS")]

y6=pilot_data$Response[which(pilot_data$SynType=="SG")]

y5
y6

length(y5)
length(y6)

table(y5, y6)

mcnemar.test(y5,y6)









# 
# another McNemar's test between G/S on one side, and SG/GS on the other
# y3=pilot_data$Response[which((pilot_data$SynType=="G")|(pilot_data$SynType=="S"))]
# 
# y4=pilot_data$Response[which((pilot_data$SynType=="GS"|pilot_data$SynType=="SG"))]
# 
# y3
# y4
# 
# length(y3)
# length(y4)
# 
# mcnemar.test(y3,y4)

foo = typeof(pilot_data$Response[which(pilot_data$SynType=="S")])

foo


baar = typeof(3)

baar


foolio = typeof(pilot_data$Response[which(pilot_data$SynType==("G" | "S"))])

fool

x1 <- c(rep("y", 34), rep("n", 12))
x2 <- rep("n", 46)

table(x1, x2)

x1 <- factor(x1, levels = c("y", "n"))
x2 <- factor(x2, levels = c("y", "n"))

table(x1, x2)


# y3=pilot_data$Response[which(pilot_data$SynType==("G" | "S"))]
# y3=pilot_data$Response[which(pilot_data$SynType=="G")] & pilot_data$Response[which(pilot_data$SynType=="S")]
y3=pilot_data$Response[which((pilot_data$SynType=="G")|(pilot_data$SynType=="S"))]

y4=pilot_data$Response[which((pilot_data$SynType=="GS")|(pilot_data$SynType=="SG"))]

# y4=pilot_data$Response[which(pilot_data$SynType==("GS" | "SG"))]

y3
y4

length(y3)
length(y4)

table(y3, y4)


# Had to explicitly list the rows and columns of the table, because 
y3 <- factor(y3, levels = c("good", "weird","bad"))
y4 <- factor(y4, levels = c("good", "weird","bad"))

table(y3, y4)

mcnemar.test(y3,y4)
# 
# # Here, the p-value is only .06, and therefore not significant by normal standards.
# 
# 
# #Another McNemar's Test to see if, within the fillers, Length has a significant effect on accuracy
# 
# y5=rsvp_actual$Prompt.ACC[which((rsvp_actual$TrialPattern=="F")&(rsvp_actual$SentenceLength<10))]
# 
# y6=rsvp_actual$Prompt.ACC[which((rsvp_actual$TrialPattern=="F")&(rsvp_actual$SentenceLength>11))]
# 
# y5
# y6
# 
# length(y5)
# length(y6)
# 
# mcnemar.test(y5,y6)
# 
# # Here, the p-value is 0.002985, length therefore has a significant effect on accuracy
