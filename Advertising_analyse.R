##Task 1##

### Load advertising data 
advertising = read.table("C:/Users/Thong/Desktop/F79MB/Advertising.txt", header = T)
attach(advertising)

#Q1(a) Plot a pairwise plots between the four variables
advertising
pairs(advertising)
pairs(data.frame(facebook,youtube,newspaper,sales), 
      col="blue", font.labels = 7, cex.labels = 2)

#Q1(b) Fit a linear regression model, sales as the response variable
model1 = lm(sales ~ facebook + youtube + newspaper)
summary(model1)
summary.aov(model1)

model2 = lm(sales ~ youtube + facebook + newspaper, data=advertising)
summary(model2)
summary.aov(model2)

model3 = lm(sales ~ youtube + facebook, data=advertising)
summary(model3)
summary.aov(model3)

par(mfrow = c(2,2))
plot(model3, which = 1:4)

#Correlation matrix for quantative variables
cor(data.frame(sales,youtube,facebook),use="pairwise.complete.obs",method="pearson")

#Extra Model 4, only youtube
model4 = lm(sales ~ youtube, data=advertising)
summary(model4)
summary.aov(model4)

#1(c) Linear Regression Model with youtube^(1/2) and fb as explanatory variables, interaction terms
model5 = lm(sales ~ sqrt(youtube) * facebook, data = advertising)
summary(model5)
summary.aov(model5)

par(mfrow = c(2,2))
plot(model5, which = 1:4)
# Residuals vs Fitted (MODEL 5)
par(mfrow = c(1,1))
plot(residuals(model5) ~ fitted(model5))
plot(lm(residuals(model5) ~ fitted(model5)))


##===================================================================##

##Task 2##
rm(list = ls(all.names=TRUE))
accidents = read.csv("C:/Users/Thong/Desktop/F79MB/ship accidents.csv", header = T)
attach(accidents)
Type = as.factor(Type)
Construction = as.factor(Construction)
summary(accidents)

#Q2(a) Plot Accidents vs Service,Type,Construction
#Accidents vs Service
#Categorized by Type
plot(Accidents ~ Service, pch = 19, col = factor(Type),
     xlab = "Months of Service", ylab = "Number of Accidents",
     main = "Scatterplot of Accidents against Service")
legend("topleft", legend = levels(factor(Type)),
       pch = 19, cex = 0.8,
       col = factor(levels(factor(Type))))

#Categorized by Period of Constructions
colors <- c("#FDAE61", # Orange
            "#D9EF8B") # Light green
plot(Accidents ~ Service, pch = 19, 
     xlab = "Months of Service", ylab = "Number of Accidents",
     col = colors[factor(Construction)],
     main = "Scatterplot of Accidents against Service")
legend("topleft", legend = levels(factor(Construction)),
       pch = 19, col = colors, cex = 0.8)

#Accidents vs Type
plot(Accidents ~ Type, col = c("#333333", "#FF6699", "#66FF66"),
     main="Boxplot of Accidents against Type of Ship", cex.main=0.8)
legend("topright", cex = 0.7, 
       c("A","B","C"), fill = c("#333333", "#FF6699", "#66FF66"), 
       xpd = TRUE, horiz = TRUE)


#Accidents vs Construction
plot(Accidents ~ Construction, col = c("#009999", "#0000FF"),
     main="Boxplot of Accidents against Type of Ship", cex.main=0.8)

crosstab = table(Type,accidents$Construction); crosstab
barplot(crosstab, col = c("red","orange", "yellow"), ylab="Frequency", xlab ="Accidents", main="Construction Periods", cex.main=0.8, cex.lab=0.8)
par(xpd = TRUE)
legend("bottom",inset = c(0, -0.75), 
       cex = 0.6, c("A","B","c"), fill = c("red", "orange", "yellow"), 
       xpd = TRUE, horiz = TRUE)

#Q2(b) Test of independence on ship type & period of construction
#Produce contigency table for period of construction and ship type
crosstab = table(Construction, accidents$Type); crosstab

#Find the expected ferquencies
chisq.test(crosstab)$expected

#chi-squared test for independence
chisq.test(crosstab)

#Q2(c) Model the data in GLM, Accidents as Poisson Res.Variable, no interaction term
modelA = glm(Accidents ~ Service + Type + Construction, family = poisson)
summary(modelA)
pchisq(modelA$deviance , df=29, lower.tail=F); 
1-pchisq(66.826, df=29)
anova(modelA,test="Chisq")

modelB = glm(Accidents ~ Service + Type, family = poisson)
summary(modelB)
pchisq(modelB$deviance , df=30, lower.tail=F); 
anova(modelB,test="Chisq")

modelC = glm(Accidents ~ Service + Construction, family = poisson)
summary(modelC)
pchisq(modelC$deviance , df=31, lower.tail=F); 
anova(modelC,test="Chisq")

modelD = glm(Accidents ~ Type + Construction, family = poisson)
summary(modelD)
pchisq(modelD$deviance , df=31, lower.tail=F); 
anova(modelD,test="Chisq")

# Change in deviance for model A and model B
modelB$deviance - modelA$deviance
modelB$df.residual - modelA$df.residual
# Test whether removing construction explanatory variable fits significantly better than model C
1 - pchisq(modelB$deviance - modelA$deviance, modelB$df.residual - modelA$df.residual)

# Change in deviance for model A and model B
modelC$deviance - modelA$deviance
modelC$df.residual - modelA$df.residual
# Test whether removing type explanatory variable fits significantly better than model C
1 - pchisq(modelC$deviance - modelA$deviance, modelC$df.residual - modelA$df.residual)

#Model A vs null model 
# Test whehter model A fits better than the null model
modelA$null.deviance - modelA$deviance
modelA$df.null - modelA$df.residual 
1 - pchisq(modelA$null.deviance - modelA$deviance, df=modelA$df.null - modelA$df.residual)
1 - pchisq(534.118,4)
  
#Plots (DRAFT 1)
#Residuals vs Fitted Values
#By Type
plot(resid(modelA, type = "deviance") ~ fitted.values(modelA), type="n",
     ylab = "Deviance Residuals", xlab = "Fitted Values",
     main = "Plot of Deviance Residuals against Fitted Values", cex.main = 0.8)
points(resid(modelA, type = "deviance")[Type=="A"] ~ fitted.values(modelA)[Type=="A"], col="#333333", pch=19)
points(resid(modelA, type = "deviance")[Type=="B"] ~ fitted.values(modelA)[Type=="B"], col="#FF6699", pch=19)
points(resid(modelA, type = "deviance")[Type=="C"] ~ fitted.values(modelA)[Type=="C"], col="#66FF66", pch=19)
legend("topright", 
       c("Type A","Type B","Type C"), col = c("#333333", "#FF6699", "#66FF66"),
       pch = 19, cex = 0.8) 
abline(h = 0,lty = 2)

#By Construction
plot(resid(modelA, type = "deviance") ~ fitted.values(modelA), type="n",
     ylab = "Deviance Residuals", xlab = "Fitted Values",
     main = "Plot of Deviance Residuals against Fitted Values", cex.main = 0.8)
points(resid(modelA, type = "deviance")[Construction=="1940-49"] ~ fitted.values(modelA)[Construction=="1940-49"], col="#FDAE61", pch=19)
points(resid(modelA, type = "deviance")[Construction=="1950-59"] ~ fitted.values(modelA)[Construction=="1950-59"], col="#D9EF8B", pch=19)
legend("topright", 
       c("1940-49","1950-59"), col = c("#FDAE61", "#D9EF8B"),
       pch = 19, cex = 0.8) 
abline(h = 0,lty = 2)

#Residuals vs Service 
#(By TYPE)
plot(resid(modelA, type = "deviance") ~ Service, xlab="Months in Service", ylab="Deviance residuals", type = "n",
     main = "Plot of Deviance Residuals against Service", cex.main = 0.8)
points(resid(modelA, type = "deviance")[Construction=="1940-49" & Type=="A"]
       ~ Service[Construction=="1940-49" & Type=="A"], col="#333333", pch=19)
points(resid(modelA, type = "deviance")[Construction=="1940-49" & Type=="B"] 
       ~ Service[Construction=="1940-49" & Type=="B"], col="#FF6699", pch=19)
points(resid(modelA, type = "deviance")[Construction=="1940-49" & Type=="C"]
       ~ Service[Construction=="1940-49" & Type=="C"], col="#66FF66", pch=19)

points(summary(modelA)$deviance.resid[Construction=="1950-59" & Type=="A"]
       ~ Service[Construction=="1950-59" & Type=="A"], col="#333333", pch=19)
points(summary(modelA)$deviance.resid[Construction=="1950-59" & Type=="B"] 
       ~ Service[Construction=="1950-59" & Type=="B"], col="#FF6699", pch=19)
points(summary(modelA)$deviance.resid[Construction=="1950-59" & Type=="C"]
       ~ Service[Construction=="1950-59" & Type=="C"], col="#66FF66", pch=19)
legend("topright", c("Type A","Type B","Type C"), col = c("#333333", "#FF6699", "#66FF66"),pch = 19, cex = 0.8) 
abline(h = 0,lty = 2)

#(BY CONSTRUCTION)
plot(resid(modelA, type = "deviance") ~ Service, xlab="Months in Service", ylab="Deviance residuals", type = "n",
     main = "Plot of Deviance Residuals against Service", cex.main = 0.8)
points(resid(modelA, type = "deviance")[Construction=="1940-49"]
       ~ Service[Construction=="1940-49"], col="#FDAE61", pch=19)
points(resid(modelA, type = "deviance")[Construction=="1950-59"] 
       ~ Service[Construction=="1950-59"], col="#D9EF8B", pch=19)
legend("topright", 
       c("1940-49","1950-59"), col = c("#FDAE61", "#D9EF8B"),
       pch = 19, cex = 0.765) 
abline(h = 0,lty = 2)

#Fitted Value vs Service
#By Type
plot(fitted(modelA) ~ Service , ylab="Fitted values", type = "n",
     main = "Plot of Fitted Values against Service")
points(fitted(modelA)[Type=="A"] ~ Service[Type=="A"], col="red", pch=18)
points(fitted(modelA)[Type=="B"] ~ Service[Type=="B"], col="orange", pch=18)
points(fitted(modelA)[Type=="C"] ~ Service[Type=="C"], col="purple", pch=18)
legend(x="topleft",cex=0.745, legend=c("Type A","Type B", "Type C"),pch=c(18, 18), col=c("red","orange","purple"))

#By Construction
plot(fitted(modelA) ~ Service , ylab="Fitted values", type = "n",
     main = "Plot of Fitted Values against Service")
points(fitted(modelA)[Construction=="1940-49"] ~ Service[Construction=="1940-49"], col="#FDAE61", pch=18)
points(fitted(modelA)[Construction=="1950-59"] ~ Service[Construction=="1950-59"], col="#D9EF8B", pch=18)
legend(x="topleft",cex=0.745, legend=c("1940-49","1950-59"),pch=c(18, 18), col=c("#FDAE61","#D9EF8B"))

#log(Accidents) vs Service
plot(log(Accidents) ~ Service, pch = 19, col = factor(Type),
     main = "Plot of log(Accidents) against Service")
legend("topleft", legend = levels(factor(Type)),
       pch = 19, cex = 0.8,
       col = factor(levels(factor(Type))))
plot(Accidents ~ Service, pch = 19, col = factor(Type))
legend("topleft", legend = levels(factor(Type)),
       pch = 19, cex = 0.8,
       col = factor(levels(factor(Type))))


#Q2(d)i. 
AIC(modelA)
AIC(modelB)-AIC(modelA)

#Q2(d)ii.
predict(modelA, data.frame(Type = "A" , Construction = "1940-49", Service = 600), type = "response")
