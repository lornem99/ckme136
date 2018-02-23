install.packages("dplyr")
library(corrplot)
library(dplyr) 

getWinningTeams <- function(teams,startYear) {
  winningteams = teams[teams$year > startYear,]
  winningteams = winningteams[winningteams$div_win == "Y" |
                              winningteams$wc_win == "Y" | 
                              winningteams$lg_win == "Y" | 
                              winningteams$ws_win == "Y",]
  
  
  return(winningteams)
}

getFieldingClean <- function(fielding) {

  # Drop all rows with NA in po or a column
  fielding_temp=fielding[!is.na(fielding$po) & !is.na(fielding$a),]
  
  # Players can play multiple positions
  # The primary position of the player is the position where he has accumulated the most po and a
  fielding_temp$po_a <- rowSums(fielding_temp[,c("po","a")])

  fielding_clean=fielding_temp %>% group_by(year,player_id) %>% filter(po_a == max(po_a))

  return(fielding_clean)
}

getWinningBatters <- function(teams,batting,fielding,salary,player,allstar,award,startYear, minAb)
{
  winningTeams=getWinningTeams(teams,startYear)
  winningBatters=merge(winningTeams[,c("year","team_id")], batting, by=c("year","team_id"))
  winningBatters=getBatters(winningBatters,fielding,salary,player,allstar,award,startYear,minAb)
  return(winningBatters)
}  

getBatters <- function(batting, fielding, salary, player, allstar,award,startYear, minAb)
{
  batters=merge(fielding[,c("player_id","pos","year","po", "a","e")],batting,by=c("player_id","year"))
#  allbatters_singlePos=allbatters_multiPos[allbatters_multiPos$po > 50 | allbatters_multiPos$a > 50,]
  batters=merge(salary[,c("year", "player_id", "salary")], batters,by=c("player_id","year"))
  batters=merge(player[,c("player_id","debut","birth_year")],batters,by=c("player_id"))
  #batters=merge(allstar[,c("player_id","year","game_id")],batters,by=c("player_id","year"))
  #batters=merge(award[,c("player_id","year","award_id")],batters,by=c("player_id","year"))
  #batters$isAllStar=!is.na(batters$game_id)
  #batters$isAwardWinner=!is.na(batters$award_id)
  batters$debut=as.Date(batters$debut)
  batters$hasFreeAgentStatus=batters$year - as.numeric(format(batters$debut,"%Y")) >= 6
  batters$age=batters$year - as.numeric(batters$birth_year)
  batters$age25to30=(batters$year - as.numeric(batters$birth_year) >=25) & 
                    (batters$year - as.numeric(batters$birth_year) < 30)
  batters$age30to35=(batters$year - as.numeric(batters$birth_year) >=30) & 
                    (batters$year - as.numeric(batters$birth_year) < 35)
  batters$age35to50=(batters$year - as.numeric(batters$birth_year) >=35)
  batters=batters[which(batters$ab > minAb),]
  batters=batters[which(batters$year > startYear),]
  batters=batters[!duplicated(batters[c("player_id","year")]),]
  return(batters)
}

performPCA <- function(object,features) {
  result=princomp(object[,features],cor=T)
  return(result)
}

performPCAForRegression <- function(object,features){
  result=prcomp(object[,features],scale=TRUE)
  return(result)
}

showBattersPairsPlot <- function(batters) {
  dev.off()
  positions=factor(winningBatters$pos)
  pairs(winningBatters[,c("salary","r", "h", "double", "triple", "hr", "rbi", "sb", "cs","bb","so","ibb","hbp", "sh","sf","g_idp","po","a","e")],
        col = positions, lower.panel = NULL, pch = 6, cex = 0.5)
  legend("bottomleft",bty = "n", legend = unique(positions), pch = 6, col = c("black","red","green","yellow","blue","violet","grey","pink","tan"),xpd = T, cex = .75, y.intersp = 0.5)
} 

showBattersCorPlot <- function(batters) {
  batters_corr=cor(batters[,c("salary","r", "h", "double", "triple", "hr", "rbi", "sb", "cs","bb","so","ibb","hbp", "sh","sf","g_idp","po","a","e","age")])
  corrplot::corrplot(batters_corr,method="pie")
  return(batters_corr)
}

showSummaryStatistics <- function(batters, features) {
  batters_temp=batters
  result=sapply(batters_temp, function (x) if (is.numeric(x)) { 1 } else { 0 })
  numeric_features=names(which(result==1))
  par(mfrow = c(2,3))
  for (feature in numeric_features)
  {
    boxplot(batters_temp[,c(feature)],main=feature)
    print(feature)
    print(summary(batters_temp[,c(feature)]))
  }  
}

removeOutliers <- function(batters, features) {
  batters_temp=batters
  for (feature in features)
  {
    result=summary(batters_temp[,c(feature)])
    q3=result[5]
    q1=result[2]
    iqr=q3-q1
    upper_fence=q3+(1.5*iqr)
    lower_fence=q1-(1.5*iqr)
    batters_temp=batters_temp[which(batters_temp[,c(feature)] < upper & 
                       batters_temp[,c(feature)] > lower),]
  }
  return(batters_temp)
}


teams = read.csv("C:/Users/Lenovo/Downloads/ckme136/team.csv")
batting = read.csv("C:/Users/Lenovo/Downloads/ckme136/batting.csv")
pitching = read.csv("C:/Users/Lenovo/Downloads/ckme136/pitching.csv")
fielding = read.csv("C:/Users/Lenovo/Downloads/ckme136/fielding.csv")
salary = read.csv("C:/Users/Lenovo/Downloads/ckme136/salary.csv")
player = read.csv("C:/Users/Lenovo/Downloads/ckme136/player.csv")
allstar = read.csv("C:/Users/Lenovo/Downloads/ckme136/all_star.csv")
award = read.csv("C:/Users/Lenovo/Downloads/ckme136/player_award.csv")
award = award[!duplicated(award[c("player_id","year")]),]

fieldingClean=getFieldingClean(fielding)

batters=getBatters(batting,fieldingClean,salary,player,allstar,award,2009,100)
batters=batters[which(batters$hasFreeAgentStatus==TRUE),]

batters=removeOutliers(batters,c("salary"))
showSummaryStatistics(batters,names(batters))

winningBatters=getWinningBatters(teams,batting,fieldingClean,salary,player,allstar,award,2009,100)
winningBatters=winningBatters[which(winningBatters$hasFreeAgentStatus==TRUE),]

showSummaryStatistics(batters,names(winningBatters))

# Data for clustering
#write.csv(winningBatters,file="C:/Users/Lenovo/Downloads/ckme136/winningbattersab200.csv",row.names=FALSE)

#showBattersPairsPlot(winningBatters)
winningBattersCor=showBattersCorPlot(batters)

#Clustering
# TO BE COMPLETED


#Dimension Reduction
pcaBasicOffensiveMetrics=performPCA(winningBatters,c("r", "h", "hr", "rbi", "bb"))
pcaBasicOffensiveMetrics$loadings
summary(pcaBasicOffensiveMetrics)

pcaAdvancedOffensiveMetrics=performPCA(winningBatters,c("r", "h", "double", "triple", "hr", "rbi", "sb", "cs","bb","so","ibb","hbp", "sh","sf","g_idp","po","a","e"))
pcaAdvancedOffensiveMetrics$loadings
summary(pcaAdvancedOffensiveMetrics)


#Regression Models (Salary ~ B0 + B1x + B2y + B3z), Salary is the dependent variable
#Model 1: Feature selection of features that are correlated with salary
#Features: Features with correlation of .2 and higher with salary
dataCorModel=winningBatters[,names(which(abs(winningBattersCor[1,]) > .2))]
corModel=lm(salary~.,data=dataCorModel)
summary(corModel)

plot(predict(corModel),dataCorModel$salary,abline(a = 0, b = 1, col = "blue"))



#Model 2:  PCA with regression 
#Features: "r", "h", "hr", "rbi", "bb"
###############################################################################
pcaRegrBom=performPCAForRegression(winningBatters,c("r", "h", "hr", "rbi", "bb"))
summary(pcaRegrBom)
pcBomScores=pcaRegrBom$x
# Choose 3 principal components because they cover 94% of the variance
bomModel=lm(salary~pcBomScores[,1:3], data=winningBatters)
summary(bomModel)

par(mfrow = c(1,1))
plot(predict(bomModel),winningBatters$salary,abline(a = 0, b = 1, col = "red"))
###############################################################################


#Model 3:  PCA with regression 
#Features: "r", "h", "double", "triple", "hr", "rbi", "sb", "cs","bb","so","ibb","hbp", "sh","sf","g_idp","po","a","e"
###############################################################################

pcaRegrAom=performPCAForRegression(winningBatters,c("r", "h", "double", "hr", "rbi", "bb","ibb","hbp"))
summary(pcaRegrAom)
pcAomScores=pcaRegrAom$x
# Choose 12 principal components because they cover 94% of the variance
aomModel=lm(salary~pcAomScores[,1:4], data=winningBatters)
summary(aomModel)

par(mfrow = c(1,1))
plot(predict(aomModel),winningBatters$salary,abline(a = 0, b = 1, col = "red"))

anova(bomModel,aomModel,corModel)
anova(bomModel,corModel)
anova(aomModel,bomModel)
anova(aomModel,corModel)
anova(corModel,aomModel)

#Model 5:  PCA with regression 
#Features: "h", "double","triple","hr", "bb","hbp"
###############################################################################
pcaRegrBom=performPCAForRegression(winningBatters,c("h", "double","triple","hr", "bb","hbp"))
summary(pcaRegrBom)
pcBomScores=pcaRegrBom$x
# Choose 3 principal components because they cover 94% of the variance
bomModel=lm(salary~pcBomScores[,1:4], data=winningBatters)
summary(bomModel)


#Model 4: PCR packages
Hitters=winningBatters[,c("salary","r", "h", "double", "triple", "hr", "rbi", "sb", "cs","bb","so","ibb","hbp", "sh","sf","g_idp","po","a","e")]
pcr.fit = pcr(salary~., data=Hitters,scale=T,validation="CV")
summary(pcr.fit)

validationplot(pcr.fit,val.type="MSEP")

train = sample(c(TRUE,FALSE), nrow(Hitters),rep=TRUE)
test  = (!train)

pcr.fit = pcr(salary~., data=Hitters,scale=T,subset=train,validation="CV")
out.pcr = RMSEP(pcr.fit)
dim(out.pcr$val)

out.pcr$comps[which.min(out.pcr$val[1,1,])]

pcr.fit = pcr(salary~., data=Hitters,scale=T,subset=train,validation="CV")
x = model.matrix(~.,data=Hitters)
y = Hitters$salary
pcr.pred = predict(pcr.fit,x[test,c("r", "h", "double", "triple", "hr", "rbi", "sb", "cs","bb","so","ibb","hbp", "sh","sf","g_idp","po","a","e")],ncomp=16)
sqrt(mean((pcr.pred - y[test])^2))

plot(pcr.pred,y[test],abline(a = 0, b = 1, col = "blue"))


# Backward and forward feature selection

# full=lm(salary~.,data=winningBatters)
# full=lm(salary~.,data=winningBatters[,names(winningBatters)[c(-1,-2,-3,-4,-6,-10,-11,-12)]])
# null=lm(salary~1,data=winningBatters)
# stepF <- stepAIC(null, scope=list(lower=null, upper=full), direction= "forward", trace=TRUE)
# sback=stepAIC(full,direction="backward")
# sboth=stepAIC(null,direction="both",scope=list(upper=full,lower=null))

# Models from backward and forward feature selection

# batters from > 2009 and ab=100
# lm(formula = salary ~ rbi + ab + bb + ibb + h + double + triple + hr + sb + cs + sh + sf + age25to30 + g, data = batters)
# r2 = .41

# lm(salary ~ rbi +  g + ab + ibb + sh + po + age + bb + e + hr + triple + double, data=batters)
# r2 = .4098

# lm(salary ~ rbi + g + ab + bb + age + ibb + sf + double + r + h+ triple + hr + sh + sb,data=batters)
# r2 = .4132

#lm(salary ~ rbi + sh + g + ab + bb + age + ibb + sf,data=batters)
# r2 = .4094

# Random Foreset regressions
# randomForest(salary ~ rbi + sh + g + ab + bb + age + ibb + sf,data=batters,mtry=3, importance=TRUE, na.action=na.omit)

# Residuals evalation

#resid(model1) 
#plot(density(resid(model1))) 
#qqnorm(resid(model1)) 
#qqline(resid(model1))