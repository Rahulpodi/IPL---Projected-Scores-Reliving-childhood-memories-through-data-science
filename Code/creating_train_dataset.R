# Packages

library(data.table)
library(stringi)
library(stringr)
library(dplyr)
        
##############################################################################################

# Reading the file which was collated in the previous code
        
data<-readRDS("E:/IPL/Working Data/all_matches_with_wickets.RDS")
transpose_data_whole<-data.frame(t(data))
        
# Home.Ground mapping file
        
homegroud<-read.csv("E:/IPL/home_ground_mapping.csv",stringsAsFactors = FALSE)
homegroud[,"Concatenate"]<-paste0(homegroud[,1],homegroud[,2])

###############################################################################################
        
# Obtaining both the 1st and 2nd innings. However, we can do prediction for the scores 
# in case of chasing too helping us find if the team will win or not
        
# 1st innings data
        
first<-data[-c(min(which(rownames(data) %like% "2nd")):length(rownames(data))),]
        
# Just in case having a transposed dataset as well for the first innings alone
        
transpose_data<-data.frame(t(first))
transpose_data$info.dates<-as.character(transpose_data$info.dates)

##############################################################################################

# Before grouping the dataset at an over level to obtain the train dataset lets
# get the strike rates of batsman,vowler economy rate and strike rate - for which
# we need to structure the data
        
# Creating the dataset of the required form - structured data

#############################################################################################

# For ball by ball datasets
        
structured_data<-data.frame(matrix(NA,112500,16))
colnames(structured_data)<-c("Match.ID","Innings.ID","Season","Batting.team","Bowling.team","Batsman","Bowler","Overs","Ball"
                                     ,"Ground","City","Extras","Extras.Runs","Home.Ground","Wicket","Total.Runs")
        
# Filling the above dataset using either data or transpose_data

# In Order of the variables only
        
structured_data[,"Match.ID"]<-sort(c(rep(rownames(transpose_data),180)),decreasing = FALSE)
structured_data[,"Innings.ID"]<-as.numeric(rep(1,nrow(structured_data)))
structured_data[,"Season"]<-as.character(stri_sub(transpose_data[match(structured_data$Match.ID,rownames(transpose_data)),"info.dates"],1,4))
structured_data[,"Batting.team"]<-as.character(transpose_data[match(structured_data$Match.ID,rownames(transpose_data)),"innings.1st.innings.team"])
structured_data[,"Bowling.team"]<-as.character(transpose_data_whole[match(structured_data$Match.ID,rownames(transpose_data_whole)),"innings.2nd.innings.team"])
        
# Creating a data set to be directly sent into the batsman column since the order are the same
        
# For batting
bat<-data.frame()
for(i in 1:625){
        temp<-data.frame(t(transpose_data[i,seq(19,1272,7)]))
        colnames(temp)<-"bat"
        bat<-rbind(bat,temp)}
        
structured_data[,"Batsman"]<-as.character(bat[,1])
        
# For bowling
bowl<-data.frame()
for(i in 1:625){
        temp<-data.frame(t(transpose_data[i,seq(20,1273,7)]))
        colnames(temp)<-"bowl"
        bowl<-rbind(bowl,temp)}
        
structured_data[,"Bowler"]<-as.character(bowl[,1])
structured_data[,"Overs"]<-as.numeric(rep(sort(rep(1:20,9),decreasing = FALSE),625))
structured_data[,"Ball"]<-as.numeric(rep(seq(1,9,1)[rep(c(TRUE,FALSE), c(9,1))],625))
structured_data[,"Ground"]<-as.character(transpose_data[match(structured_data$Match.ID,rownames(transpose_data)),"info.venue"])
structured_data[,"City"]<-as.character(transpose_data[match(structured_data$Match.ID,rownames(transpose_data)),"info.city"])
        
# For extras runs
extras<-data.frame()
for(i in 1:625){
                temp<-data.frame(t(transpose_data[i,seq(23,1276,7)]))
                colnames(temp)<-"extras"
                extras<-rbind(extras,temp)}
        
structured_data[,"Extras.Runs"]<-as.numeric(as.character(extras[,1]))
        
# Extras yes or no
structured_data[which(structured_data$Extras.Runs!=0),"Extras"]<-1
structured_data[which(structured_data$Extras.Runs==0),"Extras"]<-0

#Wickets
wickets<-data.frame()
for(i in 1:625){
        temp<-data.frame(t(transpose_data[i,seq(25,1278,7)]))
        colnames(temp)<-"wickets"
        wickets<-rbind(wickets,temp)}

structured_data[,"Wicket"]<-as.character(wickets[,1])

# Wicket yes or no column
structured_data[which(is.na(structured_data$Wicket)),"is.wickets"]<-0
structured_data[which(!is.na(structured_data$Wicket)),"is.wickets"]<-1

# For total runs
totruns<-data.frame()
for(i in 1:625){
                temp<-data.frame(t(transpose_data[i,seq(24,1277,7)]))
                colnames(temp)<-"totruns"
                totruns<-rbind(totruns,temp)}
        
structured_data[,"Total.Runs"]<-as.numeric(as.character(totruns[,1]))

# Removing temporary datasets
rm(bat,bowl,extras,temp,totruns,wickets)

structured_data[,"Total.Runs.without.extras"]<-structured_data[,"Total.Runs"]-structured_data[,"Extras.Runs"]
structured_data[,"Concatenate"]<-paste0(structured_data$Batting.team,structured_data$Ground)
structured_data[which(!is.na(match(structured_data$Concatenate,homegroud$Concatenate))),"Home.Ground"]<-1
structured_data[which(is.na(match(structured_data$Concatenate,homegroud$Concatenate))),"Home.Ground"]<-0
structured_data<-structured_data[,-ncol(structured_data)]

# Removing the original wicket column for now

structured_data<-structured_data[,-15]

# Cleaning ip the NA's

structured_data<-structured_data[which(!is.na(structured_data$Batsman)),]

# Writing the file for backup

write.csv(structured_data,"E:/IPL/Working Data/structured_data.csv",row.names = FALSE)
saveRDS(structured_data,"E:/IPL/Working Data/structured_data.RDS")

###############################################################################################

# All Batsman 1st innings strike rate

bsr<-structured_data%>%
        group_by(Batsman)%>%
        summarise(Runs=sum(Total.Runs.without.extras),Balls.Faced=n(),SR=round((sum(Total.Runs.without.extras)/n())*100,digits=2))

# All bowlers bowling economy

ber<-structured_data%>%
        group_by(Bowler)%>%
        summarise(Runs=sum(Total.Runs),Balls.Bowled=floor(n()/6),ECR=round((sum(Total.Runs)/floor(n()/6)),digits=2))

# Bowler strike rate

bosr<-structured_data%>%
        group_by(Bowler)%>%
        summarise(wickets=sum(is.wickets),Balls.Bowled=n(),BSR=round((n()/sum(is.wickets)),digits=2))

# No. of 30 by each batsman

batsman30s<-structured_data%>%
        group_by(Batsman,Match.ID)%>%
        summarise(Tot.Runs=sum(Total.Runs.without.extras))

batsman30s_v2<-batsman30s%>%
        filter(Tot.Runs>=30)%>%
        group_by(Batsman)%>%
        summarise(No.of.30s=n())

###############################################################################################

# Over by over details

# Creating train data finally

# Part1: Retrieving data from structured data

train_df<-structured_data%>%group_by(Match.ID,Overs)%>%summarise(Season=unique(Season),Runs.over=sum(Total.Runs)
                                                                 ,Extra.runs.over=sum(Extras.Runs),
                                                                 Extras.over=sum(Extras),Wickets.over=sum(is.wickets)
                                                                 ,Home.Ground=sum(Home.Ground)/n()
                                                                 ,Ground=unique(Ground)
                                                                 ,Bowlername=unique(Bowler)[1])

train_df$Ground<-as.factor(train_df$Ground)
train_df$Home.Ground<-as.factor(train_df$Home.Ground)

# Part2: Making required data from the sparsely created data set above

train_df[,"Wickets.ever"]<-data.frame(train_df%>%group_by(Match.ID,Overs)%>%summarise(Wickets.Ever.1=sum(Wickets.over))%>%mutate(Wickets.Ever=cumsum(Wickets.Ever.1)))[,4]
train_df[,"Runs.ever"]<-data.frame(train_df%>%group_by(Match.ID,Overs)%>%summarise(Runs.Ever.1=sum(Runs.over))%>%mutate(Runs.Ever=cumsum(Runs.Ever.1)))[,4]
train_df[,"Extras.ever"]<-data.frame(train_df%>%group_by(Match.ID,Overs)%>%summarise(Extras.Ever.1=sum(Extras.over))%>%mutate(Extras.Ever=cumsum(Extras.Ever.1)))[,4]
train_df[,"Extras.Runs.ever"]<-data.frame(train_df%>%group_by(Match.ID,Overs)%>%summarise(Extras.Runs.Ever.1=sum(Extra.runs.over))%>%mutate(Extras.Runs.Ever=cumsum(Extras.Runs.Ever.1)))[,4]

# Part 3: The difficult part - Batsman and bowler details

# Defining the required columns
train_df[,c("b1name","b2name","b3name","b4name","b5name")]<-NA

for(i in unique(structured_data$Match.ID))
{
        for(j in 1:20)
        {
                print(paste(i,"_",j,sep = ""))
                bat_vector<-data.frame(table(structured_data[which(structured_data$Match.ID==i & structured_data$Overs==j),c("Batsman","Ball")]$Batsman))
                
                if(nrow(bat_vector)==1)
                {
                # For batname
                train_df[which(train_df$Match.ID==i & train_df$Overs==j),"b1name"]<-as.character(bat_vector[1,1])
                
                # For balls faced
                train_df[which(train_df$Match.ID==i & train_df$Overs==j),"b1f"]<-bat_vector[1,2]
                
                }else if(nrow(bat_vector)==2)
                {
                        
                # For batname
                train_df[which(train_df$Match.ID==i & train_df$Overs==j),"b1name"]<-as.character(bat_vector[1,1])
                train_df[which(train_df$Match.ID==i & train_df$Overs==j),"b2name"]<-as.character(bat_vector[2,1])
                
                # For balls faced
                train_df[which(train_df$Match.ID==i & train_df$Overs==j),"b1f"]<-bat_vector[1,2]
                train_df[which(train_df$Match.ID==i & train_df$Overs==j),"b2f"]<-bat_vector[2,2]
                
                }else if(nrow(bat_vector)==3)
                {
                
                # For batname
                train_df[which(train_df$Match.ID==i & train_df$Overs==j),"b1name"]<-as.character(bat_vector[1,1])
                train_df[which(train_df$Match.ID==i & train_df$Overs==j),"b2name"]<-as.character(bat_vector[2,1])
                train_df[which(train_df$Match.ID==i & train_df$Overs==j),"b3name"]<-as.character(bat_vector[3,1])
                        
                # For balls faced
                train_df[which(train_df$Match.ID==i & train_df$Overs==j),"b1f"]<-bat_vector[1,2]
                train_df[which(train_df$Match.ID==i & train_df$Overs==j),"b2f"]<-bat_vector[2,2]
                train_df[which(train_df$Match.ID==i & train_df$Overs==j),"b3f"]<-bat_vector[3,2]
                
                }else if(nrow(bat_vector)==4)
                {
                
                # For batname
                train_df[which(train_df$Match.ID==i & train_df$Overs==j),"b1name"]<-as.character(bat_vector[1,1])
                train_df[which(train_df$Match.ID==i & train_df$Overs==j),"b2name"]<-as.character(bat_vector[2,1])
                train_df[which(train_df$Match.ID==i & train_df$Overs==j),"b3name"]<-as.character(bat_vector[3,1])
                train_df[which(train_df$Match.ID==i & train_df$Overs==j),"b4name"]<-as.character(bat_vector[4,1])
                
                # For balls faced
                train_df[which(train_df$Match.ID==i & train_df$Overs==j),"b1f"]<-bat_vector[1,2]
                train_df[which(train_df$Match.ID==i & train_df$Overs==j),"b2f"]<-bat_vector[2,2]
                train_df[which(train_df$Match.ID==i & train_df$Overs==j),"b3f"]<-bat_vector[3,2]
                train_df[which(train_df$Match.ID==i & train_df$Overs==j),"b4f"]<-bat_vector[4,2]
                
                }else if(nrow(bat_vector)==5)
                {
                
                # For batname
                train_df[which(train_df$Match.ID==i & train_df$Overs==j),"b1name"]<-as.character(bat_vector[1,1])
                train_df[which(train_df$Match.ID==i & train_df$Overs==j),"b2name"]<-as.character(bat_vector[2,1])
                train_df[which(train_df$Match.ID==i & train_df$Overs==j),"b3name"]<-as.character(bat_vector[3,1])
                train_df[which(train_df$Match.ID==i & train_df$Overs==j),"b4name"]<-as.character(bat_vector[4,1])
                train_df[which(train_df$Match.ID==i & train_df$Overs==j),"b5name"]<-as.character(bat_vector[5,1])
                
                # For balls faced
                train_df[which(train_df$Match.ID==i & train_df$Overs==j),"b1f"]<-bat_vector[1,2]
                train_df[which(train_df$Match.ID==i & train_df$Overs==j),"b2f"]<-bat_vector[2,2]
                train_df[which(train_df$Match.ID==i & train_df$Overs==j),"b3f"]<-bat_vector[3,2]
                train_df[which(train_df$Match.ID==i & train_df$Overs==j),"b4f"]<-bat_vector[4,2]
                train_df[which(train_df$Match.ID==i & train_df$Overs==j),"b5f"]<-bat_vector[5,2]
                
                }
                else
                {
                
                }
        }
}

############################################################################################################

# Obtaining bosr,ber,batsman strike rate

train_df[,"b1sr"]<-bsr[match(train_df$b1name,bsr$Batsman),4]
train_df[,"b2sr"]<-bsr[match(train_df$b2name,bsr$Batsman),4]
train_df[,"b3sr"]<-bsr[match(train_df$b3name,bsr$Batsman),4]
train_df[,"b4sr"]<-bsr[match(train_df$b4name,bsr$Batsman),4]
train_df[,"b5sr"]<-bsr[match(train_df$b5name,bsr$Batsman),4]

train_df[,"bosr"]<-bosr[match(train_df$Bowlername,bosr$Bowler),4]
train_df[,"ber"]<-ber[match(train_df$Bowlername,ber$Bowler),4]

# Finally, the dependent/response variable

train_df[,"Team.Final.Score"]<-data.frame(train_df%>%group_by(Match.ID)%>%summarise(Team.Final.Score=sum(Runs.over)))[match(
        train_df$Match.ID,data.frame(train_df%>%group_by(Match.ID)%>%summarise(Team.Final.Score=sum(Runs.over)))[,1]),2]

# Making NA's 0

train_df[which(is.na(train_df$b1name)),"b1name"]<-"Not available"
train_df[which(is.na(train_df$b2name)),"b2name"]<-"Not available"
train_df[which(is.na(train_df$b3name)),"b3name"]<-"Not available"
train_df[which(is.na(train_df$b4name)),"b4name"]<-"Not available"
train_df[which(is.na(train_df$b5name)),"b5name"]<-"Not available"

# Writing the file for backup

write.csv(train_df,"E:/IPL/Working Data/train_data.csv",row.names = FALSE)
saveRDS(train_df,"E:/IPL/Working Data/train_data.RDS")

# Making some imputations
train_df<-data.frame(train_df)

train_df[which(is.na(train_df$b2f)),"b2f"]<-0
train_df[which(is.na(train_df$b3f)),"b3f"]<-0
train_df[which(is.na(train_df$b4f)),"b4f"]<-0
train_df[which(is.na(train_df$b5f)),"b5f"]<-0

# Similarly for strike rates

train_df[which(is.na(train_df$b2sr)),"b2sr"]<-0
train_df[which(is.na(train_df$b3sr)),"b3sr"]<-0
train_df[which(is.na(train_df$b4sr)),"b4sr"]<-0
train_df[which(is.na(train_df$b5sr)),"b5sr"]<-0

# For bosr "inf" is replaced by average of the strike rates across the overs everywhere

train_df[which(train_df$bosr=="Inf"),"bosr"]<-round(mean(train_df[which(train_df$bosr!="Inf"),"bosr"]),digits = 2)

# Random Forest Model

# Training it only on data till 2016 and predicting it on 2017 ipl
# Obtaining train and test data

train_df_2016<-train_df[train_df$Season!="2017",]
test<-train_df[train_df$Season=="2017",]

# Running random forest model with 1000 trees and node_size 2
model_forest<-randomForest(as.numeric(Team.Final.Score)~Overs+Runs.over+Wickets.over+Home.Ground+Ground+Wickets.ever+Runs.ever+Extras.ever+Extras.Runs.ever+b1f+b2f+b1sr+b2sr+bosr+ber,importance=TRUE,data = train_df_2016,ntree=1000,nodesize=2)

# Predicting it to the test data
temp <- as.data.frame(predict(model_forest, test[,c("Overs","Runs.over","Wickets.over","Home.Ground","Ground","Wickets.ever","Runs.ever","Extras.ever","Extras.Runs.ever","b1f","b2f","b1sr","b2sr","bosr","ber")]))

# Combining with the actual test data
test_pred=cbind(test,temp[,1])
colnames(test_pred)[ncol(test_pred)]<-"rf"

# With 207 being the final score that has to be predicted at every over/point
plot(test_pred[which(test_pred$Match.ID=="1082591"),"rf"])

#######################################################################################################

# xgboost model

# Obtaining train and test data as how the xgboost model would require i.e.,
# variables only in the numeric form - employed simple one hot encoding

new_train_data<- dummy.data.frame(train_df_2016, names = c("Home.Ground","Ground"))
new_test_dat<-dummy.data.frame(test, names = c("Home.Ground","Ground"))

# Running the xgboost model with the booster being gbtree
model_xgboost<- xgboost(data = data.matrix(new_train_data[,c("Overs","Runs.over","Wickets.over","Home.Ground0","Home.Ground1","Wickets.ever","Runs.ever","Extras.ever","Extras.Runs.ever","b1f","b2f","b1sr","b2sr","bosr","ber")]), 
                        label = new_train_data$Team.Final.Score,
                        booster="gbtree",
                        eta = 0.3,
                        max_depth = 15, 
                        nrounds=100, 
                        subsample = 1,
                        colsample_bytree = 1,
                        seed = 1,
                        eval_metric = "rmse",
                        objective = "reg:linear",
                        nthread = 3
)

# Obtaining importance plot using xgBoost

names<-colnames(new_train_data[,c("Overs","Runs.over","Wickets.over","Home.Ground0","Home.Ground1","Wickets.ever","Runs.ever","Extras.ever","Extras.Runs.ever","b1f","b2f","b1sr","b2sr","bosr","ber")])
importance_matrix <- xgb.importance(names, model = model_xgboost)
xgb.plot.importance(importance_matrix)

# Test prediction for xgboost model
test_pred_xgboost<-as.data.frame(predict(model_xgboost,data.matrix(new_test_dat[,c("Overs","Runs.over","Wickets.over","Home.Ground0","Home.Ground1","Wickets.ever","Runs.ever","Extras.ever","Extras.Runs.ever","b1f","b2f","b1sr","b2sr","bosr","ber")])))

# Combining with the test data
test_pred<-cbind(test_pred,test_pred_xgboost)
colnames(test_pred)[ncol(test_pred)]<-"xgboost"

##################################################################################

# Comparison plot of rf and xgBoost

plot(test_pred[which(test_pred$Match.ID=="1082591"),"xgboost"],col=1)
points(test_pred[which(test_pred$Match.ID=="1082591"),"rf"],col=3,type = 'l')
legend("top",col=c(1,3),lty=1,cex=0.75,pt.cex=0.75,legend=c("xgBoost","randomForest"),bty = 'n')

#################################################################################

# Model Accuracy

# Error estimation - Employing rmse across the overs i.e., say in 2017 there were
# 59 times 1st over was bowled and the rmse for all the observations put together
# is calculated and idnicated in the graphs below

# Difference term

test_pred[,"rfmodel_actual_prediction_diff"]<-test_pred[,"rf"]-test_pred[,"Team.Final.Score"]
test_pred[,"boosting_actual_prediction_diff"]<-test_pred[,"xgboost"]-test_pred[,"Team.Final.Score"]

# Squaring the difference

test_pred[,"rfmodel_actual_prediction_diff_square"]<-(test_pred[,"rfmodel_actual_prediction_diff"])^2
test_pred[,"boosting_actual_prediction_diff_square"]<-(test_pred[,"boosting_actual_prediction_diff"])^2

# Obtaining MSE first

over_mse_rf<-test_pred%>%group_by(Overs)%>%summarise(num=sum(rfmodel_actual_prediction_diff_square),denom=n(),MSE=sum(rfmodel_actual_prediction_diff_square)/n())
over_mse_xgboost<-test_pred%>%group_by(Overs)%>%summarise(num=sum(boosting_actual_prediction_diff_square),denom=n(),MSE=sum(boosting_actual_prediction_diff_square)/n())

# Obtaining RMSE - Final steps and Individual plots if required

over_mse_rf[,"RMSE"]<-sqrt(over_mse_rf[,"MSE"])
plot(over_mse_rf$Overs,over_mse_rf$RMSE,xlab = "Overs",ylab = "Root Mean Square Error",main = "Over vs. Error - randomForest")

over_mse_xgboost[,"RMSE"]<-sqrt(over_mse_xgboost[,"MSE"])
plot(over_mse_xgboost$Overs,over_mse_xgboost$RMSE,xlab = "Overs",ylab = "Root Mean Square Error",main = "Over vs. Error - xgBoost")

# Error Comparison - RMSE

plot(over_mse_rf$Overs,over_mse_rf$RMSE,xlab = "Overs",ylab = "Root Mean Square Error",main = "Over vs. Error",col=2)
points(over_mse_xgboost$Overs,over_mse_xgboost$RMSE,xlab = "Overs",ylab = "Root Mean Square Error",main = "Over vs. Error",col=4)
legend("bottom",col=c(2,4),lty=1,cex=0.75,pt.cex=0.75,legend=c("randomForest","xgBoost"),bty = 'n')