cricsheet_data_collation<-function(path)
{
# Libraries

library(yaml)
library(yorkr)
library(dplyr)
library(stringi)
library(stringr)

filelist<-list.files(path)

# Reading a sample file to create an exhaustive data frame to accomodate all matches

sample<-data.frame(t(data.frame(yaml.load_file(paste(path,"/",filelist[1],sep = "")))))

# Exhaustive Empty dataframe

# Pre data frame

constant1string<-rownames(sample[1:18,])

string11st<-rep("innings.1st.innings.deliveries.",1260)
string21st<-sort(rep(seq(0.1,19.9,0.1)[rep(c(TRUE,FALSE), c(9,1))],7),decreasing = FALSE)
string31st<-rep(c(".batsman",".bowler",".non_striker",".runs.batsman",".runs.extras",".runs.total",".wicket.player_out"),180)

constant2string<-paste0(string11st,string21st,string31st)

constant3string<-"innings.2nd.innings.team"

string12nd<-rep("innings.2nd.innings.deliveries.",1260)
string22nd<-sort(rep(seq(0.1,19.9,0.1)[rep(c(TRUE,FALSE), c(9,1))],7),decreasing = FALSE)
string32nd<-rep(c(".batsman",".bowler",".non_striker",".runs.batsman",".runs.extras",".runs.total",".wicket.player_out"),180)

constant4string<-paste0(string12nd,string22nd,string32nd)

final_rownames<-c(constant1string,constant2string,constant3string,constant4string)
        
# Defining the dataframe where all are being put together

final<-data.frame(matrix(NA,length(final_rownames),length(filelist)))

rownames(final)<-final_rownames

for(i in 1:length(filelist))
{
        print(paste(i,"_",filelist[i],sep = ""))
        
        temp<-data.frame(t(data.frame(yaml.load_file(paste(path,"/",filelist[i],sep = "")))))
        
        final[,i]<-temp[match(rownames(final),rownames(temp)),1]
        
        colnames(final)[i]<-stri_split(filelist[i],regex = "\\.")[[1]][1]
        
        
}
browser()

write.csv(final,"E:/IPL/Working Data/all_matches_with_wickets.csv")

saveRDS(final,"E:/IPL/Working Data/all_matches_with_wickets.RDS")

}