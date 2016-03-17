#clean all objects from memory
rm(list=ls())

#Read the data
getwd()
setwd("D:/analyticsvidhya/Date your Data")
train_data <- read.csv("trainfiles/train/train.csv")
student_data <- read.csv("trainfiles/student/student.csv")
internship_data <- read.csv("trainfiles/internship/internship.csv")
test_data <- read.csv("test-date-your-data/test.csv")

#Merge the train nad test sets
test_data$Is_Shortlisted=NA
train_test<-rbind(train_data,test_data)

########################################
#Creating Features
#Formatting the data as required
########################################
#Formatting Date
train_test$Earliest_Start_Date<- as.Date(as.POSIXct(train_test$Earliest_Start_Date,format="%d-%m-%Y"))
#change the blank value in location to no location
train_test$Preferred_location=as.character(train_test$Preferred_location)
train_test$Preferred_location[train_test$Preferred_location==""]<-"Noloc"

#Feature 1 for ealiest start date, Normalize the date and transform to days
train_test$Earliest_Start_Date_num<-as.Date(max(train_test$Earliest_Start_Date))-as.Date(train_test$Earliest_Start_Date)
train_test$Earliest_Start_Date_num<-as.numeric(train_test$Earliest_Start_Date_num)

#Feature 2 No of students applied for every internship
#merge internship and train_test
train_test_internship<-merge(train_test, internship_data,by="Internship_ID")
#Formatting Date
train_test_internship$Start_Date <- as.Date(as.POSIXct(train_test_internship$Start_Date,format="%d-%m-%Y"))
#count the number of students
newdf<-train_test_internship[,c(1:2)]
student_for_internship<-aggregate(Student_ID~Internship_ID, newdf, FUN=function(x) length(x))
names(student_for_internship)<-c("Internship_ID","Student_count")
train_test_internship<-merge(train_test_internship,student_for_internship,by="Internship_ID")

#Feature 3
#Match the student profile with sparse matrix of intership profile for evry student 
#internship combination
#Merge with student
train_test_internship_student<-merge(train_test_internship,student_data,by="Student_ID")

#Subset only students with experience
train_test_internship_student_exp<-train_test_internship_student[train_test_internship_student$Experience_Type!="NULL",]

##############Parallel Processing#####################
library(plyr)
library(doParallel)
library(foreach)
library(doSNOW)
registerDoSNOW(makeCluster(2, type = "SOCK"))
cl <- makeCluster(2)
registerDoParallel(cl)

#Matching the profiles 
t=train_test_internship_student_exp[,c(310,22:294)]
a<-foreach(i = c(1:nrow(t)),.packages='plyr',.combine=rbind) %dopar%   {
  x<-unlist(strsplit(toupper(as.character(t[i,"Profile"])), split=" "))
#find whether there is a column
  count_profile=0
  for(j in x)
  {
    y<-j %in% toupper(names(t[,2:ncol(t)]))
    if(y==TRUE)
    {
      z<-match(j,toupper(names(t[,2:ncol(t)])))
      count_profile<-count_profile+sum(t[i,z+1])
    }
  }
  count_profile
}
train_test_internship_student_exp[,"profile_ind"]=a

#Aggregate for every student internship
profile_ind_tab<-train_test_internship_student_exp[,c("Internship_ID","Student_ID","profile_ind")]
agg_profile_ind_tab<-aggregate( profile_ind_tab[,3], profile_ind_tab[,1:2], FUN = sum )
names(agg_profile_ind_tab)=c("Internship_ID","Student_ID","Profile_count")


#Feature 4
#Match the student profile with sparse matrix of skills required for internship
#for evry student internship combination
t=train_test_internship_student_exp[,c("Internship_ID","Student_ID","Skills_required","Profile")]
b<-foreach(i = c(1:nrow(t)),.packages='plyr',.combine=rbind) %dopar%   {
  x<-unlist(strsplit(toupper(as.character(t[i,"Profile"])), split= '[ ]|[,]|[+]|[(]|[)]|[/]|[&]|[.]'))
  #find whether there is a column
  z=0
  for(j in x)
  {
    y<-j %in% unlist(strsplit(toupper(as.character(t[i,"Skills_required"])), split="[ ]|[,]|[+]|[(]|[)]|[/]|[&]|[.]"))
    if(y==TRUE)
    {
      z=z+1
    }
  }
  z
}
#Aggregate
train_test_internship_student_exp[,"skills_req_count"]=b
skills_req_ind_tab<-train_test_internship_student_exp[,c("Internship_ID","Student_ID","skills_req_count")]
agg_skills_req_ind_tab<-aggregate( skills_req_ind_tab[,3], skills_req_ind_tab[,1:2], FUN = sum )
names(agg_skills_req_ind_tab)=c("Internship_ID","Student_ID","skills_req_count")


#Feature 5
#Match the student profile with sparse matrix of skills required for internship
#for evry student internship combination
t=train_test_internship_student_exp[,c("Internship_ID","Student_ID","Internship_Profile","Profile")]
c<-foreach(i = c(1:nrow(t)),.packages='plyr',.combine=rbind) %dopar%   {
  x<-unlist(strsplit(toupper(as.character(t[i,"Profile"])), split= '[ ]|[,]|[+]|[(]|[)]|[/]|[&]|[.]'))
  #find whether there is a column
  z=0
  for(j in x)
  {
    y<-j %in% unlist(strsplit(toupper(as.character(t[i,"Internship_Profile"])), split="[ ]|[,]|[+]|[(]|[)]|[/]|[&]|[.]"))
    if(y==TRUE)
    {
      z=z+1
    }
  }
  z
}

#Aggregate
train_test_internship_student_exp[,"intern_profile_count"]=c
intern_profile_ind_tab<-train_test_internship_student_exp[,c("Internship_ID","Student_ID","intern_profile_count")]
agg_intern_profile_ind_tab<-aggregate( intern_profile_ind_tab[,3], intern_profile_ind_tab[,1:2], FUN = sum )
names(agg_intern_profile_ind_tab)=c("Internship_ID","Student_ID","intern_profile_count")


#Removing duplicate students by ranking them on rrencent experience
#Formatting Date
df<-student_data
df$Start.Date<-as.Date(as.POSIXct(df$Start.Date,format="%d-%m-%Y"))
#order based on Start.Date
df$rank<-ave(xtfrm(as.Date(df$Start.Date)),df$Student_ID ,FUN=function(x) order(x,decreasing=T) ) 

#Keep only unique students based on Rank
new_student_data<-df[df$rank==1,]

fin1<-merge(train_test_internship,new_student_data,by=c("Student_ID"),all.x=T)
fin1<-merge(fin1,agg_profile_ind_tab,by=c("Student_ID","Internship_ID"),all.x=T)
fin1<-merge(fin1,agg_intern_profile_ind_tab,by=c("Student_ID","Internship_ID"),all.x=T)
fin1<-merge(fin1,agg_skills_req_ind_tab,by=c("Student_ID","Internship_ID"),all.x=T)

final_data=fin1
names(final_data)

########################################Model Building##################################
#Two Seperate mdelels for Students with experience and no experience as these students 
#have not specified their skill sets

#Model1 for exp students
final_data_exp=final_data[final_data$Experience_Type!="NULL",]
final_data_exp$Start.Date.Num<-as.numeric(max(as.Date(final_data_exp$Start.Date))-as.Date(final_data_exp$Start.Date))
final_data_exp$Start_Date_Num<-as.numeric(max(as.Date(final_data_exp$Start_Date))-as.Date(final_data_exp$Start_Date))
final_data_exp$location_ind <- ifelse(as.character(final_data_exp$Institute_location) == as.character(final_data_exp$Preferred_location),1,0 )
final_data_exp$location_ind2 <- ifelse(as.character(final_data_exp$hometown) == as.character(final_data_exp$Preferred_location),1,0 )
final_data_exp$location_ind3 <- ifelse(as.character(final_data_exp$Location) == as.character(final_data_exp$Preferred_location),1,0 )

#columns not required for modelling
remove_column=c("Start.Date","End.Date","Start_Date","Earliest_Start_Date","rank")
final_data_exp=final_data_exp[,-match(remove_column,names(final_data_exp))]

#Convert factors to charcters and then back to factors to avoid mis match in train and test set
aschar <- function(x) as.character(x)
facttochar <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)],   
                                               aschar))
asNum <- function(x) as.integer(as.factor(x))
chartoNum <- function(d) modifyList(d, lapply(d[, sapply(d, is.character)],   
                                              asNum))

final_data_exp<-facttochar(final_data_exp)
final_data_exp<-chartoNum(final_data_exp)

#Train Set
final_data_exp_train <- final_data_exp[!is.na(final_data_exp$Is_Shortlisted),]
#Test Set
final_data_exp_test <- final_data_exp[is.na(final_data_exp$Is_Shortlisted),]

feature.names <- names(final_data_exp_train)
feature.names <- feature.names[feature.names!= "Is_Shortlisted" &
                                 feature.names!=  "Internship_ID" &
                                 feature.names!=  "Student_ID"]


#model1
set.seed(1960)
h<-sample(nrow(final_data_exp_train),floor(0.3*nrow(final_data_exp_train)))
train_sample <- final_data_exp_train[-h,]
train_val <- final_data_exp_train[h,]

library(xgboost)
dval<-xgb.DMatrix(data=data.matrix(train_val[,feature.names]),label=train_val[,7],missing=NA)
dtrain<-xgb.DMatrix(data=data.matrix(train_sample[,feature.names]),label=train_sample[,7],missing=NA)
watchlist<-list(val=dval,train=dtrain)
fin_pred_exp={}
for (eta in c(0.1,0.05,0.01) )
{
  for (colsample_bytree in c(0.2,0.4,0.6))
  {
    for(subsample in c(0.4,0.8,1))
    {
      param <- list(  objective           = "binary:logistic", 
                      booster             = "gbtree",
                      eta                 = eta,
                      max_depth           = 8, #7
                      subsample           = subsample,
                      colsample_bytree    = colsample_bytree
      )
      set.seed(1429)
      clf <- xgb.train(   params              = param, 
                          data                = dtrain, 
                          nrounds             = 1000, 
                          verbose             = 0,
                          early.stop.round    = 50,
                          watchlist           = watchlist,
                          maximize            = TRUE,
                          eval_metric       = "auc"
      )
      #[645]  val-auc:0.872786  train-auc:0.993390
      #Stopping. Best iteration: 596
      pred_exp=predict(clf,data.matrix(final_data_exp_test[,feature.names]),missing=NA)
      print(head(fin_pred_exp))
      fin_pred_exp<-cbind(fin_pred_exp,pred_exp)
    }
  }
}

#<model2
final_data_noexp=final_data[final_data$Experience_Type=="NULL",]
final_data_noexp$Start_Date_Num<-as.numeric(max(as.Date(final_data_noexp$Start_Date))-as.Date(final_data_noexp$Start_Date))
final_data_noexp$location_ind <- ifelse(as.character(final_data_noexp$Institute_location) == as.character(final_data_noexp$Preferred_location),1,0 )
final_data_noexp$location_ind2 <- ifelse(as.character(final_data_noexp$hometown) == as.character(final_data_noexp$Preferred_location),1,0 )

remove_column=c("Start.Date","End.Date","Start_Date","Earliest_Start_Date"
                ,"Profile_count","Profile","Location","Experience_Type","rank")
final_data_noexp=final_data_noexp[,-match(remove_column,names(final_data_noexp))]

#Convert factors to charcters and then back to factors to avoid mis match in train and test set
final_data_noexp<-facttochar(final_data_noexp)
final_data_noexp<-chartoNum(final_data_noexp)

feature.names <- names(final_data_noexp)
feature.names <- feature.names[feature.names!= "Is_Shortlisted" &
                                 feature.names!=  "Internship_ID" &
                                 feature.names!=  "Student_ID"]

#Train Set
final_data_noexp_train<- final_data_noexp[!is.na(final_data_noexp$Is_Shortlisted),]
#Test Set
final_data_noexp_test<- final_data_noexp[is.na(final_data_noexp$Is_Shortlisted),]

set.seed(1960)
h<-sample(nrow(final_data_noexp_train),floor(0.3*nrow(final_data_noexp_train)))
train_sample <- final_data_noexp_train[-h,]
train_val <- final_data_noexp_train[h,]

names(final_data_noexp)
library(xgboost)
dval<-xgb.DMatrix(data=data.matrix(train_val[,feature.names]),label=train_val[,7],missing=NA)
dtrain<-xgb.DMatrix(data=data.matrix(train_sample[,feature.names]),label=train_sample[,7],missing=NA)
watchlist<-list(val=dval,train=dtrain)
fin_pred_noexp={}
for (eta in c(0.1,0.05,0.01) )
{
  for (colsample_bytree in c(0.2,0.4,0.6))
  {
    for(subsample in c(0.4,0.8,1))
    {
      param <- list(  objective           = "binary:logistic", 
                      booster             = "gbtree",
                      eta                 = eta,
                      max_depth           = 8, #7
                      subsample           = subsample,
                      colsample_bytree    = colsample_bytree
      )
      set.seed(1429)
      clf <- xgb.train(   params              = param, 
                          data                = dtrain, 
                          nrounds             = 1000, 
                          verbose             = 0,
                          early.stop.round    = 50,
                          watchlist           = watchlist,
                          maximize            = TRUE,
                          eval_metric       = "auc"
      )
      pred_noexp=predict(clf,data.matrix(final_data_noexp_test[,feature.names]),missing=NA)
      fin_pred_noexp<-cbind(fin_pred_noexp,pred_noexp)
    }
  }
}

final_exp_predictions=rowMeans(fin_pred_exp)
final_noexp_predictions = rowMeans(fin_pred_noexp)

submission=data.frame(rbind(
  cbind("Internship_ID"=final_data_noexp_test$Internship_ID
        ,"Student_ID"=final_data_noexp_test$Student_ID
        ,Is_Shortlisted=final_noexp_predictions)
  , cbind("Internship_ID"=final_data_exp_test$Internship_ID
          ,"Student_ID"=final_data_exp_test$Student_ID
          ,Is_Shortlisted=final_exp_predictions)
))

write.csv(submission, "Submission/code7.csv", row.names=F)
