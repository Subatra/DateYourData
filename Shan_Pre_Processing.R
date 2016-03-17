setwd("E:\\SS\\AV\\OnlineHack\\Online Date Your Data\\files")

intrn <- read.csv("Internship.csv")
std   <- read.csv("Student.csv")
train <- read.csv("train1.csv")
test  <- read.csv("test1.csv")

library(sqldf)
std1 <- std
std1$S_Date <- std1$Start.Date
std1$E_Date <- std1$End.Date
std1$Num_Exp <- 1
std2 <- sqldf("select Student_ID, Institute_Category, Institute_location ,hometown ,Degree,          
              Stream, Current_year, Year_of_graduation, Performance_PG, PG_scale,         
              Performance_UG, UG_Scale, Performance_12th, Performance_10th, Experience_Type,  
              Profile, Location, S_Date, E_Date, SUM(Num_Exp) as Num_Exp_Row From std1 Group BY Student_ID")

# Converting S_Date, E_Date to date class
S_Date <- as.Date(std2$S_Date, "%d-%m-%Y")
E_Date <- as.Date(std2$E_Date, "%d-%m-%Y")


std2$S_Date <- S_Date
std2$E_Date <- E_Date


# tagging train and test data
train1 <- train
train1$tag <- "train"
test1 <- test
test1$tag <- "test"

#Combining train and test
test1$Is_Shortlisted <- 0
data <- rbind(train1,test1)

#combining data and std2

data1 <- merge(data,std2,by="Student_ID",all.x=TRUE)
intrn1 <- intrn[,c(1:13)]
data2 <- merge(data1,intrn1, by="Internship_ID", all.x=TRUE)

## modification of Earliest_Start_Date

ESD <- data2$Earliest_Start_Date
ESD1 <- gsub('/','-',ESD)
ESD2 <- as.Date(ESD1, "%d-%m-%Y")
data2$Earliest_Start_Date <- ESD2

## Converting "Start_Date"  to Date class
Start_Date <- data2$Start_Date
Start_Date <- as.Date(Start_Date,"%d-%m-%Y")
data2$Start_Date <- Start_Date

## Class balance
table(train$Is_Shortlisted)
#    0      1 
#168003  24579 

## Converting to factor variables Degree ,Stream , Profile
data2$Degree <- as.factor(data2$Degree)
data2$Stream <- as.factor(data2$Stream)
data2$Profile <-as.factor(data2$Profile)
data3 <- data2

# missing value treatment of data3$Preferred_location
# Lets tag it as No_Pref and create a feature to tag it
data3$Preferred_location <- as.character(data3$Preferred_location)
data3$Preferred_location <- ifelse(data3$Preferred_location=="","No_Pref",data3$Preferred_location)
data3$Preferred_location <- as.factor(data3$Preferred_location)


# substituting NA values of Degree with most common category
data3$Degree <- as.character(data3$Degree)
data3$Degree <- ifelse(is.na(data3$Degree) & data3$Stream=="Management", "MBA",data3$Degree)
data3$Degree <- ifelse(is.na(data3$Degree) & data3$Stream=="Fashion Lifestyle Business Management", "MBA",data3$Degree)
data3$Degree <- ifelse(is.na(data3$Degree) & data3$Stream=="Commence", "B.Com",data3$Degree)
data3$Degree <- ifelse(is.na(data3$Degree) & data3$Stream=="Commerce", "B.Com",data3$Degree)

data3$Degree <- ifelse(is.na(data3$Degree) & data3$Internship_Profile=="Design", "Designing",data3$Degree)
data3$Degree <- ifelse(is.na(data3$Degree) & data3$Internship_Profile=="Social Media Marketing", "Digital Marketing",data3$Degree)
data3$Degree <- ifelse(is.na(data3$Degree) & data3$Internship_Profile=="Graphic Design", "Graphic Design",data3$Degree)
data3$Degree <- ifelse(is.na(data3$Degree) & data3$Internship_Profile=="Digital Marketing", "Digital Marketing",data3$Degree)
data3$Degree <- ifelse(is.na(data3$Degree) & data3$Internship_Profile=="Illustration", "B.A.(Hons) Journalism",data3$Degree)
data3$Degree <- ifelse(is.na(data3$Degree) & data3$Internship_Profile=="Google Ad Word Management", "MBA",data3$Degree)
data3$Degree <- ifelse(is.na(data3$Degree) & data3$Internship_Profile=="Operations- Quality Analyst", "Global Business Operations (GBO)",data3$Degree)

data3$Degree <- as.factor(data3$Degree)

# substituting NA values of Stream 
data3$Stream <- as.character(data3$Stream)
data3$Stream <- ifelse(is.na(data3$Stream) & data3$Degree=="Designing", "Accessory Designing",data3$Stream)
data3$Stream <- ifelse(is.na(data3$Stream) & data3$Degree=="MCA", "Computer Application",data3$Stream)
data3$Stream <- ifelse(is.na(data3$Stream) & data3$Degree=="Post Graduate Dimploma in Management", "Marketing",data3$Stream)
data3$Stream <- ifelse(is.na(data3$Stream) & data3$Degree=="MBA", "Marketing",data3$Stream)
data3$Stream <- ifelse(is.na(data3$Stream) & data3$Degree=="B.Com (Hons.)", "Accountancy And Finance",data3$Stream)
data3$Stream <- ifelse(is.na(data3$Stream) & data3$Degree=="Graphic Design", "Visual Comm",data3$Stream)
data3$Stream <- ifelse(is.na(data3$Stream) & data3$Degree=="Bachelor of Business Admininstration", "Management",data3$Stream)
data3$Stream <- ifelse(is.na(data3$Stream) & data3$Degree=="Digital Marketing", "Commerce",data3$Stream)
data3$Stream <- ifelse(is.na(data3$Stream) & data3$Degree=="B.M.M.", "Arts",data3$Stream)
data3$Stream <- ifelse(is.na(data3$Stream) & data3$Degree=="BCA", "Computer Application",data3$Stream)
data3$Stream <- ifelse(is.na(data3$Stream) & data3$Degree=="Global Business Operations (GBO)", "Finance",data3$Stream)
data3$Stream <- ifelse(is.na(data3$Stream) & data3$Degree=="B.A.LL.B. (Hons.)", "Law",data3$Stream)
data3$Stream <- ifelse(is.na(data3$Stream) & data3$Degree=="Under", "Under",data3$Stream)
data3$Stream <- ifelse(is.na(data3$Stream) & data3$Degree=="B.A. Programme", "Arts",data3$Stream)
data3$Stream <- ifelse(is.na(data3$Stream) & data3$Degree=="B.Sc (Hons.) Computer Science", "Science",data3$Stream)
data3$Stream <- ifelse(is.na(data3$Stream) & data3$Degree=="B.S. & M.S. (Dual)", "Mathematics and Computing",data3$Stream)
data3$Stream <- ifelse(is.na(data3$Stream) & data3$Degree=="Undecided", "Undecided",data3$Stream)
data3$Stream <- ifelse(is.na(data3$Stream) & data3$Degree=="B.A.(Hons) Journalism", "Arts",data3$Stream)
data3$Stream <- ifelse(is.na(data3$Stream) & data3$Internship_Profile=="Editorial(Law)", "Law",data3$Stream)
data3$Stream <- as.factor(data3$Stream)


# Replacing NULL in Experience_Type , Profile with No_Exp
summary(data3$Experience_Type)
summary(data3$Profile)
data3$Profile <- as.character(data3$Profile)
data3$Experience_Type <- as.character(data3$Experience_Type)


table(as.factor(data3$Experience_Type))
data3$Profile[data3$Experience_Type!="NULL" & data3$Profile=="NULL"]<- "Intern"
data3$Profile[is.na(data3$Profile)] <- "Intern"

data3$Experience_Type[data3$Experience_Type=="NULL"] <- "No_Exp"
data3$Profile[data3$Profile=="NULL"] <- "No_Exp"

table(data3$Experience_Type)
sort(table(as.factor(data3$Profile)),decreasing=TRUE)[1:50] 

data3$Profile <- as.factor(data3$Profile)
data3$Experience_Type <- as.factor(data3$Experience_Type)

# NAs in S_Date , E_Date

data3$S_Date <- as.character(data3$S_Date)
data3$E_Date <- as.character(data3$E_Date)

data3$S_Date[is.na(data3$S_Date) & data3$Experience_Type=="No_Exp"] <- "2015-02-21"
data3$E_Date[is.na(data3$E_Date) & data3$Experience_Type=="No_Exp"] <- "2015-02-21"
data3$S_Date <- as.Date(data3$S_Date,"%Y-%m-%d")
data3$E_Date <- as.Date(data3$E_Date,"%Y-%m-%d")

data3$E_Date[is.na(data3$E_Date)] <- as.Date("21-02-2015", "%d-%m-%Y")
max(data3$E_Date)



#NULL values of Stipend1 (2859 NULL values)  
data3$Stipend1 <- as.character(data3$Stipend1)
data3$Stipend1 <- as.numeric(data3$Stipend1)
sum(is.na(data3$Stipend1))
sum(is.na(data3$Stipend1[data3$Stipend_Type=="unpaid"]))

## Stipend_Type == "unpaid" are NA or NULL in Stipend1; can replace them as 0
data3$Stipend1 <- ifelse(is.na(data3$Stipend1),0,data3$Stipend1)
table(data3$Stipend1[data3$Stipend_Type=="unpaid"])
# (7+5) obs in data3$Stipend1 has values otherthan 0 when Stipend_Type=="unpaid"
# Converting them to 0
#data3$Stipend1 <- ifelse(data3$Stipend_Type=="unpaid",0,data3$Stipend1)
data3$Stipend1 <- as.numeric(as.character(data3$Stipend1))
data3$Stipend1[data3$Stipend_Type=="unpaid"] <- 0

#NULL values of stipend2 (151897 NULL values) replaced  by median
data3$Stipend2 <- as.numeric(as.character(data3$Stipend2))
data3$Stipend2[data3$Stipend_Type=="unpaid"] <- 0

## NA values replaced by median
data3$Stipend2[is.na(data3$Stipend2)] <- 10000


## Capping outliersin data3$Stipend1
table(data3$Stipend1)
data3$Stipend2[data3$Stipend1==30000]

data3$Stipend1[data3$Stipend1==50000] <- 5000
data3$Stipend1[data3$Stipend1==40000] <- 4000
data3$Stipend1[data3$Stipend1==35000] <- 3500
data3$Stipend1[data3$Stipend1==30000 & data3$Stipend2==10000]<- 3000


## Capping outliersin data3$Stipend2
sort(data3$Stipend2,decreasing=TRUE)
table(data3$Stipend2)
data3$Stipend1[data3$Stipend2==150000] ##showing 8000 and 10000 . Must be wrong entry
data3$Stipend2[data3$Stipend2==150000]<- 15000

data3$Stipend1[data3$Stipend2==75000]
data3$Stipend1[data3$Stipend2==50000]


# Outliers in data2$Internship_Duration.Months.
summary(data3$Internship_Duration.Months.)
table(data3$Internship_Duration.Months.)
table(data3$Start_Date[data3$Internship_Duration.Months.==2016]) # 2014-12-15
#replacing by 24
data3$Internship_Duration.Months.<- ifelse(data3$Internship_Duration.Months.==2016,24,data3$Internship_Duration.Months.)

table(data3$Start_Date[data3$Internship_Duration.Months.==10000])
data3$Internship_Duration.Months.<- ifelse(data3$Internship_Duration.Months.==10000,10,data3$Internship_Duration.Months.)

table(data3$Start_Date[data3$Internship_Duration.Months.==20160201])
data3$Internship_Duration.Months.<- ifelse(data3$Internship_Duration.Months.==20160201,12,data3$Internship_Duration.Months.)

table(data3$Start_Date[data3$Internship_Duration.Months.==20160331])
data3$Internship_Duration.Months.<- ifelse(data3$Internship_Duration.Months.==20160331,15,data3$Internship_Duration.Months.)


# why min=0 in summary(data3$Performance_PG),summary(data3$Performance_UG),summary(data3$Performance_12th)
# summary(data3$Performance_10th)
table(data3$Performance_10th)

Performance_10th <- ifelse(data3$Performance_10th <= 10, (data3$Performance_10th*10),data3$Performance_10th)
Performance_10th <- ifelse(Performance_10th == 8.5, (Performance_10th*10),Performance_10th)
Performance_10th <- ifelse(Performance_10th < 40 , 40,Performance_10th)
data3$Performance_10th <- Performance_10th



table(data3$Performance_12th)
Performance_12th <- ifelse(data3$Performance_12th <= 10, (data3$Performance_12th*10),data3$Performance_12th)
Performance_12th <- ifelse(Performance_12th <= 10, (Performance_12th*10),Performance_12th)
Performance_12th <- ifelse(Performance_12th < 40 , 40,Performance_12th)
data3$Performance_12th <- Performance_12th

## Since UG_Scale is there, lets convert to ratio. Degree awarded student must have passed UG
table(data3$Performance_UG)
table(data3$UG_Scale[data3$Performance_UG==0.6])

Per_UG <- (data3$Performance_UG/data3$UG_Scale)*100
Per_UG <- ifelse(Per_UG <= 10, (Per_UG*10), Per_UG)
Per_UG[substr(data3$Degree,1,1)=="B" & Per_UG < 40 | substr(data3$Degree,1,1)=="M" & Per_UG < 40] <- 40

data3$Performance_UG <- Per_UG

## Since PG_Scale is there, lets convert to ratio
table(data3$Performance_PG)
Per_PG <- (data3$Performance_PG/data3$PG_scale)*100
Per_PG <- ifelse(Per_PG < 10, Per_PG*10, Per_PG)     # Per_PG=0 may be who are not PG yet
table(Per_PG)

Per_PG[substr(data3$Degree,1,1)=="M" & Per_PG < 40]<- 40
data3$Performance_PG <- Per_PG 




# Skills_required NULL
data3$Skills_required <- as.character(data3$Skills_required)
data3$Skills_required[data3$Skills_required=="NULL"] <-"No_Skill"
data3$Skills_required <- as.factor(data3$Skills_required)

#Feature Engineering
# Exp_tenure

data3$Exp_tenure <- 0
data3$Exp_tenure <- data3$E_Date - data3$S_Date
data3$Exp_tenure <- as.numeric(as.character(data3$Exp_tenure))
summary(data3$Exp_tenure)

data3$Exp_tenure[data3$Exp_tenure < 0]<- 0
table(data3$Exp_tenure)

data3$S_Date[data3$Exp_tenure==1][1:10]
data3$E_Date[data3$Exp_tenure==1][1:10]
data3$Exp_tenure[data3$Exp_tenure < 30] <- 0


#####################
## Tagging on Preferred_location

data4 <- data3
sort(table(data3$Preferred_location),decreasing=TRUE)
data4$Preferred_location <- as.character(data4$Preferred_location)
data4$Is_PlNo_Pref <- ifelse(data4$Preferred_location=="No_Pref",1,0)
data4$Is_PlIHFG    <- ifelse(data4$Preferred_location=="IHFG",1,0)
data4$Is_PlIHJB <- ifelse(data4$Preferred_location=="IHJB",1,0)
data4$Is_PlIIBD <- ifelse(data4$Preferred_location=="IIBD",1,0)
data4$Is_PlIIDB <- ifelse(data4$Preferred_location=="IIDB",1,0)
data4$Is_PlIJBG <- ifelse(data4$Preferred_location=="IJBG",1,0)
data4$Is_PlIJCE <- ifelse(data4$Preferred_location=="IJCE",1,0)
data4$Is_PlIJJI <- ifelse(data4$Preferred_location=="IJJI",1,0)
data4$Is_PlJABD <- ifelse(data4$Preferred_location=="JABD",1,0)
data4$Is_PlJBDB <- ifelse(data4$Preferred_location=="JBDB",1,0)

## Institute_location
sort(table(data4$Institute_location),decreasing=TRUE)
data4$Institute_location  <- as.character(data4$Institute_location)
data4$Is_InstLoc_IHHF <- ifelse(data4$Institute_location=="IHHF",1,0)
data4$Is_InstLoc_IHHH <- ifelse(data4$Institute_location=="IHHH",1,0) 
data4$Is_InstLoc_IHJB <- ifelse(data4$Institute_location=="IHJB",1,0)
data4$Is_InstLoc_IJCE <- ifelse(data4$Institute_location=="IJCE",1,0)
data4$Is_InstLoc_IHJC <- ifelse(data4$Institute_location=="IHJC",1,0)
data4$Is_InstLoc_IIBD <- ifelse(data4$Institute_location=="IIBD",1,0)
data4$Is_InstLoc_IIDB <- ifelse(data4$Institute_location=="IIDB",1,0)
data4$Is_InstLoc_IIGE <- ifelse(data4$Institute_location=="IIGE",1,0)
data4$Is_InstLoc_IIIF <- ifelse(data4$Institute_location=="IIIF",1,0)
data4$Is_InstLoc_IIJJ <- ifelse(data4$Institute_location=="IIJJ",1,0)
data4$Is_InstLoc_IJAB <- ifelse(data4$Institute_location=="IJAB",1,0)
data4$Is_InstLoc_IJAE <- ifelse(data4$Institute_location=="IJAE",1,0)
data4$Is_InstLoc_IJGB <- ifelse(data4$Institute_location=="IJGB",1,0)
data4$Is_InstLoc_IJBG <- ifelse(data4$Institute_location=="IJBG",1,0)


## hometown 

sort(table(data4$hometown),decreasing=TRUE)

data4$hometown  <- as.character(data4$hometown)
data4$Inf_hometown <- ifelse(data4$hometown %in% c("IHGI","IHHH","IHJB","IHJC","IIAI","IIBD","IIDB","IIGA","IIIF","IJAB","IJAE","IJBG","IJCE","IJHA","IJIG",
                        "IJJI","JAAJ","JABD","JADD","JADH","JAGD","JAHG","JBBE","JBDB","JBEB","JBEI","JBID","JCBC",
                        "JCDD","JCHJ","JDAE","JDFA","JECD","JEEH","JEHI"),1,0)


## Degree 

sort(table(data4$Degree),decreasing=TRUE)[1:10]
data4$Degree <- as.character(data4$Degree)
data4$Is_BTech <- ifelse(data4$Degree=="B.Tech",1,0)
data4$Is_BE <- ifelse(data4$Degree=="B.E",1,0)
data4$Is_MCA <- ifelse(data4$Degree=="MCA",1,0)
data4$Is_MBA <- ifelse(data4$Degree=="MBA",1,0)
data4$Is_BCom <- ifelse(data4$Degree=="B.Com" | data4$Degree=="B.Com (Hons.)",1,0)
data4$Is_PGDM <- ifelse(data4$Degree=="Post Graduate Dimploma in Management",1,0)
data4$Is_BSc <- ifelse(data4$Degree=="B.Sc",1,0)
data4$Is_BBA <- ifelse(data4$Degree=="Bachelor of Business Admininstration",1,0)
data4$Is_MTech <- ifelse(data4$Degree=="M.Tech",1,0)



## Stream
sort(table(data4$Stream),decreasing=TRUE)[1:10]
data4$Stream <- as.character(data4$Stream)

data4$Is_StrCSE<- ifelse(data4$Stream=="Computer Science & Engineering",1,0)
data4$Is_StrCS<- ifelse(data4$Stream=="Computer  Science",1,0)
data4$Is_StrECE<- ifelse(data4$Stream=="Electronics and Communication Engineering",1,0)
data4$Is_StrCoAp<- ifelse(data4$Stream=="Computer Application",1,0)
data4$Is_StrCommerce<- ifelse(data4$Stream=="Commerce",1,0)
data4$Is_StrIT<- ifelse(data4$Stream=="Information Technology",1,0)
data4$Is_StrME<- ifelse(data4$Stream=="Mechanical Engineering",1,0)
data4$Is_StrMarketing<- ifelse(data4$Stream=="Marketing",1,0)


##  Profile
sort(table(data4$Profile),decreasing=TRUE)[1:10]
data4$Profile <- as.character(data4$Profile)
data4$Is_Prof_intern <- ifelse(data4$Profile=="Intern",1,0)
data4$Is_Prof_No_Exp <- ifelse(data4$Profile=="No_Exp",1,0)
data4$Is_Prof_Marketing <- ifelse(data4$Profile=="Content Writing & Social Media Marketing" | data4$Profile=="Marketing",1,0)
data4$Is_Prof_Content <- ifelse(data4$Profile=="Content Writer" |data4$Profile=="Content Development" ,1,0)

##  Location

sort(table(data4$Location),decreasing=TRUE)[1:10]
data4$Location <- as.character(data4$Location)
data4$Is_LocatIIGB <- ifelse(data4$Location=="IIGB",1,0)
data4$Is_LocatIIDB <- ifelse(data4$Location=="IIDB",1,0)
data4$Is_LocatJEJJ <- ifelse(data4$Location=="JEJJ",1,0)
data4$Is_LocatIIBD <- ifelse(data4$Location=="IIBD",1,0)
data4$Is_LocatJABD <- ifelse(data4$Location=="JABD",1,0)


## Internship_Profile
sort(table(data4$Internship_Profile),decreasing=TRUE)[1:10]
data4$Internship_Profile <- as.character(data4$Internship_Profile)

data4$Is_IP_WD <- ifelse(data4$Internship_Profile=="Web Development",1,0)
data4$Is_IP_SD <- ifelse(data4$Internship_Profile=="Software Development",1,0)
data4$Is_IP_CW <- ifelse(data4$Internship_Profile=="Content Writing",1,0)
data4$Is_IP_AD <- ifelse(data4$Internship_Profile=="Android App Development",1,0)
data4$Is_IP_MK <- ifelse(data4$Internship_Profile=="Marketing",1,0)
data4$Is_IP_BD <- ifelse(data4$Internship_Profile=="Business Development",1,0)


## Skills_required
sort(table(data4$Skills_required),decreasing=TRUE)[1:10]
data4$Skills_required <- as.character(data4$Skills_required)
data4$Is_SR_No <- ifelse(data4$Skills_required=="No_Skill",1,0)

## Internship_Location

sort(table(data4$Internship_Location),decreasing=TRUE)[1:10]

data4$Internship_Location <- as.character(data4$Internship_Location)
data4$Is_IntrnLoc_IIDB <- ifelse(data4$Internship_Location =="IIDB",1,0)
data4$Is_IntrnLoc_IIBD <- ifelse(data4$Internship_Location =="IIBD",1,0)
data4$Is_IntrnLoc_IIGB <- ifelse(data4$Internship_Location =="IIGB",1,0)
data4$Is_IntrnLoc_JABD <- ifelse(data4$Internship_Location =="JABD",1,0)
data4$Is_IntrnLoc_JEJJ <- ifelse(data4$Internship_Location =="JEJJ",1,0)

# converting Internship_deadline to factor
data4$Internship_deadline <- as.character(data4$Internship_deadline)
data4$Internship_deadline <- as.Date(data4$Internship_deadline, "%d-%m-%Y")


# creating dummy variables of Current_year ,Experience_Type etc
library(dummies)
dummy.data.frame
ss <- data.frame(data4$Current_year,data4$Experience_Type,data4$Internship_Type,data4$Internship_category,data4$Stipend_Type)
ss1<- dummy.data.frame(ss)
data4 <- cbind(data4,ss1)

#Dropping irrelevant variables 
data4$Current_year <- NULL
data4$Experience_Type <- NULL
data4$Internship_Type <- NULL
data4$Internship_category <- NULL
data4$Stipend_Type <- NULL


# Match/ Distance between Preferred_location and Internship_Location
data4$Pref_Intern_LocMatch <- 0
data4$Pref_Intern_LocMatch[as.character(data4$Preferred_location) == as.character(data4$Internship_Location) | as.character(data4$Preferred_location)=="No_Pref"] <- 1


# Expected_Stipend (expected by student) Stipend1(min offered) Stipend2(max offered)
# Substituting Middle value of Expected_Stipend
table(data4$Expected_Stipend)

data4$Expected_Stipend <- as.character(data4$Expected_Stipend)
data4$Expected_Stipend[data4$Expected_Stipend=="10K+"] <- 10000
data4$Expected_Stipend[data4$Expected_Stipend=="2-5K"] <- 3500
data4$Expected_Stipend[data4$Expected_Stipend=="5-10K"] <- 7500
data4$Expected_Stipend[data4$Expected_Stipend=="No Expectations"] <- 0
data4$Expected_Stipend <- as.numeric(data4$Expected_Stipend)

# creating  Feature whether Expected_Stipend < Stipend1
data4$St_EMatch <- ifelse(data4$Expected_Stipend < data4$Stipend1,1,0)


# Creating Feature about range of Stipend Offered
#Stipend2 - Stipend1

data4$Stip_range <- abs(data4$Stipend2 - data4$Stipend1)

# Creating feature Minimum_Duration is less than Internship_Duration.Months.
summary(data4$Internship_Duration.Months.)
summary(data4$Minimum_Duration)

data4$Duration_Match <- 0
data4$Duration_Match <- ifelse(data4$Minimum_Duration >= data4$Internship_Duration.Months.,1,0)


#Creating Feature whether there is a match between Institute_location and Internship_Location
data4$Inst_Intern_LocMatch <- 0
data4$Inst_Intern_LocMatch[as.character(data4$Institute_location) == as.character(data4$Internship_Location) ] <- 1


#Creating Feature whether there is a match between hometown and Internship_Location
data4$hometown_Intern_LocMatch <- 0
data4$hometown_Intern_LocMatch[as.character(data4$hometown) == as.character(data4$Internship_Location) ] <- 1



# Creating feature difference between Year_of_graduation and year of Internship_deadline
library(lubridate)
data4$Dif_Yog_IntD <- 0
data4$Dif_Yog_IntD <- data4$Year_of_graduation - year(data4$Internship_deadline)

data4$Neg_Dif_Yog_IntD <- ifelse(data4$Dif_Yog_IntD > 0, 1,0)

# tagging whether a candidate  is PG 
data4$Is_PG <- 0

data4$Is_PG <- ifelse(substr(data4$Degree,1,1)=="M" | substr(data4$Degree,1,1)=="P" ,1,0)
data4$Is_PG[grep("B.E. & MBA",data4$Degree)]<- 1
data4$Is_PG[grep("B.Tech and M.Tech",data4$Degree)]<- 1
data4$Is_PG[grep("Integrated",data4$Degree)]<- 1


# tagging whether a candidate have Prof degree
data4$Is_Prof <- 0
data4$Is_Prof[grep("Tech",data4$Degree)]<- 1
data4$Is_Prof[grep("B.E",data4$Degree)]<- 1
data4$Is_Prof[grep("MCA",data4$Degree)]<- 1
data4$Is_Prof[grep("MBA",data4$Degree)]<- 1
data4$Is_Prof[grep("Management",data4$Degree)]<- 1
data4$Is_Prof[grep("Admininstration",data4$Degree)]<- 1
data4$Is_Prof[grep("Technology",data4$Degree)]<- 1
data4$Is_Prof[grep("Computer",data4$Degree)]<- 1

##Creating Feature whether there is a match between Location (Location of work experience) and Internship_Location
data4$Workex_Intern_LocMatch <- 0
data4$Workex_Intern_LocMatch[as.character(data4$Location) == as.character(data4$Internship_Location)] <- 1


# No_of_openings
# group by Internship_ID the train file to check how many applicants
# ratio of applicant to opening
RATO <- data.frame(Internship_ID = data4$Internship_ID)
RATO$Num <- 1
library(sqldf)
RATO1 <- sqldf("select Internship_ID, SUM(Num) as Num_Applicant From RATO Group BY Internship_ID")
data4 <- merge(data4,RATO1, by="Internship_ID", all.x=TRUE)

data4$Open_App_Ratio <- data4$No_of_openings/data4$Num_Applicant
#removing data4$Num_Applicant
#data4$Num_Applicant <- NULL

## any relation between Internship_deadline,Earliest_Start_Date
data4$Internship_deadline <- as.Date(data4$Internship_deadline, "%d-%m-%Y")[1:10]
data4$Diff_Intdl_StrD <- as.numeric(as.character(data4$Internship_deadline - data4$Earliest_Start_Date))

## If applicant available before internship deadline

data4$NoCross_Deadline <- ifelse(data4$Diff_Intdl_StrD > 0 ,1,0)


## Internship_deadline < 2015-01-14

data4$Internship_deadline[data4$Internship_deadline < "2015-01-13"]

table(data4$Is_Shortlisted, data4$Internship_deadline > "2015-01-13")
data4$Deadline2015 <- ifelse(data4$Internship_deadline >"2015-01-13", 1,0)


#Institute_Category
data4$Institute_Category <- as.character(data4$Institute_Category)
data4$Institute_Category <- ifelse(data4$Institute_Category=="Y",1,0)



## Dropping irrelevant variables
data5 <- data4[,c(1:2,4:5,7,10,16,18,20,21,26,30,35:139,8,9)]
names(data5) <- make.names(names(data5))

## Splitting to Train and Test 

Train <- data5[data5$tag=="train",]
Test  <- data5[data5$tag=="test",]
Train$tag <- NULL
Test$tag  <- NULL
Test$Is_Shortlisted  <- NULL

write.csv(Train,"TrainD.csv",row.names=FALSE)
write.csv(Test,"TestD.csv",row.names=FALSE)

