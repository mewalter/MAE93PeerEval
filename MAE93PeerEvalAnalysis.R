
library("tidyverse")
library("openxlsx")    # library("readxl") # this is the tidyverse installed package
library("scales")
library("lubridate")
library("rstudioapi")


BaseDir <- dirname(rstudioapi::getActiveDocumentContext()$path)
#BaseDir <- setwd("~/pCloudDrive/RProjects/PeerEvalAnalysis")
DataDir <- paste0(BaseDir,"/Data/")
setwd(DataDir)
#if( .Platform$OS.type == "unix" )
#  DataDir <- "~/Dropbox/Classes/MAE151F20/Grades/PeerEval/MidQuarter/Data"

# rm(list=ls())


file_list <- list.files()
# delete some files  
file_list <- file_list[!grepl(paste0("desktop.ini", collapse = "|"), file_list)]

DataCols <- c("X5","X6","X7","X8")

FileData <- read.xlsx(file_list[1],colNames=FALSE)
nummembers <- length(FileData$X1)-3

i <- 4   # debugging
AllRankings = list()          # initialize list variable
for (i in 1:length(file_list)) {
  cat(file_list[i],sep='\n')
  FileData <- read.xlsx(file_list[i],colNames=FALSE)
  FileData.Rankings <- FileData %>% slice(4:(nummembers+3)) %>% select(1:9) %>%   # collect values for members
    mutate_at(DataCols,as.numeric) %>% mutate(across(where(is.factor),as.character)) #%>%
#    mutate(across(DataCols,replace(FileData.Rankings[DataCols],FileData.Rankings$X4==1,NA)))
# if X4 == 1, then X5, X6, X7, X8 = NA      ... row above does not work ... rows below are ugly but work
  FileData.Rankings$X5[FileData.Rankings$X4 == 1] <- FileData.Rankings$X6[FileData.Rankings$X4 == 1] <- 
    FileData.Rankings$X7[FileData.Rankings$X4 == 1] <- FileData.Rankings$X8[FileData.Rankings$X4 == 1] <- NA
  FileData.Rankings$Rater <- gsub("_.*","",file_list[i])              # insert name of the person doing reviewing
  AllRankings[[i]] <- FileData.Rankings                               # add to master file
  # If average of X5, X6, X7, X8 = 5 AllFive <- TRUE and Print Warning 
  cat(mean(as.matrix(FileData.Rankings[DataCols]),na.rm=T),sep='\n')
}   
FinalRankings <- dplyr::bind_rows(AllRankings)       # turn list into data frame
FinalRankings <- FinalRankings %>% arrange(desc(X1))   # just to dispay names together also has comments

# For each student: get relevant columns, get sum and means, rename columns
Results <- FinalRankings %>% select(1,3:8) %>% group_by(X1) %>% summarise_all(list(mean),na.rm=T) %>% 
  rowwise() %>% mutate(Sum=sum(c(X5,X6,X7,X8),na.rm=T)) %>% mutate(Avg=mean(c(X5,X6,X7,X8),na.rm=T))  #%>%
 # rename(Name=V1, Skill=V2, Contrib=V3, Teaming=V4, OnTrack=V5, Quality=V6)

# Load team data to get team number and ucnetID into Results file ... FUTURE convert "Team X" to just "X"
# teamdata = read.csv("../GroupsWithNames.csv", header = TRUE, stringsAsFactors = FALSE)
#Results <- Results %>% left_join(teamdata, by="Name")  %>%  # does NOT remove duplicates
#  rename(TeamNum=ProjectName, NumMem=NumMembers)
# Normalize average rating by team's max score
#Results <- Results %>%  group_by(TeamNum) %>% mutate(TeamMax=max(Avg)) %>% ungroup() %>% arrange(TeamNum) %>%
#  mutate(AvgNorm = Avg/TeamMax*5) 

setwd(BaseDir)
write.csv(FinalRankings, file = "AllRankingsComments.csv",row.names=FALSE)
write.csv(Results, file = "Result4EachPerson.csv",row.names=FALSE)


