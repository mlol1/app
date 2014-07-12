#it in necessary to convert into data frames before naming the variables
percent_ALL<-data.frame(percent_ALL)
names(percent_ALL)<-c("County", "NACECODE_ALL_PERCENT")
percent_G<-data.frame(percent_G)
names(percent_G)<-c("County", "NACECODE_G_PERCENT")
percent_O<-data.frame(percent_O)
names(percent_O)<-c("County", "NACECODE_O_PERCENT")
percent_P<-data.frame(percent_P)
names(percent_P)<-c("County", "NACECODE_P_PERCENT")
percent_I<-data.frame(percent_I)
names(percent_I)<-c("County", "NACECODE_I_PERCENT")
percent_Q<-data.frame(percent_Q)
names(percent_Q)<-c("County" ,"NACECODE_Q_PERCENT")
percent_S<-data.frame(percent_S)
names(percent_S)<-c("County" ,"NACECODE_S_PERCENT")

#ordering by the County variable, necessary before joining the variables together, 
#otherwise the dataframes will join but the counties will not be the same order
percent_ALL<- percent_ALL[order(percent_ALL$County),]
percent_G<- percent_G[order(percent_G$County),]
percent_O<- percent_O[order(percent_O$County),]
percent_P<- percent_P[order(percent_P$County),]
percent_I<- percent_I[order(percent_I$County),]
percent_Q<- percent_Q[order(percent_Q$County),]
percent_S<- percent_S[order(percent_S$County),]

#putting into same order as Counties_numbers
Percent<-as.data.frame((cbind(percent_ALL,
                        percent_S, percent_G,
                        percent_O,percent_P,
                        percent_I,percent_Q)))


#combining, removing surplus county variables 
Percentage_completeness<-as.data.frame(cbind(percent_ALL,
                        Percent$NACECODE_S_PERCENT,Percent$NACECODE_G_PERCENT,Percent$NACECODE_O_PERCENT,
                        Percent$NACECODE_P_PERCENT,Percent$NACECODE_I_PERCENT,Percent$NACECODE_Q_PERCENT))


names(Percentage_completeness)<-(c("County", "ALL_PERCENT", "NACECODE_S_PERCENT", "NACECODE_G_PERCENT","NACECODE_O_PERCENT", 
"NACECODE_P_PERCENT" ,"NACECODE_I_PERCENT","NACECODE_Q_PERCENT"))
#adding column with name of original data frame for  all precent files. this is so they can be combined by row,
#while distinguishing one data frame from the other
variable<-c("percent_ALL","percent_ALL","percent_ALL","percent_ALL","percent_ALL","percent_ALL","percent_ALL","percent_ALL",
            "percent_ALL","percent_ALL","percent_ALL","percent_ALL","percent_ALL","percent_ALL","percent_ALL","percent_ALL",
            "percent_ALL","percent_ALL","percent_ALL","percent_ALL","percent_ALL","percent_ALL","percent_ALL","percent_ALL",
            "percent_ALL","percent_ALL")

percent_ALL<-cbind(percent_ALL,variable)
names(percent_ALL)<-c("County","percent","variable")

variable<-c("percent_S","percent_S","percent_S","percent_S","percent_S","percent_S","percent_S","percent_S",
            "percent_S","percent_S","percent_S","percent_S","percent_S","percent_S","percent_S","percent_S",
            "percent_S","percent_S","percent_S","percent_S","percent_S","percent_S","percent_S","percent_S",
            "percent_S","percent_S")

percent_S<-cbind(percent_S,variable)
names(percent_S)<-c("County","percent","variable")

variable<-c("percent_G","percent_G","percent_G","percent_G","percent_G","percent_G","percent_G","percent_G",
            "percent_G","percent_G","percent_G","percent_G","percent_G","percent_G","percent_G","percent_G",
            "percent_G","percent_G","percent_G","percent_G","percent_G","percent_G","percent_G","percent_G",
            "percent_G","percent_G")

percent_G<-cbind(percent_G,variable)
names(percent_G)<-c("County","percent","variable")

variable<-c("percent_O","percent_O","percent_O","percent_O","percent_O","percent_O","percent_O","percent_O",
            "percent_O","percent_O","percent_O","percent_O","percent_O","percent_O","percent_O","percent_O",
            "percent_O","percent_O","percent_O","percent_O","percent_O","percent_O","percent_O","percent_O",
            "percent_O","percent_O")

percent_O<-cbind(percent_O,variable)
names(percent_O)<-c("County","percent","variable")

variable<-c("percent_P","percent_P","percent_P","percent_P","percent_P","percent_P","percent_P","percent_P",
            "percent_P","percent_P","percent_P","percent_P","percent_P","percent_P","percent_P","percent_P",
            "percent_P","percent_P","percent_P","percent_P","percent_P","percent_P","percent_P","percent_P",
            "percent_P","percent_P")

percent_P<-cbind(percent_P,variable)
names(percent_P)<-c("County","percent","variable")

variable<-c("percent_I","percent_I","percent_I","percent_I","percent_I","percent_I","percent_I","percent_I",
            "percent_I","percent_I","percent_I","percent_I","percent_I","percent_I","percent_I","percent_I",
            "percent_I","percent_I","percent_I","percent_I","percent_I","percent_I","percent_I","percent_I",
            "percent_I","percent_I")

percent_I<-cbind(percent_I,variable)
names(percent_I)<-c("County","percent","variable")

variable<-c("percent_Q","percent_Q","percent_Q","percent_Q","percent_Q","percent_Q","percent_Q","percent_Q",
            "percent_Q","percent_Q","percent_Q","percent_Q","percent_Q","percent_Q","percent_Q","percent_Q",
            "percent_Q","percent_Q","percent_Q","percent_Q","percent_Q","percent_Q","percent_Q","percent_Q",
            "percent_Q","percent_Q")

percent_Q<-cbind(percent_Q,variable)
names(percent_Q)<-c("County","percent","variable")



#binding by rows
Percentage_completeness_rows<-as.data.frame(rbind(percent_ALL,
                                                  percent_S,percent_G,percent_O,
                                                  percent_P,percent_I,percent_Q))


#take a look to see that it turned out alright then save as a csv file
View(Percentage_completeness)
write.csv(Percentage_completeness,"C:/Users/mlol1/Desktop/OSM-GEODIRECTORY-PROJECT/Percentage_completeness.csv")
View(Percentage_completeness_rows)
write.csv(Percentage_completeness_rows,"C:/Users/mlol1/Desktop/OSM-GEODIRECTORY-PROJECT/Percentage_completeness_rows.csv")

#write.csv(percent_ALL,"C:/Users/mlol1/Desktop/OSM-GEODIRECTORY-PROJECT/percent_ALL.csv")
#write.csv(percent_S,"C:/Users/mlol1/Desktop/OSM-GEODIRECTORY-PROJECT/percent_S.csv")
#write.csv(percent_G,"C:/Users/mlol1/Desktop/OSM-GEODIRECTORY-PROJECT/percent_G.csv")
#write.csv(percent_O,"C:/Users/mlol1/Desktop/OSM-GEODIRECTORY-PROJECT/percent_O.csv")
#write.csv(percent_P,"C:/Users/mlol1/Desktop/OSM-GEODIRECTORY-PROJECT/percent_P.csv")
#write.csv(percent_I,"C:/Users/mlol1/Desktop/OSM-GEODIRECTORY-PROJECT/percent_I.csv")
#write.csv(percent_Q,"C:/Users/mlol1/Desktop/OSM-GEODIRECTORY-PROJECT/percent_Q.csv")
