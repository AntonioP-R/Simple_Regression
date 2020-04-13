F_test<-function(R_Squared,Observations,Significance_Level = 0.05){
  if(is.numeric(R_Squared)&R_Squared>=0&R_Squared<=1){
    if(is.numeric(Observations)&Observations>=3){
      if(is.numeric(Significance_Level)&Significance_Level==0.05
         |Significance_Level==0.01|Significance_Level==0.001){
        test<-((R_Squared*(Observations-2))/(1-R_Squared))
        F_DataSet <- read.csv("~/Desktop/Simple_Regression/data/Fisher.csv")
        if((Observations-2)<=30&Significance_Level==0.05){
          CriticalValues<-F_DataSet[Observations-2,2]
        }
        if((Observations-2)<=30&Significance_Level==0.01){
          CriticalValues<-F_DataSet[Observations-2,3]
        }
        if((Observations-2)<=30&Significance_Level==0.001){
          CriticalValues<-F_DataSet[Observations-2,4]
        }
        if((Observations-2)>30&((Observations-2))<=40&Significance_Level==0.05){
          CriticalValues<-F_DataSet[32,2]
        }
        if((Observations-2)>30&((Observations-2))<=40&Significance_Level==0.01){
          CriticalValues<-F_DataSet[32,3]
        }
        if((Observations-2)>30&((Observations-2))<=40&Significance_Level==0.01){
          CriticalValues<-F_DataSet[32,4]
        }
        if((Observations-2)>40&((Observations-2))<=50&Significance_Level==0.05){
          CriticalValues<-F_DataSet[33,2]
        }
        if((Observations-2)>40&((Observations-2))<=50&Significance_Level==0.01){
          CriticalValues<-F_DataSet[33,3]
        }
        if((Observations-2)>40&((Observations-2))<=50&Significance_Level==0.001){
          CriticalValues<-F_DataSet[33,4]
        }
        if((Observations-2)>50&((Observations-2))<=60&Significance_Level==0.05){
          CriticalValues<-F_DataSet[34,2]
        }
        if((Observations-2)>50&((Observations-2))<=60&Significance_Level==0.01){
          CriticalValues<-F_DataSet[34,3]
        }
        if((Observations-2)>50&((Observations-2))<=60&Significance_Level==0.001){
          CriticalValues<-F_DataSet[34,4]
        }
        if((Observations-2)>60&((Observations-2))<=70&Significance_Level==0.05){
          CriticalValues<-F_DataSet[35,2]
        }
        if((Observations-2)>60&((Observations-2))<=70&Significance_Level==0.01){
          CriticalValues<-F_DataSet[35,3]
        }
        if((Observations-2)>60&((Observations-2))<=70&Significance_Level==0.001){
          CriticalValues<-F_DataSet[35,4]
        }
        if((Observations-2)>70&((Observations-2))<=80&Significance_Level==0.05){
          CriticalValues<-F_DataSet[36,2]
        }
        if((Observations-2)>70&((Observations-2))<=80&Significance_Level==0.01){
          CriticalValues<-F_DataSet[36,3]
        }
        if((Observations-2)>70&((Observations-2))<=80&Significance_Level==0.001){
          CriticalValues<-F_DataSet[36,4]
        }
        if((Observations-2)>80&((Observations-2))<=90&Significance_Level==0.05){
          CriticalValues<-F_DataSet[37,2]
        }
        if((Observations-2)>80&((Observations-2))<=90&Significance_Level==0.01){
          CriticalValues<-F_DataSet[37,3]
        }
        if((Observations-2)>80&((Observations-2))<=90&Significance_Level==0.001){
          CriticalValues<-F_DataSet[37,4]
        }
        if((Observations-2)>90&((Observations-2))<=100&Significance_Level==0.05){
          CriticalValues<-F_DataSet[38,2]
        }
        if((Observations-2)>90&((Observations-2))<=100&Significance_Level==0.01){
          CriticalValues<-F_DataSet[38,3]
        }
        if((Observations-2)>90&((Observations-2))<=100&Significance_Level==0.001){
          CriticalValues<-F_DataSet[38,4]
        }
        if((Observations-2)>100&Significance_Level==0.05){
          CriticalValues<-F_DataSet[39,2]
        }
        if((Observations-2)>100&&Significance_Level==0.01){
          CriticalValues<-F_DataSet[39,3]
        }
        if((Observations-2)>100&Significance_Level==0.001){
          CriticalValues<-F_DataSet[39,4]
        }
        if(test<CriticalValues){
          result<-"Accept H0"
        }else{
          result<-"Refuse H0"
        }
        vd<-c("","R_Squared = 0","R_Squared > 0")
        Data<-data.frame(vd)
        row.names(Data)<-c("ASSUMPTIONS:","H0","H1")
        names(Data)<-""
        Values<-c("",round(test,3),CriticalValues,result)
        Data1<-data.frame(Values)
        names(Data1)<-""
        row.names(Data1)<-c("OUTPUTS:","Statistical Test:","Critical Value:","Decision:")
        print(Data)
        print(Data1)
      }else{
        print("Error! The possible significance level accepted are: 0.005,0.01,0.001.")
      }
    }else{
      print("Error! The observations must be numeric type and greater than three.")
    }
  }else{
    print("Error! R_Squared must be between zero and one.")
  }
}


