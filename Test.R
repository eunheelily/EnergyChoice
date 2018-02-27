
library(readr)
Data <- read_csv("Database.csv")
Market_Share_Simple <- read_csv("Market_Share_Simple.csv")
Cost_data <- read_csv("Cost.csv") 
Projection <- read_csv("Price_Projection.csv")

N = 5000 # Number of simulation
N_car_model = nrow(Data)# count number of car models
ln_Mean_VMT <- 8.87192821723217 # Mean value of Ln(VTM). THis is because the VTM distribution is lognormal distribution 
ln_SD_VMT <- 1.09899648130874 # Standard deviation of Ln(VTM) 
VMT <- exp(rnorm(n=N,mean=ln_Mean_VMT, sd=ln_SD_VMT)) # Calulate VTM from normally distributed ln(VMT)
VMT[VMT>150000]=150000 # VMT that is larger than 150,000 is 150,000
# these are the values we use and subject to change


discount = 0.2
years_own = 7
Gas_price <- 2.782
Elec_price <- 0.19
Phybrid_Gas_Percentage = 0.6
Uncertainty = 0.3
Fed <- 1
CVRP <- 1
year <- 2016
EV_rebate  <- 2500
PHEV_rebate <- 1000
Discount_PHEV <- 1000
Discount_EV <- 2000
agency <- "Sonoma"
Length <- 3
Budget <- 1500000
Marketing <-80/100
Lux_BEV_rebate <- 2
Lux_PHEV_rebate <- 2


# change the total incentive depending on the availability.    

Cost_data$incen <- rep(0, nrow=Cost_data)
for (i in 72:83){
  Cost_data$incen[i] <- ifelse(Fed == 1, Cost_data$Incentive[i],0)
  Cost_data$incen[i] <- ifelse(CVRP == 1,Cost_data$Incentive[i]+1500,Cost_data$Incentive[i])}
for (i in 84:95){
  Cost_data$incen[i] <- ifelse(Fed == 1, Cost_data$Incentive[i],0)
  Cost_data$incen[i] <- ifelse(CVRP == 1,Cost_data$Incentive[i]+2500,Cost_data$Incentive[i])}

# Calculate the new PHEV and EV price based on year and subtracted the incentive    

Cost_data$Year_Pur_Cost[72:76]<-Cost_data$Base_Pur_Cost[72:76]*(1-Projection$PHEV[match(Year,Projection$Year)])-Cost_data$incen[72:76]
Cost_data$Year_Pur_Cost[77:83]<-Cost_data$Base_Pur_Cost[77:83]*(1-Projection$PHEV[match(Year,Projection$Year)]*0.68)-Cost_data$incen[77:83]
Cost_data$Year_Pur_Cost[84:91] <- Cost_data$Base_Pur_Cost[84:91]*(1-Projection$EV[match(Year,Projection$Year)])-Cost_data$incen[84:91]
Cost_data$Year_Pur_Cost[92:93] <- Cost_data$Base_Pur_Cost[92:93]*(1-Projection$EV[match(Year,Projection$Year)]*0.681)-Cost_data$incen[92:93]
Cost_data$Year_Pur_Cost[94:95] <- Cost_data$Base_Pur_Cost[94:95]*(1-Projection$EV[match(Year,Projection$Year)]*0.96)-Cost_data$incen[94:95]

# Calculate the total purchase price - incentive + owndership cost
Cost_data[,8] <- Cost_data[,4]+Cost_data[,5]

# Generate the data sample based on the proportion of each vehicle market share.      
Cost_matrix <- matrix(rep(0,N*30), nrow=N, ncol=30)     
Cost_matrix[,1] <- as.numeric(sample(as.character(unlist(Cost_data[1:5,8])),N, prob=as.character(unlist(Cost_data[1:5,6])),replace=TRUE))
Cost_matrix[,2] <- as.numeric(sample(as.character(unlist(Cost_data[6:10,8])),N, prob=as.character(unlist(Cost_data[6:10,6])),replace=TRUE))
Cost_matrix[,3] <- as.numeric(sample(as.character(unlist(Cost_data[11:15,8])),N, prob=as.character(unlist(Cost_data[11:15,6])),replace=TRUE))
Cost_matrix[,4] <- as.numeric(sample(as.character(unlist(Cost_data[16:20,8])),N, prob=as.character(unlist(Cost_data[16:20,6])),replace=TRUE))
Cost_matrix[,5] <- as.numeric(sample(as.character(unlist(Cost_data[21:25,8])),N, prob=as.character(unlist(Cost_data[21:25,6])),replace=TRUE))
Cost_matrix[,6] <-as.numeric(sample(as.character(unlist(Cost_data[26:29,8])),N, prob=as.character(unlist(Cost_data[26:29,6])),replace=TRUE))
Cost_matrix[,7] <- as.numeric(sample(as.character(unlist(Cost_data[30:34,8])),N, prob=as.character(unlist(Cost_data[30:34,6])),replace=TRUE))
Cost_matrix[,8] <- as.numeric(sample(as.character(unlist(Cost_data[35:39,8])),N, prob=as.character(unlist(Cost_data[35:39,6])),replace=TRUE))
Cost_matrix[,9] <- as.numeric(sample(as.character(unlist(Cost_data[40:44,8])),N, prob=as.character(unlist(Cost_data[40:44,6])),replace=TRUE))
Cost_matrix[,10] <- as.numeric(sample(as.character(unlist(Cost_data[45:49,8])),N, prob=as.character(unlist(Cost_data[45:49,6])),replace=TRUE))
Cost_matrix[,11] <- as.numeric(sample(as.character(unlist(Cost_data[50:54,8])),N, prob=as.character(unlist(Cost_data[50:54,6])),replace=TRUE))
Cost_matrix[,12] <- as.numeric(sample(as.character(unlist(Cost_data[55,8])),N, prob=as.character(unlist(Cost_data[55,6])),replace=TRUE))
Cost_matrix[,13] <- as.numeric(sample(as.character(unlist(Cost_data[56:58,8])),N, prob=as.character(unlist(Cost_data[56:58,6])),replace=TRUE))
Cost_matrix[,14] <- as.numeric(sample(as.character(unlist(Cost_data[59:63,8])),N, prob=as.character(unlist(Cost_data[59:63,6])),replace=TRUE))
Cost_matrix[,15] <- as.numeric(sample(as.character(unlist(Cost_data[64,8])),N, prob=as.character(unlist(Cost_data[64,6])),replace=TRUE))
Cost_matrix[,16] <- as.numeric(sample(as.character(unlist(Cost_data[65:67,8])),N, prob=as.character(unlist(Cost_data[65:67,6])),replace=TRUE))
Cost_matrix[,17] <- as.numeric(sample(as.character(unlist(Cost_data[68,8])),N, prob=as.character(unlist(Cost_data[68,6])),replace=TRUE))
Cost_matrix[,18] <- as.numeric(sample(as.character(unlist(Cost_data[69,8])),N, prob=as.character(unlist(Cost_data[69,6])),replace=TRUE))
Cost_matrix[,19] <- as.numeric(sample(as.character(unlist(Cost_data[70:71,8])),N, prob=as.character(unlist(Cost_data[70:71,6])),replace=TRUE))
Cost_matrix[,20] <- as.numeric(sample(as.character(unlist(Cost_data[72:73,8])),N, prob=as.character(unlist(Cost_data[72:73,6])),replace=TRUE))
Cost_matrix[,21] <- as.numeric(sample(as.character(unlist(Cost_data[74:76,8])),N, prob=as.character(unlist(Cost_data[74:76,6])),replace=TRUE))
Cost_matrix[,22] <- as.numeric(sample(as.character(unlist(Cost_data[77,8])),N, prob=as.character(unlist(Cost_data[77,6])),replace=TRUE))
Cost_matrix[,23] <- as.numeric(sample(as.character(unlist(Cost_data[78:79,8])),N, prob=as.character(unlist(Cost_data[78:79,6])),replace=TRUE))
Cost_matrix[,24] <- as.numeric(sample(as.character(unlist(Cost_data[80,8])),N, prob=as.character(unlist(Cost_data[80,6])),replace=TRUE))
Cost_matrix[,25] <- as.numeric(sample(as.character(unlist(Cost_data[81:83,8])),N, prob=as.character(unlist(Cost_data[81:83,6])),replace=TRUE))
Cost_matrix[,26] <- as.numeric(sample(as.character(unlist(Cost_data[84:87,8])),N, prob=as.character(unlist(Cost_data[84:87,6])),replace=TRUE))
Cost_matrix[,27] <- as.numeric(sample(as.character(unlist(Cost_data[88:91,8])),N, prob=as.character(unlist(Cost_data[88:91,6])),replace=TRUE))
Cost_matrix[,28] <- as.numeric(sample(as.character(unlist(Cost_data[92:93,8])),N, prob=as.character(unlist(Cost_data[92:93,6])),replace=TRUE))
Cost_matrix[,29] <- as.numeric(sample(as.character(unlist(Cost_data[94,8])),N, prob=as.character(unlist(Cost_data[94,6])),replace=TRUE))
Cost_matrix[,30] <- as.numeric(sample(as.character(unlist(Cost_data[95,8])),N, prob=as.character(unlist(Cost_data[95,6])),replace=TRUE))

# make a mtrix to generate normally distributed delta
Delta_matrix <-  matrix(rep(NA,N*N_car_model),nrow=N, ncol=N_car_model) 
for (j in 1:N_car_model){
  Delta_matrix[,j] <- rnorm(n=N,mean=Data$Delta[j],sd=Data$Delta[j]*Uncertainty)
}

# Make a matrix for Total life cycle costs by each segment. 
TotalCost <- matrix(rep(NA,N*N_car_model),nrow=N, ncol=N_car_model)

N_ICEV <- sum(Data$Fuel_Type=="ICEV")
N_Hy <- sum(Data$Fuel_Type=="Hybrid")
N_PHy <- sum(Data$Fuel_Type=="Phybrid")-4
N_PHy_Lux <- 4
N_EV <- sum(Data$Fuel_Type=="EV")-2
N_EV_Lux <- 2

# the "for" functions below are to calculate total costs by each segment. 
for (i in 1:N){
  for (j in 1:(N_ICEV+N_Hy)){
    TotalCost[i,j] <- Data$Oper[j]*VMT[i]/15000+VMT[i]*Data$Fuel_gas[j]*Gas_price/discount*(1-1/(1+discount)^years_own)-Delta_matrix[i,j]+Cost_matrix[i,j] 
  }
  for (j in (1+N_ICEV+N_Hy):(N_ICEV+N_Hy+N_PHy)){
    TotalCost[i,j] <- Data$Oper[j]*VMT[i]/15000+VMT[i]*Data$Fuel_gas[j]*Gas_price*Phybrid_Gas_Percentage/discount*(1-1/(1+discount)^years_own)+VMT[i]*Data$Fuel_Elec[j]*Elec_price*(1-Phybrid_Gas_Percentage)/discount*(1-1/(1+discount)^years_own)-Delta_matrix[i,j]+Cost_matrix[i,j] 
  }
  for (j in (1+N_ICEV+N_Hy+N_PHy):(N_ICEV+N_Hy+N_PHy_Lux)){
    TotalCost[i,j] <- Data$Oper[j]*VMT[i]/15000+VMT[i]*Data$Fuel_gas[j]*Gas_price*Phybrid_Gas_Percentage/discount*(1-1/(1+discount)^years_own)+VMT[i]*Data$Fuel_Elec[j]*Elec_price*(1-Phybrid_Gas_Percentage)/discount*(1-1/(1+discount)^years_own)-Delta_matrix[i,j]+Cost_matrix[i,j] 
  }
  for (j in (1+N_ICEV+N_Hy+N_PHy+N_PHy_Lux):(N_ICEV+N_Hy+N_PHy+N_PHy_Lux+N_EV)){
    TotalCost[i,j] <- Data$Oper[j]*VMT[i]/15000+VMT[i]*Data$Fuel_Elec[j]*Elec_price/discount*(1-1/(1+discount)^years_own)-Delta_matrix[i,j]+Cost_matrix[i,j]  
  }
  for (j in (1+N_ICEV+N_Hy+N_PHy+N_PHy_Lux+N_EV):(N_ICEV+N_Hy+N_PHy+N_PHy_Lux+N_EV+N_EV_Lux)){
    TotalCost[i,j] <- Data$Oper[j]*VMT[i]/15000+VMT[i]*Data$Fuel_Elec[j]*Elec_price/discount*(1-1/(1+discount)^years_own)-Delta_matrix[i,j]+Cost_matrix[i,j]  
  }
}

# make matrix to choose the minimum cost option. 
Decision_Matrix <- matrix(rep(NA,N*N_car_model),nrow=N, ncol=N_car_model) 
# if the segment is the lowest cost, put 1, otherwise, put "0"
for (i in 1:N) {
  for (j in 1:N_car_model){
    Decision_Matrix[i,j] = ifelse(as.numeric(j)==as.numeric(which.min(TotalCost[i,1:N_car_model])),as.numeric(1),as.numeric(0))}}

# Calculate Baseline market share

Marketshare <-matrix(rep(NA,N_car_model),nrow=1, ncol=N_car_model)
for (j in 1:N_car_model){
  Marketshare[,j]=sum(Decision_Matrix[1:N,j],na.rm=TRUE)
}
Marketshare_Table <- Market_Share_Simple
Marketshare_Table[1:2,] <- Market_Share_Simple
Marketshare_Table[3,] <- Marketshare[1,]
Marketshare_Table[4,] <- Marketshare_Table[3,]/Marketshare_Table2[1,]
Marketshare_Table[5,] <- Marketshare_Table[4,]*0.853628632417563/sum(Marketshare_Table[4,])
colnames(Marketshare_Table) <- colnames(Market_Share_Simple)
rownames(Marketshare_Table) <- c("Propotion of sales from these models","Real Market Share","Counts from TCM","Recalculated Counts","Estimated Market Share")

###########################################################################


PHEV_rebate_Lux <-ifelse(Lux_PHEV_rebate==1, PHEV_rebate, 0)
Discount_PHEV_Lux<-ifelse(Lux_PHEV_rebate==1, PHEV_rebate, 0)
EV_rebate_Lux<-ifelse(Lux_BEV_rebate==1, EV_rebate, 0)
Discount_EV_Lux<-ifelse(Lux_BEV_rebate==1, EV_rebate, 0)

# Calculate the total life cycle costs but with rebates    
TotalCost2 <- matrix(rep(NA,N*N_car_model),nrow=N, ncol=N_car_model)

for (i in 1:N){
  for (j in 1:(N_ICEV+N_Hy)){
    TotalCost2[i,j] <- Data$Oper[j]*VMT[i]/15000+VMT[i]*Data$Fuel_gas[j]*Gas_price/discount*(1-1/(1+discount)^years_own)-Delta_matrix[i,j]+Cost_matrix[i,j]  
  }
  for (j in (1+N_ICEV+N_Hy):(N_ICEV+N_Hy+N_PHy)){
    TotalCost2[i,j] <- Data$Oper[j]*VMT[i]/15000+VMT[i]*Data$Fuel_gas[j]*Gas_price*Phybrid_Gas_Percentage/discount*(1-1/(1+discount)^years_own)+VMT[i]*Data$Fuel_Elec[j]*Elec_price*(1-Phybrid_Gas_Percentage)/discount*(1-1/(1+discount)^years_own)-Delta_matrix[i,j]+Cost_matrix[i,j]-PHEV_rebate-Discount_PHEV
  }
  for (j in (1+N_ICEV+N_Hy+N_PHy):(N_ICEV+N_Hy+N_PHy_Lux)){
    TotalCost2[i,j] <- Data$Oper[j]*VMT[i]/15000+VMT[i]*Data$Fuel_gas[j]*Gas_price*Phybrid_Gas_Percentage/discount*(1-1/(1+discount)^years_own)+VMT[i]*Data$Fuel_Elec[j]*Elec_price*(1-Phybrid_Gas_Percentage)/discount*(1-1/(1+discount)^years_own)-Delta_matrix[i,j]+Cost_matrix[i,j]-PHEV_rebate_Lux-Discount_PHEV_Lux
  }
  for (j in (1+N_ICEV+N_Hy+N_PHy+N_PHy_Lux):(N_ICEV+N_Hy+N_PHy+N_PHy_Lux+N_EV)){
    TotalCost2[i,j] <- Data$Oper[j]*VMT[i]/15000+VMT[i]*Data$Fuel_Elec[j]*Elec_price/discount*(1-1/(1+discount)^years_own)-Delta_matrix[i,j]+Cost_matrix[i,j]-EV_rebate-Discount_EV
  }
  for (j in (1+N_ICEV+N_Hy+N_PHy+N_PHy_Lux+N_EV):(N_ICEV+N_Hy+N_PHy+N_PHy_Lux+N_EV+N_EV_Lux)){
    TotalCost2[i,j] <- Data$Oper[j]*VMT[i]/15000+VMT[i]*Data$Fuel_Elec[j]*Elec_price/discount*(1-1/(1+discount)^years_own)-Delta_matrix[i,j]+Cost_matrix[i,j]-EV_rebate_Lux-Discount_EV_Lux
  }
}






# Make a matrix and choose the minimum cost option. 
Decision_Matrix2 <- matrix(rep(NA,N*N_car_model),nrow=N, ncol=N_car_model) 

for (i in 1:N) {
  for (j in 1:N_car_model){
    Decision_Matrix2[i,j] = ifelse(as.numeric(j)==as.numeric(which.min(TotalCost2[i,1:N_car_model])),as.numeric(1),as.numeric(0))}}

# Calculate predicted market share with rebates
Marketshare2 <-matrix(rep(NA,N_car_model),nrow=1, ncol=N_car_model)
for (j in 1:N_car_model){
  Marketshare2[,j]=sum(Decision_Matrix2[1:N,j],na.rm=TRUE)
}
Marketshare_Table2 <- Market_Share_Simple
Marketshare_Table2[1:2,] <- Market_Share_Simple
Marketshare_Table2[3,] <- Marketshare2[1,]
Marketshare_Table2[4,] <- Marketshare_Table2[3,]/Marketshare_Table2[1,]
Marketshare_Table2[5,] <- Marketshare_Table2[4,]*0.853628632417563/sum(Marketshare_Table2[4,])
colnames(Marketshare_Table2) <- colnames(Market_Share_Simple)
rownames(Marketshare_Table2) <- c("Propotion of sales from these models","Real Market Share","Counts from TCM","Recalculated Counts","Estimated Market Share")

# Calculate how many number of vehicle would be purchased in the location.
Autosale = 2086966
CA_pop = 39250017
Agency_Pop <- ifelse(Agency==1,72553,ifelse(Agency==2,870887, ifelse(Agency==3, 160106, ifelse(Agency==4,1537944,ifelse(Agency==5,764797,ifelse(Agency==6,136646,ifelse(Agency==7,1919402,ifelse(Agency==8,590698,0))))))))
P_sales <-Autosale*Agency_Pop/CA_pop/12*Length*Marketing

# Calculate the maximum number of vehicles 



if (Lux_BEV_rebate == 1){
Base_EV <- sum(Marketshare_Table[5,26:30])
Predict_EV <- sum(Marketshare_Table2[5,26:30])
} else {
Base_EV <- sum(Marketshare_Table[5,26:28])
Predict_EV <- sum(Marketshare_Table2[5,26:28])
}


if (Lux_PHEV_rebate == 1){
  Base_PHEV <- sum(Marketshare_Table[5,20:21])
  Predict_PHEV <- sum(Marketshare_Table2[5,20:21])
} else {
  Base_PHEV <- sum(Marketshare_Table[5,22:25])
  Predict_PHEV <- sum(Marketshare_Table2[5,22:25])
}

Prob_demand_EV <- ifelse((PHEV_rebate==0)&(Discount_PHEV==0), Prob_demand_EV <- 1, Prob_demand_EV <- Predict_EV/(Predict_EV+Predict_PHEV))
Prob_demand_PHEV <- 1-Prob_demand_EV

max_EV <- Prob_demand_EV*Budget/EV_rebate
max_PHEV <- Budget/PHEV_rebate*Prob_demand_PHEV

Final_EV <- ifelse(Predict_EV*P_sales>max_EV, max_EV, Predict_EV*P_sales)
Final_PHEV <- ifelse(Predict_PHEV*P_sales>max_PHEV, max_PHEV, Predict_PHEV*P_sales)

# Present the estimated results in the table. 
FinalTable <- matrix(c(Final_EV,Final_PHEV,(Predict_EV-Base_EV)/Predict_EV*Final_EV, (Predict_PHEV-Base_PHEV)/Predict_PHEV*Final_PHEV), nrow=2, ncol=2)
colnames(FinalTable)<-c("Total participation","Participation caused by incetive")
rownames(FinalTable)<-c("EV","PHEV")
FinalTable2 <- as.data.frame(FinalTable)

Finalsale <- as.matrix(FinalTable2,nrow=2, ncol=2)
barplot(t(Finalsale),col=colors()[c(12,15)])




#Emission##################################################################

TCM <- matrix(c(300,200,200,100),nrow=2, ncol=2)
E1 <- 0
E2 <-  0.09
E3 <-  0.063
E4 <-  0
E5 <-  0.145
E6 <-  0
E7 <-  0
E8 <-  0.595
E9 <-  0.095
E10 <- 0.012

Rebound <- 0.03
Trans <- 0.05
discount <-0.05
carbon_price <-13

Aveg_VTM <- 11244
Lifetime <- 15
Efficiency <- 0.3
PHEV_gas_perc <-0.6
Impact <- "Med"


G_table <- read_csv("G:/EV/Shiny/EVincentive/Emission_Gas.csv")
E_table <-read_csv("G:/EV/Shiny/EVincentive/Emission_Elec.csv")
Health_impact <- read_csv("G:/EV/Shiny/EVincentive/Health_impact.csv")
E_gas <- subset(G_table, Year==year & Agency==agency)
Emission_gas <- E_gas$CO2e/10^6

Emission_elec_CO2 <- (E1*E_table$CO2e[1]+E2*E_table$CO2e[2]+E3*E_table$CO2e[3]+E4*E_table$CO2e[4]+E5*E_table$CO2e[5]+E6*E_table$CO2e[6]+E7*E_table$CO2e[7]+E8*E_table$CO2e[8]+E9*E_table$CO2e[9]+E10*E_table$CO2e[10])/1000

Emission_elec_PM <- (E1*E_table$PM[1]+E2*E_table$PM[2]+E3*E_table$PM[3]+E4*E_table$PM[4]+E5*E_table$PM[5]+E6*E_table$PM[6]+E7*E_table$PM[7]+E8*E_table$PM[8]+E9*E_table$PM[9]+E10*E_table$PM[10])/1000

Emission_elec_Nox <- (E1*E_table$Nox[1]+E2*E_table$Nox[2]+E3*E_table$Nox[3]+E4*E_table$Nox[4]+E5*E_table$Nox[5]+E6*E_table$Nox[6]+E7*E_table$Nox[7]+E8*E_table$Nox[8]+E9*E_table$Nox[9]+E10*E_table$Nox[10])/1000

Emission_elec_Sox <- (E1*E_table$Sox[1]+E2*E_table$Sox[2]+E3*E_table$Sox[3]+E4*E_table$Sox[4]+E5*E_table$Sox[5]+E6*E_table$Sox[6]+E7*E_table$Sox[7]+E8*E_table$Sox[8]+E9*E_table$Sox[9]+E10*E_table$Sox[10])/1000


Annual_GHG_EV <- (Aveg_VTM*Emission_gas-Emission_elec_CO2*Aveg_VTM*Efficiency)*TCM[1,2]*(1+Rebound)/(1+Trans)
Annual_GHG_PHEV <-(Aveg_VTM*Emission_gas*(1-PHEV_gas_perc)-Emission_elec_CO2*Aveg_VTM*Efficiency*PHEV_gas_perc)*TCM[2,2]*(1+Rebound)/(1+Trans)
Disc_GHG_EV <- Annual_GHG_EV/discount*(1-1/(1+discount)^Lifetime)
Disc_GHG_PHEV <- Annual_GHG_PHEV/discount*(1-1/(1+discount)^Lifetime)
Total_GHG <- (Annual_GHG_EV+Annual_GHG_PHEV)*Lifetime
Total_disc_GHG <- Disc_GHG_EV + Disc_GHG_PHEV 
GHG_benefit <- carbon_price*Total_disc_GHG

T_EV_PM2.5 <- Aveg_VTM*(E_gas$PM2.5/10^6-Emission_elec_PM/2*Efficiency)*TCM[1,2]*(1+Rebound)/(1+Trans)
T_EV_PM10 <- Aveg_VTM*(E_gas$PM10/10^6-Emission_elec_PM/2*Efficiency)*TCM[1,2]*(1+Rebound)/(1+Trans)
T_EV_Nox <- Aveg_VTM*(E_gas$Nox/10^6-Emission_elec_Nox*Efficiency)*TCM[1,2]*(1+Rebound)/(1+Trans)
T_EV_Sox <- Aveg_VTM*(E_gas$Sox/10^6-Emission_elec_Sox*Efficiency)*TCM[1,2]*(1+Rebound)/(1+Trans)

T_PHEV_PM2.5 <- Aveg_VTM*(E_gas$PM2.5/10^6*(1-PHEV_gas_perc)-Emission_elec_PM/2*Efficiency*PHEV_gas_perc)*TCM[2,2]*(1+Rebound)/(1+Trans)
T_PHEV_PM10 <- Aveg_VTM*(E_gas$PM10/10^6*(1-PHEV_gas_perc)-Emission_elec_PM/2*Efficiency*PHEV_gas_perc)*TCM[2,2]*(1+Rebound)/(1+Trans)
T_PHEV_Nox <- Aveg_VTM*(E_gas$Nox/10^6*(1-PHEV_gas_perc)-Emission_elec_Nox*Efficiency*PHEV_gas_perc)*TCM[2,2]*(1+Rebound)/(1+Trans)
T_PHEV_Sox <- Aveg_VTM*(E_gas$Sox/10^6*(1-PHEV_gas_perc)-Emission_elec_Sox*Efficiency*PHEV_gas_perc)*TCM[2,2]*(1+Rebound)/(1+Trans)
Annual_total_tail <-matrix(c(T_EV_PM2.5+T_PHEV_PM2.5, T_EV_PM10+T_EV_PM10, T_EV_Nox+T_PHEV_Nox, T_EV_Sox+T_PHEV_Sox), ncol=4)
Disc_toal_tail <- Annual_total_tail/discount*(1/(1+discount)^Lifetime)
colnames(Disc_toal_tail) <- c("PM2.5","PM10","Nox","Sox")


E_impact <- ifelse(Impact=="Low",Disc_toal_tail[1]*Health_impact$PM2.5[1]+Disc_toal_tail[2]*Health_impact$PM10[1]+Disc_toal_tail[3]*Health_impact$Sox[1]+Disc_toal_tail[4]*Health_impact$Nox[1] ,ifelse(Impact=="Med",Disc_toal_tail[1]*Health_impact$PM2.5[2]+Disc_toal_tail[2]*Health_impact$PM10[2]+Disc_toal_tail[3]*Health_impact$Sox[2]+Disc_toal_tail[4]*Health_impact$Nox[2],Disc_toal_tail[1]*Health_impact$PM2.5[3]+Disc_toal_tail[2]*Health_impact$PM10[3]+Disc_toal_tail[3]*Health_impact$Sox[3]+Disc_toal_tail[4]*Health_impact$Nox[3]))

  
  
Admin_cost <- input$Length*input$Staff*input$Admin/12
Imp_cost <- input$Impcost
Total_rebates <- EV_rebate*TCM[1,2]+PHEV_rebate*ifelse(TCM[2,2]<=0, 0, TCM[2,2])

ii<- as.data.frame(c(rep(0,6)), row.names = c("GHG Reduction (ton)","GHG reduction benefits (dollar)", "Health Benefits (dollar)","Administrative Cost", "Implementation Cost", "Total rebates costs"),colnames(ii)<- c("Benefits and Costs"))

i<-matrix(seq(from=1,to=6),nrow=3,ncol=2)



