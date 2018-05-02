
library(shiny)
library(readr)
library(shinydashboard)
library(reshape2)
library(ggplot2)
library(bsplus)

# seperate normal distribution
# calculate the environmental benefits, health benenfits. 
#reactive?


# Define UI for app that draws a bar graph ----
ui <-  dashboardPage(
  dashboardHeader(title="EV Incentive Program Toolkit"),
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Model Overview", tabName = "tab_1"),
      menuItem("User Guide", tabName = "tab_2"), 
      menuItem("Toolkit", tabName = "tab_3")
      
    )), 
  dashboardBody(tabItems(
    tabItem(tabName = "tab_1",
            fluidPage(h3("Model Overview"),
                      box(width = 12, h4("Introduction"),p("This is a tool meant to help Community Choice Energy Agencies predict the costs and benefits associated with offering an incentive program to subsidize residents’ purchases of battery electric vehicles (BEVs) or plug-in hybrid electric vehicles (PHEVs). Based on incentive amount, total budget, and a variety of other program and agency specifications, the model predicts the number of vehicle purchases that will be directly caused by an incentive program, then calculates associated greenhouse gas (GHG) emissions reductions and health impacts.")
                      ),
                      box(width = 12, h4("Using the Toolkit"),p("To use this model, at a minimum, users will need to enter values into the Primary Inputs section of the Toolkit tab. This section includes: Agency (Region), Total Incentives Budget, Year, Electric Vehicele (BEV) Incentive, Plug-in Hybrid (PHEV) Incentive, and Energy mix, and incentive amounts. There are a variety of additional inputs that allow users to users add further program specifications as appropriate. A detailed breakdown of all available input options is included in the User Guide tab.")
                      ),
                      box(width = 12, h4("Results"), p("Once the user has filled in the appropriate inputs and run the model, results will be displayed on the right-hand side of the Toolkit tab. The main results show the predicted participation in the incentive program for EV and PHEV incentives. These results show the total number of predicted incentives redeemed, and the predicted number of sales directly caused by the incentive. The model then displays predicted health, greenhouse gas, and monetary impacts associated with the incentive program. ")
                      ))) , 
    tabItem(tabName = "tab_2",
            fluidPage(h3("User Guide"),
                      box( width = 12, h4("Primary Inputs"),p("These inputs represent the minimum amount of information necessary to run the model. They are:"), 
                           br(),tags$div(tags$ul(tags$li("Agency (Region): Which CCE Agency will be running the program. The model uses this) information to set the correct population level and predict local emissions impacts."), 
                                                 tags$li("Total Incentives Budget: The total available budget for providing EV and PHEV incentives."),
                                                 tags$li("Year: The year that the incentives program will run."),
                                                 tags$li("Electric Vehicle (BEV) Incentive: The dollar amount that the agency will offer for each electric vehicle purchase."), 
                                                 tags$li("Plug-in Hybrid (PHEV) Incentive: The dollar amount that the agency will offer for each plug-in hybrid purchase."), 
                                                 tags$li("Energy mix: These values specify the composition of the energy mix that is used to charge electric vehicles."),  style = "font-size: 13px"))),    box( width = 12, h4("Incentive Details"),p("These allow agencies to add further details to their incentive offerings. These are included with
                                                                                                                                                                                                                                                       general default values that can be altered if necessary to match the agency’s needs."), br(),tags$div(tags$ul(tags$li("Include incentive for High end BEV and luxury PHEV: These are Yes/No inputs set at No by default. If switched to Yes, the model will include Tesla and luxury plug-in hybrid vehicles among those that receive their respective incentives."), 
                                                                                                                                                                                                                                                                                                                                                                     tags$li("Federal Tax Credit Availability/Clean Vehicle Rebate Project Availability: These are Yes/No inputs set at Yes by default. If switched to No the model will remove that credit or rebate from its calculations of vehicle cost."), 
                                                                                                                                                                                                                                                                                                                                                                     tags$li("Additional Discount EV/Plug-in: These inputs give the user the option to add additional discounts on the cost of BEVs or PHEVs. These are not included in the agency’s overall program costs and may represent discounts offered by vehicle dealers or manufacturers. They benefit the customer but are not costs incurred by the agency.")),  style = "font-size: 13px")),
                      box( width = 12, h4("Program Details"), p("These allow the user to add details about their program, including administrative costs and program length. Defaults are provided based on the pilot incentive program that Sonoma Clean Power ran in 2016. Inputs include:"), br(), tags$div(tags$ul(tags$li("Program length: The number of months that the incentive program will run, with the default set at 12."), tags$li("Number of staff required: The number of full-time employees needed to run the program."), tags$li("Administrative costs per person: The salary and administrative costs per full time employee working on the program."), tags$li("Additional implementation costs: Any additional costs to run the program that the user anticipates. Defaults have been set based on the costs to run Sonoma Clean Power’s pilot EV program."),tags$li("Percent Net Revenue: This input allows the user to set the portion of electricity sales that goes to revenues with a default set at 10%."), tags$li("Marketing effectiveness: This input represents a way to account for the role of marketing on influencing program effectiveness. The user may input the percentage of  eligible customers they expect will be aware of the program being offered. This percentage directly modifies the predicted number of rebates redeemed. Because this only modifies the number of people aware of available discounts, it does not take into account marketing that changes the likelihood of customers taking advantage of the discounts (i.e. marketing that is more or less persuasive).", footer = NULL, status = NULL,solidHeader = FALSE, background = NULL, height = NULL, collapsible = FALSE, collapsed = FALSE)),  style = "font-size: 13px")
                      ))), 
    tabItem(tabName ="tab_3",
            fluidPage(
              titlePanel("To get results, click Calculate button and wait"),
              sidebarLayout(  
                sidebarPanel(
                  selectInput(inputId="Agency", "Agency (region)",
                              choices = list("Apple Valley" = "Apple Valley", "San Francisco" = "San Francisco", "Lancaster" = "Lancaster", "MCE" ="MCE", "Peninsula"="Peninsula", "Redwood Coast"="Redwood Coast", "Silicon Valley"="Silicon Valley", "Sonoma"="Sonoma"), selected = "Sonoma")%>%
                    shinyInput_label_embed(
                      shiny_iconlink() %>%
                        bs_embed_tooltip(
                          title = "Which CCE Agency will be running the program. The model uses this information to set the correct population level and predict local emissions impacts.",placement = "right")),
                  numericInput(inputId ="Budget", "Total Incentive Budget ($)", 
                               value = 1500000)%>%
                    shinyInput_label_embed(
                      shiny_iconlink() %>%
                        bs_embed_tooltip(
                          title = "The total available budget for providing EV and PHEV incentives.",placement = "right")),
                  selectInput(inputId="Year","Year from 2016 to 2030",
                              choices = list(2016,2017,2018,2019,2020,2021,2022,2023, 2024,2025,2026,2027,2028,2029,2030), selected = 2017)%>%
                    shinyInput_label_embed(
                      shiny_iconlink() %>%
                        bs_embed_tooltip(
                          title = "The year that the incentives program will run.",placement = "right")),
                  numericInput(inputId ="EV_rebate","Electric Vehicle (BEV) Incentive",  value = 2000)%>%
                  shinyInput_label_embed(
                    shiny_iconlink() %>%
                      bs_embed_tooltip(
                        title = "The dollar amount that the agency will offer for each electric vehicle purchase.",placement = "right")),
                  numericInput(inputId ="PHEV_rebate", "Electric Vehicle (PHEV) Incentive",  value = 1000)%>%
                    shinyInput_label_embed(
                      shiny_iconlink() %>%
                        bs_embed_tooltip(
                          title = "The dollar amount that the agency will offer for each plug-in hybrid purchase.",placement = "right")),
                  numericInput(inputId ="Energymix1", "Energy Mix - Coal (%)", 
                               value = 0),
                  numericInput(inputId ="Energymix2", "Energy Mix - Natural Gas (%)", 
                               value = 0),
                  numericInput(inputId ="Energymix3","Energy Mix - Geothermal (%)", 
                               value = 8),
                  numericInput(inputId ="Energymix4","Energy Mix - Petroleum (%)", 
                               value = 0),
                  numericInput(inputId ="Energymix5","Energy Mix - Large Hydro (%)", 
                               value = 49),
                  numericInput(inputId ="Energymix6","Energy Mix - Biomass (%)", 
                               value = 0),
                  numericInput(inputId ="Energymix7", "Energy Mix - Biogas (%)", 
                               value = 0),
                  numericInput(inputId ="Energymix8", "Energy Mix - Eligible Renewable (%)", 
                               value = 33),
                  numericInput(inputId ="Energymix9","Energy Mix - Nuclear (%)", 
                               value = 0),
                  numericInput(inputId ="Energymix10", "Energy Mix - Other (%)", 
                               value = 10), 
                  selectInput(inputId ="Lux_BEV", "Include incentive for High-end BEV (e.g. Tesla)", choices = list("Yes"=1, "No"=2), selected = 2),
                  selectInput(inputId ="Lux_PHEV", "Include incentive for Luxury PHEV (e.g. Audi A3 e-tron)", choices = list("Yes"=1, "No"=2), selected = 2),
                  selectInput(inputId ="Fed", "Federal Tax Credit Availability", choices = list("Yes"=1, "No"=2), selected = 1),
                  selectInput(inputId ="CVRP", "Clean Vehicle Rebate Project (CVRP) Availability", choices = list("Yes"=1, "No"=2), selected = 1),
                  numericInput(inputId ="Discount_EV","Additional discount BEV (e.g. dealer discount)", 
                               value = 0),
                  numericInput(inputId ="Discount_PHEV", "Additional discount PHEV (e.g. dealer discount)",
                               value = 0), 
                  numericInput(inputId ="Length", "Program Length (month)", 
                               value = 4),
                  numericInput(inputId ="Staff", "Number of staff reqired", 
                               value = 5),
                  numericInput(inputId ="Admincost", "Administrative Cost ($/person/year)", 
                               value = 124000),
                  numericInput(inputId ="Impcost", "Additional Implementation Costs ($)", 
                               value = 80000),
                  sliderInput(inputId ="Profit", "Profit portion (%)",
                              min = 0, max = 100, value = 10),
                  sliderInput(inputId ="Marketing", "Marketing Effectiveness (%)",
                              min = 0, max = 100, value = 50),
                  numericInput(inputId ="Gas", "California Average Gasoline Price ($/gallon)", 
                               value = 2.78),
                  numericInput(inputId ="Elec", "California Average Electricity Rate ($/kwh)", 
                               value = 0.19),
                  numericInput(inputId ="Rebound", "Rebound Effect (%)", value = 3),
                  numericInput(inputId ="Trans", "Transmission Losses (%)", value = 5),
                  numericInput(inputId ="Discount", "Discount rate (%)", value = 5),
                  numericInput(inputId ="carbon_p", "Carbon Value (dollar per ton CO2e)", value = 13),    
                  selectInput(inputId="Impact", "Value of Health Impact Estimates", choices = list("Low","Mid","High"), selected = "High")), 
mainPanel(fluidRow(actionButton("go", "Calculate"),br(),br(),
        column(12, box(h4("The Estimated Number of Sales"), tableOutput("table1"), height = 150, width = 350)),
        column(12, box(plotOutput("plot1"), height = 420, width = 350)),
        column(12, box(h4("Total Benefits and Costs"),tableOutput("table2"), height = 370, width = 350)),
        column(12, box(plotOutput("plot2"), height = 420, width = 350)))
                )))
    ))))
server <- function(input, output) {
  
  TCM <- eventReactive(input$go, {
    Data <- read_csv("Database.csv")
    Market_Share_Simple <- read_csv("Market_Share_Simple.csv")
    Cost_data <- read_csv("Cost.csv") 
    Projection <- read_csv("Price_Projection.csv")
    N = 1000 # Number of simulation
    N_car_model = nrow(Data)# count number of car models
    Need_col = N_car_model+1
    ln_Mean_VMT <- 8.87192821723217 # Mean value of Ln(VTM). THis is because the VTM distribution is lognormal distribution 
    ln_SD_VMT <- 1.09899648130874 # Standard deviation of Ln(VTM) 
    VMT <- exp(rnorm(n=N,mean=ln_Mean_VMT, sd=ln_SD_VMT)) # Calulate VTM from normally distributed ln(VMT)
    VMT[VMT>150000]=150000 # VMT that is larger than 150,000 is 150,000
    # these are the values we use and subject to change
    
    discount = 0.2
    years_own = 7
    Gas_price <- input$Gas
    Elec_price <- input$Elec
    Phybrid_Gas_Percentage = 0.6
    Uncertainty = 0.3
    year <- input$Year
    EV_rebate  <- input$EV_rebate
    PHEV_rebate <- input$PHEV_rebate
    Discount_PHEV <- input$Discount_PHEV
    Discount_EV <- input$Discount_EV
    Agency <- input$Agency
    Length <- input$Length
    Fed <- input$Fed
    CVRP <- input$CVRP
    Marketing <- input$Marketing/100
    Budget <- input$Budget
    Lux_BEV_rebate <- input$Lux_BEV
    Lux_PHEV_rebate <- input$Lux_PHEV
    lease <- 0.4
    
    
    # change the total incentive depending on the availability.    
    
    Cost_data$incen <- rep(0, nrow=Cost_data)
    for (i in 72:83){
      Cost_data$incen[i] <- ifelse(Fed == 1, Cost_data$Incentive[i],0)
      Cost_data$incen[i] <- ifelse(CVRP == 1,Cost_data$Incentive[i]+1500,Cost_data$Incentive[i])}
    for (i in 84:95){
      Cost_data$incen[i] <- ifelse(Fed == 1, Cost_data$Incentive[i],0)
      Cost_data$incen[i] <- ifelse(CVRP == 1,Cost_data$Incentive[i]+2500,Cost_data$Incentive[i])}
    
    # Calculate the new PHEV and EV price based on year and subtract the incentive    
    
    Cost_data$Year_Pur_Cost[72:76]<-Cost_data$Base_Pur_Cost[72:76]*(1-Projection$PHEV[match(year,Projection$Year)])-Cost_data$incen[72:76]
    Cost_data$Year_Pur_Cost[77:83]<-Cost_data$Base_Pur_Cost[77:83]*(1-Projection$PHEV[match(year,Projection$Year)]*0.68)-Cost_data$incen[77:83]
    Cost_data$Year_Pur_Cost[84:91] <- Cost_data$Base_Pur_Cost[84:91]*(1-Projection$EV[match(year,Projection$Year)])-Cost_data$incen[84:91]
    Cost_data$Year_Pur_Cost[92:93] <- Cost_data$Base_Pur_Cost[92:93]*(1-Projection$EV[match(year,Projection$Year)]*0.681)-Cost_data$incen[92:93]
    Cost_data$Year_Pur_Cost[94:95] <- Cost_data$Base_Pur_Cost[94:95]*(1-Projection$EV[match(year,Projection$Year)]*0.96)-Cost_data$incen[94:95]
    
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
    N_PHy <- 2
    N_PHy_Lux <- 4
    N_EV <- 3
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
    Marketshare_Table[4,] <- Marketshare_Table[3,]/Marketshare_Table[1,]
    Marketshare_Table[5,] <- Marketshare_Table[4,]*0.853628632417563/sum(Marketshare_Table[4,])
    colnames(Marketshare_Table) <- colnames(Market_Share_Simple)
    rownames(Marketshare_Table) <- c("Propotion of sales from these models","Real Market Share","Counts from TCM","Recalculated Counts","Estimated Market Share")
    
    ###########################################################################
    
    
    PHEV_rebate_Lux <-ifelse(Lux_PHEV_rebate==1, PHEV_rebate, 0)
    Discount_PHEV_Lux<-ifelse(Lux_PHEV_rebate==1, Discount_PHEV, 0)
    EV_rebate_Lux<-ifelse(Lux_BEV_rebate==1, EV_rebate, 0)
    Discount_EV_Lux<-ifelse(Lux_BEV_rebate==1, Discount_EV, 0)
    
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
    
    Agency_Pop <- ifelse(Agency== "Apple Valley",72553,ifelse(Agency=="San Francisco",870887, ifelse(Agency=="Lancaster", 160106, ifelse(Agency=="MCE",1537944,ifelse(Agency=="Peninsula",764797,ifelse(Agency=="Redwood Coast",136646,ifelse(Agency=="Silicon Valley",1919402,ifelse(Agency=="Sonoma",590698,0))))))))
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
      Base_PHEV <- sum(Marketshare_Table[5,20:25])
      Predict_PHEV <- sum(Marketshare_Table2[5,20:25])
    } else {
      Base_PHEV <- sum(Marketshare_Table[5,20:21])
      Predict_PHEV <- sum(Marketshare_Table2[5,20:21])
    }
    
    
    Prob_demand_EV <- ifelse((PHEV_rebate==0)&(Discount_PHEV==0), Prob_demand_EV <- 1, Prob_demand_EV <- Predict_EV/(Predict_EV+Predict_PHEV))
    Prob_demand_PHEV <- 1-Prob_demand_EV
    
    max_total <- Budget/(EV_rebate*Prob_demand_EV+PHEV_rebate*Prob_demand_PHEV)
    max_EV <- Prob_demand_EV*max_total
    max_PHEV <- Prob_demand_PHEV*max_total
    
    Final_EV <- ifelse((Predict_EV*P_sales*(1+(lease)/(1-lease)))>max_EV, max_EV, Predict_EV*P_sales*(1+(lease)/(1-lease)))
    Final_PHEV <- ifelse((Predict_PHEV*P_sales)>max_PHEV, max_PHEV, Predict_PHEV*P_sales)
    
    # Present the estimated results in the table. 
    FinalTable <- matrix(c(Final_EV,Final_PHEV,ifelse((Predict_EV-Base_EV)/Predict_EV*Final_EV>0,(Predict_EV-Base_EV)/Predict_EV*Final_EV, 0),ifelse((Predict_PHEV-Base_PHEV)/Predict_PHEV*Final_PHEV>0,(Predict_PHEV-Base_PHEV)/Predict_PHEV*Final_PHEV,0)), nrow=2, ncol=2)
    colnames(FinalTable)<-c("Total Sales","Sales caused by incetive")
    rownames(FinalTable)<-c("EV","PHEV")
    print(FinalTable)
  })
  
  BC <- reactive({
    TCM <- TCM()
    Aveg_VTM <- 11244
    Lifetime <- 15
    agency <-input$Agency
    year <- input$Year
    E1 <-input$Energymix1/100
    E2 <- input$Energymix2/100
    E3 <- input$Energymix3/100
    E4 <- input$Energymix4/100
    E5 <- input$Energymix5/100
    E6 <- input$Energymix6/100
    E7 <- input$Energymix7/100
    E8 <- input$Energymix8/100
    E9 <- input$Energymix9/100
    E10 <- input$Energymix10/100
    Rebound <- input$Rebound/100
    Trans <- input$Trans/100
    discount <-input$Discount/100
    carbon_price <- input$carbon_p
    Impact <- input$Impact
    EV_rebate <- input$EV_rebate
    PHEV_rebate <- input$PHEV_rebate
    Length <- input$Length
    Staff <- input$Staff
    Admincost <- input$Admincost
    Elec_price <- input$Elec
    
    Aveg_VTM <- 11244
    Lifetime <- 15
    Efficiency <- 0.3
    PHEV_gas_perc <-0.6
    
    
    G_table <- read_csv("Emission_Gas.csv")
    E_table <-read_csv("Emission_Elec.csv")
    Health_impact <- read_csv("Health_impact.csv")
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
    GHG_benefits <- carbon_price*Total_disc_GHG
    
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
    
    
    H_impact <- ifelse(Impact=="Low",Disc_toal_tail[1]*Health_impact$PM2.5[1]+Disc_toal_tail[2]*Health_impact$PM10[1]+Disc_toal_tail[3]*Health_impact$Sox[1]+Disc_toal_tail[4]*Health_impact$Nox[1] ,ifelse(Impact=="Med",Disc_toal_tail[1]*Health_impact$PM2.5[2]+Disc_toal_tail[2]*Health_impact$PM10[2]+Disc_toal_tail[3]*Health_impact$Sox[2]+Disc_toal_tail[4]*Health_impact$Nox[2],Disc_toal_tail[1]*Health_impact$PM2.5[3]+Disc_toal_tail[2]*Health_impact$PM10[3]+Disc_toal_tail[3]*Health_impact$Sox[3]+Disc_toal_tail[4]*Health_impact$Nox[3]))
    
    Admin_cost <- Length*Staff*Admincost/12
    Imp_cost <- input$Impcost
    Total_rebates <- EV_rebate*TCM[1,2]+PHEV_rebate*ifelse(TCM[2,2]<=0, 0, TCM[2,2])
    Revenue <- Elec_price*(Aveg_VTM*Efficiency*TCM[1,2]*(1+Rebound)/(1+Trans)+Aveg_VTM*Efficiency*PHEV_gas_perc*TCM[2,2]*(1+Rebound)/(1+Trans))/discount*(1-1/(1+discount)^Lifetime)*input$Profit/100
    BCR <- (GHG_benefits+H_impact+Revenue)/(Admin_cost+Imp_cost+Total_rebates)
    Cost_GHG <- (Admin_cost+Imp_cost+Total_rebates)/Total_GHG
    Benefit <- matrix(c(GHG_benefits, H_impact, Revenue,Total_GHG, Cost_GHG, Admin_cost, Imp_cost, Total_rebates, BCR),nrow=5, ncol=2)
    colnames(Benefit)<- c("Benefits", "Costs")
    rownames(Benefit)<- c("a","b","c","d","e")
    
    return(Benefit)
  })   
  
  output$table1 <- renderTable({
    TCM <- TCM()
    FinalTable <- as.data.frame(TCM)  
  }, rownames = TRUE, colnames = TRUE, digits=0)
  
  output$plot1 <- renderPlot({
    TCM <- TCM()
    Finalsale <- as.data.frame(TCM)
    Finalsale[,3] <- TCM[,1]-TCM[,2]
    Final1 <-Finalsale[1:2,3]
    Final2 <-Finalsale[1:2,2]
    Final <- as.data.frame(cbind(Final1, Final2))
    colnames(Final) <-c("Remaining", "Caused by incentive")
    DF <- data.frame(Final)
    DF$Type <- c("EV","PHEV")
    DF1 <- melt(DF, id.var="Type")
    library(ggplot2)
    ggplot(DF1, aes(x = Type, y = value, fill = variable)) + 
      geom_bar(stat = "identity")+
      ggtitle("The Number of EV and PHEV Sales through EV Program")+
      ylab("Number of Sales")+
      xlab("")+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5), legend.title=element_blank(), text = element_text(size=15))
  })
  
  output$table2 <- renderTable({ 
    BC <- BC()
    Total_Value <- c(BC[4,1],BC[1,1],BC[2,1],BC[3,1],BC[1,2],BC[2,2],BC[3,2],BC[4,2],BC[5,1])
    Cost_Benefit <- as.data.frame(Total_Value, row.names = c("GHG Reduction (ton)","GHG reduction benefits (dollar)", "Health Benefits (dollar)","Revenue (dollar)","Administrative Cost (dollar)", "Implementation Cost (dollar)", "Total rebates costs (dollar)","Benefit Cost Ratio","Cost of GHG reduction (dollar/tonCO2e)"))
  },rownames = TRUE, colnames=TRUE, digits=2)
  
  output$plot2 <- renderPlot({
    BC <-BC()
    DF <- matrix(rep(0,12),nrow=2,ncol=6)
    DF[1,1:3] <- BC[1:3,1]
    DF[2,4:6] <- BC[1:3,2]
    colnames(DF) <-c("Benefit:GHG Reduction","Benefit:Health","Benefit:Revenue","Cost:Total Rebates","Cost:Implementation","Cost:Administration")
    DF <- data.frame(DF)
    DF$Type <- c("Benefits","Costs")
    DF1 <- melt(DF, id.var="Type")
    ggplot(DF1, aes(x = Type, y = value, fill = variable)) + 
      geom_bar(stat = "identity")+
      ggtitle("Overall Benefits and Costs")+
      ylab("Monetary Value (dollar)")+
      xlab("")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5), legend.position="bottom", legend.title=element_blank(), text = element_text(size=15))+guides(fill=guide_legend(nrow=2,byrow=TRUE))
    
    
    
    
    
  })
  
}

shinyApp(ui, server) 