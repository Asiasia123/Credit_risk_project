#----Analysis: credit risk---- 


#----Credit risk by PD range with EAD, PD, LGD > 0 (89% of observations)----
CR <- MD_all %>%
  mutate(PD_range = cut(MD_all$PD, breaks = c(0, 0.0015, 0.0025, 0.0050, 0.0075 , 0.0250 , 0.100 , 0.9999, 1.01),
                                             labels = c("0.00 to <0.15", "0.15 to < 0.25", "0.25 to < 0.50",
                                                     "0.50 to < 0.75", "0.75 to < 2.50", "2.50 to < 10.00",
                                                     "10.00 to < 100","100(Default)" ))) %>%
  filter(EAD_counted>0)%>%
  filter(PD_calib>0)%>%
  filter(LGD_counted>0) %>%
  select(PERIOD, CUSTOMER_ID, IRB, CustType,  PD_calib, LGD_counted, ORIGINAL_EXPOSURE, 
         EAD_counted, EL_counted, RWA, Risk_Weight, PD_range)

#----Period 1, Total IRB exposures, all Customers----
CR_1_IRB <- CR %>%
  filter(PERIOD==1)%>%
  #filter(IRB==3) %>%
  #filter(CustType %in% c(5, 6))%>%
  group_by (PD_range) %>%
  summarise(Original_Exposure = round(sum(ORIGINAL_EXPOSURE)/1),
            EAD = round(sum(EAD_counted)/1),
            Weighted_PD = round((weighted.mean(PD_calib, w = EAD_counted)*100), 2),
            Sum_of_Obligors = length(unique(CUSTOMER_ID)),                
            Weighted_LGD = round((weighted.mean(LGD_counted, w = EAD_counted)*100), 2),
            RWA = round(sum(RWA)/1),
            Risk_Weight_perc = round(mean((Risk_Weight)*100),2),
            EL = round(sum(EL_counted)/1))

#----Period 1, Total IRB exposures, Rated Customers----
CR_1_IRB_RC <- CR %>%
  filter(PERIOD==1)%>%
  #filter(IRB==3) %>%
  filter(CustType %in% c(5, 6)) %>%
  group_by (PD_range) %>%
  summarise(Original_Exposure = round(sum(ORIGINAL_EXPOSURE)/1),
            EAD = round(sum(EAD_counted)/1),
            Weighted_PD = round((weighted.mean(PD_calib, w = EAD_counted)*100), 2),
            Sum_of_Obligors = length(unique(CUSTOMER_ID)),                
            Weighted_LGD = round((weighted.mean(LGD_counted, w = EAD_counted)*100), 2),
            RWA = round(sum(RWA)/1),
            Risk_Weight_perc = round(mean((Risk_Weight)*100),2),
            EL = round(sum(EL_counted)/1)) 

#----Period 1, Total IRB exposures, Unrated Customers----
CR_1_IRB_URC <- CR %>%
  filter(PERIOD==1)%>%
  #filter(IRB==3) %>%
  filter(CustType %in% c(7)) %>%
  group_by (PD_range) %>%
  summarise(Original_Exposure = round(sum(ORIGINAL_EXPOSURE)/1),
            EAD = round(sum(EAD_counted)/1),
            Weighted_PD = round((weighted.mean(PD_calib, w = EAD_counted)*100), 2),
            Sum_of_Obligors = length(unique(CUSTOMER_ID)),                
            Weighted_LGD = round((weighted.mean(LGD_counted, w = EAD_counted)*100), 2),
            RWA = round(sum(RWA)/1),
            Risk_Weight_perc = round(mean((Risk_Weight)*100),2),
            EL = round(sum(EL_counted)/1))

#----Period 1, AIRB exposures----
CR_1_AIRB <- CR %>%
  filter(PERIOD==1)%>%
  filter(IRB==3) %>%
  #filter(CustType %in% c(5, 6)) %>%
  group_by (PD_range) %>%
  summarise(Original_Exposure = round(sum(ORIGINAL_EXPOSURE)/1),
            EAD = round(sum(EAD_counted)/1),
            Weighted_PD = round((weighted.mean(PD_calib, w = EAD_counted)*100), 2),
            Sum_of_Obligors = length(unique(CUSTOMER_ID)),                
            Weighted_LGD = round((weighted.mean(LGD_counted, w = EAD_counted)*100), 2),
            RWA = round(sum(RWA)/1),
            Risk_Weight_perc = round(mean((Risk_Weight)*100),2),
            EL = round(sum(EL_counted)/1))

#----Period 1, FIRB exposures----
CR_1_FIRB <- CR %>%
  filter(PERIOD==1)%>%
  filter(IRB==4) %>%
  #filter(CustType %in% c(5, 6)) %>%
  group_by (PD_range) %>%
  summarise(Original_Exposure = round(sum(ORIGINAL_EXPOSURE)/1),
            EAD = round(sum(EAD_counted)/1),
            Weighted_PD = round((weighted.mean(PD_calib, w = EAD_counted)*100), 2),
            Sum_of_Obligors = length(unique(CUSTOMER_ID)),                
            Weighted_LGD = round((weighted.mean(LGD_counted, w = EAD_counted)*100), 2),
            RWA = round(sum(RWA)/1),
            Risk_Weight_perc = round(mean((Risk_Weight)*100),2),
            EL = round(sum(EL_counted)/1))

#----Period 2, Total IRB exposures, all Customers----
CR_2_IRB <- CR %>%
  filter(PERIOD==2)%>%
  #filter(IRB==3) %>%
  #filter(CustType %in% c(5, 6))%>%
  group_by (PD_range) %>%
  summarise(Original_Exposure = round(sum(ORIGINAL_EXPOSURE)/1),
            EAD = round(sum(EAD_counted)/1),
            Weighted_PD = round((weighted.mean(PD_calib, w = EAD_counted)*100), 2),
            Sum_of_Obligors = length(unique(CUSTOMER_ID)),                
            Weighted_LGD = round((weighted.mean(LGD_counted, w = EAD_counted)*100), 2),
            RWA = round(sum(RWA)/1),
            Risk_Weight_perc = round(mean((Risk_Weight)*100),2),
            EL = round(sum(EL_counted)/1))

#----Period 2, Total IRB exposures, Rated Customers----
CR_2_IRB_RC <- CR %>%
  filter(PERIOD==2)%>%
  #filter(IRB==3) %>%
  filter(CustType %in% c(5, 6)) %>%
  group_by (PD_range) %>%
  summarise(Original_Exposure = round(sum(ORIGINAL_EXPOSURE)/1),
            EAD = round(sum(EAD_counted)/1),
            Weighted_PD = round((weighted.mean(PD_calib, w = EAD_counted)*100), 2),
            Sum_of_Obligors = length(unique(CUSTOMER_ID)),                
            Weighted_LGD = round((weighted.mean(LGD_counted, w = EAD_counted)*100), 2),
            RWA = round(sum(RWA)/1),
            Risk_Weight_perc = round(mean((Risk_Weight)*100),2),
            EL = round(sum(EL_counted)/1))

#----Period 2, Total IRB exposures, Unrated Customers----
CR_2_IRB_URC <- CR %>%
  filter(PERIOD==2)%>%
  #filter(IRB==3) %>%
  filter(CustType %in% c(7)) %>%
  group_by (PD_range) %>%
  summarise(Original_Exposure = round(sum(ORIGINAL_EXPOSURE)/1),
            EAD = round(sum(EAD_counted)/1),
            Weighted_PD = round((weighted.mean(PD_calib, w = EAD_counted)*100), 2),
            Sum_of_Obligors = length(unique(CUSTOMER_ID)),                
            Weighted_LGD = round((weighted.mean(LGD_counted, w = EAD_counted)*100), 2),
            RWA = round(sum(RWA)/1),
            Risk_Weight_perc = round(mean((Risk_Weight)*100),2),
            EL = round(sum(EL_counted)/1))

#----Period 2, AIRB exposures----
CR_2_AIRB <- CR %>%
  filter(PERIOD==2)%>%
  filter(IRB==3) %>%
  #filter(CustType %in% c(5, 6)) %>%
  group_by (PD_range) %>%
  summarise(Original_Exposure = round(sum(ORIGINAL_EXPOSURE)/1),
            EAD = round(sum(EAD_counted)/1),
            Weighted_PD = round((weighted.mean(PD_calib, w = EAD_counted)*100), 2),
            Sum_of_Obligors = length(unique(CUSTOMER_ID)),                
            Weighted_LGD = round((weighted.mean(LGD_counted, w = EAD_counted)*100), 2),
            RWA = round(sum(RWA)/1),
            Risk_Weight_perc = round(mean((Risk_Weight)*100),2),
            EL = round(sum(EL_counted)/1))

#----Period 2, FIRB exposures----
CR_2_FIRB <- CR %>%
  filter(PERIOD==2)%>%
  filter(IRB==4) %>%
  #filter(CustType %in% c(5, 6)) %>%
  group_by (PD_range) %>%
  summarise(Original_Exposure = round(sum(ORIGINAL_EXPOSURE)/1),
            EAD = round(sum(EAD_counted)/1),
            Weighted_PD = round((weighted.mean(PD_calib, w = EAD_counted)*100), 2),
            Sum_of_Obligors = length(unique(CUSTOMER_ID)),                
            Weighted_LGD = round((weighted.mean(LGD_counted, w = EAD_counted)*100), 2),
            RWA = round(sum(RWA)/1),
            Risk_Weight_perc = round(mean((Risk_Weight)*100),2),
            EL = round(sum(EL_counted)/1))

