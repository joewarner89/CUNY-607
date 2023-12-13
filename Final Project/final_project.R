# read the hospital data 
diabetic <- read.csv("diabetic_data.csv", stringsAsFactors = F)
data_info <- read.csv("IDS_mapping.csv", stringsAsFactors = F)


# Data maping 
library(tidyverse)
admin <- data_info %>% select(1,2) %>% na.omit()
disch <- data_info %>% select(3,4) %>% na.omit()
admin_sc <- data_info %>% select(5,6) %>% na.omit()
diabetic <- diabetic %>% inner_join(admin,by =  "admission_type_id") %>% 
  inner_join(disch,by = "discharge_disposition_id") %>% 
  inner_join(admin_sc,by = "admission_source_id") %>% 
  select(1:5,7,admission_type_name,discharge_disposition_id,discharge_disposition_name
        ,admission_source_id,admission_source_name,9:51)


ggplot(diabetic) + geom_bar(aes(x = admission_type_name))
diabetic %>%  group_by(admission_type_name) %>% summarise(count = n()) %>% group_by(admission_type_name) %>%
  
  ggplot( mapping = aes(x=admission_type_name, y=count, fill=count))+ 
  geom_bar(position = "dodge", stat="identity")+
  scale_fill_gradient(low = "grey20", high = "springgreen3")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+labs(height=10, width=5)+
  geom_text(aes(label = admission_type_name),color="white",hjust= 1.5, vjust = 0, size = 3, angle = 90, position = position_dodge(width = 1))

############################

admission_profile <- diabetic %>%  group_by(admission_type_name,admission_source_name) %>% summarise(count = n()) %>% group_by(admission_type_name) %>%
  arrange(desc(count))
head(admission_profile)  

str(diabetic)
summary(diabetic)

######################
library(skimr)
ts <- skim(diabetic)

data.frame(colums = ts$skim_variable,
           unique_value = ts$character.n_unique)
diabetic %>%  group_by(race) %>% summarise(count = n()) 
# Fixing label name 
diabetic$race[diabetic$race == "AfricanAmerican"] <- "African American"
# Replacing ? with NA 
diabetic[diabetic == '?'] <- NA
#count the missing value with mark"?" and "Unknown/Invalid"
sapply(diabetic, function(x) sum(is.na(x)))
sapply(diabetic, function(x) sum(is.null(x)))
diabetic[diabetic == "NULL" ] <- NA

#selecting na value
s <- na.omit(diabetic)

s %>%  group_by(admission_type_name) %>% summarise(count = n()) %>% group_by(admission_type_name) %>%
  
  ggplot( mapping = aes(x=admission_type_name, y=count, fill=count))+ 
  geom_bar(position = "dodge", stat="identity")+
  scale_fill_gradient(low = "grey20", high = "springgreen3")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+labs(height=10, width=5)+
  geom_text(aes(label = admission_type_name),color="white",hjust= 1.5, vjust = 0, size = 3, angle = 90, position = position_dodge(width = 1))


diabetic %>% select(citoglipton,examide,troglitazone,glimepiride.pioglitazone,
                    metformin.rosiglitazone,acetohexamide,tolbutamide) %>%
  distinct()

diabetic  %>% group_by(specialty = medical_specialty) %>%
  summarise(No_of_encounter = n()) %>%
  arrange(desc(No_of_encounter)) %>%
  top_n(15, wt = No_of_encounter) %>% 
  ggplot(aes(x = specialty, y = No_of_encounter, fill = No_of_encounter)) +
  geom_bar(position = "dodge", stat="identity")+
  scale_fill_gradient(low = "blue2", high = "springgreen3")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+labs(height=10, width=5)+
  coord_flip() + labs(title = "Number of Hospital encounter Types by Specialty", x = "Hospital Specialty", y = "no of Encounters")
  
diabetic$diag_1 <- str_replace_all(diabetic$diag_1, "[[:punct:]]", "")
diabetic$diag_2 <- str_replace_all(diabetic$diag_2, "[[:punct:]]", "")
diabetic$diag_3 <- str_replace_all(diabetic$diag_3, "[[:punct:]]", "")


icd <- read.delim("https://raw.githubusercontent.com/drobbins/ICD9/master/icd9.txt",stringsAsFactors = F) %>% rename(diag_1 = 1,
                                                                                                                     Short_desc = 3)

diabetic %>% group_by(encounter_id, diag_1,diag_2,diag_3) %>% summarise(No_of_encounter = n()) %>% arrange(desc(No_of_encounter)) %>%
  select(,2) %>% inner_join(icd,by="DIAGNOSIS.CODE")


diag1 <- diabetic %>% group_by(encounter_id, diag_1)  %>% arrange(diag_1) %>%
  inner_join(icd,by="diag_1")  %>% select(encounter_id,diag_1,Short_desc)
########### graph main diagnosis 

 stt <- diabetic   %>%
   inner_join(icd,by="diag_1")  %>% select(encounter_id,diag_1,Short_desc) %>% 
   group_by(Short_desc) %>%  summarize(No_of_encounter = n()) %>% 
   arrange(desc(No_of_encounter)) %>%
   top_n(15, wt = No_of_encounter) %>% head(20) %>% 
  ggplot(aes(x = Short_desc, y = No_of_encounter, fill = No_of_encounter)) +
  geom_bar(position = "dodge", stat="identity")+
  scale_fill_gradient(low = "blue2", high = "springgreen3")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+labs(height=10, width=5)+
  coord_flip() + labs(title = "Number of Hospital encounter Types by Specialty", x = "Hospital Specialty", y = "no of Encounters")


diag2 <- diabetic %>% group_by(encounter_id, diag_2)  %>%
  inner_join(icd,by="diag_1")  %>% select(encounter_id,diag_2,Short_desc)
# create a caetgorical 
diag1 %>% mutate(categories= if_else(diag_1, c("2511", "2512", "24901", "24930", "24931", "24941", "24951"," 24961", '24971', "24981", "24991", "25002", "25003", "25030", "25031",
                                               "25032", "25033", "25042", "25043,", "25052", "25053", "25062", "25063", "25072", "25073", "25082", "25083", "25092", "25093"),"Diabetes Poor Control"))
diag_cate <- c("2511", "2512", "24901", "24930", "24931", "24941", "24951"," 24961", '24971', "24981", "24991", "25002", "25003", "25030", "25031",
               "25032", "25033", "25042", "25043,", "25052", "25053", "25062", "25063", "25072", "25073", "25082", "25083", "25092", "25093")



hospD<- mutate(diabetic, primary_diagnosis =
                 ifelse(str_detect(diag_1, "V") | str_detect(diag_1, "E"),"Other", 
                        # disease codes starting with V or E are in ???other??? category;
                        ifelse(str_detect(diag_1, "250"), "Poor Diabetes",
                               ifelse((as.integer(diag_1) >= 390 & as.integer(diag_1) <= 459) | as.integer(diag_1) == 785, "Circulatory",
                                      ifelse((as.integer(diag_1) >= 460 & as.integer(diag_1) <= 519) | as.integer(diag_1) == 786, "Respiratory", 
                                             ifelse((as.integer(diag_1) >= 520 & as.integer(diag_1) <= 579) | as.integer(diag_1) == 787, "Digestive", 
                                                    ifelse((as.integer(diag_1) >= 580 & as.integer(diag_1) <= 629) | as.integer(diag_1) == 788, "Genitourinary",
                                                           ifelse((as.integer(diag_1) >= 140 & as.integer(diag_1) <= 239), "Neoplasms",  
                                                                  ifelse((as.integer(diag_1) >= 710 & as.integer(diag_1) <= 739), "Musculoskeletal",          
                                                                         ifelse((as.integer(diag_1) >= 800 & as.integer(diag_1) <= 999), "Injury",                    
                                                                                "Other"))))))))))


mutate(diabetic, primary_diagnosis =
         ifelse(str_detect(diag_1, "V") | str_detect(diag_1, "E"),"Other", 
                # disease codes starting with V or E are in ???other??? category;
                ifelse(str_detect(diag_1, "250"), "Diabetes",
                       ifelse((as.integer(diag_1) %in% diag_cate & as.integer(diag_1) <= 459) | as.integer(diag_1) == 785, "Circulatory",
                              ifelse((as.integer(diag_1) >= 460 & as.integer(diag_1) <= 519) | as.integer(diag_1) == 786, "Respiratory", 
                                     ifelse((as.integer(diag_1) >= 520 & as.integer(diag_1) <= 579) | as.integer(diag_1) == 787, "Digestive", 
                                            ifelse((as.integer(diag_1) >= 580 & as.integer(diag_1) <= 629) | as.integer(diag_1) == 788, "Genitourinary",
                                                   ifelse((as.integer(diag_1) >= 140 & as.integer(diag_1) <= 239), "Neoplasms",  
                                                          ifelse((as.integer(diag_1) >= 710 & as.integer(diag_1) <= 739), "Musculoskeletal",          
                                                                 ifelse((as.integer(diag_1) >= 800 & as.integer(diag_1) <= 999), "Injury",                    
                                                                        "Other"))))))))))


hospD %>%  group_by(primary_diagnosis) %>% summarise(count = n()) %>% group_by(primary_diagnosis) %>%
  
  ggplot( mapping = aes(x=primary_diagnosis, y=count, fill=count))+ 
  geom_bar(position = "dodge", stat="identity")+
  scale_fill_gradient(low = "grey20", high = "springgreen3")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+labs(height=10, width=5)+
  geom_text(aes(label = primary_diagnosis),color="white",hjust= 1.5, vjust = 0, size = 3, angle = 90, position = position_dodge(width = 1))
