#**READ IN THE DATASET**
  #598 obs of 37 variables
library(readxl)
EOH_EtD <- read_excel("file pathway")
View(EOH_EtD)

#**LOAD ALL PACKAGES**
library(tidyverse)
library(forcats)
library(MetBrewer)
library(epiR)
library(grid)
library(writexl)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(ggstatsplot)
library(data.table)
library(gtools)
library(plotly)
library(modeldb)

#**DATA MANAGEMENT**

#Creating a new variable that sums the number of codes applied to each item
EOH_EtD <- EOH_EtD %>%
  mutate(CodeSum=rowSums(across(c("Problem","Desirable effects",
                                  "Undesirable effects",
                                  "Certainty of evidence",
                                  "Values","Balance of effects",
                                  "Resources required",
                                  "Cost effectiveness","Equity",
                                  "Acceptability","Feasibility"))))

#Creating a new variable with all topic categories
  #One framework is coded to multiple topics; these items were 
  #already counted twice by summing columns in the descriptive stats
EOH_EtD <- EOH_EtD %>%
  mutate(Topic=factor(case_when(`Topic: Site remediation`==1 ~ 
                                  "Site Remediation",
                                `Topic: Water mgt`==1 ~ "Water Management",
                                `Topic: Waste / sanitation`==1 ~ "Sanitation",
                                `Topic: Workplace`==1 ~ "Workplace",
                                `Topic: Emissions`==1 ~ "Emissions",
                                `Topic: Chem Alt`==1 ~ "Chemical Alternatives",
                                `Topic: Pub Health`==1 ~ "Public Health",
                                `Topic: Emerg / Disaster`==1 ~ "Disaster",
                                `Topic: Unspecified`==1 ~ "Unspecified")))

#Creating a new variable with all dev org categories
  #No items coded to multiple dev orgs
EOH_EtD <- EOH_EtD %>%
  mutate(DevelopmentOrg=factor(case_when(`Dev org: Gov't`==1 ~ "Government",
                                         `Dev org: NGO`==1 ~ "NGO",
                                         `Dev org: Industry`==1 ~ "Industry",
                                         `Dev org: Academic`==1 ~ "Academic")))

#Creating a new variable with all unique item categories
  #Each item only counted as unique 1x
EOH_EtD <- EOH_EtD %>%
  mutate(UniqueItem=factor(case_when(`Guidance2`==1 ~ "Guidance",
                                     `Context`==1 ~ "Scope/Context",
                                     `Judgement`==1 ~ "Detailed Judgement")))

#NOTE: "Guidance2" is the category for items that we coded as guidance 
  #during our SECOND round of item review when we picked out the items 
  #that were unique within the criteria to which they had been coded.
#"Guidance1" is from the FIRST round of coding when we indicated items that
  #could *possibly* be guidance as we were applying the criterion codes; 
  #this indicator was carried into SECOND round of coding so I considered the
  #second round to be the definitive guidance code.

#**DESCRIPTIVE STATS**

#TABLE 3
  #Summary of GRADE EtD criterion codes and unique codes applied to 
  #discovered EOH decision factors from published/gray literature

Problem=sum(EOH_EtD$Problem,na.rm=TRUE)
DesEffect=sum(EOH_EtD$`Desirable effects`,na.rm=TRUE)
UndesEffect=sum(EOH_EtD$`Undesirable effects`,na.rm=TRUE)
COE=sum(EOH_EtD$`Certainty of evidence`,na.rm=TRUE)
Values=sum(EOH_EtD$Values,na.rm=TRUE)
BOE=sum(EOH_EtD$`Balance of effects`,na.rm=TRUE)
Cost=sum(EOH_EtD$`Cost effectiveness`,na.rm=TRUE)
Resources=sum(EOH_EtD$`Resources required`,na.rm=TRUE)
Equity=sum(EOH_EtD$Equity,na.rm=TRUE)
Acceptability=sum(EOH_EtD$Acceptability,na.rm=TRUE)
Feasibility=sum(EOH_EtD$Feasibility,na.rm=TRUE)

Criteria=matrix(c(Problem,DesEffect,UndesEffect,COE,Values,BOE,Resources,
                  Cost,Equity,Acceptability,Feasibility),nr=1,byrow=T)
Criteria
colnames(Criteria)=c("Problem","Desirable Effects","Undesirable Effects",
                     "Certainty of Evidence","Values","Balance of Effects",
                     "Resources Required","Cost Effectiveness","Equity",
                     "Acceptability","Feasibility")
rownames(Criteria)="Item Count"
Criteria
Criteria/560

#Counting unique items
Guidance=sum(EOH_EtD$Guidance2,na.rm=TRUE)
Scope=sum(EOH_EtD$Context,na.rm=TRUE)
DetailJudge=sum(EOH_EtD$Judgement,na.rm=TRUE)

Unique=matrix(c(Guidance,Scope,DetailJudge),nr=1,byrow=T)
Unique

#Subsetting unique
SubGuidance=EOH_EtD[which(EOH_EtD$Guidance2=="1"),]
SubScope=EOH_EtD[which(EOH_EtD$Context=="1"),]
SubDetail=EOH_EtD[which(EOH_EtD$Judgement=="1"),]

SubGuidance <- SubGuidance %>% 
  select(Problem:UniqueItem)
SubScope <- SubScope %>% 
  select(Problem:UniqueItem)
SubDetail <- SubDetail %>% 
  select(Problem:UniqueItem)

#NOTE: The sum of unique items is not going to be accurate with this code bc
  #some items have been coded as related to more than one criterion BUT only
  #contributed a *unique* consideration to one of those
#Must refer back to the original data set for manual counts of unique 
  #items within each criterion

#TABLE 2
  #Contingency table of discovered EOH decision factors coded to each 
  #GRADE EtD criterion by framework characteristics

table(EOH_EtD$DevelopmentOrg,EOH_EtD$Problem)
table(EOH_EtD$DevelopmentOrg,EOH_EtD$`Desirable effects`)
table(EOH_EtD$DevelopmentOrg,EOH_EtD$`Undesirable effects`)
table(EOH_EtD$DevelopmentOrg,EOH_EtD$`Certainty of evidence`)
table(EOH_EtD$DevelopmentOrg,EOH_EtD$Values)
table(EOH_EtD$DevelopmentOrg,EOH_EtD$`Balance of effects`)
table(EOH_EtD$DevelopmentOrg,EOH_EtD$`Cost effectiveness`)
table(EOH_EtD$DevelopmentOrg,EOH_EtD$`Resources required`)
table(EOH_EtD$DevelopmentOrg,EOH_EtD$Equity)
table(EOH_EtD$DevelopmentOrg,EOH_EtD$Acceptability)
table(EOH_EtD$DevelopmentOrg,EOH_EtD$Feasibility)

#PERCENT OF DEV ORG

#Subsetting by Dev Org
SubGovt=EOH_EtD[which(EOH_EtD$`Dev org: Gov't`=="1"),]
SubNGO=EOH_EtD[which(EOH_EtD$`Dev org: NGO`=="1"),]
SubIndustry=EOH_EtD[which(EOH_EtD$`Dev org: Industry`=="1"),]
SubAcademic=EOH_EtD[which(EOH_EtD$`Dev org: Academic`=="1"),]
Govt=sum(EOH_EtD$`Dev org: Gov't`,na.rm=TRUE)

GovProblem=sum(SubGovt$Problem,na.rm=TRUE)
GovDesEffect=sum(SubGovt$`Desirable effects`,na.rm=TRUE)
GovUndesEffect=sum(SubGovt$`Undesirable effects`,na.rm=TRUE)
GovCOE=sum(SubGovt$`Certainty of evidence`,na.rm=TRUE)
GovValues=sum(SubGovt$Values,na.rm=TRUE)
GovBOE=sum(SubGovt$`Balance of effects`,na.rm=TRUE)
GovCost=sum(SubGovt$`Cost effectiveness`,na.rm=TRUE)
GovResources=sum(SubGovt$`Resources required`,na.rm=TRUE)
GovEquity=sum(SubGovt$Equity,na.rm=TRUE)
GovAcceptability=sum(SubGovt$Acceptability,na.rm=TRUE)
GovFeasibility=sum(SubGovt$Feasibility,na.rm=TRUE)

GovCriteria=matrix(c(GovProblem,GovDesEffect,GovUndesEffect,GovCOE,
                     GovValues,GovBOE,GovCost,GovResources,GovEquity,
                     GovAcceptability,GovFeasibility),nr=1,byrow=T)
GovCriteria
colnames(GovCriteria)=c("Problem","Desirable Effects","Undesirable Effects",
                        "Certainty of Evidence","Values","Balance of Effects",
                        "Resources Required","Cost Effectiveness","Equity",
                        "Acceptability","Feasibility")
GovCriteria
SumGov=sum(GovProblem,GovDesEffect,GovUndesEffect,GovCOE,
           GovValues,GovBOE,GovCost,GovResources,GovEquity,
           GovAcceptability,GovFeasibility)
round((GovCriteria/460)*100,1)

NGO=sum(EOH_EtD$`Dev org: NGO`,na.rm=TRUE)

NGOProblem=sum(SubNGO$Problem,na.rm=TRUE)
NGODesEffect=sum(SubNGO$`Desirable effects`,na.rm=TRUE)
NGOUndesEffect=sum(SubNGO$`Undesirable effects`,na.rm=TRUE)
NGOCOE=sum(SubNGO$`Certainty of evidence`,na.rm=TRUE)
NGOValues=sum(SubNGO$Values,na.rm=TRUE)
NGOBOE=sum(SubNGO$`Balance of effects`,na.rm=TRUE)
NGOCost=sum(SubNGO$`Cost effectiveness`,na.rm=TRUE)
NGOResources=sum(SubNGO$`Resources required`,na.rm=TRUE)
NGOEquity=sum(SubNGO$Equity,na.rm=TRUE)
NGOAcceptability=sum(SubNGO$Acceptability,na.rm=TRUE)
NGOFeasibility=sum(SubNGO$Feasibility,na.rm=TRUE)

NGOCriteria=matrix(c(NGOProblem,NGODesEffect,NGOUndesEffect,NGOCOE,
                     NGOValues,NGOBOE,NGOCost,NGOResources,NGOEquity,
                     NGOAcceptability,NGOFeasibility),nr=1,byrow=T)
NGOCriteria
colnames(NGOCriteria)=c("Problem","Desirable Effects","Undesirable Effects",
                        "Certainty of Evidence","Values","Balance of Effects",
                        "Resources Required","Cost Effectiveness","Equity",
                        "Acceptability","Feasibility")
NGOCriteria
SumNGO=sum(NGOProblem,NGODesEffect,NGOUndesEffect,NGOCOE,
           NGOValues,NGOBOE,NGOCost,NGOResources,NGOEquity,
           NGOAcceptability,NGOFeasibility)
round((NGOCriteria/78)*100,1)

Industry=sum(EOH_EtD$`Dev org: Industry`,na.rm=TRUE)

IndustryProblem=sum(SubIndustry$Problem,na.rm=TRUE)
IndustryDesEffect=sum(SubIndustry$`Desirable effects`,na.rm=TRUE)
IndustryUndesEffect=sum(SubIndustry$`Undesirable effects`,na.rm=TRUE)
IndustryCOE=sum(SubIndustry$`Certainty of evidence`,na.rm=TRUE)
IndustryValues=sum(SubIndustry$Values,na.rm=TRUE)
IndustryBOE=sum(SubIndustry$`Balance of effects`,na.rm=TRUE)
IndustryCost=sum(SubIndustry$`Cost effectiveness`,na.rm=TRUE)
IndustryResources=sum(SubIndustry$`Resources required`,na.rm=TRUE)
IndustryEquity=sum(SubIndustry$Equity,na.rm=TRUE)
IndustryAcceptability=sum(SubIndustry$Acceptability,na.rm=TRUE)
IndustryFeasibility=sum(SubIndustry$Feasibility,na.rm=TRUE)

IndustryCriteria=matrix(c(IndustryProblem,IndustryDesEffect,
                          IndustryUndesEffect,IndustryCOE,
                          IndustryValues,IndustryBOE,IndustryCost,
                          IndustryResources,IndustryEquity,
                          IndustryAcceptability,IndustryFeasibility),
                        nr=1,byrow=T)
IndustryCriteria
colnames(IndustryCriteria)=c("Problem","Desirable Effects",
                             "Undesirable Effects",
                             "Certainty of Evidence","Values",
                             "Balance of Effects","Resources Required",
                             "Cost Effectiveness","Equity",
                             "Acceptability","Feasibility")
IndustryCriteria
SumIndustry=sum(IndustryProblem,IndustryDesEffect,
                IndustryUndesEffect,IndustryCOE,
                IndustryValues,IndustryBOE,IndustryCost,
                IndustryResources,IndustryEquity,
                IndustryAcceptability,IndustryFeasibility)
round((IndustryCriteria/70)*100,1)

Academic=sum(EOH_EtD$`Dev org: Academic`,na.rm=TRUE)

AcProblem=sum(SubAcademic$Problem,na.rm=TRUE)
AcDesEffect=sum(SubAcademic$`Desirable effects`,na.rm=TRUE)
AcUndesEffect=sum(SubAcademic$`Undesirable effects`,na.rm=TRUE)
AcCOE=sum(SubAcademic$`Certainty of evidence`,na.rm=TRUE)
AcValues=sum(SubAcademic$Values,na.rm=TRUE)
AcBOE=sum(SubAcademic$`Balance of effects`,na.rm=TRUE)
AcCost=sum(SubAcademic$`Cost effectiveness`,na.rm=TRUE)
AcResources=sum(SubAcademic$`Resources required`,na.rm=TRUE)
AcEquity=sum(SubAcademic$Equity,na.rm=TRUE)
AcAcceptability=sum(SubAcademic$Acceptability,na.rm=TRUE)
AcFeasibility=sum(SubAcademic$Feasibility,na.rm=TRUE)

AcademicCriteria=matrix(c(AcProblem,AcDesEffect,
                          AcUndesEffect,AcCOE,
                          AcValues,AcBOE,AcCost,
                          AcResources,AcEquity,
                          AcAcceptability,AcFeasibility),
                        nr=1,byrow=T)
AcademicCriteria
colnames(AcademicCriteria)=c("Problem","Desirable Effects",
                             "Undesirable Effects",
                             "Certainty of Evidence","Values",
                             "Balance of Effects","Resources Required",
                             "Cost Effectiveness","Equity",
                             "Acceptability","Feasibility")
AcademicCriteria
SumAcademic=sum(AcProblem,AcDesEffect,
                AcUndesEffect,AcCOE,
                AcValues,AcBOE,AcCost,
                AcResources,AcEquity,
                AcAcceptability,AcFeasibility)
round((AcademicCriteria/421)*100,1)

#PERCENT OF TOPIC

#Sum of Topics
SiteRemediation=sum(EOH_EtD$`Topic: Site remediation`)
WaterMgt=sum(EOH_EtD$`Topic: Water mgt`)
Sanitation=sum(EOH_EtD$`Topic: Waste / sanitation`)
Workplace=sum(EOH_EtD$`Topic: Workplace`)
Emissions=sum(EOH_EtD$`Topic: Emissions`)
ChemicalAlternatives=sum(EOH_EtD$`Topic: Chem Alt`)
PublicHealth=sum(EOH_EtD$`Topic: Pub Health`)
EmergencyDisaster=sum(EOH_EtD$`Topic: Emerg / Disaster`)
Unspecified=sum(EOH_EtD$`Topic: Unspecified`)

#Subsetting by Topic
SubSite=EOH_EtD[which(EOH_EtD$`Topic: Site remediation`=="1"),]
SubWater=EOH_EtD[which(EOH_EtD$`Topic: Water mgt`=="1"),]
SubSanitation=EOH_EtD[which(EOH_EtD$`Topic: Waste / sanitation`=="1"),]
SubWork=EOH_EtD[which(EOH_EtD$`Topic: Workplace`=="1"),]
SubEmissions=EOH_EtD[which(EOH_EtD$`Topic: Emissions`=="1"),]
SubChem=EOH_EtD[which(EOH_EtD$`Topic: Chem Alt`=="1"),]
SubPH=EOH_EtD[which(EOH_EtD$`Topic: Pub Health`=="1"),]
SubEmerg=EOH_EtD[which(EOH_EtD$`Topic: Emerg / Disaster`=="1"),]
SubUnspecified=EOH_EtD[which(EOH_EtD$`Topic: Unspecified`=="1"),]

WorkProblem=sum(SubWork$Problem,na.rm=TRUE)
WorkDesEffect=sum(SubWork$`Desirable effects`,na.rm=TRUE)
WorkUndesEffect=sum(SubWork$`Undesirable effects`,na.rm=TRUE)
WorkCOE=sum(SubWork$`Certainty of evidence`,na.rm=TRUE)
WorkValues=sum(SubWork$Values,na.rm=TRUE)
WorkBOE=sum(SubWork$`Balance of effects`,na.rm=TRUE)
WorkCost=sum(SubWork$`Cost effectiveness`,na.rm=TRUE)
WorkResources=sum(SubWork$`Resources required`,na.rm=TRUE)
WorkEquity=sum(SubWork$Equity,na.rm=TRUE)
WorkAcceptability=sum(SubWork$Acceptability,na.rm=TRUE)
WorkFeasibility=sum(SubWork$Feasibility,na.rm=TRUE)

WorkCriteria=matrix(c(WorkProblem,WorkDesEffect,
                      WorkUndesEffect,WorkCOE,
                      WorkValues,WorkBOE,WorkCost,
                      WorkResources,WorkEquity,
                      WorkAcceptability,WorkFeasibility),
                    nr=1,byrow=T)
WorkCriteria
colnames(WorkCriteria)=c("Problem","Desirable Effects","Undesirable Effects",
                         "Certainty of Evidence","Values",
                         "Balance of Effects","Resources Required",
                         "Cost Effectiveness","Equity",
                         "Acceptability","Feasibility")
WorkCriteria
SumWork=sum(WorkProblem,WorkDesEffect,
            WorkUndesEffect,WorkCOE,
            WorkValues,WorkBOE,WorkCost,
            WorkResources,WorkEquity,
            WorkAcceptability,WorkFeasibility)
round((WorkCriteria/24)*100,1)

PHProblem=sum(SubPH$Problem,na.rm=TRUE)
PHDesEffect=sum(SubPH$`Desirable effects`,na.rm=TRUE)
PHUndesEffect=sum(SubPH$`Undesirable effects`,na.rm=TRUE)
PHCOE=sum(SubPH$`Certainty of evidence`,na.rm=TRUE)
PHValues=sum(SubPH$Values,na.rm=TRUE)
PHBOE=sum(SubPH$`Balance of effects`,na.rm=TRUE)
PHCost=sum(SubPH$`Cost effectiveness`,na.rm=TRUE)
PHResources=sum(SubPH$`Resources required`,na.rm=TRUE)
PHEquity=sum(SubPH$Equity,na.rm=TRUE)
PHAcceptability=sum(SubPH$Acceptability,na.rm=TRUE)
PHFeasibility=sum(SubPH$Feasibility,na.rm=TRUE)

PHCriteria=matrix(c(PHProblem,PHDesEffect,
                    PHUndesEffect,PHCOE,
                    PHValues,PHBOE,PHCost,
                    PHResources,PHEquity,
                    PHAcceptability,PHFeasibility),
                  nr=1,byrow=T)
PHCriteria
colnames(PHCriteria)=c("Problem","Desirable Effects","Undesirable Effects",
                       "Certainty of Evidence","Values","Balance of Effects",
                       "Resources Required","Cost Effectiveness","Equity",
                       "Acceptability","Feasibility")
PHCriteria
SumPH=sum(PHProblem,PHDesEffect,
          PHUndesEffect,PHCOE,
          PHValues,PHBOE,PHCost,
          PHResources,PHEquity,
          PHAcceptability,PHFeasibility)
round((PHCriteria/191)*100,1)

table(EOH_EtD$Topic,EOH_EtD$Problem)
table(EOH_EtD$Topic,EOH_EtD$`Desirable effects`)
table(EOH_EtD$Topic,EOH_EtD$`Undesirable effects`)
table(EOH_EtD$Topic,EOH_EtD$`Certainty of evidence`)
table(EOH_EtD$Topic,EOH_EtD$Values)
table(EOH_EtD$Topic,EOH_EtD$`Balance of effects`)
table(EOH_EtD$Topic,EOH_EtD$`Cost effectiveness`)
table(EOH_EtD$Topic,EOH_EtD$`Resources required`)
table(EOH_EtD$Topic,EOH_EtD$Equity)
table(EOH_EtD$Topic,EOH_EtD$Acceptability)
table(EOH_EtD$Topic,EOH_EtD$Feasibility)

#**ANALYSIS**

#Chi Square vs. Fisher's Exact
#NULL HYPOTHESIS: even distribution items regardless of topic/dev org
#Counts for certain cells are 0 (<5); this means data do not meet 
  #assumptions for chi square
#Non-parametric test alternative: fishers exact test

#Testing by Development Org
chisq.test(EOH_EtD$DevelopmentOrg,EOH_EtD$Problem)
fisher.test(EOH_EtD$DevelopmentOrg,EOH_EtD$Problem)

chisq.test(EOH_EtD$DevelopmentOrg,EOH_EtD$`Desirable effects`)
fisher.test(EOH_EtD$DevelopmentOrg,EOH_EtD$`Desirable effects`)

chisq.test(EOH_EtD$DevelopmentOrg,EOH_EtD$`Undesirable effects`)
fisher.test(EOH_EtD$DevelopmentOrg,EOH_EtD$`Undesirable effects`)

chisq.test(EOH_EtD$DevelopmentOrg,EOH_EtD$`Certainty of evidence`)
fisher.test(EOH_EtD$DevelopmentOrg,EOH_EtD$`Certainty of evidence`)

chisq.test(EOH_EtD$DevelopmentOrg,EOH_EtD$Values)
fisher.test(EOH_EtD$DevelopmentOrg,EOH_EtD$Values)

chisq.test(EOH_EtD$DevelopmentOrg,EOH_EtD$`Balance of effects`)
fisher.test(EOH_EtD$DevelopmentOrg,EOH_EtD$`Balance of effects`)

chisq.test(EOH_EtD$DevelopmentOrg,EOH_EtD$`Cost effectiveness`)
fisher.test(EOH_EtD$DevelopmentOrg,EOH_EtD$`Cost effectiveness`)

chisq.test(EOH_EtD$DevelopmentOrg,EOH_EtD$`Resources required`)
fisher.test(EOH_EtD$DevelopmentOrg,EOH_EtD$`Resources required`)

chisq.test(EOH_EtD$DevelopmentOrg,EOH_EtD$Equity)
fisher.test(EOH_EtD$DevelopmentOrg,EOH_EtD$Equity)

chisq.test(EOH_EtD$DevelopmentOrg,EOH_EtD$Acceptability)
fisher.test(EOH_EtD$DevelopmentOrg,EOH_EtD$Acceptability)

chisq.test(EOH_EtD$DevelopmentOrg,EOH_EtD$Feasibility)
fisher.test(EOH_EtD$DevelopmentOrg,EOH_EtD$Feasibility)

#Testing by Topic
chisq.test(EOH_EtD$Topic,EOH_EtD$Problem)
fisher.test(EOH_EtD$Topic,EOH_EtD$Problem)

chisq.test(EOH_EtD$Topic,EOH_EtD$`Desirable effects`)
fisher.test(EOH_EtD$Topic,EOH_EtD$`Desirable effects`)

chisq.test(EOH_EtD$Topic,EOH_EtD$`Undesirable effects`)
fisher.test(EOH_EtD$Topic,EOH_EtD$`Undesirable effects`)

chisq.test(EOH_EtD$Topic,EOH_EtD$`Certainty of evidence`)
fisher.test(EOH_EtD$Topic,EOH_EtD$`Certainty of evidence`)

chisq.test(EOH_EtD$Topic,EOH_EtD$Values)
fisher.test(EOH_EtD$Topic,EOH_EtD$Values)

chisq.test(EOH_EtD$Topic,EOH_EtD$`Balance of effects`)
fisher.test(EOH_EtD$Topic,EOH_EtD$`Balance of effects`)

chisq.test(EOH_EtD$Topic,EOH_EtD$`Cost effectiveness`)
fisher.test(EOH_EtD$Topic,EOH_EtD$`Cost effectiveness`)

chisq.test(EOH_EtD$Topic,EOH_EtD$`Resources required`)
fisher.test(EOH_EtD$Topic,EOH_EtD$`Resources required`)

chisq.test(EOH_EtD$Topic,EOH_EtD$Equity)
fisher.test(EOH_EtD$Topic,EOH_EtD$Equity)

chisq.test(EOH_EtD$Topic,EOH_EtD$Acceptability)
fisher.test(EOH_EtD$Topic,EOH_EtD$Acceptability)

chisq.test(EOH_EtD$Topic,EOH_EtD$Feasibility)
fisher.test(EOH_EtD$Topic,EOH_EtD$Feasibility,simulate.p.value=TRUE)

#**TRANSFORM DATA FOR VIZ**

EOH_EtD_Frame <- EOH_EtD %>% 
  select(Reference, Unique:Feasibility) %>%
  pivot_longer(!Reference, names_to = "Criteria",
               values_to = "Count") %>%
  filter(Count==1)

EOH_EtD_DevOrg <- EOH_EtD %>% 
  select(DevelopmentOrg, Unique:Feasibility) %>%
  pivot_longer(!DevelopmentOrg, names_to = "Criteria",
               values_to = "Count") %>%
  filter(Count==1)

EOH_EtD_Topic <- EOH_EtD %>% 
  select(Topic, Unique:Feasibility) %>%
  pivot_longer(!Topic, names_to = "Criteria",
               values_to = "Count") %>%
  filter(Count==1)

CriteriaSum <- data.frame(name=c("Unique","No code","Problem",
                                 "Desirable effects",
                                 "Undesirable effects",
                                 "Certainty of evidence",
                                 "Values","Balance of effects",
                                 "Resources required",
                                 "Cost effectiveness",
                                 "Equity",
                                 "Acceptability","Feasibility") ,  
                          value=c(90,23,156,104,206,44,54,
                                  41,84,32,32,102,174))

#**VISUALIZATIONS**

#HISTOGRAM

#Count of sum of number of codes applied to each item
EOH_EtD %>% ggplot(aes(CodeSum,fill = ..x..)) +
  geom_histogram() +
  scale_fill_gradientn(colors = met.brewer("Pissaro"),
                       name = "No. of Codes Applied") +
  theme_bw() + 
  scale_x_continuous(breaks = seq(0, 5, 1)) +
  stat_bin(binwidth=1) + ylim(c(0,250)) +
  stat_bin(binwidth=1, geom="text", aes(label=..count..),vjust=-1) +
  xlab("Codes Applied Per Item") +
  ylab("Item Count")

#BAR CHARTS

#Count of Items Overall
#Changing the order of criteria (bars)
CriteriaSum1 <- CriteriaSum
CriteriaSum1$name <- factor(CriteriaSum1$name,
                            levels = c("Feasibility","Acceptability","Equity",
                                       "Cost effectiveness",
                                       "Resources required",
                                       "Balance of effects","Values",
                                       "Certainty of evidence",
                                       "Undesirable effects",
                                       "Desirable effects","Problem",
                                       "Unique","No code"))
#Plotting the item counts
ggplot(CriteriaSum1, aes(x=name, y=value, fill=name)) + 
  geom_bar(stat = "identity",alpha=0.8) +
  coord_flip() +
  scale_fill_manual(values=met.brewer("Signac"),name = "Codes Applied") +
  xlab("GRADE EtD Criteria") +
  ylab("Item Count")

#Count of Items per Framework
#Changing the order of criteria (bars)
EOH_EtD_Frame1 <- EOH_EtD_Frame
EOH_EtD_Frame1$Criteria <- factor(EOH_EtD_Frame1$Criteria,
                                  levels = c("No code","Unique","Problem",
                                             "Desirable effects",
                                             "Undesirable effects",
                                             "Certainty of evidence","Values",
                                             "Balance of effects",
                                             "Resources required",
                                             "Cost effectiveness","Equity",
                                             "Acceptability","Feasibility"))

#Plotting item counts
ggplot(EOH_EtD_Frame1, 
       aes(y=Reference, 
           fill = Criteria)) + 
  geom_bar(stat="Count",alpha=0.75) +
  scale_fill_manual(values=met.brewer("Signac", direction=-1)
                    ,name = "Codes Applied") +
  scale_color_manual(values=met.brewer("Signac", direction=-1)
                     ,name = "Codes Applied") +
  scale_x_continuous(name ="Item Count", 
                     limits=c(0,150)) +
  ylab("") +
  ggtitle("Count of Items Per Framework")

#Proportion of Items Per Framework
ggplot(EOH_EtD_Frame1, 
       aes(y=Reference, 
           fill = Criteria)) + 
  geom_bar(position = "fill",alpha=0.75) +
  scale_fill_manual(values=met.brewer("Signac", direction=-1)
                    ,name = "Codes Applied") +
  scale_color_manual(values=met.brewer("Signac", direction=-1)
                     ,name = "Codes Applied") +
  xlab("Item Proportion") +
  ylab("") +
  ggtitle("Proportion of Items Per Framework")

#Count of Items Per Dev Org
#Changing the order of criteria (bars)
EOH_EtD_DevOrg1 <- EOH_EtD_DevOrg
EOH_EtD_DevOrg1$Criteria <- factor(EOH_EtD_DevOrg1$Criteria,
                                   levels = c("Feasibility",
                                              "Acceptability","Equity",
                                              "Cost effectiveness",
                                              "Resources required",
                                              "Balance of effects","Values",
                                              "Certainty of evidence",
                                              "Undesirable effects",
                                              "Desirable effects","Problem",
                                              "Unique"))

#Plotting the item counts
ggplot(na.omit(EOH_EtD_DevOrg1), 
       aes(y=Criteria, 
           fill = DevelopmentOrg)) + 
  geom_bar(stat="Count",alpha=0.8) +
  scale_fill_manual(values=met.brewer("Java", direction=1)) +
  scale_color_manual(values=met.brewer("Java", direction=1)) +
  xlab("Item Count") +
  ylab("") +
  ggtitle("Count of Items Per Criterion by Development Organization Type")

#Proportion of Items Per Dev Org
ggplot(na.omit(EOH_EtD_DevOrg1), 
       aes(y=Criteria, 
           fill = DevelopmentOrg)) + 
  geom_bar(position = "fill",alpha=0.8) +
  scale_fill_manual(values=met.brewer("Java", direction=1)) +
  scale_color_manual(values=met.brewer("Java", direction=1)) +
  xlab("Item Proportion") +
  ylab("") +
  ggtitle("Proportion of Items Per Criterion by Development Organization")

#Count of Items Per Topic
#Changing the order of criteria (bars)
EOH_EtD_Topic1 <- EOH_EtD_Topic
EOH_EtD_Topic1$Criteria <- factor(EOH_EtD_Topic1$Criteria,
                                  levels = c("Feasibility",
                                             "Acceptability","Equity",
                                             "Cost effectiveness",
                                             "Resources required",
                                             "Balance of effects","Values",
                                             "Certainty of evidence",
                                             "Undesirable effects",
                                             "Desirable effects","Problem",
                                             "Unique"))
#Plotting the item counts
ggplot(na.omit(EOH_EtD_Topic1), 
       aes(y=Criteria, 
           fill = Topic)) + 
  geom_bar(stat="Count",alpha=0.8) +
  scale_fill_manual(values=met.brewer("Cross", 9)) +
  scale_color_manual(values=met.brewer("Cross", 9)) +
  xlab("Item Count") +
  ylab("") +
  ggtitle("Count of Items Per Criterion by Framework Topic")

#Proportion of Items Per Topic
ggplot(na.omit(EOH_EtD_Topic1), 
       aes(y=Criteria, 
           fill = Topic)) + 
  geom_bar(position = "fill",alpha=0.8) +
  scale_fill_manual(values=met.brewer("Cross", 9)) +
  scale_color_manual(values=met.brewer("Cross", 9)) +
  xlab("Item Proportion") +
  ylab("") +
  ggtitle("Proportion of Items Per Criterion by Framework Topic")

#CRAMER'S V

#Cramer's V Development Org
DevOrgCorDF <- EOH_EtD_DevOrg1 %>% 
  add_dummy_variables(DevelopmentOrg, values = 
                        c("ref", levels(EOH_EtD_DevOrg1$DevelopmentOrg))) %>% 
  add_dummy_variables(Criteria, values = 
                        c("ref", levels(EOH_EtD_DevOrg1$Criteria))) %>% 
  select(-Count)

cat_varDevOrg <- colnames(DevOrgCorDF)

cv.test = function(x,y) {
  CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
              (length(x)[1] * (min(length(unique(x))[1],
                                   length(unique(y))[1]) - 1)))
  return(as.numeric(CV))}
v_cramer_all <- function(cat_var, df){cat_var_grid <- 
  data.table(combinations(n = length(cat_var), 
                          r = 2, v = cat_var, repeats.allowed = FALSE))
do.call(rbind,apply(cat_var_grid, 1, function(x){
  tmp <- as.character(x)
  vec1 <- unlist(df[,tmp[1], with = FALSE])
  vec2 <- unlist(df[,tmp[2], with = FALSE])
  data.table(
    variable_x = tmp[1],
    variable_y = tmp[2],
    chi2 = chisq.test(x = vec1, vec2, correct=FALSE)$p.value,
    v_cramer = cv.test(x = vec1, y = vec2))}))}

resultsDevOrg <- v_cramer_all(cat_var = cat_varDevOrg, df = DevOrgCorDF)
resultsDevOrg_new <- subset(resultsDevOrg, !(variable_x %in% 
                                               c("DevelopmentOrg_Industry",
                                                 "DevelopmentOrg_Government",
                                                 "DevelopmentOrg_Academic")))
resultsDevOrg_new <- subset(resultsDevOrg_new, 
                            !(variable_y %in% 
                                c("Criteria_Balance of effects",
                                  "Criteria_Certainty of evidence",
                                  "Criteria_Cost effectiveness",
                                  "Criteria_Desirable effects",
                                  "Criteria_Equity",
                                  "Criteria_Feasibility",
                                  "Criteria_Problem",
                                  "Criteria_Resources required",
                                  "Criteria_Undesirable effects",
                                  "Criteria_Unique",
                                  "Criteria_Values")))

gDO <- ggplot(resultsDevOrg_new, aes(variable_x, variable_y)) +
  geom_tile(aes(fill = v_cramer), colour = "black") +
  geom_text(aes(label = round(v_cramer, digits=2)), 
            color = "black", size = 2) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_fill_gradientn(colors=met.brewer
                       (name="OKeeffe2", 
                         direction=1, 
                         override.order = F)) +
  theme_bw() + xlab(NULL) + ylab(NULL) + 
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  ggtitle("Cramer's V heatmap: criteria vs. development organization") +
  theme(plot.title = element_text(size = 11))

ggplotly(gDO)

#Export results
write_xlsx(resultsDevOrg_new,"file location")

#Cramer's V Topic
TopicCorDF <- EOH_EtD_Topic1 %>% 
  add_dummy_variables(Topic, values = 
                        c("ref", levels(EOH_EtD_Topic1$Topic))) %>% 
  add_dummy_variables(Criteria, values = 
                        c("ref", levels(EOH_EtD_Topic1$Criteria))) %>% 
  select(-Count)

cat_varTopic <- colnames(TopicCorDF)

cv.test = function(x,y) {
  CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
              (length(x)[1] * (min(length(unique(x))[1],
                                   length(unique(y))[1]) - 1)))
  return(as.numeric(CV))}
v_cramer_all <- function(cat_var, df){cat_var_grid <- 
  data.table(combinations(n = length(cat_var), 
                          r = 2, v = cat_var, repeats.allowed = FALSE))
do.call(rbind,apply(cat_var_grid, 1, function(x){
  tmp <- as.character(x)
  vec1 <- unlist(df[,tmp[1], with = FALSE])
  vec2 <- unlist(df[,tmp[2], with = FALSE])
  data.table(
    variable_x = tmp[1],
    variable_y = tmp[2],
    chi2 = chisq.test(x = vec1, vec2, correct=FALSE)$p.value,
    v_cramer = cv.test(x = vec1, y = vec2))}))}

resultsTopic <- v_cramer_all(cat_var = cat_varTopic, df = TopicCorDF)
resultsTopic_new <- subset(resultsTopic, !(variable_x %in% 
                                             c("Topic_Chemical Alternatives",
                                               "Topic_Disaster",
                                               "Topic_Emissions",
                                               "Topic_Public Health",
                                               "Topic_Sanitation",
                                               "Topic_Site Remediation",
                                               "Topic_Unspecified",
                                               "Topic_Water Management")))
resultsTopic_new <- subset(resultsTopic_new, !(variable_y %in% 
                                                 c("Criteria_Balance of 
                                                   effects",
                                                   "Criteria_Certainty of
                                                   evidence",
                                                   "Criteria_Cost 
                                                   effectiveness",
                                                   "Criteria_Desirable 
                                                   effects",
                                                   "Criteria_Equity",
                                                   "Criteria_Feasibility",
                                                   "Criteria_Problem",
                                                   "Criteria_Resources 
                                                   required",
                                                   "Criteria_Undesirable 
                                                   effects",
                                                   "Criteria_Unique",
                                                   "Criteria_Values")))

gTop <- ggplot(resultsTopic_new, aes(variable_x, variable_y)) +
  geom_tile(aes(fill = v_cramer), colour = "black") +
  geom_text(aes(label = round(v_cramer, digits=2)), 
            color = "black", size = 2) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_fill_gradientn(colors=met.brewer
                       (name="OKeeffe2", 
                         direction=1, 
                         override.order = F)) +
  theme_bw() + xlab(NULL) + ylab(NULL) + 
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  ggtitle("Cramer's V heatmap: criteria vs. topic") +
  theme(plot.title = element_text(size = 11))

ggplotly(gTop)

#Export results
write_xlsx(resultsTopic_new,"file location")
