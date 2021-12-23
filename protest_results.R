library(cjoint)
library(ggplot2)
library(readxl)
library(survey)
protest_data_incomplete <- read_excel("C:/Users/theod/Desktop/dissertation/experiment/experiment_data_results/pori_data.xlsx")

protest_data_incomplete<-subset(protest_data_incomplete, party!=1)
protest_data_incomplete<-subset(protest_data_incomplete, rating!=9)
protest_data_incomplete$selected[which(protest_data_incomplete$selected==9)]<-0
protest_data_incomplete$age<-2021-protest_data_incomplete$yr_birth
protest_data_1<-protest_data_incomplete[complete.cases(protest_data_incomplete$age),]
protest_data_2<-protest_data_1[complete.cases(protest_data_1$education),]
protest_data_2$gender[which(protest_data_2$gender==9)]<-NA
protest_data<-protest_data_2[complete.cases(protest_data_2$gender),]

protest_data$theme<-factor(protest_data$theme,levels = c("medical","tax","Putonghua","firewall"), labels = c("Improve healthcare services","Oppose the Goods and Services Tax","Oppose the Mandatory Putonghua Policy","Oppose the Great Firewall"))
protest_data$media<-factor(protest_data$media,levels = c("no media","broadcast"), labels = c("No international media present","Associated Press and Reuters will broadcast"))
protest_data$turnout<-factor(protest_data$turnout,levels = c("10000","1e+05","5e+05"), labels = c("10,000","100,000","500,000"))
protest_data$govt<-factor(protest_data$govt,levels = c("refuse","grant"), labels = c("Letter of no objection refused","Letter of no objection granted"))
names(protest_data)[names(protest_data)=="theme"]<-"Theme"
names(protest_data)[names(protest_data)=="media"]<-"Media attention"
names(protest_data)[names(protest_data)=="turnout"]<-"Expected turnout"
names(protest_data)[names(protest_data)=="govt"]<-"Police response"

######################
# Demographics plots #
######################
## Age ##
age <- data.frame(data=c("Sample","Sample","Sample","Sample","Sample","Population","Population","Population","Population","Population"),percent=c(0.154,0.279,0.229,0.184,0.154,0.157,0.162,0.169,0.193,0.294),age=c("18-29","30-39","40-49","50-59","60 or above","18-29","30-39","40-49","50-59","60 or above"))
age$data <- factor(age$data,levels = c("Sample","Population"))
age$age <- factor(age$age,levels = c("60 or above","50-59","40-49","30-39","18-29"))
ggplot(age, aes(x=data, y=percent, fill=age)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(x=NULL, y="Percentage") +
  theme(text = element_text(size=25),
        legend.text = element_text(size=20),
        legend.title = element_blank())

## Educational attainment ##
educ <- data.frame(data=c("Sample","Sample","Sample","Population","Population","Population"),percent=c(0.0908,0.245,0.664,0.336,0.402,0.262),edu=c("Lower secondary or below","Upper secondary/ tertiary(non-degree)","Tertiary(degree) or above","Lower secondary or below","Upper secondary/ tertiary(non-degree)","Tertiary(degree) or above"))
educ$data <- factor(educ$data,levels = c("Sample","Population"))
educ$edu <- factor(educ$edu,levels = c("Tertiary(degree) or above","Upper secondary/ tertiary(non-degree)","Lower secondary or below"))
ggplot(educ, aes(x=data, y=percent, fill=edu)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(x=NULL, y="Percentage") +
  theme(text = element_text(size=25),
        legend.text = element_text(size=20),
        legend.title = element_blank())

## Gender ##
sex <- data.frame(data=c("Sample","Sample","Population","Population"),percent=c(0.584,0.416,0.471,0.529),gender=c("Male","Female","Male","Female"))
sex$data <- factor(sex$data,levels = c("Sample","Population"))
sex$gender <- factor(sex$gender,levels = c("Female","Male"))
ggplot(sex, aes(x=data, y=percent, fill=gender)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(x=NULL, y="Percentage") +
  theme(text = element_text(size=25),
        legend.text = element_text(size=20),
        legend.title = element_blank())

## Political inclination ##
partyid <- data.frame(data=c("Sample","Sample","Sample","Sample","Population","Population","Population","Population"),percent=c(0.373,0.356,0.222,0.049,0.243,0.112,0.255,0.39),party=c("Democrat","Localist","Centrist","Others","Democrat","Localist","Centrist","Others"))
partyid$data <- factor(partyid$data,levels = c("Sample","Population"))
partyid$party <- factor(partyid$party,levels = c("Others","Centrist","Localist","Democrat"))
ggplot(partyid, aes(x=data, y=percent, fill=party)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(x=NULL, y="Percentage") +
  theme(text = element_text(size=25),
        legend.text = element_text(size=20),
        legend.title = element_blank())

######################
# Pooled Analysis #
######################
r.1<-amce(selected~`Theme`+`Media attention`+`Expected turnout`+`Police response`, data=protest_data, cluster=T, respondent.id = "respondent")
summary(r.1)
plot(r.1, xlab="Change in Pr(Participate in protests)", group.order = c("Theme","Media attention","Expected turnout","Police response"), main="Pooled Analysis for protest attributes", text.size = 15, point.size = 0.8, dodge.size = 2)

r.2<-amce(rating~`Theme`+`Media attention`+`Expected turnout`+`Police response`, data=protest_data, cluster=T, respondent.id = "respondent")
summary(r.2)
plot(r.2, xlab="Change in rating of protests", group.order = c("Theme","Media attention","Expected turnout","Police response"), main="Pooled Analysis for protest attributes", text.size = 13)


######################
# Post-materialism ###
######################
protest_data$postmat_1[which(protest_data$postmat_1==1 | protest_data$postmat_1==3 | protest_data$postmat_1==9)]<-0
protest_data$postmat_1[which(protest_data$postmat_1==2 | protest_data$postmat_1==4)]<-1
protest_data$postmat_2[which(protest_data$postmat_2==1 | protest_data$postmat_2==3 | protest_data$postmat_2==9)]<-0
protest_data$postmat_2[which(protest_data$postmat_2==2 | protest_data$postmat_2==4)]<-1
protest_data$postmat_score<-protest_data$postmat_1+protest_data$postmat_2

postmaterialist<-subset(protest_data, subset=postmat_score==2)
semimaterialist<-subset(protest_data, subset=postmat_score==1)
materialist<-subset(protest_data, subset=postmat_score==0)
r_postmaterialist<-amce(selected~`Theme`+`Media attention`+`Expected turnout`+`Police response`, data=postmaterialist, cluster=T, respondent.id = "respondent")
summary(r_postmaterialist)
plot(r_postmaterialist, xlab="Change in Pr(Participate in protests)", group.order = c("Theme","Media attention","Expected turnout","Police response"), main="Postmaterialists", text.size = 13)

r_semimaterial<-amce(selected~`Theme`+`Media attention`+`Expected turnout`+`Police response`, data=semimaterialist, cluster=T, respondent.id = "respondent")
summary(r_semimaterial)
plot(r_semimaterial, xlab="Change in Pr(Participate in protests)", group.order = c("Theme","Media attention","Expected turnout","Police response"), main="Mixed values individuals", text.size = 13)

r_material<-amce(selected~`Theme`+`Media attention`+`Expected turnout`+`Police response`, data=materialist, cluster=T, respondent.id = "respondent")
summary(r_material)
plot(r_material, xlab="Change in Pr(Participate in protests)", group.order = c("Theme","Media attention","Expected turnout","Police response"), main="Materialists", text.size = 13)

## Average Component Interaction Effect by post-materialism score ##
protest_data$postmat_score<-factor(protest_data$postmat_score, levels = c("0","1","2"))
names(protest_data)[names(protest_data)=="postmat_score"]<-"Post-materialism score"
results1 <- amce(selected ~ `Theme`*`Post-materialism score`+`Media attention`*`Post-materialism score`+`Expected turnout`*`Post-materialism score`+`Police response`*`Post-materialism score`, data=protest_data, 
                cluster=F, na.ignore=T, respondent.id = "respondent",respondent.varying = "Post-materialism score")
summary(results1)
plot(results1, plot.display="interaction", xlab="Change in Pr(Participate in protests)", group.order = c("Theme","Media attention","Expected turnout","Police response"), text.size = 15, point.size = 0.8, dodge.size = 2, theme=ggplot::theme_bw())

####################################################
# Subset by confidence in future personal freedom ##
####################################################
low_conf_free<-subset(protest_data, subset=conf_free<=2)
high_conf_free<-subset(protest_data, subset=conf_free>=3 & conf_free<=5)
r_lowconf<-amce(selected~`Theme`+`Media attention`+`Expected turnout`+`Police response`, data=low_conf_free, cluster=T, respondent.id = "respondent")
summary(r_lowconf)
plot(r_lowconf, xlab="Change in Pr(Participate in protests)", group.order = c("Theme","Media attention","Expected turnout","Police response"), main="Low confidence in personal freedom", text.size = 13)
r_highconf<-amce(selected~`Theme`+`Media attention`+`Expected turnout`+`Police response`, data=high_conf_free, cluster=T, respondent.id = "respondent")
summary(r_highconf)
plot(r_highconf, xlab="Change in Pr(Participate in protests)", group.order = c("Theme","Media attention","Expected turnout","Police response"), main="High confidence in personal freedom", text.size = 13)

## Average Component Interaction Effect by confidence in personal freedom ##
protest_data$conf_free[which(protest_data$conf_free==1 | protest_data$conf_free==2)]<-0
protest_data$conf_free[which(protest_data$conf_free>=3 & protest_data$conf_free<=5)]<-1
protest_data$conf_free[which(protest_data$conf_free==9)]<-NA
protest_data$conf_free<-factor(protest_data$conf_free, levels = c("0","1"))
names(protest_data)[names(protest_data)=="conf_free"]<-"Confidence in personal freedom"
results2 <- amce(selected ~ `Theme`*`Confidence in personal freedom`+`Media attention`*`Confidence in personal freedom`+`Expected turnout`*`Confidence in personal freedom`+`Police response`*`Confidence in personal freedom`, data=protest_data, 
                cluster=F, na.ignore=T, respondent.id = "respondent",respondent.varying = "Confidence in personal freedom")
summary(results2)
plot(results2, plot.display="interaction", xlab="Change in Pr(Participate in protests)", group.order = c("Theme","Media attention","Expected turnout","Police response"), text.size = 15, point.size = 0.8, dodge.size = 2, theme=ggplot::theme_bw())

##############################################
# By dissatisfaction with economic condition #
##############################################
low_econ_sat<-subset(protest_data, subset=econ_satisfied<=4)
high_econ_sat<-subset(protest_data, subset=econ_satisfied>=5 & econ_satisfied<=10)
r_loweconsat<-amce(selected~`Theme`+`Media attention`+`Expected turnout`+`Police response`, data=low_econ_sat, cluster=T, respondent.id = "respondent")
summary(r_loweconsat)
plot(r_loweconsat, xlab="Change in Pr(Participate in protests)", group.order = c("Theme","Media attention","Expected turnout","Police response"), main="High dissatisfaction with financial situation", text.size = 13)
r_higheconsat<-amce(selected~`Theme`+`Media attention`+`Expected turnout`+`Police response`, data=high_econ_sat, cluster=T, respondent.id = "respondent")
summary(r_higheconsat)
plot(r_higheconsat, xlab="Change in Pr(Participate in protests)", group.order = c("Theme","Media attention","Expected turnout","Police response"), main="Low dissatisfaction with financial situation", text.size = 13)

## Average Component Interaction Effect by dissatisfaction with economic condition ##
protest_data$econ_satisfied[which(protest_data$econ_satisfied<=4)]<-1
protest_data$econ_satisfied[which(protest_data$econ_satisfied>=5 & protest_data$econ_satisfied<=10)]<-0
protest_data$econ_satisfied[which(protest_data$econ_satisfied==99)]<-NA
protest_data$econ_satisfied<-factor(protest_data$econ_satisfied, levels = c("0","1"))
names(protest_data)[names(protest_data)=="econ_satisfied"]<-"Dissatisfaction with financial situation"
results3 <- amce(selected ~ `Theme`*`Dissatisfaction with financial situation`+`Media attention`*`Dissatisfaction with financial situation`+`Expected turnout`*`Dissatisfaction with financial situation`+`Police response`*`Dissatisfaction with financial situation`, data=protest_data, 
                 cluster=F, na.ignore=T, respondent.id = "respondent",respondent.varying = "Dissatisfaction with financial situation")
summary(results3)
plot(results3, plot.display="interaction", xlab="Change in Pr(Participate in protests)", group.order = c("Theme","Media attention","Expected turnout","Police response"), text.size = 15, point.size = 0.8, dodge.size = 2, theme=ggplot::theme_bw())

######################
# By partisanship #
######################
dem<-subset(protest_data, subset=party==2)
r_dem<-amce(selected~`Theme`+`Media attention`+`Expected turnout`+`Police response`, data=dem, cluster=T, respondent.id = "respondent")
summary(r_dem)
plot(r_dem, xlab="Change in Pr(Participate in protests)", group.order = c("Theme","Media attention","Expected turnout","Police response"), main="Democrats", text.size = 15, point.size = 0.8, dodge.size = 2)
r_dem_rating<-amce(rating~`Theme`+`Media attention`+`Expected turnout`+`Police response`, data=dem, cluster=T, respondent.id = "respondent")
summary(r_dem_rating)
plot(r_dem_rating, xlab="Change in rating of protests (Democrats)", group.order = c("Theme","Media attention","Expected turnout","Police response"), main="Rating for Democrats", text.size = 13)

localist<-subset(protest_data, subset=party==3)
r_localist<-amce(selected~`Theme`+`Media attention`+`Expected turnout`+`Police response`, data=localist, cluster=T, respondent.id = "respondent")
summary(r_localist)
plot(r_localist, xlab="Change in Pr(Participate in protests)", group.order = c("Theme","Media attention","Expected turnout","Police response"), main="Localists", text.size = 15, point.size = 0.8, dodge.size = 2)
r_localist_rating<-amce(rating~`Theme`+`Media attention`+`Expected turnout`+`Police response`, data=localist, cluster=T, respondent.id = "respondent")
summary(r_localist_rating)
plot(r_localist_rating, xlab="Change in rating of protests (Localists)", group.order = c("Theme","Media attention","Expected turnout","Police response"), main="Rating for Localists", text.size = 13)

centrist<-subset(protest_data, subset=party==4)
r_centrist<-amce(selected~`Theme`+`Media attention`+`Expected turnout`+`Police response`, data=centrist, cluster=T, respondent.id = "respondent")
summary(r_centrist)
plot(r_centrist, xlab="Change in Pr(Participate in protests)", group.order = c("Theme","Media attention","Expected turnout","Police response"), main="Centrists", text.size = 15, point.size = 0.8, dodge.size = 2)
r_centrist_rating<-amce(rating~`Theme`+`Media attention`+`Expected turnout`+`Police response`, data=centrist, cluster=T, respondent.id = "respondent")
summary(r_centrist_rating)
plot(r_centrist_rating, xlab="Change in rating of protests (Centrists)", group.order = c("Theme","Media attention","Expected turnout","Police response"), main="Rating for Centrists", text.size = 13)

## Average Component Interaction Effect by partisanship ##
protest_data$party[which(protest_data$party==2)]<-"Democrats"
protest_data$party[which(protest_data$party==3)]<-"Localists"
protest_data$party[which(protest_data$party==4)]<-"Non-partisans"
protest_data$party<-factor(protest_data$party, levels = c("Democrats","Localists","Non-partisans"))
names(protest_data)[names(protest_data)=="party"]<-"Partisanship"
results4 <- amce(selected ~ `Theme`*`Partisanship`+`Media attention`*`Partisanship`+`Expected turnout`*`Partisanship`+`Police response`*`Partisanship`, data=protest_data, 
                 cluster=F, na.ignore=T, respondent.id = "respondent",respondent.varying = "Partisanship")
summary(results4)
plot(results4, plot.display="interaction", xlab="Change in Pr(Participate in protests)", group.order = c("Theme","Media attention","Expected turnout","Police response"), text.size = 14, point.size = 0.8, dodge.size = 2, theme=ggplot::theme_bw())

#######################################################################################
###### Divide the data into different subgroups based on age, education, and gender ###
#######################################################################################
protest_data$age_group<-NA
protest_data$age_group[which(protest_data$age>=18 & protest_data$age<=29)]<-1
protest_data$age_group[which(protest_data$age>=30 & protest_data$age<=39)]<-2
protest_data$age_group[which(protest_data$age>=40 & protest_data$age<=49)]<-3
protest_data$age_group[which(protest_data$age>=50 & protest_data$age<=59)]<-4
protest_data$age_group[which(protest_data$age>=60)]<-5
protest_data$edu_group<-NA
protest_data$edu_group[which(protest_data$education%in%c(1,2,3))]<-1
protest_data$edu_group[which(protest_data$education%in%c(4,5))]<-2
protest_data$edu_group[which(protest_data$education%in%c(6,7))]<-3
protest_data$type<-NA
protest_data$type[which(protest_data$age_group==1 & protest_data$edu_group==1 & protest_data$gender==1)]<-1
protest_data$type[which(protest_data$age_group==1 & protest_data$edu_group==2 & protest_data$gender==1)]<-2
protest_data$type[which(protest_data$age_group==1 & protest_data$edu_group==3 & protest_data$gender==1)]<-3
protest_data$type[which(protest_data$age_group==1 & protest_data$edu_group==1 & protest_data$gender==2)]<-4
protest_data$type[which(protest_data$age_group==1 & protest_data$edu_group==2 & protest_data$gender==2)]<-5
protest_data$type[which(protest_data$age_group==1 & protest_data$edu_group==3 & protest_data$gender==2)]<-6
protest_data$type[which(protest_data$age_group==2 & protest_data$edu_group==1 & protest_data$gender==1)]<-7
protest_data$type[which(protest_data$age_group==2 & protest_data$edu_group==2 & protest_data$gender==1)]<-8
protest_data$type[which(protest_data$age_group==2 & protest_data$edu_group==3 & protest_data$gender==1)]<-9
protest_data$type[which(protest_data$age_group==2 & protest_data$edu_group==1 & protest_data$gender==2)]<-10
protest_data$type[which(protest_data$age_group==2 & protest_data$edu_group==2 & protest_data$gender==2)]<-11
protest_data$type[which(protest_data$age_group==2 & protest_data$edu_group==3 & protest_data$gender==2)]<-12
protest_data$type[which(protest_data$age_group==3 & protest_data$edu_group==1 & protest_data$gender==1)]<-13
protest_data$type[which(protest_data$age_group==3 & protest_data$edu_group==2 & protest_data$gender==1)]<-14
protest_data$type[which(protest_data$age_group==3 & protest_data$edu_group==3 & protest_data$gender==1)]<-15
protest_data$type[which(protest_data$age_group==3 & protest_data$edu_group==1 & protest_data$gender==2)]<-16
protest_data$type[which(protest_data$age_group==3 & protest_data$edu_group==2 & protest_data$gender==2)]<-17
protest_data$type[which(protest_data$age_group==3 & protest_data$edu_group==3 & protest_data$gender==2)]<-18
protest_data$type[which(protest_data$age_group==4 & protest_data$edu_group==1 & protest_data$gender==1)]<-19
protest_data$type[which(protest_data$age_group==4 & protest_data$edu_group==2 & protest_data$gender==1)]<-20
protest_data$type[which(protest_data$age_group==4 & protest_data$edu_group==3 & protest_data$gender==1)]<-21
protest_data$type[which(protest_data$age_group==4 & protest_data$edu_group==1 & protest_data$gender==2)]<-22
protest_data$type[which(protest_data$age_group==4 & protest_data$edu_group==2 & protest_data$gender==2)]<-23
protest_data$type[which(protest_data$age_group==4 & protest_data$edu_group==3 & protest_data$gender==2)]<-24
protest_data$type[which(protest_data$age_group==5 & protest_data$edu_group==1 & protest_data$gender==1)]<-25
protest_data$type[which(protest_data$age_group==5 & protest_data$edu_group==2 & protest_data$gender==1)]<-26
protest_data$type[which(protest_data$age_group==5 & protest_data$edu_group==3 & protest_data$gender==1)]<-27
protest_data$type[which(protest_data$age_group==5 & protest_data$edu_group==1 & protest_data$gender==2)]<-28
protest_data$type[which(protest_data$age_group==5 & protest_data$edu_group==2 & protest_data$gender==2)]<-29
protest_data$type[which(protest_data$age_group==5 & protest_data$edu_group==3 & protest_data$gender==2)]<-30

####################################################
# Post-stratificaion by age, education, and gender #
####################################################
ps.weights <- data.frame(
  type=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30),
  Freq=c(29.8,230.6,231.6,17.7,197.7,267.6,47.3,210.4,204.9,62.3,245.9,237.1,99.6,213.1,152.7,155.8,271.8,160.2,187.4,256.4,113.3,249.4,300.4,91.1,529.4,249.8,93.7,694.5,211.5,47.7)
)
ps.weights$pop.pct <- ps.weights$Freq/sum(ps.weights$Freq)*100
ps.weights$svy.pct <- prop.table(table(protest_data$type))*100
ps.weights$svyweight <- ps.weights$pop.pct/ps.weights$svy.pct

protest_data$wt <- NA
protest_data$wt[which(protest_data$type==1)]<-1.678
protest_data$wt[which(protest_data$type==2)]<-2.165
protest_data$wt[which(protest_data$type==3)]<-0.498
protest_data$wt[which(protest_data$type==4)]<-2.991
protest_data$wt[which(protest_data$type==5)]<-1.965
protest_data$wt[which(protest_data$type==6)]<-1.130
protest_data$wt[which(protest_data$type==7)]<-2.664
protest_data$wt[which(protest_data$type==8)]<-1.277
protest_data$wt[which(protest_data$type==9)]<-0.238
protest_data$wt[which(protest_data$type==10)]<-10.526
protest_data$wt[which(protest_data$type==11)]<-2.027
protest_data$wt[which(protest_data$type==12)]<-0.453
protest_data$wt[which(protest_data$type==13)]<-1.870
protest_data$wt[which(protest_data$type==14)]<-1.161
protest_data$wt[which(protest_data$type==15)]<-0.261
protest_data$wt[which(protest_data$type==16)]<-2.393
protest_data$wt[which(protest_data$type==17)]<-1.940
protest_data$wt[which(protest_data$type==18)]<-0.444
protest_data$wt[which(protest_data$type==19)]<-1.881
protest_data$wt[which(protest_data$type==20)]<-1.733
protest_data$wt[which(protest_data$type==21)]<-0.320
protest_data$wt[which(protest_data$type==22)]<-3.046
protest_data$wt[which(protest_data$type==23)]<-1.611
protest_data$wt[which(protest_data$type==24)]<-0.371
protest_data$wt[which(protest_data$type==25)]<-5.709
protest_data$wt[which(protest_data$type==26)]<-1.472
protest_data$wt[which(protest_data$type==27)]<-0.418
protest_data$wt[which(protest_data$type==28)]<-6.286
protest_data$wt[which(protest_data$type==29)]<-1.284
protest_data$wt[which(protest_data$type==30)]<-0.281

#########################################
###### AMCE after post-stratification ###
#########################################
r.1_post<-amce(selected~`Theme`+`Media attention`+`Expected turnout`+`Police response`, data=protest_data, cluster=T, respondent.id = "respondent", weights = "wt")
summary(r.1_post)
plot(r.1_post, xlab="Change in Pr(Participate in protests)", group.order = c("Theme","Media attention","Expected turnout","Police response"), main="Pooled Analysis for post-stratified sample", text.size = 15, point.size = 0.8, dodge.size = 2)

r.2_post<-amce(rating~`Theme`+`Media attention`+`Expected turnout`+`Police response`, data=protest_data, cluster=T, respondent.id = "respondent", weights = "wt")
summary(r.2_post)
plot(r.2_post, xlab="Change in rating of protests",group.order = c("Theme","Media attention","Expected turnout","Police response"), main="Pooled Analysis for post-stratified samples", text.size = 13)


#####################################
###### Marginal means ###############
#####################################
library(cregg)
r.1_mm<-mm(selected~`Theme`+`Media attention`+`Expected turnout`+`Police response`, level_order = "descending", data=protest_data, id = "respondent", h0 = 0.5)
plot(r.1_mm, vline=0.5, size=3, theme = ggplot2::theme_bw(base_size = 18))

results1_mm <- cj(selected ~ `Theme`+`Media attention`+`Expected turnout`+`Police response`, data=protest_data, 
                 level_order="descending", id = "respondent", estimate="mm", by=~`Post-materialism score`)
plot(results1_mm, vline=0.5, size=3, theme = ggplot2::theme_bw(base_size = 18)) + ggplot2::facet_wrap(~BY)

results2_mm <- cj(selected ~ `Theme`+`Media attention`+`Expected turnout`+`Police response`, data = protest_data,
                 level_order="descending", id = "respondent", estimate="mm", by=~`Confidence in personal freedom`)
plot(results2_mm, vline=0.5, size=3, theme = ggplot2::theme_bw(base_size = 18)) + ggplot2::facet_wrap(~BY)


results3_mm <- cj(selected ~ `Theme`+`Media attention`+`Expected turnout`+`Police response`, data=protest_data, 
                 level_order="descending", id = "respondent", estimate="mm", by=~`Dissatisfaction with financial situation`)
plot(results3_mm, vline=0.5, size=3, theme = ggplot2::theme_bw(base_size = 18)) + ggplot2::facet_wrap(~BY)

results4_mm <- cj(selected ~ `Theme`+`Media attention`+`Expected turnout`+`Police response`, data=protest_data, 
                  level_order="descending", id = "respondent", estimate="mm", by=~`Partisanship`)
plot(results4_mm, vline=0.5, size=3, theme = ggplot2::theme_bw(base_size = 18)) + ggplot2::facet_wrap(~BY)

r.1_post_mm <- mm(selected~`Theme`+`Media attention`+`Expected turnout`+`Police response`, data=protest_data, 
                      level_order="descending", id = "respondent", weights = ~wt, h0=0.5)
plot(r.1_post_mm, vline=0.5, size=3, theme = ggplot2::theme_bw(base_size = 18))
