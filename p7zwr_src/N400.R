#N400
# installing packages

library(reshape)
library(effects)
library(tidyverse)
library(car)
library(ez)
library(effects)
library(ggplot2)
library(DescTools)
library(lsr)

#import the data for N400
mydata_n4 =read.csv(file=file.choose(), header=T)
str(mydata_n4)
#Half of the trials should be grammatical.
mydata_n4 %>%
  group_by(Condition) %>%
  summarise(no_rows = length(Condition))
#data visualization
ggplot(mydata_n4, aes(x = Condition, y = Voltage)) + geom_boxplot() + facet_grid(.~ Specificity,)+
  scale_x_discrete(guide = guide_axis(angle = 45))
#ANOVA
#create the two-way interaction variables for N400 
mydata_n4$cohe = interaction(mydata_n4$Condition, mydata_n4$Hemisphere)
mydata_n4$coca = interaction(mydata_n4$Condition, mydata_n4$Caudality)
mydata_n4$heca = interaction(mydata_n4$Hemisphere, mydata_n4$Caudality)
mydata_n4$cosp = interaction(mydata_n4$Condition, mydata_n4$Specificity)
mydata_n4$hesp = interaction(mydata_n4$Hemisphere, mydata_n4$Specificity)
mydata_n4$casp = interaction(mydata_n4$Caudality, mydata_n4$Specificity)

#subset everything to lateral and midline for N400:
mydata_n4_lat = subset(mydata_n4, mydata_n4$Hemisphere != "M")
mydata_n4_lat = droplevels(mydata_n4_lat)
mydata_n4_mid = subset(mydata_n4, mydata_n4$Hemisphere == "M")
mydata_n4_mid = droplevels(mydata_n4_mid)

#Compute all means
library(Rmisc)
N4_lat_means =  summarySE(mydata_n4_lat, measurevar="Voltage", groupvars=c("Condition", "Hemisphere", "Caudality", "Specificity"))
N4_mid_means =  summarySE(mydata_n4_mid, measurevar="Voltage", groupvars=c("Condition", "Hemisphere", "Caudality", "Specificity"))

############ N400 ##############
## lateral
#data visualization
library(ggplot2)
ggplot(mydata_n4_lat, aes(x = Condition, y = Voltage)) + geom_boxplot() + facet_grid(.~ Specificity,)+
scale_x_discrete(guide = guide_axis(angle = 45)) 
#ezanova
library(ez)
anova_N4_lat = ezANOVA(data=mydata_n4_lat, 
                      dv=Voltage, 
                      wid=Subject, 
                      within=.(Condition, Hemisphere, Caudality, Specificity), 
                      within_full=.(Condition, Hemisphere, Caudality, Specificity), 
                      observed=.(Hemisphere, Caudality),
                      detailed=TRUE)
anova_N4_lat
#$ANOVA
#Effect DFn DFd          SSn         SSd
#1                                 (Intercept)   1  10 298.49294417 257.5103362
#2                                   Condition   1  10  20.89173572  66.8441479
#3                                  Hemisphere   1  10  14.17190783  10.0355763
#4                                   Caudality   1  10   5.00185753  65.7629369
#5                                 Specificity   1  10  28.19323080  73.4822050
#6                        Condition:Hemisphere   1  10   2.76244286  32.9424902
#7                         Condition:Caudality   1  10   4.89769599   9.5170729
#8                        Hemisphere:Caudality   1  10   0.47962156   9.0257793
#9                       Condition:Specificity   1  10   0.54522273  27.7104927
#10                     Hemisphere:Specificity   1  10   0.20946228   7.3175883
#11                      Caudality:Specificity   1  10   4.50493258  23.4107523
#12             Condition:Hemisphere:Caudality   1  10   1.51528896   1.1450704
#13           Condition:Hemisphere:Specificity   1  10   0.81913654   3.9862509
#14            Condition:Caudality:Specificity   1  10   1.20721724  11.4876178
#15           Hemisphere:Caudality:Specificity   1  10   0.02207042   3.8385242
#16 Condition:Hemisphere:Caudality:Specificity   1  10   1.13109725   0.8181704
#F           p p<.05          ges
#1  11.59149371 0.006718627     * 3.175286e-01
#2   3.12543975 0.107519573       3.153710e-02
#3  14.12166811 0.003735135     * 2.208984e-02
#4   0.76058913 0.403597735       7.796426e-03
#5   3.83674263 0.078597768       4.209509e-02
#6   0.83856528 0.381359616       4.305837e-03
#7   5.14622095 0.046687187     * 7.634069e-03
#8   0.53139075 0.482745986       7.475891e-04
#9   0.19675678 0.666794685       8.491204e-04
#10  0.28624497 0.604329279       3.264901e-04
#11  1.92430065 0.195520710       7.021866e-03
#12 13.23315125 0.004553802     * 2.361890e-03
#13  2.05490460 0.182233805       1.276793e-03
#14  1.05088562 0.329461671       1.881697e-03
#15  0.05749716 0.815339988       3.440131e-05
#16 13.82471529 0.003987337     * 1.763048e-03

#the effect of hemisphere on voltage
anova_n4_hem = summary(aov(Voltage ~ Hemisphere , data = mydata_n4_lat))
anova_n4_hem
PostHocTest(aov(Voltage ~ Hemisphere , data = mydata_n4_lat), method="hsd", conf.level=0.95)
#Posthoc multiple comparisons of means : Tukey HSD 
#95% family-wise confidence level

#$Hemisphere
#diff    lwr.ci    upr.ci    pval    
#R-L 0.5675287 0.2383076 0.8967498 0.00075 ***
  
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

> 
#subset
mydata_n4_la = subset(mydata_n4_lat, mydata_n4_lat$heca == "L.Ant")
mydata_n4_la = droplevels(mydata_n4_la)

mydata_n4_ra = subset(mydata_n4_lat, mydata_n4_lat$heca == "R.Ant")
mydata_p6_ra = droplevels(mydata_n4_ra)

mydata_n4_lp = subset(mydata_n4_lat, mydata_n4_lat$heca == "L.Post")
mydata_n4_lp = droplevels(mydata_n4_lp)

mydata_n4_rp = subset(mydata_n4_lat, mydata_n4_lat$heca == "R.Post")
mydata_n4_rp = droplevels(mydata_n4_rp)

#visualizing the data
ggplot(mydata_n4_la, aes(x = Condition, y = Voltage)) + geom_boxplot() + facet_grid(.~ Specificity,)
ggplot(mydata_n4_ra, aes(x = Condition, y = Voltage)) + geom_boxplot() + facet_grid(.~ Specificity,)
ggplot(mydata_n4_lp, aes(x = Condition, y = Voltage)) + geom_boxplot() + facet_grid(.~ Specificity,)
ggplot(mydata_n4_rp, aes(x = Condition, y = Voltage)) + geom_boxplot() + facet_grid(.~ Specificity,)
# run anovas again with this
# LA
anova_N4_la = summary(aov(Voltage ~ Condition , data = mydata_n4_la))
anova_N4_la
#  Df Sum Sq Mean Sq F value Pr(>F)
#Condition     1    0.3   0.287   0.067  0.796
#Residuals   174  740.7   4.257  

#RA
anova_N4_ra = summary(aov(Voltage ~ Condition , data = mydata_n4_ra))
anova_N4_ra
#Df Sum Sq Mean Sq F value Pr(>F)  
#Condition     1   27.6  27.570   5.799 0.0171 *
# Residuals   174  827.3   4.754                 
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
ggplot(mydata_n4_ra, aes(x = Condition, y = Voltage)) + geom_boxplot()


#LP
anova_N4_lp = summary(aov(Voltage ~ Condition , data = mydata_n4_lp))
anova_N4_lp
# Df Sum Sq Mean Sq F value  Pr(>F)   
#Condition     1   40.4   40.36   8.078 0.00502 **
#Residuals   174  869.3    5.00                   
#---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>
ggplot(mydata_n4_lp, aes(x = Condition, y = Voltage)) + geom_boxplot()

#RP
anova_N4_rp = summary(aov(Voltage ~ Condition , data = mydata_n4_rp))
anova_N4_rp
# Df Sum Sq Mean Sq F value  Pr(>F)   
#Condition     1   52.1   52.05   10.13 0.00173 **
#Residuals   174  894.5    5.14                   
#---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
ggplot(mydata_n4_rp, aes(x = Condition, y = Voltage)) + geom_boxplot()

########post hoc tests for the lateral data#######

PostHocTest(aov(Voltage ~ Condition , data = mydata_n4_la), method="hsd", conf.level=0.95)
#Posthoc multiple comparisons of means : Tukey HSD 
#95% family-wise confidence level
#
#$Condition
#diff   lwr.ci    upr.ci   pval    
#AverageUngrammatical-AverageGrammatical -0.08070805 -0.69463 0.5332139 0.7956    
#
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

  
  PostHocTest(aov(Voltage ~ Condition , data = mydata_n4_ra), method="hsd", conf.level=0.95)

#  Posthoc multiple comparisons of means : Tukey HSD 
#  95% family-wise confidence level
#  
#  $Condition
#  diff   lwr.ci   upr.ci   pval    
#  AverageUngrammatical-AverageGrammatical 0.7915735 0.142789 1.440358 0.0171 *  
#    
#    ---
#    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  
# ---
PostHocTest(aov(Voltage ~ Condition , data = mydata_n4_lp), method="hsd", conf.level=0.95)
#Posthoc multiple comparisons of means : Tukey HSD 
#95% family-wise confidence level

#$Condition
#diff    lwr.ci   upr.ci   pval    
#AverageUngrammatical-AverageGrammatical 0.9577105 0.2926433 1.622778 0.0050 ** 
#  
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


#> 
PostHocTest(aov(Voltage ~ Condition , data = mydata_n4_rp), method="hsd", conf.level=0.95)
#Posthoc multiple comparisons of means : Tukey HSD 
#95% family-wise confidence level

#$Condition
#diff    lwr.ci   upr.ci   pval    
#AverageUngrammatical-AverageGrammatical 1.087689 0.4130593 1.762318 0.0017 ** 
  
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1



###############midline#######
# midline
library(ez)
anova_N4_mid = ezANOVA(data=mydata_n4_mid, 
                       dv=Voltage, 
                       wid=Subject, 
                       within=.(Condition, Caudality,Specificity), 
                       within_full=.(Condition, Caudality,Specificity), 
                       observed=.(Caudality), 
                       detailed=TRUE)
anova_N4_mid
#nothing
#$ANOVA
#Effect DFn DFd         SSn       SSd           F
#1                     (Intercept)   1  10 444.8171095 323.69850 13.74171070
#2                       Condition   1  10  42.6059526 101.27508  4.20695342
#3                       Caudality   2  20  44.0735134 129.08137  3.41439780
#4                     Specificity   1  10  24.9691547  75.18995  3.32080993
#5             Condition:Caudality   2  20   4.1202102  19.69983  2.09149531
#6           Condition:Specificity   1  10   0.3189297  54.90262  0.05809007
#7           Caudality:Specificity   2  20   5.9480449  45.76768  1.29961688
#8 Condition:Caudality:Specificity   2  20   2.1604409  33.20481  0.65064085
#p p<.05          ges
#1 0.004061472     * 0.3464471862
#2 0.067385694       0.0483209702
#3 0.053001503       0.0525233645
#4 0.098405069       0.0288964343
#5 0.149692445       0.0049101442
#6 0.814410256       0.0003799311
#7 0.294688246       0.0070884145
#8 0.532405580       0.0025746444

#$`Mauchly's Test for Sphericity`
#Effect         W           p p<.05
#3                       Caudality 0.2397530 0.001617851     *
#  5             Condition:Caudality 0.5073555 0.047196064     *
#  7           Caudality:Specificity 0.3142375 0.005465892     *
#8 Condition:Caudality:Specificity 0.5686473 0.078848504      

#$`Sphericity Corrections`
#Effect       GGe      p[GG] p[GG]<.05       HFe     p[HF]
#3                       Caudality 0.5681021 0.08738017           0.5921981 0.0850048
#5             Condition:Caudality 0.6699519 0.16954924           0.7354966 0.1656535
#7           Caudality:Specificity 0.5932034 0.28616364           0.6268995 0.2876267
#8 Condition:Caudality:Specificity 0.6986398 0.48356749           0.7770842 0.4979817
#p[HF]<.05
#3          

PostHocTest(aov(Voltage ~ Condition *Caudality , data = mydata_n4_mid), method="hsd", conf.level=0.95)

#Posthoc multiple comparisons of means : Tukey HSD 
#95% family-wise confidence level

#$Condition
#diff    lwr.ci   upr.ci   pval    
#AverageUngrammatical-AverageGrammatical 1.202735 0.4096486 1.995821 0.0032 ** 
  
#  $Caudality
#diff     lwr.ci     upr.ci   pval    
#Med-Ant   0.7453113 -0.5981029  2.0887254 0.3906    
#Post-Ant -0.6694046 -1.8328354  0.4940262 0.3641    
#Post-Med -1.4147158 -2.5781467 -0.2512850 0.0126 *  
  
#  $`Condition:Caudality`
#diff     lwr.ci     upr.ci   pval    
#AverageUngrammatical:Ant-AverageGrammatical:Ant     0.63690250 -1.6792616  2.9530666 0.9685    
#AverageGrammatical:Med-AverageGrammatical:Ant       0.37890032 -1.9372638  2.6950644 0.9971    
#AverageUngrammatical:Med-AverageGrammatical:Ant     1.74862468 -0.5675394  4.0647888 0.2544    
#AverageGrammatical:Post-AverageGrammatical:Ant     -1.05203168 -3.0578886  0.9538253 0.6572    
#AverageUngrammatical:Post-AverageGrammatical:Ant    0.35012500 -1.6557320  2.3559820 0.9960    
#AverageGrammatical:Med-AverageUngrammatical:Ant    -0.25800218 -2.5741663  2.0581619 0.9995    
#AverageUngrammatical:Med-AverageUngrammatical:Ant   1.11172218 -1.2044419  3.4278863 0.7369    
#AverageGrammatical:Post-AverageUngrammatical:Ant   -1.68893418 -3.6947911  0.3169228 0.1528    
#AverageUngrammatical:Post-AverageUngrammatical:Ant -0.28677750 -2.2926345  1.7190795 0.9985    
#AverageUngrammatical:Med-AverageGrammatical:Med     1.36972436 -0.9464397  3.6858885 0.5306    
#AverageGrammatical:Post-AverageGrammatical:Med     -1.43093200 -3.4367890  0.5749250 0.3154    
#AverageUngrammatical:Post-AverageGrammatical:Med   -0.02877532 -2.0346323  1.9770816 1.0000    
#AverageGrammatical:Post-AverageUngrammatical:Med   -2.80065636 -4.8065133 -0.7947994 0.0012 ** 
#  AverageUngrammatical:Post-AverageUngrammatical:Med -1.39849968 -3.4043566  0.6073573 0.3412    
#AverageUngrammatical:Post-AverageGrammatical:Post   1.40215668 -0.2356187  3.0399320 0.1397    

#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

PostHocTest(aov(Voltage ~ Specificity *Caudality , data = mydata_n4_mid), method="hsd", conf.level=0.95)
#Posthoc multiple comparisons of means : Tukey HSD 
#95% family-wise confidence level

#$Specificity
#diff    lwr.ci     upr.ci   pval    
#unspec-spec -0.7202541 -1.525279 0.08477138 0.0792 .  

#$Caudality
#diff     lwr.ci     upr.ci   pval    
#Med-Ant   0.7453112 -0.6183265  2.1089490 0.4015    
#Post-Ant -0.6694046 -1.8503495  0.5115404 0.3749    
#Post-Med -1.4147158 -2.5956608 -0.2337709 0.0143 *  
  
#  $`Specificity:Caudality`
#diff     lwr.ci      upr.ci   pval    
#unspec:Ant-spec:Ant    -1.2116380 -3.5626694  1.13939328 0.6740    
#spec:Med-spec:Ant       0.7027186 -1.6483127  3.05374992 0.9550    
#unspec:Med-spec:Ant    -0.4237341 -2.7747655  1.92729719 0.9954    
#spec:Post-spec:Ant     -1.1394923 -3.1755451  0.89656061 0.5910    
#unspec:Post-spec:Ant   -1.4109550 -3.4470078  0.62509788 0.3481    
#spec:Med-unspec:Ant     1.9143566 -0.4366747  4.26538797 0.1813    
#unspec:Med-unspec:Ant   0.7879039 -1.5631274  3.13893524 0.9280    
#spec:Post-unspec:Ant    0.0721458 -1.9639071  2.10819865 1.0000    
#unspec:Post-unspec:Ant -0.1993169 -2.2353698  1.83673592 0.9998    
#unspec:Med-spec:Med    -1.1264527 -3.4774841  1.22457860 0.7383    
#spec:Post-spec:Med     -1.8422108 -3.8782637  0.19384202 0.1009    
#unspec:Post-spec:Med   -2.1136736 -4.1497264 -0.07762071 0.0369 *  
#  spec:Post-unspec:Med   -0.7157581 -2.7518110  1.32029474 0.9130    
#unspec:Post-unspec:Med -0.9872208 -3.0232737  1.04883202 0.7284    
#unspec:Post-spec:Post  -0.2714627 -1.9338929  1.39096747 0.9971    

#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


##########four interaction######
###data visualization
ggplot(mydata_n4_lat, aes(x = Condition, y = Voltage)) + geom_boxplot() + facet_grid(.~ Specificity,)+
  scale_x_discrete(guide = guide_axis(angle = 45))
# run anovas to check interaction between specificity and condition
# LA
anova_N4_la_sp = summary(aov(Voltage ~ Condition *Specificity , data = mydata_n4_la))
anova_N4_la_sp
#
#Df Sum Sq Mean Sq F value   Pr(>F)    
#Condition               1    0.3    0.29   0.073 0.787377    
#Specificity             1   50.7   50.74  12.919 0.000425 ***
 # Condition:Specificity   1   14.5   14.48   3.688 0.056468 .  
#Residuals             172  675.5    3.93                     
#---
 # Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>  

#RA
anova_N4_ra_sp = summary(aov(Voltage ~ Condition *Specificity , data = mydata_n4_ra))
anova_N4_ra_sp
#  Df Sum Sq Mean Sq F value   Pr(>F)    
#Condition               1   27.6   27.57   6.180 0.013875 *  
#Specificity             1   59.9   59.93  13.433 0.000329 ***
#Condition:Specificity   1    0.0    0.02   0.004 0.950442    
#Residuals             172  767.3    4.46                     
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 

#LP
anova_N4_lp_sp = summary(aov(Voltage ~ Condition* Specificity , data = mydata_n4_lp))
anova_N4_lp_sp
#Df Sum Sq Mean Sq F value Pr(>F)   
#Condition               1   40.4   40.36   8.049 0.0051 **
#Specificity             1    6.7    6.66   1.329 0.2506   
#Condition:Specificity   1    0.3    0.27   0.054 0.8170   
#Residuals             172  862.4    5.01                  
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 

#RP
anova_N4_rp_sp = summary(aov(Voltage ~ Condition* Specificity , data = mydata_n4_rp))
anova_N4_rp_sp
#                      Df Sum Sq Mean Sq F value  Pr(>F)   
#Condition               1   52.1   52.05  10.174 0.00169 **
#Specificity             1   14.4   14.39   2.813 0.09535 . 
#Condition:Specificity   1    0.0    0.04   0.008 0.92899   
#Residuals             172  880.1    5.12                   
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 

#######post hoc test for ezanova######
library(magrittr)
library(tidyverse)
library(ggformula)
library(DescTools)
library(ez)
library(lsr)
library(knitr)
##regional post hoc####
PostHocTest(aov(Voltage ~ Condition *Specificity , data = mydata_n4_la), method="hsd", conf.level=0.95)

#Posthoc multiple comparisons of means : Tukey HSD 
#95% family-wise confidence level

#$Condition
#diff     lwr.ci    upr.ci   pval    
#AverageUngrammatical-AverageGrammatical -0.08070805 -0.6704264 0.5090103 0.7874    

#$Specificity
#diff    lwr.ci     upr.ci    pval    
#unspec-spec -1.073848 -1.663567 -0.4841299 0.00042 ***
  
#$`Condition:Specificity`
#diff     lwr.ci      upr.ci    pval    
#AverageUngrammatical:spec-AverageGrammatical:spec      0.4930256 -0.6031225  1.58917373 0.64853    
#AverageGrammatical:unspec-AverageGrammatical:spec     -0.5001145 -1.5962627  0.59603357 0.63799    
#AverageUngrammatical:unspec-AverageGrammatical:spec   -1.1545563 -2.2507044 -0.05840814 0.03470 *  
#AverageGrammatical:unspec-AverageUngrammatical:spec   -0.9931402 -2.0892883  0.10300795 0.09084 .  
#AverageUngrammatical:unspec-AverageUngrammatical:spec -1.6475819 -2.7437300 -0.55143375 0.00079 ***
#AverageUngrammatical:unspec-AverageGrammatical:unspec -0.6544417 -1.7505898  0.44170641 0.41074    

---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  
PostHocTest(aov(Voltage ~ Condition *Specificity , data = mydata_n4_ra), method="hsd", conf.level=0.95)
#Posthoc multiple comparisons of means : Tukey HSD 
#95% family-wise confidence level

#$Condition
#diff    lwr.ci   upr.ci   pval    
#AverageUngrammatical-AverageGrammatical 0.7915735 0.1630637 1.420083 0.0139 *  
  
#$Specificity
#diff    lwr.ci     upr.ci    pval    
#unspec-spec -1.167048 -1.795558 -0.5385384 0.00033 ***
  
#$`Condition:Specificity`
#diff     lwr.ci      upr.ci    pval    
#AverageUngrammatical:spec-AverageGrammatical:spec      0.7717543 -0.3964980  1.94000657 0.31965    
#AverageGrammatical:unspec-AverageGrammatical:spec     -1.1868674 -2.3551197 -0.01861508 0.04492 *  
#AverageUngrammatical:unspec-AverageGrammatical:spec   -0.3754747 -1.5437270  0.79277762 0.83832    
#AverageGrammatical:unspec-AverageUngrammatical:spec   -1.9586217 -3.1268740 -0.79036936 0.00014 ***
#AverageUngrammatical:unspec-AverageUngrammatical:spec -1.1472290 -2.3154813  0.02102335 0.05632 .  
#AverageUngrammatical:unspec-AverageGrammatical:unspec  0.8113927 -0.3568596  1.97964501 0.27600    

## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

PostHocTest(aov(Voltage ~ Condition *Specificity , data = mydata_n4_lp), method="hsd", conf.level=0.95)
#Posthoc multiple comparisons of means : Tukey HSD 
#95% family-wise confidence level

#$Condition
#diff    lwr.ci   upr.ci   pval    
#AverageUngrammatical-AverageGrammatical 0.9577105 0.2914055 1.624016 0.0051 ** 
  
#$Specificity
#diff    lwr.ci    upr.ci   pval    
#unspec-spec -0.3891027 -1.055408 0.2772023 0.2506    

#$`Condition:Specificity`
#diff     lwr.ci     upr.ci   pval    
#AverageUngrammatical:spec-AverageGrammatical:spec      0.8794967 -0.3590081  2.1180015 0.2572    
#AverageGrammatical:unspec-AverageGrammatical:spec     -0.4673165 -1.7058213  0.7711883 0.7617    
#AverageUngrammatical:unspec-AverageGrammatical:spec    0.5686078 -0.6698970  1.8071125 0.6333    
#AverageGrammatical:unspec-AverageUngrammatical:spec   -1.3468132 -2.5853180 -0.1083084 0.0272 *  
#AverageUngrammatical:unspec-AverageUngrammatical:spec -0.3108890 -1.5493938  0.9276158 0.9150    
#AverageUngrammatical:unspec-AverageGrammatical:unspec  1.0359242 -0.2025806  2.2744290 0.1358    

#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


PostHocTest(aov(Voltage ~ Condition *Specificity , data = mydata_n4_rp), method="hsd", conf.level=0.95)
#Posthoc multiple comparisons of means : Tukey HSD 
#95% family-wise confidence level

#$Condition
#diff    lwr.ci   upr.ci   pval    
#AverageUngrammatical-AverageGrammatical 1.087689 0.4145895 1.760788 0.0017 ** 
  
#  $Specificity
#diff    lwr.ci    upr.ci   pval    
#unspec-spec -0.5718885 -1.244988 0.1012108 0.0953 .  

#$`Condition:Specificity`
#diff     lwr.ci     upr.ci   pval    
#AverageUngrammatical:spec-AverageGrammatical:spec      1.0572552 -0.1938785  2.3083888 0.1295    
#AverageGrammatical:unspec-AverageGrammatical:spec     -0.6023220 -1.8534557  0.6488117 0.5966    
#AverageUngrammatical:unspec-AverageGrammatical:spec    0.5158003 -0.7353334  1.7669340 0.7085    
#AverageGrammatical:unspec-AverageUngrammatical:spec   -1.6595772 -2.9107109 -0.4084435 0.0040 ** 
#  AverageUngrammatical:unspec-AverageUngrammatical:spec -0.5414549 -1.7925886  0.7096788 0.6760    
#AverageUngrammatical:unspec-AverageGrammatical:unspec  1.1181223 -0.1330114  2.3692560 0.0978 .  

#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


########ezanova
library(ez)
anova_N4_lat = ezANOVA(data=mydata_n4_lat, 
                       dv=Voltage, 
                       wid=Subject, 
                       within=.(Condition, Hemisphere, Caudality, Specificity), 
                       within_full=.(Condition, Hemisphere, Caudality, Specificity), 
                       observed=.(Hemisphere, Caudality),
                       detailed=TRUE)
anova_N4_lat
#$ANOVA
#Effect DFn DFd          SSn         SSd
#1                                 (Intercept)   1  10 298.49294417 257.5103362
#2                                   Condition   1  10  20.89173572  66.8441479
#3                                  Hemisphere   1  10  14.17190783  10.0355763
#4                                   Caudality   1  10   5.00185753  65.7629369
#5                                 Specificity   1  10  28.19323080  73.4822050
#6                        Condition:Hemisphere   1  10   2.76244286  32.9424902
#7                         Condition:Caudality   1  10   4.89769599   9.5170729
#8                        Hemisphere:Caudality   1  10   0.47962156   9.0257793
#9                       Condition:Specificity   1  10   0.54522273  27.7104927
#10                     Hemisphere:Specificity   1  10   0.20946228   7.3175883
#11                      Caudality:Specificity   1  10   4.50493258  23.4107523
#12             Condition:Hemisphere:Caudality   1  10   1.51528896   1.1450704
#13           Condition:Hemisphere:Specificity   1  10   0.81913654   3.9862509
#14            Condition:Caudality:Specificity   1  10   1.20721724  11.4876178
#15           Hemisphere:Caudality:Specificity   1  10   0.02207042   3.8385242
#16 Condition:Hemisphere:Caudality:Specificity   1  10   1.13109725   0.8181704
#F           p p<.05          ges
#1  11.59149371 0.006718627     * 3.175286e-01
#2   3.12543975 0.107519573       3.153710e-02
#3  14.12166811 0.003735135     * 2.208984e-02
#4   0.76058913 0.403597735       7.796426e-03
#5   3.83674263 0.078597768       4.209509e-02
#6   0.83856528 0.381359616       4.305837e-03
#7   5.14622095 0.046687187     * 7.634069e-03
#8   0.53139075 0.482745986       7.475891e-04
#9   0.19675678 0.666794685       8.491204e-04
#10  0.28624497 0.604329279       3.264901e-04
#11  1.92430065 0.195520710       7.021866e-03
#12 13.23315125 0.004553802     * 2.361890e-03
#13  2.05490460 0.182233805       1.276793e-03
#14  1.05088562 0.329461671       1.881697e-03
#15  0.05749716 0.815339988       3.440131e-05
#16 13.82471529 0.003987337     * 1.763048e-03

######post hoc test####
mydata_n4_lat
PostHocTest(aov(Voltage ~ Condition:Hemisphere:Caudality:Specificity , data = mydata_n4_lat), method="hsd", conf.level=0.95)
#Posthoc multiple comparisons of means : Tukey HSD 
#95% family-wise confidence level

#$`Condition:Hemisphere:Caudality:Specificity`
#diff      lwr.ci       upr.ci
#AverageUngrammatical:L:Ant:spec-AverageGrammatical:L:Ant:spec          0.49302561 -1.08453695  2.070588179
#AverageGrammatical:R:Ant:spec-AverageGrammatical:L:Ant:spec            0.37035893 -1.20720363  1.947921497
#AverageUngrammatical:R:Ant:spec-AverageGrammatical:L:Ant:spec          1.14211320 -0.43544936  2.719675770
#AverageGrammatical:L:Post:spec-AverageGrammatical:L:Ant:spec          -0.97717625 -2.55473882  0.600386315
#AverageUngrammatical:L:Post:spec-AverageGrammatical:L:Ant:spec        -0.09767952 -1.67524209  1.479883042
#AverageGrammatical:R:Post:spec-AverageGrammatical:L:Ant:spec          -0.30272850 -1.88029107  1.274834065
#AverageUngrammatical:R:Post:spec-AverageGrammatical:L:Ant:spec         0.75452666 -0.82303591  2.332089224
#AverageGrammatical:L:Ant:unspec-AverageGrammatical:L:Ant:spec         -0.50011455 -2.07767711  1.077448020
#AverageUngrammatical:L:Ant:unspec-AverageGrammatical:L:Ant:spec       -1.15455625 -2.73211882  0.423006315
#AverageGrammatical:R:Ant:unspec-AverageGrammatical:L:Ant:spec         -0.81650845 -2.39407102  0.761054111
#AverageUngrammatical:R:Ant:unspec-AverageGrammatical:L:Ant:spec       -0.00511575 -1.58267832  1.572446815
#AverageGrammatical:L:Post:unspec-AverageGrammatical:L:Ant:spec        -1.44449273 -3.02205529  0.133069838
#AverageUngrammatical:L:Post:unspec-AverageGrammatical:L:Ant:spec      -0.40856850 -1.98613107  1.168994065
#AverageGrammatical:R:Post:unspec-AverageGrammatical:L:Ant:spec        -0.90505052 -2.48261309  0.672512042
#AverageUngrammatical:R:Post:unspec-AverageGrammatical:L:Ant:spec       0.21307177 -1.36449079  1.790634338
#AverageGrammatical:R:Ant:spec-AverageUngrammatical:L:Ant:spec         -0.12266668 -1.70022925  1.454895883
#AverageUngrammatical:R:Ant:spec-AverageUngrammatical:L:Ant:spec        0.64908759 -0.92847497  2.226650156
#AverageGrammatical:L:Post:spec-AverageUngrammatical:L:Ant:spec        -1.47020186 -3.04776443  0.107360701
#AverageUngrammatical:L:Post:spec-AverageUngrammatical:L:Ant:spec      -0.59070514 -2.16826770  0.986857429
#AverageGrammatical:R:Post:spec-AverageUngrammatical:L:Ant:spec        -0.79575411 -2.37331668  0.781808451
#AverageUngrammatical:R:Post:spec-AverageUngrammatical:L:Ant:spec       0.26150105 -1.31606152  1.839063611
#AverageGrammatical:L:Ant:unspec-AverageUngrammatical:L:Ant:spec       -0.99314016 -2.57070272  0.584422406
#AverageUngrammatical:L:Ant:unspec-AverageUngrammatical:L:Ant:spec     -1.64758186 -3.22514443 -0.070019299
#AverageGrammatical:R:Ant:unspec-AverageUngrammatical:L:Ant:spec       -1.30953407 -2.88709663  0.268028497
#AverageUngrammatical:R:Ant:unspec-AverageUngrammatical:L:Ant:spec     -0.49814136 -2.07570393  1.079421201
#AverageGrammatical:L:Post:unspec-AverageUngrammatical:L:Ant:spec      -1.93751834 -3.51508091 -0.359955776
#AverageUngrammatical:L:Post:unspec-AverageUngrammatical:L:Ant:spec    -0.90159411 -2.47915668  0.675968451
#AverageGrammatical:R:Post:unspec-AverageUngrammatical:L:Ant:spec      -1.39807614 -2.97563870  0.179486429
#AverageUngrammatical:R:Post:unspec-AverageUngrammatical:L:Ant:spec    -0.27995384 -1.85751641  1.297608724
#AverageUngrammatical:R:Ant:spec-AverageGrammatical:R:Ant:spec          0.77175427 -0.80580829  2.349316838
#AverageGrammatical:L:Post:spec-AverageGrammatical:R:Ant:spec          -1.34753518 -2.92509775  0.230027383
#AverageUngrammatical:L:Post:spec-AverageGrammatical:R:Ant:spec        -0.46803845 -2.04560102  1.109524111
#AverageGrammatical:R:Post:spec-AverageGrammatical:R:Ant:spec          -0.67308743 -2.25065000  0.904475133
#AverageUngrammatical:R:Post:spec-AverageGrammatical:R:Ant:spec         0.38416773 -1.19339484  1.961730292
#AverageGrammatical:L:Ant:unspec-AverageGrammatical:R:Ant:spec         -0.87047348 -2.44803604  0.707089088
#AverageUngrammatical:L:Ant:unspec-AverageGrammatical:R:Ant:spec       -1.52491518 -3.10247775  0.052647383
#AverageGrammatical:R:Ant:unspec-AverageGrammatical:R:Ant:spec         -1.18686739 -2.76442995  0.390695179
#AverageUngrammatical:R:Ant:unspec-AverageGrammatical:R:Ant:spec       -0.37547468 -1.95303725  1.202087883
#AverageGrammatical:L:Post:unspec-AverageGrammatical:R:Ant:spec        -1.81485166 -3.39241422 -0.237289094
#AverageUngrammatical:L:Post:unspec-AverageGrammatical:R:Ant:spec      -0.77892743 -2.35649000  0.798635133
#AverageGrammatical:R:Post:unspec-AverageGrammatical:R:Ant:spec        -1.27540945 -2.85297202  0.302153111
#AverageUngrammatical:R:Post:unspec-AverageGrammatical:R:Ant:spec      -0.15728716 -1.73484972  1.420275406
#AverageGrammatical:L:Post:spec-AverageUngrammatical:R:Ant:spec        -2.11928945 -3.69685202 -0.541726889
#AverageUngrammatical:L:Post:spec-AverageUngrammatical:R:Ant:spec      -1.23979273 -2.81735529  0.337769838
#AverageGrammatical:R:Post:spec-AverageUngrammatical:R:Ant:spec        -1.44484170 -3.02240427  0.132720861
#AverageUngrammatical:R:Post:spec-AverageUngrammatical:R:Ant:spec      -0.38758655 -1.96514911  1.189976020
#AverageGrammatical:L:Ant:unspec-AverageUngrammatical:R:Ant:spec       -1.64222775 -3.21979032 -0.064665185
#AverageUngrammatical:L:Ant:unspec-AverageUngrammatical:R:Ant:spec     -2.29666945 -3.87423202 -0.719106889
#AverageGrammatical:R:Ant:unspec-AverageUngrammatical:R:Ant:spec       -1.95862166 -3.53618422 -0.381059094
#AverageUngrammatical:R:Ant:unspec-AverageUngrammatical:R:Ant:spec     -1.14722895 -2.72479152  0.430333611
#AverageGrammatical:L:Post:unspec-AverageUngrammatical:R:Ant:spec      -2.58660593 -4.16416850 -1.009043367
#AverageUngrammatical:L:Post:unspec-AverageUngrammatical:R:Ant:spec    -1.55068170 -3.12824427  0.026880861
#AverageGrammatical:R:Post:unspec-AverageUngrammatical:R:Ant:spec      -2.04716373 -3.62472629 -0.469601162
#AverageUngrammatical:R:Post:unspec-AverageUngrammatical:R:Ant:spec    -0.92904143 -2.50660400  0.648521133
#AverageUngrammatical:L:Post:spec-AverageGrammatical:L:Post:spec        0.87949673 -0.69806584  2.457059292
#AverageGrammatical:R:Post:spec-AverageGrammatical:L:Post:spec          0.67444775 -0.90311482  2.252010315
#AverageUngrammatical:R:Post:spec-AverageGrammatical:L:Post:spec        1.73170291  0.15414034  3.309265474
#AverageGrammatical:L:Ant:unspec-AverageGrammatical:L:Post:spec         0.47706170 -1.10050086  2.054624270
#AverageUngrammatical:L:Ant:unspec-AverageGrammatical:L:Post:spec      -0.17738000 -1.75494257  1.400182565
#AverageGrammatical:R:Ant:unspec-AverageGrammatical:L:Post:spec         0.16066780 -1.41689477  1.738230361
#AverageUngrammatical:R:Ant:unspec-AverageGrammatical:L:Post:spec       0.97206050 -0.60550207  2.549623065
#AverageGrammatical:L:Post:unspec-AverageGrammatical:L:Post:spec       -0.46731648 -2.04487904  1.110246088
#AverageUngrammatical:L:Post:unspec-AverageGrammatical:L:Post:spec      0.56860775 -1.00895482  2.146170315
#AverageGrammatical:R:Post:unspec-AverageGrammatical:L:Post:spec        0.07212573 -1.50543684  1.649688292
#AverageUngrammatical:R:Post:unspec-AverageGrammatical:L:Post:spec      1.19024802 -0.38731454  2.767810588
#AverageGrammatical:R:Post:spec-AverageUngrammatical:L:Post:spec       -0.20504898 -1.78261154  1.372513588
#AverageUngrammatical:R:Post:spec-AverageUngrammatical:L:Post:spec      0.85220618 -0.72535638  2.429768747
#AverageGrammatical:L:Ant:unspec-AverageUngrammatical:L:Post:spec      -0.40243502 -1.97999759  1.175127542
#AverageUngrammatical:L:Ant:unspec-AverageUngrammatical:L:Post:spec    -1.05687673 -2.63443929  0.520685838
#AverageGrammatical:R:Ant:unspec-AverageUngrammatical:L:Post:spec      -0.71882893 -2.29639150  0.858733633
#AverageUngrammatical:R:Ant:unspec-AverageUngrammatical:L:Post:spec     0.09256377 -1.48499879  1.670126338
#AverageGrammatical:L:Post:unspec-AverageUngrammatical:L:Post:spec     -1.34681320 -2.92437577  0.230749361
#AverageUngrammatical:L:Post:unspec-AverageUngrammatical:L:Post:spec   -0.31088898 -1.88845154  1.266673588
#AverageGrammatical:R:Post:unspec-AverageUngrammatical:L:Post:spec     -0.80737100 -2.38493357  0.770191565
#AverageUngrammatical:R:Post:unspec-AverageUngrammatical:L:Post:spec    0.31075130 -1.26681127  1.888313861
#AverageUngrammatical:R:Post:spec-AverageGrammatical:R:Post:spec        1.05725516 -0.52030741  2.634817724
#AverageGrammatical:L:Ant:unspec-AverageGrammatical:R:Post:spec        -0.19738605 -1.77494861  1.380176520
#AverageUngrammatical:L:Ant:unspec-AverageGrammatical:R:Post:spec      -0.85182775 -2.42939032  0.725734815
#AverageGrammatical:R:Ant:unspec-AverageGrammatical:R:Post:spec        -0.51377995 -2.09134252  1.063782611
#AverageUngrammatical:R:Ant:unspec-AverageGrammatical:R:Post:spec       0.29761275 -1.27994982  1.875175315
#AverageGrammatical:L:Post:unspec-AverageGrammatical:R:Post:spec       -1.14176423 -2.71932679  0.435798338
#AverageUngrammatical:L:Post:unspec-AverageGrammatical:R:Post:spec     -0.10584000 -1.68340257  1.471722565
#AverageGrammatical:R:Post:unspec-AverageGrammatical:R:Post:spec       -0.60232202 -2.17988459  0.975240542
#AverageUngrammatical:R:Post:unspec-AverageGrammatical:R:Post:spec      0.51580027 -1.06176229  2.093362838
#AverageGrammatical:L:Ant:unspec-AverageUngrammatical:R:Post:spec      -1.25464120 -2.83220377  0.322921361
#AverageUngrammatical:L:Ant:unspec-AverageUngrammatical:R:Post:spec    -1.90908291 -3.48664547 -0.331520344
#AverageGrammatical:R:Ant:unspec-AverageUngrammatical:R:Post:spec      -1.57103511 -3.14859768  0.006527451
#AverageUngrammatical:R:Ant:unspec-AverageUngrammatical:R:Post:spec    -0.75964241 -2.33720497  0.817920156
#AverageGrammatical:L:Post:unspec-AverageUngrammatical:R:Post:spec     -2.19901939 -3.77658195 -0.621456821
#AverageUngrammatical:L:Post:unspec-AverageUngrammatical:R:Post:spec   -1.16309516 -2.74065772  0.414467406
#AverageGrammatical:R:Post:unspec-AverageUngrammatical:R:Post:spec     -1.65957718 -3.23713975 -0.082014617
#AverageUngrammatical:R:Post:unspec-AverageUngrammatical:R:Post:spec   -0.54145489 -2.11901745  1.036107679
#AverageUngrammatical:L:Ant:unspec-AverageGrammatical:L:Ant:unspec     -0.65444170 -2.23200427  0.923120861
#AverageGrammatical:R:Ant:unspec-AverageGrammatical:L:Ant:unspec       -0.31639391 -1.89395647  1.261168656
#AverageUngrammatical:R:Ant:unspec-AverageGrammatical:L:Ant:unspec      0.49499880 -1.08256377  2.072561361
#AverageGrammatical:L:Post:unspec-AverageGrammatical:L:Ant:unspec      -0.94437818 -2.52194075  0.633184383
#AverageUngrammatical:L:Post:unspec-AverageGrammatical:L:Ant:unspec     0.09154605 -1.48601652  1.669108611
#AverageGrammatical:R:Post:unspec-AverageGrammatical:L:Ant:unspec      -0.40493598 -1.98249854  1.172626588
#AverageUngrammatical:R:Post:unspec-AverageGrammatical:L:Ant:unspec     0.71318632 -0.86437625  2.290748883
#AverageGrammatical:R:Ant:unspec-AverageUngrammatical:L:Ant:unspec      0.33804780 -1.23951477  1.915610361
#AverageUngrammatical:R:Ant:unspec-AverageUngrammatical:L:Ant:unspec    1.14944050 -0.42812207  2.727003065
#AverageGrammatical:L:Post:unspec-AverageUngrammatical:L:Ant:unspec    -0.28993648 -1.86749904  1.287626088
#AverageUngrammatical:L:Post:unspec-AverageUngrammatical:L:Ant:unspec   0.74598775 -0.83157482  2.323550315
#AverageGrammatical:R:Post:unspec-AverageUngrammatical:L:Ant:unspec     0.24950573 -1.32805684  1.827068292
#AverageUngrammatical:R:Post:unspec-AverageUngrammatical:L:Ant:unspec   1.36762802 -0.20993454  2.945190588
#AverageUngrammatical:R:Ant:unspec-AverageGrammatical:R:Ant:unspec      0.81139270 -0.76616986  2.388955270
#AverageGrammatical:L:Post:unspec-AverageGrammatical:R:Ant:unspec      -0.62798427 -2.20554684  0.949578292
#AverageUngrammatical:L:Post:unspec-AverageGrammatical:R:Ant:unspec     0.40793995 -1.16962261  1.985502520
#AverageGrammatical:R:Post:unspec-AverageGrammatical:R:Ant:unspec      -0.08854207 -1.66610463  1.489020497
#AverageUngrammatical:R:Post:unspec-AverageGrammatical:R:Ant:unspec     1.02958023 -0.54798234  2.607142792
#AverageGrammatical:L:Post:unspec-AverageUngrammatical:R:Ant:unspec    -1.43937698 -3.01693954  0.138185588
#AverageUngrammatical:L:Post:unspec-AverageUngrammatical:R:Ant:unspec  -0.40345275 -1.98101532  1.174109815
#AverageGrammatical:R:Post:unspec-AverageUngrammatical:R:Ant:unspec    -0.89993477 -2.47749734  0.677627792
#AverageUngrammatical:R:Post:unspec-AverageUngrammatical:R:Ant:unspec   0.21818752 -1.35937504  1.795750088
#AverageUngrammatical:L:Post:unspec-AverageGrammatical:L:Post:unspec    1.03592423 -0.54163834  2.613486792
#AverageGrammatical:R:Post:unspec-AverageGrammatical:L:Post:unspec      0.53944220 -1.03812036  2.117004770
#AverageUngrammatical:R:Post:unspec-AverageGrammatical:L:Post:unspec    1.65756450  0.08000193  3.235127065
#AverageGrammatical:R:Post:unspec-AverageUngrammatical:L:Post:unspec   -0.49648202 -2.07404459  1.081080542
#AverageUngrammatical:R:Post:unspec-AverageUngrammatical:L:Post:unspec  0.62164027 -0.95592229  2.199202838
#AverageUngrammatical:R:Post:unspec-AverageGrammatical:R:Post:unspec    1.11812230 -0.45944027  2.695684861
#pval    
#AverageUngrammatical:L:Ant:spec-AverageGrammatical:L:Ant:spec         0.99952    
#AverageGrammatical:R:Ant:spec-AverageGrammatical:L:Ant:spec           0.99999    
#AverageUngrammatical:R:Ant:spec-AverageGrammatical:L:Ant:spec         0.48052    
#AverageGrammatical:L:Post:spec-AverageGrammatical:L:Ant:spec          0.74506    
#AverageUngrammatical:L:Post:spec-AverageGrammatical:L:Ant:spec        1.00000    
#AverageGrammatical:R:Post:spec-AverageGrammatical:L:Ant:spec          1.00000    
#AverageUngrammatical:R:Post:spec-AverageGrammatical:L:Ant:spec        0.95949    
#AverageGrammatical:L:Ant:unspec-AverageGrammatical:L:Ant:spec         0.99943    
#AverageUngrammatical:L:Ant:unspec-AverageGrammatical:L:Ant:spec       0.46036    
#AverageGrammatical:R:Ant:unspec-AverageGrammatical:L:Ant:spec         0.92290    
#AverageUngrammatical:R:Ant:unspec-AverageGrammatical:L:Ant:spec       1.00000    
#AverageGrammatical:L:Post:unspec-AverageGrammatical:L:Ant:spec        0.11679    
#AverageUngrammatical:L:Post:unspec-AverageGrammatical:L:Ant:spec      0.99995    
#AverageGrammatical:R:Post:unspec-AverageGrammatical:L:Ant:spec        0.83949    
#AverageUngrammatical:R:Post:unspec-AverageGrammatical:L:Ant:spec      1.00000    
#AverageGrammatical:R:Ant:spec-AverageUngrammatical:L:Ant:spec         1.00000    
#AverageUngrammatical:R:Ant:spec-AverageUngrammatical:L:Ant:spec       0.99002    
#AverageGrammatical:L:Post:spec-AverageUngrammatical:L:Ant:spec        0.10012    
#AverageUngrammatical:L:Post:spec-AverageUngrammatical:L:Ant:spec      0.99625    
#AverageGrammatical:R:Post:spec-AverageUngrammatical:L:Ant:spec        0.93703    
#AverageUngrammatical:R:Post:spec-AverageUngrammatical:L:Ant:spec      1.00000    
#AverageGrammatical:L:Ant:unspec-AverageUngrammatical:L:Ant:spec       0.72151    
#AverageUngrammatical:L:Ant:unspec-AverageUngrammatical:L:Ant:spec     0.03047 *  
#AverageGrammatical:R:Ant:unspec-AverageUngrammatical:L:Ant:spec       0.24111    
#AverageUngrammatical:R:Ant:unspec-AverageUngrammatical:L:Ant:spec     0.99946    
#AverageGrammatical:L:Post:unspec-AverageUngrammatical:L:Ant:spec      0.00285 ** 
#AverageUngrammatical:L:Post:unspec-AverageUngrammatical:L:Ant:spec    0.84346    
#AverageGrammatical:R:Post:unspec-AverageUngrammatical:L:Ant:spec      0.15229    
#AverageUngrammatical:R:Post:unspec-AverageUngrammatical:L:Ant:spec    1.00000    
#AverageUngrammatical:R:Ant:spec-AverageGrammatical:R:Ant:spec         0.95097    
#AverageGrammatical:L:Post:spec-AverageGrammatical:R:Ant:spec          0.19949    
#AverageUngrammatical:L:Post:spec-AverageGrammatical:R:Ant:spec        0.99974    
#AverageGrammatical:R:Post:spec-AverageGrammatical:R:Ant:spec          0.98574    
#AverageUngrammatical:R:Post:spec-AverageGrammatical:R:Ant:spec        0.99998    
#AverageGrammatical:L:Ant:unspec-AverageGrammatical:R:Ant:spec         0.87658    
#AverageUngrammatical:L:Ant:unspec-AverageGrammatical:R:Ant:spec       0.07100 .  
#AverageGrammatical:R:Ant:unspec-AverageGrammatical:R:Ant:spec         0.40926    
#AverageUngrammatical:R:Ant:unspec-AverageGrammatical:R:Ant:spec       0.99998    
#AverageGrammatical:L:Post:unspec-AverageGrammatical:R:Ant:spec        0.00824 ** 
#AverageUngrammatical:L:Post:unspec-AverageGrammatical:R:Ant:spec      0.94706    
#AverageGrammatical:R:Post:unspec-AverageGrammatical:R:Ant:spec        0.28296    
#AverageUngrammatical:R:Post:unspec-AverageGrammatical:R:Ant:spec      1.00000    
#AverageGrammatical:L:Post:spec-AverageUngrammatical:R:Ant:spec        0.00051 ***
#AverageUngrammatical:L:Post:spec-AverageUngrammatical:R:Ant:spec      0.33092    
#AverageGrammatical:R:Post:spec-AverageUngrammatical:R:Ant:spec        0.11655    
#AverageUngrammatical:R:Post:spec-AverageUngrammatical:R:Ant:spec      0.99998    
#AverageGrammatical:L:Ant:unspec-AverageUngrammatical:R:Ant:spec       0.03168 *  
#AverageUngrammatical:L:Ant:unspec-AverageUngrammatical:R:Ant:spec     8.1e-05 ***
#AverageGrammatical:R:Ant:unspec-AverageUngrammatical:R:Ant:spec       0.00235 ** 
#AverageUngrammatical:R:Ant:unspec-AverageUngrammatical:R:Ant:spec     0.47220    
#AverageGrammatical:L:Post:unspec-AverageUngrammatical:R:Ant:spec      3.0e-06 ***
#AverageUngrammatical:L:Post:unspec-AverageUngrammatical:R:Ant:spec    0.05995 .  
#AverageGrammatical:R:Post:unspec-AverageUngrammatical:R:Ant:spec      0.00103 ** 
#AverageUngrammatical:R:Post:unspec-AverageUngrammatical:R:Ant:spec    0.81049    
#AverageUngrammatical:L:Post:spec-AverageGrammatical:L:Post:spec       0.86745    
#AverageGrammatical:R:Post:spec-AverageGrammatical:L:Post:spec         0.98545    
#AverageUngrammatical:R:Post:spec-AverageGrammatical:L:Post:spec       0.01612 *  
#AverageGrammatical:L:Ant:unspec-AverageGrammatical:L:Post:spec        0.99968    
#AverageUngrammatical:L:Ant:unspec-AverageGrammatical:L:Post:spec      1.00000    
#AverageGrammatical:R:Ant:unspec-AverageGrammatical:L:Post:spec        1.00000    
#AverageUngrammatical:R:Ant:unspec-AverageGrammatical:L:Post:spec      0.75242    
#AverageGrammatical:L:Post:unspec-AverageGrammatical:L:Post:spec       0.99975    
#AverageUngrammatical:L:Post:unspec-AverageGrammatical:L:Post:spec     0.99753    
#AverageGrammatical:R:Post:unspec-AverageGrammatical:L:Post:spec       1.00000    
#AverageUngrammatical:R:Post:unspec-AverageGrammatical:L:Post:spec     0.40404    
#AverageGrammatical:R:Post:spec-AverageUngrammatical:L:Post:spec       1.00000    
#AverageUngrammatical:R:Post:spec-AverageUngrammatical:L:Post:spec     0.89384    
#AverageGrammatical:L:Ant:unspec-AverageUngrammatical:L:Post:spec      0.99996    
#AverageUngrammatical:L:Ant:unspec-AverageUngrammatical:L:Post:spec    0.62102    
#AverageGrammatical:R:Ant:unspec-AverageUngrammatical:L:Post:spec      0.97358    
#AverageUngrammatical:R:Ant:unspec-AverageUngrammatical:L:Post:spec    1.00000    
#AverageGrammatical:L:Post:unspec-AverageUngrammatical:L:Post:spec     0.20023    
#AverageUngrammatical:L:Post:unspec-AverageUngrammatical:L:Post:spec   1.00000    
#AverageGrammatical:R:Post:unspec-AverageUngrammatical:L:Post:spec     0.92936    
#AverageUngrammatical:R:Post:unspec-AverageUngrammatical:L:Post:spec   1.00000    
#AverageUngrammatical:R:Post:spec-AverageGrammatical:R:Post:spec       0.62040    
#AverageGrammatical:L:Ant:unspec-AverageGrammatical:R:Post:spec        1.00000    
#AverageUngrammatical:L:Ant:unspec-AverageGrammatical:R:Post:spec      0.89418    
#AverageGrammatical:R:Ant:unspec-AverageGrammatical:R:Post:spec        0.99922    
#AverageUngrammatical:R:Ant:unspec-AverageGrammatical:R:Post:spec      1.00000    
#AverageGrammatical:L:Post:unspec-AverageGrammatical:R:Post:spec       0.48109    
#AverageUngrammatical:L:Post:unspec-AverageGrammatical:R:Post:spec     1.00000    
#AverageGrammatical:R:Post:unspec-AverageGrammatical:R:Post:spec       0.99538    
#AverageUngrammatical:R:Post:unspec-AverageGrammatical:R:Post:spec     0.99918    
#AverageGrammatical:L:Ant:unspec-AverageUngrammatical:R:Post:spec      0.31041    
#AverageUngrammatical:L:Ant:unspec-AverageUngrammatical:R:Post:spec    0.00367 ** 
#AverageGrammatical:R:Ant:unspec-AverageUngrammatical:R:Post:spec      0.05228 .  
#AverageUngrammatical:R:Ant:unspec-AverageUngrammatical:R:Post:spec    0.95708    
#AverageGrammatical:L:Post:unspec-AverageUngrammatical:R:Post:spec     0.00023 ***
#AverageUngrammatical:L:Post:unspec-AverageUngrammatical:R:Post:spec   0.44667    
#AverageGrammatical:R:Post:unspec-AverageUngrammatical:R:Post:spec     0.02790 *  
#AverageUngrammatical:R:Post:unspec-AverageUngrammatical:R:Post:spec   0.99857    
#AverageUngrammatical:L:Ant:unspec-AverageGrammatical:L:Ant:unspec     0.98917    
#AverageGrammatical:R:Ant:unspec-AverageGrammatical:L:Ant:unspec       1.00000    
#AverageUngrammatical:R:Ant:unspec-AverageGrammatical:L:Ant:unspec     0.99950    
#AverageGrammatical:L:Post:unspec-AverageGrammatical:L:Ant:unspec      0.79064    
#AverageUngrammatical:L:Post:unspec-AverageGrammatical:L:Ant:unspec    1.00000    
#AverageGrammatical:R:Post:unspec-AverageGrammatical:L:Ant:unspec      0.99996    
#AverageUngrammatical:R:Post:unspec-AverageGrammatical:L:Ant:unspec    0.97541    
#AverageGrammatical:R:Ant:unspec-AverageUngrammatical:L:Ant:unspec     1.00000    
#AverageUngrammatical:R:Ant:unspec-AverageUngrammatical:L:Ant:unspec   0.46862    
#AverageGrammatical:L:Post:unspec-AverageUngrammatical:L:Ant:unspec    1.00000    
#AverageUngrammatical:L:Post:unspec-AverageUngrammatical:L:Ant:unspec  0.96328    
#AverageGrammatical:R:Post:unspec-AverageUngrammatical:L:Ant:unspec    1.00000    
#AverageUngrammatical:R:Post:unspec-AverageUngrammatical:L:Ant:unspec  0.17962    
#AverageUngrammatical:R:Ant:unspec-AverageGrammatical:R:Ant:unspec     0.92657    
#AverageGrammatical:L:Post:unspec-AverageGrammatical:R:Ant:unspec      0.99286    
#AverageUngrammatical:L:Post:unspec-AverageGrammatical:R:Ant:unspec    0.99995    
#AverageGrammatical:R:Post:unspec-AverageGrammatical:R:Ant:unspec      1.00000    
#AverageUngrammatical:R:Post:unspec-AverageGrammatical:R:Ant:unspec    0.66508    
#AverageGrammatical:L:Post:unspec-AverageUngrammatical:R:Ant:unspec    0.12035    
#AverageUngrammatical:L:Post:unspec-AverageUngrammatical:R:Ant:unspec  0.99996    
#AverageGrammatical:R:Post:unspec-AverageUngrammatical:R:Ant:unspec    0.84534    
#AverageUngrammatical:R:Post:unspec-AverageUngrammatical:R:Ant:unspec  1.00000    
#AverageUngrammatical:L:Post:unspec-AverageGrammatical:L:Post:unspec   0.65495    
#AverageGrammatical:R:Post:unspec-AverageGrammatical:L:Post:unspec     0.99863    
#AverageUngrammatical:R:Post:unspec-AverageGrammatical:L:Post:unspec   0.02832 *  
#  AverageGrammatical:R:Post:unspec-AverageUngrammatical:L:Post:unspec   0.99948    
#AverageUngrammatical:R:Post:unspec-AverageUngrammatical:L:Post:unspec 0.99357    
#AverageUngrammatical:R:Post:unspec-AverageGrammatical:R:Post:unspec   0.51988    

#---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#mean for the lateral data
fmean <- mydata_n4_lat %>%
  group_by(Condition, Specificity,Hemisphere, Caudality) %>%
  summarise(
    mean= mean(Voltage),
    median=median(Voltage),
    sd= sd(Voltage),
    n = n(),
    var=var(Voltage)
  )
)
fmean
# A tibble: 16 x 9
# Groups:   Condition, Specificity, Hemisphere [8]
#Condition            Specificity Hemisphere Caudality   mean median    sd     n   var
#<fct>                <fct>       <fct>      <fct>      <dbl>  <dbl> <dbl> <int> <dbl>
#  1 AverageGrammatical   spec        L          Ant       1.53    1.37   1.76    44  3.11
#2 AverageGrammatical   spec        L          Post      0.553   0.993  2.25    44  5.07
#3 AverageGrammatical   spec        R          Ant       1.90    1.99   2.07    44  4.30
#4 AverageGrammatical   spec        R          Post      1.23    1.36   1.99    44  3.94
#5 AverageGrammatical   unspec      L          Ant       1.03    1.14   1.99    44  3.95
#6 AverageGrammatical   unspec      L          Post      0.0852 -0.159  2.24    44  5.00
#7 AverageGrammatical   unspec      R          Ant       0.713   0.719  2.10    44  4.40
#8 AverageGrammatical   unspec      R          Post      0.625   0.913  2.45    44  6.02
#9 AverageUngrammatical spec        L          Ant       2.02    1.30   2.11    44  4.47
#10 AverageUngrammatical spec        L          Post      1.43    1.38   2.46    44  6.04
#11 AverageUngrammatical spec        R          Ant       2.67    1.49   2.49    44  6.18
#12 AverageUngrammatical spec        R          Post      2.28    1.88   2.72    44  7.39
#13 AverageUngrammatical unspec      L          Ant       0.375   0.681  2.04    44  4.18
#14 AverageUngrammatical unspec      L          Post      1.12    1.01   1.99    44  3.94
#15 AverageUngrammatical unspec      R          Ant       1.52    1.70   1.72    44  2.96
#16 AverageUngrammatical unspec      R          Post      1.74    1.68   1.77    44  3.12
#>


##mean comparison
  PostHocTest(aov(Voltage ~Hemisphere:Caudality:Specificity , data = mydata_n4_lat), method="hsd", conf.level=0.95)

#Posthoc multiple comparisons of means : Tukey HSD 
#95% family-wise confidence level

#$`Hemisphere:Caudality:Specificity`
#diff     lwr.ci      upr.ci    pval    
#R:Ant:spec-L:Ant:spec        0.50972326 -0.4913323  1.51077880  0.7809    
#L:Post:spec-L:Ant:spec      -0.78394069 -1.7849962  0.21711485  0.2524    
#R:Post:spec-L:Ant:spec      -0.02061373 -1.0216693  0.98044181  1.0000    
#L:Ant:unspec-L:Ant:spec     -1.07384820 -2.0749037 -0.07279266  0.0256 *  
#  R:Ant:unspec-L:Ant:spec     -0.65732491 -1.6583804  0.34373063  0.4851    
#L:Post:unspec-L:Ant:spec    -1.17304342 -2.1740990 -0.17198788  0.0093 ** 
#  R:Post:unspec-L:Ant:spec    -0.59250218 -1.5935577  0.40855336  0.6207    
#L:Post:spec-R:Ant:spec      -1.29366395 -2.2947195 -0.29260841  0.0024 ** 
#  R:Post:spec-R:Ant:spec      -0.53033699 -1.5313925  0.47071855  0.7439    
#L:Ant:unspec-R:Ant:spec     -1.58357147 -2.5846270 -0.58251593 5.1e-05 ***
#  R:Ant:unspec-R:Ant:spec     -1.16704817 -2.1681037 -0.16599263  0.0099 ** 
#  L:Post:unspec-R:Ant:spec    -1.68276668 -2.6838222 -0.68171114 1.1e-05 ***
#  R:Post:unspec-R:Ant:spec    -1.10222544 -2.1032810 -0.10116990  0.0194 *  
#  R:Post:spec-L:Post:spec      0.76332697 -0.2377286  1.76438251  0.2850    
#L:Ant:unspec-L:Post:spec    -0.28990751 -1.2909631  0.71114803  0.9877    
#R:Ant:unspec-L:Post:spec     0.12661578 -0.8744398  1.12767132  0.9999    
#L:Post:unspec-L:Post:spec   -0.38910273 -1.3901583  0.61195281  0.9370    
#R:Post:unspec-L:Post:spec    0.19143851 -0.8096170  1.19249405  0.9991    
#L:Ant:unspec-R:Post:spec    -1.05323448 -2.0542900 -0.05217894  0.0311 *  
#  R:Ant:unspec-R:Post:spec    -0.63671118 -1.6377667  0.36434436  0.5281    
#L:Post:unspec-R:Post:spec   -1.15242969 -2.1534852 -0.15137415  0.0116 *  
#  R:Post:unspec-R:Post:spec   -0.57188845 -1.5729440  0.42916709  0.6630    
#R:Ant:unspec-L:Ant:unspec    0.41652330 -0.5845322  1.41757884  0.9114    
#L:Post:unspec-L:Ant:unspec  -0.09919522 -1.1002508  0.90186032  1.0000    
#R:Post:unspec-L:Ant:unspec   0.48134602 -0.5197095  1.48240156  0.8274    
#L:Post:unspec-R:Ant:unspec  -0.51571851 -1.5167741  0.48533703  0.7704    
#R:Post:unspec-R:Ant:unspec   0.06482273 -0.9362328  1.06587827  1.0000    
#R:Post:unspec-L:Post:unspec  0.58054124 -0.4205143  1.58159678  0.6454    

# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

