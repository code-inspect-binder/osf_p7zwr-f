#P600
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

#import the data for P600
mydata_p6 =read.csv(file=file.choose(), header=T)
str(mydata_p6)
#Half of the trials should be grammatical.
mydata_p6 %>%
  group_by(Condition) %>%
  summarise(no_rows = length(Condition))
#data visualization
ggplot(mydata_p6, aes(x = Condition, y = Voltage)) + geom_boxplot() + facet_grid(.~ Specificity,)+
       scale_x_discrete(guide = guide_axis(angle = 45))
#ANOVA
#create the two-way interaction variables for P600 
mydata_p6$cohe = interaction(mydata_p6$Condition, mydata_p6$Hemisphere)
mydata_p6$coca = interaction(mydata_p6$Condition, mydata_p6$Caudality)
mydata_p6$heca = interaction(mydata_p6$Hemisphere, mydata_p6$Caudality)
mydata_p6$cosp = interaction(mydata_p6$Condition, mydata_p6$Specificity)
mydata_p6$hesp = interaction(mydata_p6$Hemisphere, mydata_p6$Specificity)
mydata_p6$casp = interaction(mydata_p6$Caudality, mydata_p6$Specificity)

#subset everything to lateral and midline for P600:
mydata_p6_lat = subset(mydata_p6, mydata_p6$Hemisphere != "M")
mydata_p6_lat = droplevels(mydata_p6_lat)
mydata_p6_mid = subset(mydata_p6, mydata_p6$Hemisphere == "M")
mydata_p6_mid = droplevels(mydata_p6_mid)

#Compute all means
library(Rmisc)
P6_lat_means =  summarySE(mydata_p6_lat, measurevar="Voltage", groupvars=c("Condition", "Hemisphere", "Caudality", "Specificity"))
P6_mid_means =  summarySE(mydata_p6_mid, measurevar="Voltage", groupvars=c("Condition", "Hemisphere", "Caudality", "Specificity"))

############ P600 ##############
## lateral
#visualization
library(ggplot2)
ggplot(mydata_p6_lat, aes(x = Condition, y = Voltage)) + geom_boxplot() + facet_grid(.~ Specificity,)+
      scale_x_discrete(guide = guide_axis(angle = 45)) 
library(ez)
anova_P6_lat = ezANOVA(data=mydata_p6_lat, 
                       dv=Voltage, 
                       wid=Subject, 
                       within=.(Condition, Hemisphere, Caudality, Specificity), 
                       within_full=.(Condition, Hemisphere, Caudality, Specificity), 
                       observed=.(Hemisphere, Caudality), 
                       detailed=TRUE)
anova_P6_lat
#$ANOVA
#Effect DFn DFd          SSn         SSd
#1                                 (Intercept)   1  10 810.49783718 347.4004010
#2                                   Condition   1  10  16.56014400  49.0817774
#3                                  Hemisphere   1  10   4.68814694  24.3285156
#4                                   Caudality   1  10  61.92769571  71.5749280
#5                                 Specificity   1  10   1.57767930  43.4698543
#6                        Condition:Hemisphere   1  10   8.46335030  29.6267414
#7                         Condition:Caudality   1  10   4.65062044  12.2749610
#8                        Hemisphere:Caudality   1  10   0.02286640   4.1754245
#9                       Condition:Specificity   1  10   2.59840745  73.1333687
#10                     Hemisphere:Specificity   1  10   1.39325566  20.7624413
#11                      Caudality:Specificity   1  10   0.71475799  27.6551963
#12             Condition:Hemisphere:Caudality   1  10   3.13199848   2.1549794
#13           Condition:Hemisphere:Specificity   1  10   0.03216501   5.5981934
#14            Condition:Caudality:Specificity   1  10   0.11586882  15.3314860
#15           Hemisphere:Caudality:Specificity   1  10   0.25331182   4.6406133
#16 Condition:Hemisphere:Caudality:Specificity   1  10   0.17946175   0.9452209
# F            p p<.05          ges
#1  23.33036562 0.0006916217     * 4.977799e-01
#2   3.37399028 0.0960940359       1.984944e-02
#3   1.92701726 0.1952315391       5.733140e-03
#4   8.65214921 0.0147487816     * 7.573145e-02
#5   0.36293641 0.5602919016       1.925631e-03
#6   2.85665919 0.1218772797       1.034984e-02
#7   3.78870485 0.0802177640       5.687249e-03
#8   0.05476425 0.8196926878       2.796335e-05
#9   0.35529711 0.5643773490       3.167530e-03
#10  0.67104616 0.4317766103       1.703814e-03
#11  0.25845341 0.6222110433       8.740783e-04
#12 14.53377473 0.0034162165     * 3.830124e-03
#13  0.05745605 0.8154046431       3.933462e-05
#14  0.07557573 0.7889812696       1.416961e-04
#15  0.54585850 0.4769977923       3.097753e-04
#16  1.89862224 0.1982822834       2.194640e-04


mydata_p6_la = subset(mydata_p6_lat, mydata_p6_lat$heca == "L.Ant")
mydata_p6_la = droplevels(mydata_p6_la)

mydata_p6_ra = subset(mydata_p6_lat, mydata_p6_lat$heca == "R.Ant")
mydata_p6_ra = droplevels(mydata_p6_ra)

mydata_p6_lp = subset(mydata_p6_lat, mydata_p6_lat$heca == "L.Post")
mydata_p6_lp = droplevels(mydata_p6_lp)

mydata_p6_rp = subset(mydata_p6_lat, mydata_p6_lat$heca == "R.Post")
mydata_p6_rp = droplevels(mydata_p6_rp)
#visualizing the data
#add +scale_x_discrete(guide = guide_axis(angle = 45)) to the plot to change the angle of the lables
#on the X axis
ggplot(mydata_p6_la, aes(x = Condition, y = Voltage)) + geom_boxplot() + facet_grid(.~ Specificity,)+
  scale_x_discrete(guide = guide_axis(angle = 45)) 
ggplot(mydata_p6_ra, aes(x = Condition, y = Voltage)) + geom_boxplot() + facet_grid(.~ Specificity,)+
  scale_x_discrete(guide = guide_axis(angle = 45)) 
ggplot(mydata_p6_lp, aes(x = Condition, y = Voltage)) + geom_boxplot() + facet_grid(.~ Specificity,)+
  scale_x_discrete(guide = guide_axis(angle = 45)) 
ggplot(mydata_p6_rp, aes(x = Condition, y = Voltage)) + geom_boxplot() + facet_grid(.~ Specificity,)+
  scale_x_discrete(guide = guide_axis(angle = 45)) 
# run anovas again with this
# LA
anova_P6_la = summary(aov(Voltage ~ Condition , data = mydata_p6_la))
anova_P6_la
#Df Sum Sq Mean Sq F value Pr(>F)
#Condition     1    7.7   7.651   1.467  0.227
#Residuals   174  907.5   5.215               
#>

#RA
anova_P6_ra = summary(aov(Voltage ~ Condition, data = mydata_p6_ra))
anova_P6_ra
#
#Df Sum Sq Mean Sq F value  Pr(>F)   
#Condition     1   43.5   43.45   8.723 0.00358 **
# Residuals   174  866.7    4.98                   
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 

ggplot(mydata_p6_ra, aes(x = Condition, y = Voltage)) + geom_boxplot()

#LP
anova_P6_lp = summary(aov(Voltage ~ Condition, data = mydata_p6_lp))
anova_P6_lp
# Df Sum Sq Mean Sq F value Pr(>F)  
#Condition     1   25.9  25.873   4.018 0.0466 *
# Residuals   174 1120.3   6.438                 
#---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 

ggplot(mydata_p6_lp, aes(x = Condition, y = Voltage)) + geom_boxplot()

#RP
anova_P6_rp = summary(aov(Voltage ~ Condition, data = mydata_p6_rp))
anova_P6_rp
#Df Sum Sq Mean Sq F value  Pr(>F)   
#Condition     1   54.2   54.25   9.139 0.00288 **
#Residuals   174 1032.8    5.94                   
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 

ggplot(mydata_p6_rp, aes(x = Condition, y = Voltage)) + geom_boxplot()
###############midline#######
# midline
library(ez)
anova_P6_mid = ezANOVA(data=mydata_p6_mid, 
                             dv=Voltage, 
                             wid=Subject, 
                             within=.(Condition, Caudality,Specificity), 
                             within_full=.(Condition, Caudality,Specificity), 
                             observed=.(Caudality), 
                             detailed=TRUE)

anova_P6_mid
#nothing?
#$ANOVA
#Effect DFn DFd         SSn       SSd          F
#1                     (Intercept)   1  10 957.9342079 510.23123 18.7745115
#2                       Condition   1  10  32.6764818  65.04237  5.0238766
#3                       Caudality   2  20  82.6441479 102.14659  8.0907395
#4                     Specificity   1  10   0.8695276  56.56300  0.1537273
#5             Condition:Caudality   2  20   3.4905455  25.71589  1.3573496
#6           Condition:Specificity   1  10   2.0011652 109.67418  0.1824646
#7           Caudality:Specificity   2  20   1.8279568  70.85612  0.2579815
#8 Condition:Caudality:Specificity   2  20   0.3804823  30.58995  0.1243815
#p p<.05          ges
#1 0.001482660     * 0.4749074368
#2 0.048887932     * 0.0299279322
#3 0.002663402     * 0.0780278296
#4 0.703224752       0.0008202843
#5 0.280046452       0.0032955714
#6 0.678314909       0.0018858215
#7 0.775141310       0.0017258512
#8 0.883720618       0.0003592294

#$`Mauchly's Test for Sphericity`
#Effect         W            p p<.05
#3                       Caudality 0.3947255 0.0152520578     *
#  5             Condition:Caudality 0.3643135 0.0106325745     *
#  7           Caudality:Specificity 0.2112735 0.0009158035     *
#  8 Condition:Caudality:Specificity 0.5489333 0.0672726010      

#$`Sphericity Corrections`
#Effect       GGe      p[GG] p[GG]<.05       HFe
#3                       Caudality 0.6229464 0.01091631         * 0.6685331
#5             Condition:Caudality 0.6113641 0.27589225           0.6522533
#7           Caudality:Specificity 0.5590569 0.64753686           0.5797897
#8 Condition:Caudality:Specificity 0.6891482 0.80762755           0.7632633
#p[HF] p[HF]<.05
#3 0.009188647         *
#  5 0.277080871          
#7 0.655696997          
#8 0.830064510  


PostHocTest(aov(Voltage ~ Caudality , data = mydata_p6_mid), method="hsd", conf.level=0.95)

#Posthoc multiple comparisons of means : Tukey HSD 
#95% family-wise confidence level

#$Caudality
#diff    lwr.ci     upr.ci   pval    
#Med-Ant   0.1373953 -1.334743  1.6095334 0.9735    
#Post-Ant -1.6055950 -2.880504 -0.3306861 0.0093 ** 
#Post-Med -1.7429904 -3.017899 -0.4680814 0.0042 ** 
  
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1



########post hoc tests for the lateral data#######

PostHocTest(aov(Voltage ~ Caudality , data = mydata_p6_lat), method="hsd", conf.level=0.95)

#Posthoc multiple comparisons of means : Tukey HSD 
#95% family-wise confidence level

#$Caudality
#diff    lwr.ci     upr.ci   pval    
#Post-Ant -1.186359 -1.543025 -0.8296933 <2e-16 ***
  
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


PostHocTest(aov(Voltage ~ Condition , data = mydata_p6_la), method="hsd", conf.level=0.95)
#Posthoc multiple comparisons of means : Tukey HSD 
#95% family-wise confidence level

#$Condition
#diff    lwr.ci    upr.ci   pval    
#AverageUngrammatical-AverageGrammatical -0.4169967 -1.096499 0.2625059 0.2275    

#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  
  
PostHocTest(aov(Voltage ~ Condition , data = mydata_p6_ra), method="hsd", conf.level=0.95)

#Posthoc multiple comparisons of means : Tukey HSD 
#95% family-wise confidence level

#$Condition
#diff   lwr.ci   upr.ci   pval    
#AverageUngrammatical-AverageGrammatical 0.7915735 0.142789 1.440358 0.0171 *  
  
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

 # ---
PostHocTest(aov(Voltage ~ Condition , data = mydata_p6_lp), method="hsd", conf.level=0.95)
#Posthoc multiple comparisons of means : Tukey HSD 
#95% family-wise confidence level

#$Condition
#diff     lwr.ci   upr.ci   pval    
#AverageUngrammatical-AverageGrammatical 0.7668198 0.01182639 1.521813 0.0466 *  
  
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


#> 
PostHocTest(aov(Voltage ~ Condition , data = mydata_p6_rp), method="hsd", conf.level=0.95)
#Posthoc multiple comparisons of means : Tukey HSD 
#95% family-wise confidence level

#$Condition
#diff    lwr.ci   upr.ci   pval    
#AverageUngrammatical-AverageGrammatical 1.110373 0.3854545 1.835292 0.0029 ** 
  
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1



#data summary

fmean <- mydata_p6_lat %>%
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
#Condition            Specificity Hemisphere Caudality  mean median    sd     n   var
#<fct>                <fct>       <fct>      <fct>     <dbl>  <dbl> <dbl> <int> <dbl>
#1 AverageGrammatical   spec        L          Ant       2.74   2.88   1.93    44  3.72
#2 AverageGrammatical   spec        L          Post      0.848  0.877  2.26    44  5.12
#3 AverageGrammatical   spec        R          Ant       2.47   2.97   2.38    44  5.68
#4 AverageGrammatical   spec        R          Post      1.19   1.15   2.15    44  4.64
#5 AverageGrammatical   unspec      L          Ant       2.85   2.73   2.71    44  7.37
#6 AverageGrammatical   unspec      L          Post      1.14   0.634  2.71    44  7.34
#7 AverageGrammatical   unspec      R          Ant       2.31   2.90   2.07    44  4.31
#8 AverageGrammatical   unspec      R          Post      1.16   1.16   2.40    44  5.76
#9 AverageUngrammatical spec        L          Ant       2.65   2.14   2.35    44  5.50
#10 AverageUngrammatical spec        L          Post      1.72   1.12   2.67    44  7.15
#11 AverageUngrammatical spec        R          Ant       3.73   3.32   2.60    44  6.74
#12 AverageUngrammatical spec        R          Post      2.58   2.15   2.93    44  8.57
#13 AverageUngrammatical unspec      L          Ant       2.11   2.41   2.09    44  4.35
#14 AverageUngrammatical unspec      L          Post      1.81   1.29   2.53    44  6.40
#15 AverageUngrammatical unspec      R          Ant       3.05   3.30   1.79    44  3.19
#16 AverageUngrammatical unspec      R          Post      1.98   1.76   2.21    44  4.87
