#chi-squared test

#Stress tolerance common garden

###harvest, control, bolt date###
chisq.test(modeldata$BoltedatH, modeldata$Origin)
plot(modeldata$BoltedatH, modeldata$Origin, xlab="Bolted by harvest?", ylab="Origin", 
     main="Proportion bolted at harvest")

####lf disc, most eaten, from scan, binomial####
modeldata<-lfdisc[!is.na(lfdisc$eat.bin),]

chisq.test(modeldata$eat.bin, modeldata$Origin)
#plot(modeldata$BoltedatH, modeldata$Origin, xlab="Bolted by harvest?", ylab="Origin", 
     main="Proportion bolted at harvest")


#####if anova of mixed models give result with 0 d.f.###########
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# Data: modeldata
# Models:
#   model3: CrownH.log ~ Origin + (1 | blank)
# model2: CrownH.log ~ Origin + (1 | PopID)
# Df     AIC     BIC logLik  Chisq Chi Df Pr(>Chisq)    
# model3  4 -125.05 -110.90 66.523                             
# model2  4 -162.13 -147.98 85.066 37.085      0  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

dchisq(X,df) #where X is the Chisq value from the anova table (37.085 in your case) 
#and df is Df in the anova table +1 (0 + 1 in your case)

#So I get 5.7997e-10 whereas the table says 2.2e-16 so you could still just report P<0.001, 
#but this obviously will matter if you have p-values > 0.001