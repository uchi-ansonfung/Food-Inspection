
# GLM selection ABQ ------------------------------------------------

#cluster at restaruant level for each locale
##we are using glmmTMB since it can use all 6 CPU cores (change to core count you have)
##no random intercept better

abq0.1 <- glmmTMB(totalct ~ 1 + (1|FacilityID), family ="poisson", 
                  control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mabq)

abq0.2 <- glmmTMB(notminorct ~ 1 + (1|FacilityID), family ="poisson", 
                  control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mabq)

abq0.3 <- glmmTMB(totalct ~ type_mobile + (1|FacilityID), family ="poisson", 
                  control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mabq)
##test_performance shows abq0.3 much better than intercept only. good.

abq0.4 <- glmmTMB(notminorct ~ type_mobile + (1|FacilityID), family ="poisson", 
                  control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mabq)
##test_performance shows abq0.4 much better than intercept only. good.

##add in weather controls
abq1.1 <- glmmTMB(totalct ~ type_mobile + prcp + tmax + (1|FacilityID), family = "poisson", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mabq)

abq1.2 <- glmmTMB(notminorct ~ type_mobile + prcp + tmax + (1|FacilityID), family = "poisson", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mabq)

##alternative, controls by time
abq2.1 <- glmmTMB(totalct ~ type_mobile +  year_2022 + (1|FacilityID), family = "poisson", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mabq)

abq2.2 <- glmmTMB(notminorct ~ type_mobile + year_2022 + (1|FacilityID), family = "poisson", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mabq)

##alternative, controls by census

abq3.1 <- glmmTMB(totalct ~ type_mobile + income + age + pct_white + (1|FacilityID), family = "poisson", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mabq)

abq3.2 <- glmmTMB(notminorct ~ type_mobile + income + age + pct_white + (1|FacilityID), family = "poisson", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mabq)


##alternative, weather and census controls

abq4.1 <- glmmTMB(totalct ~ type_mobile + prcp + tmax + income + age + pct_white + (1|FacilityID), family = "poisson", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mabq)

abq4.2 <- glmmTMB(notminorct ~ type_mobile + prcp + tmax + income + age + pct_white + (1|FacilityID), family = "poisson", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mabq)

##alternative, weather and time controls

abq5.1 <- glmmTMB(totalct ~ type_mobile + prcp + tmax + dotw + month + year + (1|FacilityID), family = "poisson", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mabq)

abq5.2 <- glmmTMB(notminorct ~ type_mobile + prcp + tmax + dotw + month + year + (1|FacilityID), family = "poisson", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mabq)

##compare performance
compare_performance(abq0.3,abq1.1,abq1.3,abq2.1,abq3.1,abq4.1,abq5.1)
test_performance(abq0.3,abq1.1,abq1.3,abq2.1,abq3.1,abq4.1,abq5.1) ##abq3.1 is best

compare_performance(abq0.4,abq1.2,abq2.2,abq3.2,abq4.2,abq5.2)
test_performance(abq0.4,abq1.2,abq2.2,abq3.2,abq4.2,abq5.2)  ##abq3.2 and abq4.2 are good; abq3.2 has lower BIC.

##select abq3.1 and abq3.2 as our models to output
tab_model(abq0.3,abq1.1,abq2.1,abq3.1,abq0.4,abq1.2,abq2.2,abq3.2, file = "Outputs/Models/Abq.html")
tab_model(abq2.1,abq2.2, file = "Outputs/Models/Abq212.html")

# GLM Selection Bos -------------------------------------------------------

#cluster at restaruant level for each locale
##we are using glmmTMB since it can use all 6 CPU cores (change to core count you have in 00_Setup)
##overdispersion so we'll use nbinom

bos0.1 <- glmmTMB(totalct ~ 1 + (1|FacilityID), family ="nbinom2", 
                  control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mbos)

bos0.2 <- glmmTMB(notminorct ~ 1 + (1|FacilityID), family ="nbinom2", 
                  control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mbos)

bos0.3 <- glmmTMB(totalct ~ type_mobile + (1|FacilityID), family ="nbinom2", 
                  control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mbos)
##test_performance(bos0.1,bos0.3) ##bos0.3 is much better. good.

bos0.4 <- glmmTMB(notminorct ~ type_mobile + (1|FacilityID), family ="nbinom2", 
                  control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mbos)
##test_performance(bos0.2,bos0.4) ##bos0.4 is a little better, but not by much.

##add in weather controls
bos1.1 <- glmmTMB(totalct ~ type_mobile + prcp + tmax + (1|FacilityID), family = "nbinom2", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mbos)

bos1.2 <- glmmTMB(notminorct ~ type_mobile + prcp + tmax + (1|FacilityID), family = "nbinom2", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mbos)

##alternative, controls by time
bos2.1 <- glmmTMB(totalct ~ type_mobile + dotw + month + year + (1|FacilityID), family = "nbinom2", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mbos)

bos2.2 <- glmmTMB(notminorct ~ type_mobile + dotw + month + year + (1|FacilityID), family = "nbinom2", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mbos)

##alternative, controls by census

bos3.1 <- glmmTMB(totalct ~ type_mobile + income + age + pct_white + (1|FacilityID), family = "nbinom2", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mbos)

bos3.2 <- glmmTMB(notminorct ~ type_mobile + income + age + pct_white + (1|FacilityID), family = "nbinom2", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mbos)

##alternative, weather and census controls

bos4.1 <- glmmTMB(totalct ~ type_mobile + prcp + tmax + income + age + pct_white + (1|FacilityID), family = "nbinom2", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mbos)

bos4.2 <- glmmTMB(notminorct ~ type_mobile + prcp + tmax + income + age + pct_white + (1|FacilityID), family = "nbinom2", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mbos)


##alternative, weather and time controls

bos5.1 <- glmmTMB(totalct ~ type_mobile + prcp + tmax + dotw + month + year + (1|FacilityID), family = "nbinom2", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mbos)

bos5.2 <- glmmTMB(notminorct ~ type_mobile + prcp + tmax + dotw + month + year + (1|FacilityID), family = "nbinom2", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mbos)

##select  models to output
tab_model(bos0.3,bos1.1,bos4.1,bos5.1,bos0.4,bos1.2,bos4.2,bos5.2, file = "Outputs/Models/Bos.html")

# GLM Selection Chi -------------------------------------------------------

#cluster at restaruant level for each locale
##we are using glmmTMB since it can use all 6 CPU cores (change to core count you have)
##overdispersion

chi0.1 <- glmmTMB(totalct ~ 1 + (1|FacilityID), family ="nbinom2", 
                  control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mchi)

chi0.2 <- glmmTMB(notminorct ~ 1 + (1|FacilityID), family ="nbinom2", 
                  control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mchi)

chi0.3 <- glmmTMB(totalct ~ type_mobile + (1|FacilityID), family ="nbinom2", 
                  control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mchi)
##test_performance much better.

chi0.4 <- glmmTMB(notminorct ~ type_mobile + (1|FacilityID), family ="nbinom2", 
                  control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mchi)
##not much change.

##add in weather controls
chi1.1 <- glmmTMB(totalct ~ type_mobile + prcp + tmax + (1|FacilityID), family = "nbinom2", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mchi)

chi1.2 <- glmmTMB(notminorct ~ type_mobile + prcp + tmax + (1|FacilityID), family = "nbinom2", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mchi)

##alternative, controls by time
chi2.1 <- glmmTMB(totalct ~ type_mobile + dotw + month + year + (1|FacilityID), family = "nbinom2", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mchi)

chi2.2 <- glmmTMB(notminorct ~ type_mobile + dotw + month + year + (1|FacilityID), family = "nbinom2", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mchi)

##alternative, controls by census

chi3.1 <- glmmTMB(totalct ~ type_mobile + income + age + pct_white + (1|FacilityID), family = "nbinom2", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mchi)

chi3.2 <- glmmTMB(notminorct ~ type_mobile + income + age + pct_white + (1|FacilityID), family = "nbinom2", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mchi)

##alternative, weather and census controls

chi4.1 <- glmmTMB(totalct ~ type_mobile + prcp + tmax + income + age + pct_white + (1|FacilityID), family = "nbinom2", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mchi)

chi4.2 <- glmmTMB(notminorct ~ type_mobile + prcp + tmax + income + age + pct_white + (1|FacilityID), family = "nbinom2", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mchi)

##alternative, weather and time controls

chi5.1 <- glmmTMB(totalct ~ type_mobile + prcp + tmax + dotw + month + year + (1|FacilityID), family = "nbinom2", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mchi)
##performance strongly favors.

chi5.2 <- glmmTMB(notminorct ~ type_mobile + prcp + tmax + dotw + month + year + (1|FacilityID), family = "nbinom2", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mchi)

##select  models to output
tab_model(chi0.3,chi1.1,chi0.4,chi1.2,chi5.2, file = "Outputs/Models/Chi.html")
tab_model(chi2.1,chi2.2, file = "Outputs/Models/Chi2.html")

# GLM Selection Louisv ----------------------------------------------------

#cluster at restaruant level for each locale
##we are using glmmTMB since it can use all 6 CPU cores (change to core count you have)
##no overdispersion, poisson is OK

louisv0.1 <- glmmTMB(totalct ~ 1 + (1|FacilityID), family ="poisson", 
                  control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mlouisv)

louisv0.2 <- glmmTMB(criticalct ~ 1 + (1|FacilityID), family ="poisson", 
                  control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mlouisv)

louisv0.3 <- glmmTMB(totalct ~ type_mobile + (1|FacilityID), family ="poisson", 
                  control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mlouisv)

trunc1 <- glmmTMB(totalct ~ type_mobile + (1|FacilityID), family ="truncated_poisson",ziformula = ~., 
                  control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mlouisv)

trunc2 <- glmmTMB(totalct ~ type_mobile + (1|FacilityID), family ="truncated_nbinom2",ziformula = ~., 
                  control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L),optimizer=optim,
                                           optArgs=list(method="BFGS")), data=mlouisv)

trunc3 <- glmmTMB(totalct ~ type_mobile + (1|FacilityID), family ="truncated_nbinom1",ziformula = ~., 
                  control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L),optimizer=optim,
                                           optArgs=list(method="BFGS")), data=mlouisv)

louisv0.4 <- glmmTMB(criticalct ~ type_mobile + (1|FacilityID), family ="poisson", 
                  control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mlouisv)

louisv0.5 <- glmmTMB(totalct ~ type_mobile + proxban + (1|FacilityID), family ="poisson", 
                     control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mlouisv)
##test_performance slightly favors louisv0.5, although its marginal.  Favor simpler model.

louisv0.6 <- glmmTMB(criticalct ~ type_mobile + proxban + (1|FacilityID), family ="poisson", 
                     control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mlouisv)
##way better than baseline.

##add in weather controls
louisv1.1 <- glmmTMB(totalct ~ type_mobile + prcp + tmax + (1|FacilityID), family = "poisson", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mlouisv)

louisv1.2 <- glmmTMB(criticalct ~ type_mobile + prcp + tmax + (1|FacilityID), family = "poisson", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mlouisv)

louisv1.3 <- glmmTMB(totalct ~ type_mobile + prcp + tmax + proxban + (1|FacilityID), family = "poisson", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mlouisv)

louisv1.4 <- glmmTMB(criticalct ~ type_mobile + prcp + tmax + proxban + (1|FacilityID), family = "poisson", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mlouisv)


##alternative, controls by time.  Fails to converge.
louisv2.1 <- glmmTMB(totalct ~ type_mobile + dotw + month + year + (1|FacilityID), family = "poisson", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mlouisv)
##no appreciable incerase in model quality.

louisv2.2 <- glmmTMB(criticalct ~ type_mobile + dotw + month + year + (1|FacilityID), family = "poisson", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mlouisv)

louisv2.4 <- glmmTMB(criticalct ~ type_mobile + dotw + month + year + proxban + (1|FacilityID), family = "poisson", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mlouisv)
##this has high risk of correlation between time vars and proxban; although it passes multicollinearity tests

##alternative, controls by census. Fails to converge.

louisv3.1 <- glmmTMB(totalct ~ type_mobile + income + age + pct_white + (1|FacilityID), family = "poisson", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mlouisv)

louisv3.2 <- glmmTMB(criticalct ~ type_mobile + income + age + pct_white + (1|FacilityID), family = "poisson", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mlouisv)

louisv3.4 <- glmmTMB(criticalct ~ type_mobile + income + age + pct_white + proxban + (1|FacilityID), family = "poisson", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mlouisv)


##alternative, weather and census controls. Fails to converge.

louisv4.1 <- glmmTMB(totalct ~ type_mobile + prcp + tmax + income + age + pct_white + (1|FacilityID), family = "poisson", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mlouisv)

louisv4.2 <- glmmTMB(criticalct ~ type_mobile + prcp + tmax + income + age + pct_white + (1|FacilityID), family = "poisson", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mlouisv)

louisv4.4 <- glmmTMB(criticalct ~ type_mobile + prcp + tmax + income + age + pct_white + proxban + (1|FacilityID), family = "poisson", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mlouisv)


##alternative, weather and time controls

louisv5.1 <- glmmTMB(totalct ~ type_mobile + prcp + tmax + dotw + month + year + (1|FacilityID), family = "poisson", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mlouisv)
##no appreciable increase in model quality, favor simpler model.

louisv5.2 <- glmmTMB(criticalct ~ type_mobile + prcp + tmax + dotw + month + year + (1|FacilityID), family = "poisson", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mlouisv)

louisv5.4 <- glmmTMB(criticalct ~ type_mobile + prcp + tmax + dotw + month + year + proxban + (1|FacilityID), family = "poisson", control = glmmTMBControl(parallel = getOption("glmmTMB.cores", 1L)), data=mlouisv)
##this has high risk of correlation between time vars and proxban; although it passes multicollinearity tests

##select louisv0.3 and louisv1.4 as our models to output
tab_model(louisv0.3,louisv1.1, louisv1.3, louisv0.4, louisv1.2,louisv1.4, file = "Outputs/Models/Louisv.html")



