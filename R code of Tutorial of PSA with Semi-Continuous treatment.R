
newdata=read.csv("Data of Algebra Nation.csv")

## dispersion test to select appropriate distribution
library(AER)

rd <- glm(followed ~ ., data = newdata, family = poisson)
dispersiontest(rd,trafo=1)


### ZINB model
tryzi=zeroinfl(followed ~ pretest + yearsteaching + yearsAN + ANTotalTime + 
                 minority + lowses + tquestion + mengagement  +coursetype_1+ clusterid_1  +clusterid_2 + clusterid_3 + 
                 clusterid_4 + clusterid_5 + clusterid_6 + clusterid_7 + clusterid_8 + 
                 clusterid_9 + clusterid_10 + clusterid_11 + clusterid_12 + 
                 clusterid_13 + clusterid_14 + clusterid_15 + clusterid_16 + 
                 clusterid_17 + clusterid_18 + clusterid_19  + 
                 districtname_1 + districtname_2 + sectionid_1 + 
                 sectionid_2 + sectionid_3 , data=newdata, dist = "negbin", link="logit")

outzi=summary(tryzi)
zczi=outzi$coefficients$zero[,1] # parameters for binary part
nzczi=outzi$coefficients$count[,1] # parameters for positive part
theta.hat=outzi$theta #

# gps of zinb   conditional mean
pnzi=exp(zczi[1]+ as.matrix(newdata1[,c(2:9, 12:36)]) %*% zczi[2:34])
pzi=exp(nzczi[1]+ as.matrix(newdata1[,c(2:9, 12:36)]) %*% nzczi[2:34])
GPSzi=pzi/(1+pnzi)  
summary(GPSzi)
newdata1$GPSzi=GPSzi


# covaraite balance
covariatesname=names(newdata1[,c(2:9, 12:36)])
balancetable=data.frame()
for (var in 1:length(covariatesname)){
  balformula=paste("followed~GPSzi+", covariatesname[var], sep="")
  maxeff=max(abs(coef(lm(balformula, newdata))[-(1:2)]))
  balancetable=rbind(balancetable, c(var, maxeff))
  
}
names(balancetable)=c("variable", "coef")
balancetable$variable=covariatesname
balancetable$coef=balancetable$coef/sd(newdata1$followed)


meanstcoef=mean(balancetable$coef)
which(balancetable$coef>0.05)


###ATE
outzinb=glm(posttest~followed+GPSzi+mengagement+coursetype_1+clusterid_1 + clusterid_7+
              +districtname_1 +  districtname_2, data=newdata)
resultzinb=summary(outzinb)
resultzinb$coefficients[2,]
