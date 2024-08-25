length(BRA2$Golcasa[BRA2$Temporada=="2012"])
length(BRA2$Golcasa[BRA2$Temporada=="2018"])
table(BRA2$Golcasa[BRA2$Temporada])

sum((BRA2$Golcasa[BRA2$Temporada=="2012"]))
n=tapply(BRA2$Golcasa,BRA2$Temporada,sum)
View(n)

tapply(BRA2$Res=="C", BRA2$Temporada=="2012", sum)
v=tapply(BRA2$Res=="C", BRA2$Temporada, sum)
View(v)

c=tapply(BRA2$Res=="C",BRA2$Temporada,sum)
medc=tapply(BRA2$Res=="C",BRA2$Temporada,mean)
medianc=tapply(BRA2$Res=="C",BRA2$Temporada,median)

e=tapply(BRA2$Res=="E",BRA2$Temporada,sum)
mede=tapply(BRA2$Res=="E",BRA2$Temporada,mean)
mediane=tapply(BRA2$Res=="E",BRA2$Temporada,median)

v=tapply(BRA2$Res=="V",BRA2$Temporada,sum)
miv=tapply(BRA2$Res=="V",BRA2$Temporada,mean)
medianv=tapply(BRA2$Res=="V",BRA2$Temporada,median)

g=tapply(BRA2$Total,BRA2$Temporada,sum)
med=round(tapply(BRA2$Total,BRA2$Temporada,mean),3)
medi=round(tapply(BRA2$Total,BRA2$Temporada,median),3)

#histograma do total#
par(mfrow=c(1,2))
hist(med,probability = TRUE,
     main="Hist. médias total",
     ylab = "densidade",
     xlab="média de total",
     ylim = c(0,10))
plot(density(mede),main = "Hist. medianas total",
     xlab = "média do total",
     ylab = "densidade de frequencia",
     col="blue",
     lwd=3)
lines(density(mede),col="green",lwd=3)

hist(mediane,probability = TRUE,
     main="Hist. mediana total",
     ylab = "densidade",
     xlab="mediana do total",
     ylim = c(0,10))
plot(density(mede),main = "Hist. mediana total",
     xlab = "mediana do total",
     ylab = "densidade de frequencia",
     col="blue",
     lwd=3)
lines(density(mede),col="green",lwd=3)
############################################
propor_c=round((c*100)/380,2)
View(propor_c)

propor_e=round((e*100)/380,2)
View(propor_e)

propor_v=round((v*100)/380,2)
View(propor_v)

tabpropor=cbind(propor_c,propor_e,propor_v,n,v,g)
View(tabpropor)
colnames(tabpropor)=c("proporção C", "proporção E", "proporção V","golcasa",
                      "vitórias mandante","total de gols")

#histogramas médias#
par(mfrow=c(1,3))
hist(mede,probability = TRUE,
     main="Hist. médias E",
     ylab = "densidade",
     xlab="média de empates por temporada",
     ylim = c(0,40))
plot(density(mede),main = "Hist. médias E",
     xlab = "média dos empates por temporada",
     ylab = "densidade de frequencia",
     col="blue",
     lwd=3)
lines(density(mede),col="green",lwd=3)

hist(medc,probability = TRUE,
     main="Hist. médias C",
     ylab = "densidade",
     xlab="média de C por temporada",
     ylim = c(0,20))
plot(density(medc),main = "Hist. médias C",
     xlab = "média dos C por temporada",
     ylab = "densidade de frequencia",
     col="blue",
     lwd=3)
lines(density(medc),col="green",lwd=3)

hist(medv,probability = TRUE,
     main="Hist. médias V",
     ylab = "densidade",
     xlab="média de V por temporada",
     ylim = c(0,20))
plot(density(medv),main = "Hist. médias V",
     xlab = "média dos V por temporada",
     ylab = "densidade de frequencia",
     col="blue",
     lwd=3)
lines(density(medv),col="green",lwd=3)

#histograma das medianas####
par(mfrow=c(1,3))
hist(mediane,probability = TRUE,
     main="Hist. medianas E",
     ylab = "densidade",
     xlab="medianas de empates por temporada")
plot(density(mede),main = "Hist. mmedianas E",
     xlab = "mediana dos empates por temporada",
     ylab = "densidade de frequencia",
     col="blue",
     lwd=3)
lines(density(mede),col="green",lwd=3)

hist(medc,probability = TRUE,
     main="Hist. mediana C",
     ylab = "densidade",
     xlab="mediana de C por temporada",
     ylim = c(0,20))
plot(density(medc),main = "Hist. mediana C",
     xlab = "mediana dos C por temporada",
     ylab = "densidade de frequencia",
     col="blue",
     lwd=3)
lines(density(medc),col="green",lwd=3)

hist(medv,probability = TRUE,
     main="Hist. mediana V",
     ylab = "densidade",
     xlab="mediana de V por temporada",
     ylim = c(0,20))
plot(density(medv),main = "Hist. mediana V",
     xlab = "mediana dos V por temporada",
     ylab = "densidade de frequencia",
     col="blue",
     lwd=3)
lines(density(medv),col="green",lwd=3)
###carol#######################################################

"TABELA POR ESTADO"


TM= BRA2[BRA2$Estado=="Sao Paulo",]
View (TM )


tapply(TM$Golcasa, TM$Periodo=="Noite",sum)
b =tapply(TM$Golcasa, TM$Periodo=="Noite",sum)
View(b)
######################################################


TF= BRA2[BRA2$Estado=="Rio de Janeiro",]
View (TF)


tapply(TF$Golcasa, TF$Periodo=="Noite",sum)
c=tapply(TF$Golcasa, TF$Periodo=="Noite",sum)
View(c)


#####################################################

TG= BRA2[BRA2$Estado=="Rio Grande do Sul",]
View (TG)


tapply(TG$Golcasa, TG$Periodo=="Noite",sum)
d=tapply(TG$Golcasa, TG$Periodo=="Noite",sum)
View(d)


########################################
TH= BRA2[BRA2$Estado=="Parana",]
View (TH)


tapply(TH$Golcasa, TH$Periodo=="Noite",sum)
e=tapply(TH$Golcasa, TH$Periodo=="Noite",sum)
View(e)


#########################################################

TI= BRA2[BRA2$Estado=="Santa Catarina",]
View (TI)


tapply(TI$Golcasa, TI$Periodo=="Noite",sum)
f=tapply(TI$Golcasa, TI$Periodo=="Noite",sum)
View(f)

###############################################

TJ= BRA2[BRA2$Estado=="Minas Gerais",]
View (TJ)


tapply(TJ$Golcasa, TJ$Periodo=="Noite",sum)
j=tapply(TJ$Golcasa, TJ$Periodo=="Noite",sum)
View(j)


##################################################


TK= BRA2[BRA2$Estado=="Bahia",]
View (TK)


tapply(TK$Golcasa, TK$Periodo=="Noite",sum)
g=tapply(TK$Golcasa, TK$Periodo=="Noite",sum)
View(g)

############################################

TL= BRA2[BRA2$Estado=="Pernambuco",]
View (TL)


tapply(TL$Golcasa,TL$Periodo=="Noite",sum)
h=tapply(TL$Golcasa, TL$Periodo=="Noite",sum)
View(h)
###########################################

TR= BRA2[BRA2$Estado=="Goias",]
View (TR)


tapply(TR$Golcasa,TR$Periodo=="Noite",sum)
i=tapply(TR$Golcasa, TR$Periodo=="Noite",sum)
View(i)
#################################################

tabela_certa = rbind(b,c,d,e,f,j,g,h,i)
View(tabela_certa)
#o cbind deixa os numeros um do lado do outro, tipo uma tabela no console 
# o rbind deixa como linhas 

rownames(tabela_certa)= c ("Sao Paulo","Rio de Janeiro",
                           "Rio Grande do Sul","Parana",
                           "Santa Catarina","Minas Gerais",
                           "Bahia","Pernambuco","Goias")
View(tabela_certa)

colnames(tabela_certa)= c ("Tarde","Noite")
View(tabela_certa)

par(mfrow=c(1,2))
hist(BRA2$Total[BRA2$Periodo=="Noite"],probability = TRUE,
     main="total noite",
     ylab = "densidade",
     xlab="gols por rodada",
     ylim = c(0,0.4))
lines(density(BRA2$Total),col="green",lwd=3)

hist(BRA2$Total[BRA2$Periodo=="Tarde"],probability = TRUE,
     main="total dia",
     ylab = "densidade",
     xlab="gols por rodada",
     ylim = c(0,0.5))
lines(density(BRA2$Total),col="blue",lwd=3)



