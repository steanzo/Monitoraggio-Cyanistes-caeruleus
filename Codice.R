na_get = function(data){
  na_vars = sapply(data, function(col) sum(is.na(col))) #contiamo gli na per colonna
  na_vars = sort(na_vars[na_vars > 0]) # ordiniamo per ordine
  na_vars = data.frame(
    variabile = names(na_vars),
    freq_assoluta = as.numeric(na_vars),
    freq_relativa = round(as.numeric(na_vars)/nrow(data), 4)
  )
  na_vars
}

library(dplyr)


data = read.csv("Dataset.csv", na.strings = ".")

indici_ignore <- c(1,10,11,12,20,27,30,31,32,42,44,48,49,50,52,
                   seq(54,58),63,65,66,69,70,72,79,83,seq(86,90),
                   92,93,seq(96,100),seq(102,107),seq(109,111),
                   seq(113,124),146,147,157,159,161,162)

data = tibble(data[,-indici_ignore])


####### Imputazione NA
#######
#imputazione stagione
range(data$jan.date.of.adult.measurment[data$season_color=="s"], na.rm=TRUE)
range(data$jan.date.of.adult.measurment[data$season_color=="w" & data$jan.date.of.adult.measurment<150], na.rm=TRUE)
range(data$jan.date.of.adult.measurment[data$season_color=="w" & data$jan.date.of.adult.measurment>150], na.rm=TRUE)


hist(data$jan.date.of.adult.measurment, nclass=200)
abline(v=9)
abline(v=49)
abline(v=122)
abline(v=160)
abline(v=337)
abline(v=353)

abline(v=79, col="red")
abline(v=171, col="red")
abline(v=355, col="red")

abline(v=100, col="green", lwd=3)
abline(v=280, col="green", lwd=3)


library(ggplot2)
ggplot() + 
  geom_histogram(aes(x = data$jan.date.of.adult.measurment), bins = 200) +
  geom_vline(aes(xintercept=79,color="soglia reali"), linetype="dashed", size=0.5) +
  geom_vline(aes(xintercept=171,color="soglia reali"), linetype="dashed", size=0.5) +
  geom_vline(aes(xintercept=355,color="soglia reali"), linetype="dashed", size=0.5) +
  geom_vline(aes(xintercept=105,color="soglia considerata"), linetype="dashed", size=0.5) +
  geom_vline(aes(xintercept=280,color="soglia considerata"), linetype="dashed", size=0.5) +
  geom_vline(aes(xintercept=9,color="soglia osservata"), linetype="dashed", size=0.5) +
  geom_vline(aes(xintercept=49,color="soglia osservata"), linetype="dashed", size=0.5) +
  geom_vline(aes(xintercept=122,color="soglia osservata"), linetype="dashed", size=0.5) +
  geom_vline(aes(xintercept=160,color="soglia osservata"), linetype="dashed", size=0.5) +
  geom_vline(aes(xintercept=337,color="soglia osservata"), linetype="dashed", size=0.5) +
  geom_vline(aes(xintercept=353,color="soglia osservata"), linetype="dashed", size=0.5) +
  scale_color_manual(name = NULL, values = c(`soglia reali` = "red", `soglia considerata` = "green", `soglia osservata` = "gray")) +
  theme_bw() +
  xlab("Giorni") + ylab("Frequenza") +
  ggtitle("Istogramma giorni di misurazione")

data$season_color[data$jan.date.of.adult.measurment > 100 & data$jan.date.of.adult.measurment < 280] = "s"
data$season_color[!(data$jan.date.of.adult.measurment > 100 & data$jan.date.of.adult.measurment < 280)] = "w"


####### 

####### 
#imputazione NA variabili ambientali con media per box

tmp = data.frame(rbind(cbind(data$ckr_ln.polygon,
                             data$ckr_BT50m,
                             data$ckr_ln.NNBT,
                             data$ckr_GT50m,
                             data$ckr_Oaks50m,
                             data$ckr_sqrt.edge,
                             data$ckr_Altitude.m.,
                             data$ckr_Calcium,
                             data$rear.Box),
                       cbind(data$ad_ln.polygon,
                             data$ad_BT50m,
                             data$ad_ln.NNBT,
                             data$ad_GT50m,
                             data$ad_Oaks50m,
                             data$ad_sqrt.edge,
                             data$ad_Altitude.m.,
                             data$ad_Calcium,
                             data$adult.Boxno)))
colnames(tmp) = c("mean.ln.polygon", "mean.BT50m", "mean.ln.NNBT", "mean.GT50m", "mean.Oaks50m", "mean.sqrt.edge", "mean.Altitude.m.", "mean.Calcium", "BoxNum")
tmp = tmp[!is.na(tmp$BoxNum),]
tmp = tibble(tmp)

print(group_by(tmp, BoxNum) %>% summarise_all(function(x){var(as.numeric(x), na.rm=TRUE)}), n=10)

mean.by.box = group_by(tmp, BoxNum) %>% summarise_all(function(x){mean(as.numeric(x), na.rm=TRUE)})

tmp.2 = left_join(data[,grep("ckr_|rear.Box", colnames(data))][,-(1+c(1,4,8))], mean.by.box, join_by("rear.Box"=="BoxNum"))
tmp.2 = tmp.2[!is.na(tmp.2$rear.Box),]

mask = is.na(tmp.2)[,grep("ckr_", colnames(tmp.2))]
tmp.2 = data.frame(tmp.2)
tmp.2[,grep("ckr_", colnames(tmp.2))][mask] = tmp.2[,grep("mean.", colnames(tmp.2))][mask]


data$ckr_BT50m = as.double(data$ckr_BT50m)
data$ckr_GT50m = as.double(data$ckr_GT50m)
data$ckr_Oaks50m = as.double(data$ckr_Oaks50m)
data[,grep("ckr_", colnames(data))][,-c(1,4,8)][!is.na(data$rear.Box),] = tibble(tmp.2[,grep("ckr_", colnames(tmp.2))])

sort(colSums(is.na(data))/nrow(data))


tmp.2 = left_join(data[,grep("ad_|adult.Boxno", colnames(data))][,-c(1,2+c(1,4,8))], mean.by.box, join_by("adult.Boxno"=="BoxNum"))
tmp.2 = tmp.2[!is.na(tmp.2$adult.Boxno),]

mask = is.na(tmp.2)[,grep("ad_", colnames(tmp.2))]
tmp.2 = data.frame(tmp.2)
tmp.2[,grep("ad_", colnames(tmp.2))][mask] = tmp.2[,grep("mean.", colnames(tmp.2))][mask]


data$ad_BT50m = as.double(data$ad_BT50m)
data$ad_GT50m = as.double(data$ad_GT50m)
data$ad_Oaks50m = as.double(data$ad_Oaks50m)
data$ad_Calcium = as.double(data$ad_Calcium)
data[,grep("ad_", colnames(data))][,-c(1,1+c(1,4,8))][!is.na(data$adult.Boxno),] = tibble(tmp.2[,grep("ad_", colnames(tmp.2))])


plot(data$ckr_Altitude.m., data$ckr_Calcium, pch=16, cex=0.5)

#imputazione NA restanti variabili ambientali con media per area (avrebbe senso imputare incrociando le altre info ambientali... ma per tante oss. mancano tutte)
tmp = data.frame(rbind(cbind(data$ckr_ln.polygon,
                             data$ckr_BT50m,
                             data$ckr_ln.NNBT,
                             data$ckr_GT50m,
                             data$ckr_Oaks50m,
                             data$ckr_sqrt.edge,
                             data$ckr_Altitude.m.,
                             data$ckr_Calcium,
                             data$rear.area),
                       cbind(data$ad_ln.polygon,
                             data$ad_BT50m,
                             data$ad_ln.NNBT,
                             data$ad_GT50m,
                             data$ad_Oaks50m,
                             data$ad_sqrt.edge,
                             data$ad_Altitude.m.,
                             data$ad_Calcium,
                             data$adult.area)))
colnames(tmp) = c("mean.ln.polygon", "mean.BT50m", "mean.ln.NNBT", "mean.GT50m", "mean.Oaks50m", "mean.sqrt.edge", "mean.Altitude.m.", "mean.Calcium", "Area")
tmp = tmp[!is.na(tmp$Area),]
tmp = tibble(tmp)

print(group_by(tmp, Area) %>% summarise_all(function(x){var(as.numeric(x), na.rm=TRUE)}), n=10)

mean.by.area = group_by(tmp, Area) %>% summarise_all(function(x){mean(as.numeric(x), na.rm=TRUE)})

tmp.2 = left_join(data[,grep("ckr_|rear.area", colnames(data))][,-(1+c(1,4,8))], mean.by.area, join_by("rear.area"=="Area"))
tmp.2 = tmp.2[!is.na(tmp.2$rear.area),]


mask = is.na(tmp.2)[,grep("ckr_", colnames(tmp.2))]
tmp.2 = data.frame(tmp.2)
tmp.2[,grep("ckr_", colnames(tmp.2))][mask] = tmp.2[,grep("mean.", colnames(tmp.2))][mask]

data$ckr_Calcium = as.double(data$ckr_Calcium)
data[,grep("ckr_", colnames(data))][,-c(1,4,8)][!is.na(data$rear.area),] = tibble(tmp.2[,grep("ckr_", colnames(tmp.2))])


tmp.2 = left_join(data[,grep("ad_|adult.area", colnames(data))][,-c(1,2+c(1,4,8))], mean.by.area, join_by("adult.area"=="Area"))
tmp.2 = tmp.2[!is.na(tmp.2$adult.area),]

mask = is.na(tmp.2)[,grep("ad_", colnames(tmp.2))]
tmp.2 = data.frame(tmp.2)
tmp.2[,grep("ad_", colnames(tmp.2))][mask] = tmp.2[,grep("mean.", colnames(tmp.2))][mask]

data[,grep("ad_", colnames(data))][,-c(1,1+c(1,4,8))][!is.na(data$adult.area),] = tibble(tmp.2[,grep("ad_", colnames(tmp.2))])



#imputazione NA numero rear sibs conteggiando il numero di pulcini con lo stessa rear.breed.ID
rm(tmp)
tmp = data %>% distinct(chick.ring.number, .keep_all = TRUE) %>% group_by(rear.breed.ID) %>% summarise(num = n(), rr = mean(rear_sibs, na.rm=TRUE))
tmp = tmp[!(is.na(tmp$rear.breed.ID)),]
smoothScatter(tmp[,2:3])
predicted = predict(lm(rr~num+I(num^2), data=tmp), data.frame(num=tmp$num))
points(tmp$num, predicted, col="red", pch=16)
abline(0,1)

tmp$predicted <- predicted

ggplot(tmp, aes(x = num, y = rr)) +
  stat_density2d(aes(fill = after_stat(density)^0.4), geom = "tile", contour = FALSE) +
  geom_point(alpha = 0.1, size = 0.5) +
  scale_fill_continuous(name = "densità", low = "white", high = "dodgerblue4") +
  geom_point(aes(x = num, y = predicted), col = "red") +
  theme_classic() +
  ylab("media pulcini nido") + 
  xlab("numero pulcini nido osservati") +
  geom_abline(slope =1 )+ 
  ggtitle("Relazione numero pulcini osservati per nido con media numero pulcini per nido")


tmp$rr[is.na(tmp$rr)] = predicted[is.na(tmp$rr)]
tmp = left_join(data[,"rear.breed.ID"], tmp, "rear.breed.ID")
data$rear_sibs[is.na(data$rear_sibs)] = tmp$rr[is.na(data$rear_sibs)]

#cap rear sibs
sel = !is.na(data$rear_sibs) & (data$rear_sibs<data$d14.reared.sibs)
sel[is.na(sel)] = FALSE
data$rear_sibs[sel] = data$d14.reared.sibs[sel]

#imputazione NA d0.natal.total.sibs conteggiando il numero di pulcini con lo stesso natal.breed.ID
rm(tmp)
tmp = data %>% distinct(chick.ring.number, .keep_all = TRUE) %>% group_by(natal.breed.ID) %>% summarise(num = n(), rr = mean(d0.natal.total.sibs, na.rm=TRUE))
tmp = tmp[!(is.na(tmp$natal.breed.ID)),]
smoothScatter(tmp[,2:3])
predicted = predict(lm(rr~num+I(num^2), data=tmp), data.frame(num=tmp$num))
points(tmp$num, predicted, col="red", pch=16)
abline(0,1)


ggplot(tmp, aes(x = num, y = rr)) +
  stat_density2d(aes(fill = after_stat(density)^0.4), geom = "tile", contour = FALSE) +
  geom_point(alpha = 0.1, size = 0.5) +
  scale_fill_continuous(name = "densità", low = "white", high = "dodgerblue4") +
  geom_point(aes(x = num, y = predicted), col = "red") +
  theme_classic() +
  ylab("media pulcini schiusi per nido") + 
  xlab("numero pulcini osservati per nido") +
  geom_abline(slope =1 )+ 
  ggtitle("Relazione pulcini osservati per nido con media pulcini schiusi per nido")


tmp$rr[is.na(tmp$rr)] = predicted[is.na(tmp$rr)]
tmp = left_join(data[,"natal.breed.ID"], tmp, "natal.breed.ID")
data$d0.natal.total.sibs[is.na(data$d0.natal.total.sibs)] = tmp$rr[is.na(data$d0.natal.total.sibs)]

#cap d0.natal.total.sibs
sel = !is.na(data$d0.natal.total.sibs) & (data$d0.natal.total.sibs>data$natal.CS)
sel[is.na(sel)] = FALSE
data$d0.natal.total.sibs[sel] = data$natal.CS[sel]


#imputazione NA adult.CS usando i natal.CS con corrispondente breed id 
rm(tmp)
tmp = data %>% group_by(natal.breed.ID) %>% summarise(nn = mean(natal.CS, na.rm=TRUE))
tmp2 = data %>% group_by(adult.breed.ID) %>% summarise(nn = mean(adult.CS, na.rm=TRUE))
tmp3 = left_join(tmp, tmp2, join_by(natal.breed.ID == adult.breed.ID))
print(tmp3, n=100)
smoothScatter(tmp3[,2:3])
tmp3[is.na(tmp3[,3]),3] = tmp3[is.na(tmp3[,3]),2]
tmp3 = tmp3[!(is.na(tmp$natal.breed.ID)),]
tmp3 = left_join(data[,"adult.breed.ID"], tmp3, join_by(adult.breed.ID == natal.breed.ID))
data$adult.CS[is.na(data$adult.CS)] = tmp3$nn.y[is.na(data$adult.CS)]

#imputazione NA No.chs.hatched usando i d0.natal.total.sibs con corrispondente breed id 
rm(tmp)
tmp = data %>% group_by(natal.breed.ID) %>% summarise(nn = mean(d0.natal.total.sibs, na.rm=TRUE))
tmp2 = data %>% group_by(adult.breed.ID) %>% summarise(nn = mean(No.chs.hatched, na.rm=TRUE))
tmp3 = left_join(tmp, tmp2, join_by(natal.breed.ID == adult.breed.ID))
print(tmp3, n=100)
smoothScatter(tmp3[,2:3])
tmp3[is.na(tmp3[,3]),3] = tmp3[is.na(tmp3[,3]),2]
tmp3 = tmp3[!(is.na(tmp$natal.breed.ID)),]
tmp3 = left_join(data[,"adult.breed.ID"], tmp3, join_by(adult.breed.ID == natal.breed.ID))
data$No.chs.hatched[is.na(data$No.chs.hatched)] = tmp3$nn.y[is.na(data$No.chs.hatched)]

#imputazione NA Cs.at.start.of.rearing usando i rear_sibs con corrispondente breed id 
tmp = data %>% group_by(rear.breed.ID) %>% summarise(nn = mean(rear_sibs, na.rm=TRUE))
tmp2 = data %>% group_by(adult.breed.ID) %>% summarise(nn = mean(Cs.at.start.of.rearing, na.rm=TRUE))
tmp3 = left_join(tmp, tmp2, join_by(rear.breed.ID == adult.breed.ID))
print(tmp3, n=100)
smoothScatter(tmp3[,2:3])
tmp3[is.na(tmp3[,3]),3] = tmp3[is.na(tmp3[,3]),2]
tmp3 = tmp3[!(is.na(tmp$rear.breed.ID)),]
tmp3 = left_join(data[,"adult.breed.ID"], tmp3, join_by(adult.breed.ID == rear.breed.ID))
data$Cs.at.start.of.rearing[is.na(data$Cs.at.start.of.rearing)] = tmp3$nn.y[is.na(data$Cs.at.start.of.rearing)]


#imputazione NA d14.chicks..reared. usando i d14.reared.sibs con corrispondente breed id 
tmp = data %>% group_by(rear.breed.ID) %>% summarise(nn = mean(d14.reared.sibs, na.rm=TRUE))
tmp2 = data %>% group_by(adult.breed.ID) %>% summarise(nn = mean(d14.chicks..reared., na.rm=TRUE))
tmp3 = left_join(tmp, tmp2, join_by(rear.breed.ID == adult.breed.ID))
print(tmp3, n=100)
smoothScatter(tmp3[,2:3])
tmp3[is.na(tmp3[,3]),3] = tmp3[is.na(tmp3[,3]),2]
tmp3 = tmp3[!(is.na(tmp$rear.breed.ID)),]
tmp3 = left_join(data[,"adult.breed.ID"], tmp3, join_by(adult.breed.ID == rear.breed.ID))
data$d14.chicks..reared.[is.na(data$d14.chicks..reared.)] = tmp3$nn.y[is.na(data$d14.chicks..reared.)]

####### 


#######
# Creo i dataset separati

#creo dataset pulcini
chicks = data[!is.na(data$chick.ring.number),]

#pulcini che hanno anche la risposta
chicks.resp = chicks[!is.na(chicks$headpc1) & !is.na(chicks$wingpc1) & !is.na(chicks$chestpc1),]

#adulti ed adulti con risposta
adults <- data[!is.na(data$adult.ring.number),]
adults.resp = data[!is.na(data$adult.ring.number) & !is.na(data$headpc1) & !is.na(data$wingpc1) &
                     !is.na(data$chestpc1),]

na_get(adults)

#pulcini con adulti che hanno la risposta
chick.full = chicks.resp[!is.na(chicks.resp$genetic.mom.Ring) &
                           !is.na(chicks.resp$natal.nest.dad.Ring) &
                           chicks.resp$genetic.mom.Ring %in% adults.resp$adult.ring.number &
                           chicks.resp$natal.nest.dad.Ring %in% adults.resp$adult.ring.number,]



var_eliminare_pulcini <- c("natal.Box", "natal_box_year", "gen_mom_ID_ring", "natal.LD", "natal.OH",
                           "rear.Box", "rear_mom_ID_ring", "rear_dad_ID_ring", "chick_trt",  "ckr_Thiessen.polygon..ha.",
                           "ckr_NNBT.m.", "ckr_Edge..m.", "Date.of.day14", "measuring.event..unique.ID.",
                           "adult.Boxno", "adult.breed.ID", "adult.LD", "adult.CS", "adultOH", "No.chs.hatched", 
                           "adult.Manipulation", "Cs.in", "Cs.out", "Cs.at.start.of.rearing", "d14.chicks..reared.",
                           "no.reared.chs.fledged", "year.of.measure...1", "jan.date.of.adult.measurment",
                           "relative.year.of.adult.measure", "unique.row.number", "relative.year_color", "colour.measure", 
                           "hlcuvs", "hlcsws", "hlcmws", "wlcuvs" , "wlcsws", "wlcmws", "clcuvs",  "clcsws", "clcmws",
                           #"headpc1", "headpc2", "headpc3", "wingpc1", "wingpc2", "wingpc3", "chestpc1", "chestpc2", "chestpc3",
                           "ad_Thiessen.polygon..ha.", "ad_NNBT.m.", "ad_Edge..m.", "hatched.1")

chick.full <- chick.full %>% dplyr::select( -one_of(var_eliminare_pulcini))


# elimino fino al adulti_ring_number (che tengo)
var_elimino_genitori <- c("adult.Boxno", "adult.LD", "adultOH","adult.Manipulation", "Cs.in", "Cs.out", 
                          "year.of.measure...1", "jan.date.of.adult.measurment", "relative.year.of.adult.measure", 
                          "unique.row.number", "relative.year_color", "colour.measure", 
                          "hlcuvs", "hlcsws", "hlcmws", "wlcuvs", "wlcsws", "wlcmws", "clcuvs",  "clcsws", "clcmws", 
                          #"headpc1", "headpc2", "headpc3", "wingpc1", "wingpc2", "wingpc3", "chestpc1", "chestpc2", "chestpc3",
                          "ad_Thiessen.polygon..ha.", "ad_NNBT.m.", "ad_Edge..m.")

adults <- adults[,-c(1:41)] %>% dplyr::select(-one_of(var_elimino_genitori))

#creo tassi per pulcini (sopravvivenza schisura, primi 14 giorni e allevamento)
chick.full$natal.surv.rate=chick.full$d14.natal.total.sibs/chick.full$d0.natal.total.sibs
chick.full$natal.hatch.rate = chick.full$d0.natal.total.sibs/chick.full$natal.CS
chick.full$rear.surv.rate = chick.full$d14.reared.sibs/chick.full$rear_sibs


# creo tasso di sopravvivenza dei pulcini per l'adulto con adult.CS e No.chs.hatched
adults$adult.natal.hatch.rate <- adults$No.chs.hatched/adults$adult.CS
adults$adult.rear.surv.rate <- adults$d14.chicks..reared./adults$Cs.at.start.of.rearing


#imputazione NA adult.natal.hatch.rate e adult.rear.surv.rate con media per lo stesso uccello
tmp = adults %>% group_by(adult.ring.number) %>% summarise(mhr = mean(adult.natal.hatch.rate, na.rm=TRUE),
                                                           msr = mean(adult.rear.surv.rate, na.rm=TRUE))
tmp2 = left_join(adults, tmp, "adult.ring.number")
tmp2 = tmp2[,c(1,(ncol(tmp2)-3):ncol(tmp2))]

adults$adult.natal.hatch.rate[is.na(adults$adult.natal.hatch.rate)] = tmp2$mhr[is.na(adults$adult.natal.hatch.rate)]
adults$adult.rear.surv.rate[is.na(adults$adult.rear.surv.rate)] = tmp2$msr[is.na(adults$adult.rear.surv.rate)]


# cap dei tassi e trasformazione in categoriali
#hist(chick.full$natal.surv.rate, nclass=100)
#abline(v=0.9)
chick.full$natal.surv.rate <- ifelse(chick.full$natal.surv.rate >=0.9, 1, 0)
#hist(chick.full$natal.hatch.rate)
chick.full$natal.hatch.rate <- ifelse(chick.full$natal.hatch.rate  >=0.9, 1, 0)
#hist(chick.full$rear.surv.rate)
chick.full$rear.surv.rate <- ifelse(chick.full$rear.surv.rate >=0.9, 1, 0)
#hist(adults$adult.natal.hatch.rate)
adults$adult.natal.hatch.rate <- ifelse(adults$adult.natal.hatch.rate >=0.9, 1, 0)
#hist(adults$adult.rear.surv.rate)
adults$adult.rear.surv.rate <- ifelse(adults$adult.rear.surv.rate >=0.9, 1, 0)

#######

## Creo dataset Genitori natali -------------
adults.natal <- adults[!is.na(adults$headpc1) & !is.na(adults$wingpc1) & !is.na(adults$chestpc1),]
adults.natal <- adults.natal[,-c(8:10)] #tolgo variabili reared
colnames(adults.natal)


##################
#controllo di ulteriori NA (facendo matching tra figli ed adulti) ed eventuale imputazione
t1 <- merge(adults.natal, chick.full, by.x = "adult.ring.number", by.y = "natal.nest.dad.Ring", all.x = T)
t2 <- merge(adults.natal, chick.full, by.x = "adult.ring.number", by.y = "genetic.mom.Ring", all.x = T)
t1[,33] <- NULL
t2[,33] <- NULL

tmp <- rbind(t1,t2) %>% dplyr :: select (adult.ring.number, chick.ring.number, adult.natal.hatch.rate, adult.rear.surv.rate,
                                         rear.surv.rate, natal.hatch.rate, adult.breed.ID)

tmp$adult.rear.surv.rate[is.na(tmp$adult.rear.surv.rate)] <- tmp$rear.surv.rate[is.na(tmp$adult.rear.surv.rate)]
tmp$adult.natal.hatch.rate[is.na(tmp$adult.natal.hatch.rate)] <- tmp$natal.hatch.rate[is.na(tmp$adult.natal.hatch.rate)]

tmp2 <- tmp %>% group_by(adult.ring.number, adult.breed.ID) %>% summarise(adult.natal.hatch.rate = mean(adult.natal.hatch.rate, na.rm = T),
                                                                          adult.rear.surv.rate = mean(adult.rear.surv.rate, na.rm =T))


tmp3 <- left_join(adults.natal[, c("adult.ring.number","adult.breed.ID")], tmp2, join_by(adult.ring.number == adult.ring.number,
                                                                                         adult.breed.ID ==adult.breed.ID))

adults.natal$adult.rear.surv.rate[is.na(adults.natal$adult.rear.surv.rate )] <- tmp3$adult.rear.surv.rate[is.na(adults.natal$adult.rear.surv.rate )]
adults.natal$adult.natal.hatch.rate[is.na(adults.natal$adult.natal.hatch.rate )] <- tmp3$adult.natal.hatch.rate[is.na(adults.natal$adult.natal.hatch.rate )]


#imputazione età adulti
adults.natal$age[is.na(adults.natal$age)] <- mean(adults.natal$age, na.rm = T)


## Creo dataset genitori allevamento
adults.rear <- adults[, -c(14:22)] #tolgo colori
colnames(adults.rear)


# ANALISI MODELLO UNICO

stima.mod.cambio.unici <- function(seed, PCA = FALSE){
  set.seed(seed)
  chick.full.unique = distinct(chick.full[sample(1:nrow(chick.full), nrow(chick.full), FALSE),], chick.ring.number, .keep_all = TRUE)
  adults.natal.unique = distinct(adults.natal[sample(1:nrow(adults.natal), nrow(adults.natal), FALSE),], adult.ring.number, .keep_all = TRUE)
  adults.rear.unique = distinct(adults.rear[sample(1:nrow(adults.rear), nrow(adults.rear), FALSE),], adult.ring.number, .keep_all = TRUE)
  
  data.complete = chick.full.unique
  
  data.complete = inner_join(data.complete,
                             adults.natal.unique,
                             join_by(genetic.mom.Ring == adult.ring.number), suffix=c("", ".genetic.mom"))
  data.complete = inner_join(data.complete,
                             adults.natal.unique,
                             join_by(natal.nest.dad.Ring == adult.ring.number), suffix=c("", ".genetic.dad"))
  data.complete = left_join(data.complete,
                            adults.rear,
                            join_by(rear.mom.Ring == adult.ring.number, rear.breed.ID == adult.breed.ID), suffix=c("", ".rear.mom"))
  data.complete = left_join(data.complete,
                            adults.rear,
                            join_by(rear.dad.Ring == adult.ring.number, rear.breed.ID == adult.breed.ID), suffix=c("", ".rear.dad"))
  
  length(unique(data.complete$chick.ring.number))
  data.complete = data.complete %>% arrange(chick.ring.number)
  
  #rimozione variabili non di interesse
  #######
  data.complete$chick.ring.number = NULL
  data.complete$genetic.mom.Ring = NULL
  data.complete$natal.nest.dad.Ring = NULL
  data.complete$rear.mom.Ring = NULL
  data.complete$rear.dad.Ring = NULL
  data.complete$adult.ring.number = NULL
  data.complete$adult.breed.ID = NULL
  data.complete$adult.breed.ID.genetic.dad = NULL
  
  data.complete$adult.CS = NULL
  data.complete$No.chs.hatched = NULL
  data.complete$hatched.1 = NULL
  data.complete$final.sex.genetic.mom = NULL
  data.complete$adult.CS.genetic.dad = NULL
  data.complete$No.chs.hatched.genetic.dad = NULL
  data.complete$hatched.1.genetic.dad = NULL
  data.complete$final.sex.genetic.dad = NULL
  
  data.complete$adult.area.rear.mom = NULL
  data.complete$adult.breed.ID.rear.mom = NULL
  data.complete$year.of.adult.measurement.rear.mom = NULL
  data.complete$adult.CS.rear.mom = NULL
  data.complete$No.chs.hatched.rear.mom = NULL
  data.complete$hatched.1.rear.mom = NULL
  data.complete$Cs.at.start.of.rearing = NULL
  data.complete$d14.chicks..reared. = NULL
  #data.complete$no.reared.chs.fledged
  data.complete$final.sex.rear.mom = NULL
  data.complete$season_color.rear.mom = NULL
  data.complete$ad_ln.polygon.rear.mom = NULL
  data.complete$ad_BT50m.rear.mom = NULL
  data.complete$ad_ln.NNBT.rear.mom = NULL
  data.complete$ad_GT50m.rear.mom = NULL
  data.complete$ad_Oaks50m.rear.mom = NULL
  data.complete$ad_sqrt.edge.rear.mom = NULL
  data.complete$ad_Altitude.m..rear.mom = NULL
  data.complete$ad_Calcium.rear.mom = NULL
  data.complete$adult.natal.hatch.rate.rear.mom = NULL
  data.complete$adult.rear.surv.rate.rear.mom = NULL
  
  data.complete$adult.area.rear.dad = NULL
  data.complete$adult.breed.ID.rear.dad = NULL
  data.complete$year.of.adult.measurement.rear.dad = NULL
  data.complete$adult.CS.rear.dad = NULL
  data.complete$No.chs.hatched.rear.dad = NULL
  data.complete$hatched.1.rear.dad = NULL
  data.complete$Cs.at.start.of.rearing.rear.dad = NULL
  data.complete$d14.chicks..reared..rear.dad = NULL
  #data.complete$no.reared.chs.fledged.rear.dad
  data.complete$final.sex.rear.dad = NULL
  data.complete$season_color.rear.dad = NULL
  data.complete$ad_ln.polygon.rear.dad = NULL
  data.complete$ad_BT50m.rear.dad = NULL
  data.complete$ad_ln.NNBT.rear.dad = NULL
  data.complete$ad_GT50m.rear.dad = NULL
  data.complete$ad_Oaks50m.rear.dad = NULL
  data.complete$ad_sqrt.edge.rear.dad = NULL
  data.complete$ad_Altitude.m..rear.dad = NULL
  data.complete$ad_Calcium.rear.dad = NULL
  data.complete$adult.natal.hatch.rate.rear.dad = NULL
  data.complete$adult.rear.surv.rate.rear.dad = NULL
  
  
  data.complete$natal.breed.ID = NULL
  data.complete$rear.breed.ID = NULL
  #######
  
  
  #data.complete$no.reared.chs.fledged[is.na(data.complete$no.reared.chs.fledged)] = data.complete$no.reared.chs.fledged.rear.dad[is.na(data.complete$no.reared.chs.fledged)]
  data.complete$no.reared.chs.fledged.rear.dad = NULL
  data.complete$no.reared.chs.fledged = NULL #conteneva la stessa informazione di d14 reared sibs
  
  data.complete[is.na(data.complete$final.sex),]$final.sex = 1 #imputazione singolo pulcino 
  
  #imputazione NA età 
  data.complete$age.rear.dad[is.na(data.complete$age.rear.dad)] = mean(adults.rear$age, na.rm = TRUE)
  data.complete$age.rear.mom[is.na(data.complete$age.rear.mom)] =  mean(adults.rear$age, na.rm = TRUE)
  
  
  #data.complete$sibs.diff <- data.complete$rear_sibs -  data.complete$d0.natal.total.sibs
  data.complete$sibs.log.rapp<- log(data.complete$rear_sibs/data.complete$d0.natal.total.sibs) #creazione log rapporto
  #tolgo informazione (potenzialmente ridondante rispetto alla presenza del log rapporto e d14 reared sibs)
  data.complete$rear_sibs <- NULL
  
  #tolgo variabili confondenti
  data.complete$day.14.weight <- NULL
  data.complete$day.14.tarsus <- NULL
  data.complete$X1.chick.survival.to.first.breed.season <- NULL
  
  
  # conversione in fattore dell'anno di rilevazione
  data.complete$year.of.adult.measurement <- as.factor(data.complete$year.of.adult.measurement)
  data.complete$year.of.adult.measurement.genetic.mom <- as.factor(data.complete$year.of.adult.measurement.genetic.mom)
  data.complete$year.of.adult.measurement.genetic.dad <- as.factor(data.complete$year.of.adult.measurement.genetic.dad)
  data.complete$natal.year <- as.factor(data.complete$natal.year)
  
  if (PCA == TRUE){
    # PCA delle variabili ambientali
    pc <- prcomp(data.complete[,grepl("ad_|ckr_", colnames(data.complete))], scale = T)
    #plot(cumsum(pc$sdev^2/sum(pc$sdev^2)), ty = "l")
    
    #biplot(pc)
    
    data.complete[,grepl("ad_|ckr_", colnames(data.complete))] <- NULL #rimuovo var utilizzate per creare le componenti
    data.complete <- tibble(cbind(data.complete, pc$x[,1:10])) #aggiungo le componenti
  }
  
  # # Modello lineare -------------
  # mod.compl = lm(cbind(headpc3, wingpc3, chestpc3,
  #                      headpc2, wingpc2, chestpc2,
  #                      headpc1, wingpc1, chestpc1)~., data=data.complete)
  # 
  # X = model.matrix(mod.compl)
  # X = X[,which(rowSums(is.na(coef(mod.compl)))==0)]
  # mod.compl = lm(cbind(headpc3, wingpc3, chestpc3,
  #                      headpc2, wingpc2, chestpc2,
  #                      headpc1, wingpc1, chestpc1)~-1+X, data=data.complete)
  # #summary(mod.compl)
  # 
  # 
  mod = lm(cbind(headpc3, wingpc3, chestpc3,
                 headpc2, wingpc2, chestpc2,
                 headpc1, wingpc1, chestpc1)~., data=data.complete[,!grepl("Area|area", colnames(data.complete))])
  # summary(mod)
  # #dim(coef(mod))
  # 
  # anova(mod.compl, mod)
  # # elimino area
  
  if (PCA == FALSE){
    var.obs <- sum(diag(var(data.complete[,22:30])))
    var.mod <- sum(diag(var(mod$fitted.values)))
    R2.mod <- var.mod/var.obs
    
    coef.ambiente <- mod$coefficients[grepl("ckr_|ad_", rownames(mod$coefficients)),]
    
    std.error.mod <- sapply(summary(mod), function(x) x$coefficients[,2])
    std.error.ambiente <- std.error.mod[grepl("ckr_|ad_", rownames(std.error.mod)),]
    
    p.value.mod<- sapply(summary(mod), function(x) x$coefficients[,4])
    p.value.ambiente <- p.value.mod[grepl("ckr_|ad_", rownames(p.value.mod)),]
  } else{
    var.obs <- sum(diag(var(data.complete[,14:22])))
    var.mod <- sum(diag(var(mod$fitted.values)))
    R2.mod <- var.mod/var.obs
    
    coef.ambiente <- mod$coefficients[grepl("PC", rownames(mod$coefficients)),]
    
    std.error.mod <- sapply(summary(mod), function(x) x$coefficients[,2])
    std.error.ambiente <- std.error.mod[grepl("PC", rownames(std.error.mod)),]
    
    p.value.mod<- sapply(summary(mod), function(x) x$coefficients[,4])
    p.value.ambiente <- p.value.mod[grepl("PC", rownames(p.value.mod)),]
  }
  
  
  ## testiamo fattori genetici ----------
  mod.senza.colori = lm(cbind(headpc3, wingpc3, chestpc3,
                              headpc2, wingpc2, chestpc2,
                              headpc1, wingpc1, chestpc1)~., data=data.complete[,!grepl("Area|area", colnames(data.complete)) &
                                                                                  !grepl("pc.\\.", colnames(data.complete)) & 
                                                                                  !grepl("genetic.mom|genetic.dad", colnames(data.complete)) &
                                                                                  !grepl("adult.natal.hatch.rate|adult.rear.surv.rate", colnames(data.complete))])# |

  
  anova(mod, mod.senza.colori)
  
  var.mod.senza.colori <- sum(diag(var(mod.senza.colori$fitted.values)))
  R2.mod.senza.colori <- var.mod.senza.colori/var.obs
  R2.mod.senza.colori.adj = 1-(1-R2.mod.senza.colori)*(nrow(data.complete)-1)/(nrow(data.complete)-nrow(coef(mod.senza.colori)))
  
  
  ## testiamo trattamento -------------
  mod.no.tratt <- lm(cbind(headpc3, wingpc3, chestpc3,
                           headpc2, wingpc2, chestpc2,
                           headpc1, wingpc1, chestpc1)~., data=data.complete[,!grepl("Area|area", colnames(data.complete)) &
                                                                               !grepl("HOME", colnames(data.complete)) &
                                                                               !grepl("rear", colnames(data.complete)) &
                                                                               !grepl("sibs.", colnames(data.complete)) |
                                                                               grepl("adult.rear", colnames(data.complete))])
  
  anova(mod, mod.no.tratt)
  
  var.mod.no.tratt <- sum(diag(var(mod.no.tratt$fitted.values)))
  R2.mod.no.tratt <- var.mod.no.tratt/var.obs
  R2.mod.no.tratt.adj = 1-(1-R2.mod.no.tratt)*(nrow(data.complete)-1)/(nrow(data.complete)-nrow(coef(mod.no.tratt)))
  
  ## modello no ambientali -------------
  if (PCA == FALSE){
    mod.no.ambiente.luogo <- lm(cbind(headpc3, wingpc3, chestpc3,
                                      headpc2, wingpc2, chestpc2,
                                      headpc1, wingpc1, chestpc1)~., data=data.complete[,!grepl("Area|area", colnames(data.complete)) &
                                                                                          !grepl("ckr_|ad_", colnames(data.complete))])
  } else{
    mod.no.ambiente.luogo <- lm(cbind(headpc3, wingpc3, chestpc3,
                                      headpc2, wingpc2, chestpc2,
                                      headpc1, wingpc1, chestpc1)~., data=data.complete[,!grepl("Area|area", colnames(data.complete)) &
                                                                                          !grepl("PC", colnames(data.complete))])
    
  }
  
  anova(mod, mod.no.ambiente.luogo)
  
  var.mod.no.ambiente <- sum(diag(var(mod.no.ambiente.luogo$fitted.values)))
  R2.mod.no.ambiente <- var.mod.no.ambiente/var.obs
  R2.mod.no.ambiente.adj = 1-(1-R2.mod.no.ambiente)*(nrow(data.complete)-1)/(nrow(data.complete)-nrow(coef(mod.no.ambiente.luogo)))
  
  test1 <- anova(mod, mod.senza.colori)
  test2 <- anova(mod, mod.no.tratt)
  test3 <- anova(mod, mod.no.ambiente.luogo)
  
  vet.test <- c(test1$`Pr(>F)`[2],test2$`Pr(>F)`[2],test3$`Pr(>F)`[2])
  names(vet.test) <- c("test genetico", "test trattamento", "test ambiente")
  
  vet.R2 <- c(R2.mod.senza.colori, R2.mod.no.tratt, R2.mod.no.ambiente)
  vet.R2.adj <- c(R2.mod.senza.colori.adj, R2.mod.no.tratt.adj, R2.mod.no.ambiente.adj)
  names(vet.R2) <- c("mod no genetico", "mod no trattamento", "mod no ambiente")
  names(vet.R2.adj) <- c("mod no genetico", "mod no trattamento", "mod no ambiente")
  
  var.mod.compl <- sum(diag(var(mod$fitted.values)))
  R2.mod.compl = var.mod.compl/var.obs
  R2.mod.compl.adj = 1-(1-R2.mod.compl)*(nrow(data.complete)-1)/(nrow(data.complete)-nrow(coef(mod)))
  
 
  return (list(test.anova = vet.test,
                 rapporto.R2 = c(vet.R2)/R2.mod.compl,
                 rapporto.R2.adj = c(vet.R2.adj)/R2.mod.compl.adj,
                 coef.ambiente = coef.ambiente,
                 std.error.ambiente = std.error.ambiente,
                 p.value.ambiente = p.value.ambiente))
}



x <- c(1:1000)
matrice.pvalue <- matrix(NA, length(x),3)
matrice.R2 <- matrix(NA, length(x),3)
matrice.R2.adj <- matrix(NA, length(x),3)

for(el in x){ 
  res <- stima.mod.cambio.unici(el, PCA = TRUE) #cambiare in PCA=FALSE per l'analisi senza pca
  matrice.pvalue[el, ] <- res$test.anova
  matrice.R2[el,] <- res$rapporto.R2
  matrice.R2.adj[el,] <- res$rapporto.R2.adj
  
  if (el == 1){
    mtr.coef.ambiente <- as.data.frame(res$coef.ambiente)
    mtr.std.error.ambiente <- as.data.frame(res$std.error.ambiente)
    mtr.p.value.ambiente <- as.data.frame(res$p.value.ambiente)
  } else{
    mtr.coef.ambiente <- cbind(mtr.coef.ambiente, res$coef.ambiente)
    mtr.std.error.ambiente <- cbind(mtr.std.error.ambiente, res$std.error.ambiente)
    mtr.p.value.ambiente <- cbind(mtr.p.value.ambiente, res$p.value.ambiente)
  }
}

library(ggplot2)

matrice.pvalue = as.data.frame(matrice.pvalue)
plt1.1 = ggplot(matrice.pvalue) +
  #geom_density(aes(x = V3, color="ambiente"), fill="dodgerblue4", alpha=.5) +
  geom_density(aes(x = V1, color = "genetica"), fill="pink", alpha=.5) +
  #geom_density(aes(x = V2, color = "trattamento"), fill="dodgerblue", alpha=.5) +
  labs(x = "R^2 modello ridotto / R^2 modello completo",
       y = "Density",
       color = "Legend") +
  scale_color_manual(values = colors) + geom_vline(xintercept = 0.05/3, col="red") +
  theme_bw()

plt1.1
# plt1.2
# plt1.3
# plt1.1.pca
# plt1.2.pca
# plt1.3.pca



colors <- c("genetica" = "pink", "ambiente" = "dodgerblue4", "trattamento" = "dodgerblue")

matrice.R2 <- as.data.frame(matrice.R2)

plt1 = ggplot(matrice.R2) +
  geom_density(aes(x = V3, color="ambiente"), fill="dodgerblue4", alpha=.5) +
  geom_density(aes(x = V1, color = "genetica"), fill="pink", alpha=.5) +
  geom_density(aes(x = V2, color = "trattamento"), fill="dodgerblue", alpha=.5) +
  labs(x = "R^2 modello ridotto / R^2 modello completo",
       y = "Density",
       color = "Legend") +
  scale_color_manual(values = colors) +
  theme_bw() + theme(legend.position="none")

matrice.R2.adj <- as.data.frame(matrice.R2.adj)

plt2 = ggplot(matrice.R2.adj) +
  geom_density(aes(x = V3, color="ambiente"), fill="dodgerblue4", alpha=.5) +
  geom_density(aes(x = V1, color = "genetica"), fill="pink", alpha=.5) +
  geom_density(aes(x = V2, color = "trattamento"), fill="dodgerblue", alpha=.5) +
  labs(x = "R^2 corr. modello ridotto / R^2 corr. modello completo",
       y = "Density",
       color = "Legend") +
  scale_color_manual(values = colors) +
  theme_bw()

# remotes::install_github("larmarange/JLutils")
library(JLutils)
leg = get_legend(plt2)

grid.arrange(plt1, plt2 + theme(legend.position="none"), leg, ncol=3, widths=c(2,2,0.8))





matrice.evalue <- apply(matrice.pvalue, 2, 
                        function(x) (1 - x + x * log(x)) / (x * (-log(x))^2))

adjust_e_values <- function(e_values) {
  names(e_values) <- c(1:length(e_values))
  sorted_e_values <- sort(e_values)
  S <- cumsum(sorted_e_values)
  tmp = sorted_e_values
  for(k in 1:length(sorted_e_values)){
    tmp[k]  = sorted_e_values[k]
    for (i in 1:k){
      e = (S[i]+ sorted_e_values[k])/(i+1)
      if(e < tmp[k]) tmp[k] = e
    }
  }
  tmp <- tmp[sort(names(tmp))]
  return(tmp)
}

matrice_evalues_adj <- t(apply(matrice.evalue, 1, adjust_e_values))

colors <- c("genetica" = "pink", "ambiente" = "dodgerblue4", "trattamento" = "dodgerblue")
matrice_evalues_adj <- as.data.frame(matrice_evalues_adj)
colnames(matrice_evalues_adj) = c("V1", "V2", "V3")

plt2.3 = ggplot(matrice_evalues_adj) +
  #geom_density(aes(x = V3, color="ambiente"), fill="dodgerblue4", alpha=.5) +
  #geom_density(aes(x = V1, color = "genetica"), fill="pink", alpha=.5) +
  geom_density(aes(x = V2, color = "trattamento"), fill="dodgerblue", alpha=.5) +
  labs(x = "R^2 modello ridotto / R^2 modello completo",
       y = "Density",
       color = "Legend") +
  scale_color_manual(values = colors) + geom_vline(xintercept = colMeans(matrice_evalues_adj)[2], col="dodgerblue3") + 
  geom_vline(xintercept = 1, col="red") + 
  theme_bw()

plt2.3
# plt2.1
# plt2.2
# plt2.1.pca
# plt2.2.pca
# plt2.3.pca

evalues <- colMeans(matrice_evalues_adj)
evalues
adjust_e_values(colMeans(matrice.evalue))




# ANALISI MODELLO A DUE STEP

# Trasformo colori genitori in residui -----------
tmp <- adults.natal%>%
  dplyr::select(headpc1, headpc2, headpc3, wingpc1, wingpc2, wingpc3, chestpc1, chestpc2, chestpc3, ad_ln.polygon,
                ad_BT50m, ad_ln.NNBT, ad_GT50m, ad_Oaks50m, ad_sqrt.edge, ad_Altitude.m., ad_Calcium,
                adult.natal.hatch.rate, adult.rear.surv.rate, age, season_color, year.of.adult.measurement, final.sex)

select.chicks = adults.natal$adult.ring.number %in% unique(chick.full$chick.ring.number)

tmp$year.of.adult.measurement = factor(tmp$year.of.adult.measurement) #trasformo in fattore l'anno

#non voglio usare le risposte del secondo modello
selected.train = (rowSums(is.na(tmp))==0 & !select.chicks) #seleziono le righe senza NA (la maggior parte sono uccelli che non sono ne genitori)
tmp = tmp[selected.train,]

mod_colori_genitori <- lm(cbind(headpc1, headpc2, headpc3,
                                wingpc1, wingpc2, wingpc3,
                                chestpc1, chestpc2, chestpc3) ~ ., data = tmp) #non sembra essere necessaria interazione con final.sex

adults.natal[selected.train, 11:19] <- matrix(mod_colori_genitori$residuals, ncol=9, byrow = FALSE)

tmp.chicks <- adults.natal%>%
  dplyr::select(headpc1, headpc2, headpc3, wingpc1, wingpc2, wingpc3, chestpc1, chestpc2, chestpc3, ad_ln.polygon,
                ad_BT50m, ad_ln.NNBT, ad_GT50m, ad_Oaks50m, ad_sqrt.edge, ad_Altitude.m., ad_Calcium,
                adult.natal.hatch.rate, adult.rear.surv.rate, age, season_color, year.of.adult.measurement, final.sex)
tmp.chicks$year.of.adult.measurement = factor(tmp.chicks$year.of.adult.measurement) #trasformo in fattore l'anno
tmp.chicks = tmp.chicks[select.chicks,]

adults.natal[!selected.train, 11:19] <- NA
#previsione per i pulcini che ho tolto (perchè non volevo usare la risposta del prossimo modello)
adults.natal[select.chicks, 11:19] = tmp.chicks[,1:9] - predict(mod_colori_genitori, tmp.chicks[,-c(1:9)])
adults.natal <- adults.natal[,c(1,11:19)] #elimino variabili, tengo solo colore e ID



stima.mod.cambio.unici.duestep <- function(seed){
  set.seed(seed)
  chick.full.unique = distinct(chick.full[sample(1:nrow(chick.full), nrow(chick.full), FALSE),], chick.ring.number, .keep_all = TRUE)
  adults.natal.unique = distinct(adults.natal[sample(1:nrow(adults.natal), nrow(adults.natal), FALSE),], adult.ring.number, .keep_all = TRUE)
  adults.rear.unique = distinct(adults.rear[sample(1:nrow(adults.rear), nrow(adults.rear), FALSE),], adult.ring.number, .keep_all = TRUE)
  
  data.complete = chick.full.unique
  data.complete = inner_join(data.complete,
                             adults.natal.unique,
                             join_by(genetic.mom.Ring == adult.ring.number), suffix=c("", ".genetic.mom"))
  data.complete = inner_join(data.complete,
                             adults.natal.unique,
                             join_by(natal.nest.dad.Ring == adult.ring.number), suffix=c("", ".genetic.dad"))
  data.complete = left_join(data.complete,
                            adults.rear,
                            join_by(rear.mom.Ring == adult.ring.number, rear.breed.ID == adult.breed.ID), suffix=c("", ".rear.mom"))
  data.complete = left_join(data.complete,
                            adults.rear,
                            join_by(rear.dad.Ring == adult.ring.number, rear.breed.ID == adult.breed.ID), suffix=c("", ".rear.dad"))

  
  
  data.complete$chick.ring.number = NULL
  data.complete$genetic.mom.Ring = NULL
  data.complete$natal.nest.dad.Ring = NULL
  data.complete$rear.mom.Ring = NULL
  data.complete$rear.dad.Ring = NULL
  data.complete$adult.ring.number = NULL
  data.complete$adult.breed.ID = NULL
  data.complete$adult.breed.ID.genetic.dad = NULL
  
  data.complete$adult.CS = NULL
  data.complete$No.chs.hatched = NULL
  data.complete$hatched.1 = NULL
  data.complete$final.sex.genetic.mom = NULL
  data.complete$adult.CS.genetic.dad = NULL
  data.complete$No.chs.hatched.genetic.dad = NULL
  data.complete$hatched.1.genetic.dad = NULL
  data.complete$final.sex.genetic.dad = NULL
  
  data.complete$adult.area.rear.mom = NULL
  data.complete$adult.breed.ID.rear.mom = NULL
  data.complete$year.of.adult.measurement.rear.mom = NULL
  data.complete$adult.CS.rear.mom = NULL
  data.complete$No.chs.hatched.rear.mom = NULL
  data.complete$hatched.1.rear.mom = NULL
  data.complete$Cs.at.start.of.rearing = NULL
  data.complete$d14.chicks..reared. = NULL
  #data.complete$no.reared.chs.fledged
  data.complete$final.sex.rear.mom = NULL
  data.complete$season_color.rear.mom = NULL
  data.complete$ad_ln.polygon.rear.mom = NULL
  data.complete$ad_BT50m.rear.mom = NULL
  data.complete$ad_ln.NNBT.rear.mom = NULL
  data.complete$ad_GT50m.rear.mom = NULL
  data.complete$ad_Oaks50m.rear.mom = NULL
  data.complete$ad_sqrt.edge.rear.mom = NULL
  data.complete$ad_Altitude.m..rear.mom = NULL
  data.complete$ad_Calcium.rear.mom = NULL
  data.complete$adult.natal.hatch.rate.rear.mom = NULL
  data.complete$adult.rear.surv.rate.rear.mom = NULL
  ############
  data.complete$adult.natal.hatch.rate = NULL
  data.complete$adult.rear.surv.rate = NULL
  
  
  data.complete$adult.area.rear.dad = NULL
  data.complete$adult.breed.ID.rear.dad = NULL
  data.complete$year.of.adult.measurement.rear.dad = NULL
  data.complete$adult.CS.rear.dad = NULL
  data.complete$No.chs.hatched.rear.dad = NULL
  data.complete$hatched.1.rear.dad = NULL
  data.complete$Cs.at.start.of.rearing.rear.dad = NULL
  data.complete$d14.chicks..reared..rear.dad = NULL
  #data.complete$no.reared.chs.fledged.rear.dad
  data.complete$final.sex.rear.dad = NULL
  data.complete$season_color.rear.dad = NULL
  data.complete$ad_ln.polygon.rear.dad = NULL
  data.complete$ad_BT50m.rear.dad = NULL
  data.complete$ad_ln.NNBT.rear.dad = NULL
  data.complete$ad_GT50m.rear.dad = NULL
  data.complete$ad_Oaks50m.rear.dad = NULL
  data.complete$ad_sqrt.edge.rear.dad = NULL
  data.complete$ad_Altitude.m..rear.dad = NULL
  data.complete$ad_Calcium.rear.dad = NULL
  data.complete$adult.natal.hatch.rate.rear.dad = NULL
  data.complete$adult.rear.surv.rate.rear.dad = NULL
  
  data.complete$natal.breed.ID = NULL
  data.complete$rear.breed.ID = NULL
  
  #data.complete$no.reared.chs.fledged[is.na(data.complete$no.reared.chs.fledged)] = data.complete$no.reared.chs.fledged.rear.dad[is.na(data.complete$no.reared.chs.fledged)]
  data.complete$no.reared.chs.fledged.rear.dad = NULL
  data.complete$no.reared.chs.fledged = NULL
  
  data.complete[is.na(data.complete$final.sex),]$final.sex = 1 #imputazione di un caso singolo
  
  #imputazione NA età
  data.complete$age.rear.dad[is.na(data.complete$age.rear.dad)] = mean(adults.rear$age, na.rm = TRUE)
  data.complete$age.rear.mom[is.na(data.complete$age.rear.mom)] =  mean(adults.rear$age, na.rm = TRUE)
  
  
  #data.complete$sibs.diff <- data.complete$rear_sibs -  data.complete$d0.natal.total.sibs
  data.complete$sibs.log.rapp<- log(data.complete$rear_sibs/data.complete$d0.natal.total.sibs)
  data.complete$rear_sibs <- NULL
  
  
  data.complete$day.14.weight <- NULL
  data.complete$day.14.tarsus <- NULL
  data.complete$X1.chick.survival.to.first.breed.season <- NULL
  

  # convertiamo in fattore:
  data.complete$year.of.adult.measurement <- as.factor(data.complete$year.of.adult.measurement)
  data.complete$natal.year <- as.factor(data.complete$natal.year)

  
  # # Modello lineare -------------
  # mod.compl = lm(cbind(headpc3, wingpc3, chestpc3,
  #                      headpc2, wingpc2, chestpc2,
  #                      headpc1, wingpc1, chestpc1)~., data=data.complete)
  # 
  # X = model.matrix(mod.compl)
  # X = X[,which(rowSums(is.na(coef(mod.compl)))==0)]
  # mod.compl = lm(cbind(headpc3, wingpc3, chestpc3,
  #                      headpc2, wingpc2, chestpc2,
  #                      headpc1, wingpc1, chestpc1)~-1+X, data=data.complete)
  # #summary(mod.compl)
  
  
  mod = lm(cbind(headpc3, wingpc3, chestpc3,
                 headpc2, wingpc2, chestpc2,
                 headpc1, wingpc1, chestpc1)~., data=data.complete[,!grepl("Area|area", colnames(data.complete))])
  # summary(mod)
  # 
  # anova(mod.compl, mod)
  # elimino area
  
  var.obs <- sum(diag(var(data.complete[,22:30])))
  var.mod <- sum(diag(var(mod$fitted.values)))
  R2.mod <- var.mod/var.obs
  
  coef.ambiente <- mod$coefficients[grepl("ckr_|ad_", rownames(mod$coefficients)),]
  
  std.error.mod<- sapply(summary(mod), function(x) x$coefficients[,2])
  std.error.ambiente <- std.error.mod[grepl("ckr_|ad_", rownames(std.error.mod)),]
  
  p.value.mod<- sapply(summary(mod), function(x) x$coefficients[,4])
  p.value.ambiente <- p.value.mod[grepl("ckr_|ad_", rownames(p.value.mod)),]
  
  ## testiamo fattori genetici ----------
  mod.senza.colori = lm(cbind(headpc3, wingpc3, chestpc3,
                              headpc2, wingpc2, chestpc2,
                              headpc1, wingpc1, chestpc1)~., data=data.complete[,!grepl("Area|area", colnames(data.complete)) &
                                                                                  !grepl("pc.\\.", colnames(data.complete))])
  colnames(data.complete)
  #summary(mod.senza.colori)
  anova(mod, mod.senza.colori)
  
  var.mod.senza.colori <- sum(diag(var(mod.senza.colori$fitted.values)))
  R2.mod.senza.colori <- var.mod.senza.colori/var.obs
  R2.mod.senza.colori.adj = 1-(1-R2.mod.senza.colori)*(nrow(data.complete)-1)/(nrow(data.complete)-nrow(coef(mod.senza.colori)))
  
  ## testiamo trattamento -------------
  mod.no.tratt <- lm(cbind(headpc3, wingpc3, chestpc3,
                           headpc2, wingpc2, chestpc2,
                           headpc1, wingpc1, chestpc1)~., data=data.complete[,!grepl("Area|area", colnames(data.complete)) &
                                                                               !grepl("HOME", colnames(data.complete)) &
                                                                               !grepl("rear|sibs.", colnames(data.complete))])

  anova(mod, mod.no.tratt)
  var.mod.no.tratt <- sum(diag(var(mod.no.tratt$fitted.values)))
  R2.mod.no.tratt <- var.mod.no.tratt/var.obs
  R2.mod.no.tratt.adj = 1-(1-R2.mod.no.tratt)*(nrow(data.complete)-1)/(nrow(data.complete)-nrow(coef(mod.no.tratt)))

  ## modello no ambientali -------------
  mod.no.ambiente.luogo <- lm(cbind(headpc3, wingpc3, chestpc3,
                                    headpc2, wingpc2, chestpc2,
                                    headpc1, wingpc1, chestpc1)~., data=data.complete[,!grepl("Area|area", colnames(data.complete)) &
                                                                                        !grepl("ckr_|ad_", colnames(data.complete))])
  
  anova(mod, mod.no.ambiente.luogo)
  
  var.mod.no.ambiente <- sum(diag(var(mod.no.ambiente.luogo$fitted.values)))
  R2.mod.no.ambiente <- var.mod.no.ambiente/var.obs
  R2.mod.no.ambiente.adj = 1-(1-R2.mod.no.ambiente)*(nrow(data.complete)-1)/(nrow(data.complete)-nrow(coef(mod.no.ambiente.luogo)))
  
  test1 <- anova(mod, mod.senza.colori)
  test2 <- anova(mod,mod.no.tratt)
  test3 <- anova(mod,mod.no.ambiente.luogo)
  vet.test <- c(test1$`Pr(>F)`[2],test2$`Pr(>F)`[2],test3$`Pr(>F)`[2])
  names(vet.test) <- c("test genetico", "test trattamento", "test ambiente")
  
  vet.R2 <- c(R2.mod.senza.colori, R2.mod.no.tratt, R2.mod.no.ambiente)
  vet.R2.adj <- c(R2.mod.senza.colori.adj, R2.mod.no.tratt.adj, R2.mod.no.ambiente.adj)
  names(vet.R2) <- c("mod no genetico", "mod no trattamento", "mod no ambiente")
  names(vet.R2.adj) <- c("mod no genetico", "mod no trattamento", "mod no ambiente")
  
  var.mod.compl <- sum(diag(var(mod$fitted.values)))
  R2.mod.compl = var.mod.compl/var.obs
  R2.mod.compl.adj = 1-(1-R2.mod.compl)*(nrow(data.complete)-1)/(nrow(data.complete)-nrow(coef(mod)))
  
  return (list(test.anova = vet.test,
                 rapporto.R2 = c(vet.R2)/R2.mod.compl,
                 rapporto.R2.adj = c(vet.R2.adj)/R2.mod.compl.adj,
                 coef.ambiente = coef.ambiente,
                 std.error.ambiente = std.error.ambiente,
                 p.value.ambiente = p.value.ambiente))
}



x <- c(1:1000)
matrice.pvalue <- matrix(NA, length(x),3)
matrice.R2 <- matrix(NA, length(x),3)
matrice.R2.adj <- matrix(NA, length(x),3)

for(el in x){ 
  res <- stima.mod.cambio.unici.duestep(el)
  matrice.pvalue[el, ] <- res$test.anova
  matrice.R2[el,] <- res$rapporto.R2
  matrice.R2.adj[el,] <- res$rapporto.R2.adj
  
  if (el == 1){
    mtr.coef.ambiente <- as.data.frame(res$coef.ambiente)
    mtr.std.error.ambiente <- as.data.frame(res$std.error.ambiente)
    mtr.p.value.ambiente <- as.data.frame(res$p.value.ambiente)
  } else{
    mtr.coef.ambiente <- cbind(mtr.coef.ambiente, res$coef.ambiente)
    mtr.std.error.ambiente <- cbind(mtr.std.error.ambiente, res$std.error.ambiente)
    mtr.p.value.ambiente <- cbind(mtr.p.value.ambiente, res$p.value.ambiente)
  }
}




colors <- c("genetica" = "pink", "ambiente" = "dodgerblue4", "trattamento" = "dodgerblue")

matrice.pvalue <- as.data.frame(matrice.pvalue)

plt1.1.res = ggplot(matrice.pvalue) +
  #geom_density(aes(x = V3, color="ambiente"), fill="dodgerblue4", alpha=.5) +
  geom_density(aes(x = V1, color = "genetica"), fill="pink", alpha=.5) +
  #geom_density(aes(x = V2, color = "trattamento"), fill="dodgerblue", alpha=.5) +
  labs(x = "R^2 modello ridotto / R^2 modello completo",
       y = "Density",
       color = "Legend") +
  scale_color_manual(values = colors) + geom_vline(xintercept = 0.05/3, col="red") + 
  theme_bw()

plt1.1.res
# plt1.2.res
# plt1.3.res




matrice.R2 <- as.data.frame(matrice.R2)

plt1 = ggplot(matrice.R2) +
  geom_density(aes(x = V3, color="ambiente"), fill="dodgerblue4", alpha=.5) +
  geom_density(aes(x = V1, color = "genetica"), fill="pink", alpha=.5) +
  geom_density(aes(x = V2, color = "trattamento"), fill="dodgerblue", alpha=.5) +
  labs(x = "R^2 modello ridotto / R^2 modello completo",
       y = "Density",
       color = "Legend") +
  scale_color_manual(values = colors) +
  theme_bw() + theme(legend.position="none")

matrice.R2.adj <- as.data.frame(matrice.R2.adj)

plt2 = ggplot(matrice.R2.adj) +
  geom_density(aes(x = V3, color="ambiente"), fill="dodgerblue4", alpha=.5) +
  geom_density(aes(x = V1, color = "genetica"), fill="pink", alpha=.5) +
  geom_density(aes(x = V2, color = "trattamento"), fill="dodgerblue", alpha=.5) +
  labs(x = "R^2 corr. modello ridotto / R^2 corr. modello completo",
       y = "Density",
       color = "Legend") +
  scale_color_manual(values = colors) +
  theme_bw()

# remotes::install_github("larmarange/JLutils")
# library(JLutils)
leg = get_legend(plt2)

grid.arrange(plt1, plt2 + theme(legend.position="none"), leg, ncol=3, widths=c(2,2,0.8))



# Correzioen test anova ----------------------
matrice.evalue <- apply(matrice.pvalue, 2, 
                        function(x) (1 - x + x * log(x)) / (x * (-log(x))^2))

adjust_e_values <- function(e_values) {
  names(e_values) <- c(1:length(e_values))
  sorted_e_values <- sort(e_values)
  S <- cumsum(sorted_e_values)
  tmp = sorted_e_values
  for(k in 1:length(sorted_e_values)){
    tmp[k]  = sorted_e_values[k]
    for (i in 1:k){
      e = (S[i]+ sorted_e_values[k])/(i+1)
      if(e < tmp[k]) tmp[k] = e
    }
  }
  tmp <- tmp[sort(names(tmp))]
  return(tmp)
}

matrice_evalues_adj <- t(apply(matrice.evalue, 1, adjust_e_values))



matrice_evalues_adj <- as.data.frame(matrice_evalues_adj)
colnames(matrice_evalues_adj) = c("V1", "V2", "V3")
plt2.3.res = ggplot(matrice_evalues_adj) +
  #geom_density(aes(x = V3, color="ambiente"), fill="dodgerblue4", alpha=.5) +
  #geom_density(aes(x = V1, color = "genetica"), fill="pink", alpha=.5) +
  geom_density(aes(x = V2, color = "trattamento"), fill="dodgerblue", alpha=.5) +
  labs(x = "R^2 modello ridotto / R^2 modello completo",
       y = "Density",
       color = "Legend") +
  scale_color_manual(values = colors) + geom_vline(xintercept = colMeans(matrice_evalues_adj)[2], col="dodgerblue3") + 
  geom_vline(xintercept = 1, col="red") + 
  theme_bw()

# plt2.1.res
# plt2.2.res
plt2.3.res

evalues <- colMeans(matrice_evalues_adj)
evalues
adjust_e_values(colMeans(matrice.evalue))

