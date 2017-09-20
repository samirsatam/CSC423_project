library(car)
library(MASS)

Auto.Dataset_FromCSV <- read.csv("E:/Users/XI/Desktop/AutoDataset.csv", na.strings=c("NA","?",""))
Auto.Dataset <- na.omit(Auto.Dataset_FromCSV)

AutoWithDummy <- Auto.Dataset[, c("NORMLOSS", "WHEEL", "LENGTH", "WIDTH", "HEIGHT", "CURB", "CC", "BORE", "STROKE", "CPRATIO", "HP", "RPM", "CTYMPG", "HWYMPG", "PRICE")]
AutoWithDummy$RISK_minus1 = ifelse(Auto.Dataset$RISK == -1, 1, 0)
AutoWithDummy$RISK_0 = ifelse(Auto.Dataset$RISK == 0, 1, 0)
AutoWithDummy$RISK_1 = ifelse(Auto.Dataset$RISK == 1, 1, 0)
AutoWithDummy$RISK_2 = ifelse(Auto.Dataset$RISK == 2, 1, 0)
AutoWithDummy$RISK_3 = ifelse(Auto.Dataset$RISK == 3, 1, 0)
AutoWithDummy$MAKE_bmw = ifelse(Auto.Dataset$MAKE == 'bmw', 1, 0)
AutoWithDummy$MAKE_chv = ifelse(Auto.Dataset$MAKE == 'chevrolet', 1, 0)
AutoWithDummy$MAKE_dodge = ifelse(Auto.Dataset$MAKE == 'dodge', 1, 0)
AutoWithDummy$MAKE_honda = ifelse(Auto.Dataset$MAKE == 'honda', 1, 0)
AutoWithDummy$MAKE_jag = ifelse(Auto.Dataset$MAKE == 'jaguar', 1, 0)
AutoWithDummy$MAKE_mazda = ifelse(Auto.Dataset$MAKE == 'mazda', 1, 0)
AutoWithDummy$MAKE_merc = ifelse(Auto.Dataset$MAKE == 'mercedes-benz', 1, 0)
AutoWithDummy$MAKE_mit = ifelse(Auto.Dataset$MAKE == 'mitsubishi', 1, 0)
AutoWithDummy$MAKE_nissan = ifelse(Auto.Dataset$MAKE == 'nissan', 1, 0)
AutoWithDummy$MAKE_peugot = ifelse(Auto.Dataset$MAKE == 'peugot', 1, 0)
AutoWithDummy$MAKE_ply = ifelse(Auto.Dataset$MAKE == 'plymouth', 1, 0)
AutoWithDummy$MAKE_porsche = ifelse(Auto.Dataset$MAKE == 'porsche', 1, 0)
AutoWithDummy$MAKE_saab = ifelse(Auto.Dataset$MAKE == 'saab', 1, 0)
AutoWithDummy$MAKE_subr = ifelse(Auto.Dataset$MAKE == 'subaru', 1, 0)
AutoWithDummy$MAKE_toty = ifelse(Auto.Dataset$MAKE == 'toyota', 1, 0)
AutoWithDummy$MAKE_vlkswg = ifelse(Auto.Dataset$MAKE == 'volkswagen', 1, 0)
AutoWithDummy$MAKE_volvo = ifelse(Auto.Dataset$MAKE == 'volvo', 1, 0)

AutoWithDummy$FUEL = ifelse(Auto.Dataset$FUEL == 'gas', 1, 0)

AutoWithDummy$ASPIRATION = ifelse(Auto.Dataset$ASPIRATION == 'turbo', 1, 0)

AutoWithDummy$DOOR = ifelse(Auto.Dataset$DOOR == 'two', 1, 0)

AutoWithDummy$BODY_hdtp = ifelse(Auto.Dataset$BODY == 'hardtop', 1, 0)
AutoWithDummy$BODY_hchbk = ifelse(Auto.Dataset$BODY == 'hatchback', 1, 0)
AutoWithDummy$BODY_sedan = ifelse(Auto.Dataset$BODY == 'sedan', 1, 0)
AutoWithDummy$BODY_wagon = ifelse(Auto.Dataset$BODY == 'wagon', 1, 0)

AutoWithDummy$DRIVE_fwd = ifelse(Auto.Dataset$DRIVE == 'fwd', 1, 0)
AutoWithDummy$DRIVE_rwd = ifelse(Auto.Dataset$DRIVE == 'rwd', 1, 0)

#AutoWithDummy$ENGINE = ifelse(Auto.Dataset$ENGINE == 'rear', 1, 0)

AutoWithDummy$ENG_TYPE_l = ifelse(Auto.Dataset$ENG_TYPE == 'l', 1, 0)
AutoWithDummy$ENG_TYPE_ohc = ifelse(Auto.Dataset$ENG_TYPE == 'ohc', 1, 0)
AutoWithDummy$ENG_TYPE_ohcf = ifelse(Auto.Dataset$ENG_TYPE == 'ohcf', 1, 0)
AutoWithDummy$ENG_TYPE_ohcv = ifelse(Auto.Dataset$ENG_TYPE == 'ohcv', 1, 0)

AutoWithDummy$CYL_4 = ifelse(Auto.Dataset$CYL == 'four', 1, 0)
AutoWithDummy$CYL_5 = ifelse(Auto.Dataset$CYL == 'five', 1, 0)
AutoWithDummy$CYL_6 = ifelse(Auto.Dataset$CYL == 'six', 1, 0)
AutoWithDummy$CYL_8 = ifelse(Auto.Dataset$CYL == 'eight', 1, 0)

AutoWithDummy$FUELSYS_2bbl = ifelse(Auto.Dataset$FUELSYS == '2bbl', 1, 0)
AutoWithDummy$FUELSYS_idi = ifelse(Auto.Dataset$FUELSYS == 'idi', 1, 0)
AutoWithDummy$FUELSYS_mfi = ifelse(Auto.Dataset$FUELSYS == 'mfi', 1, 0)
AutoWithDummy$FUELSYS_mpfi = ifelse(Auto.Dataset$FUELSYS == 'mpfi', 1, 0)
AutoWithDummy$FUELSYS_spdi = ifelse(Auto.Dataset$FUELSYS == 'spdi', 1, 0)

# Find the correlations.
AutoCorrelations <- Auto.Dataset[, c("NORMLOSS", "WHEEL", "LENGTH", "WIDTH", "HEIGHT", "CURB", "CC", "BORE", "STROKE", "CPRATIO", "HP", "RPM", "CTYMPG", "HWYMPG", "PRICE")]
cor(AutoCorrelations)

# Make the scatterplot
plot(AutoCorrelations)

#Remove correlated variables (LENGTH, WIDTH, CURB, HP, CTYMPG, HWYMPG)
AutoWithoutCor <- AutoWithDummy[, c(-3, -4, -6,-11, -13, -14)]

#Stepwise selection
min.model = lm(PRICE ~ 1, data = AutoWithoutCor)
# Make a Full model with all variables, and run Stepwise selection.
biggest = formula(lm(PRICE ~ ., data = AutoWithoutCor))
#full.model = lm(biggest, data = AutoWithDummy)
stepwise_Model = step(min.model, direction = 'both', scope = biggest, trace = 1)

# Calculate the Variance Inflation Factor for the result of stepwise selection.
vif(stepwise_Model)

#We see that the best model is with the lowest AIC scrore of 2315.16. 
#In addition, we find that the VIF of each variables in this model is less than 10, 
#showing no correlation existed between variables. 
#Therefore, we will use the following variables to build the model
#CC, WHEEL, NORMLOSS, HEIGHT, MAKE_merc, MAKE_bmw, MAKE_porsche, 
#MAKE_Saab, MAKE_jag, MAKE_volvo, MAKE_mazda, MAKE_honda, MAKE_nissan, 
#MAKE_mit, FUELSYS_mpfi, ASPIRATION, CYL_8, CYL_4, CYL_5, DRIVE_fwd, DOOR, RISK_1, BODY_hdtp

#--------------------------------------------------------------------------------------------#
#Construct plots to determine interaction terms.
AutoIntPlot<- Auto.Dataset
AutoIntPlot$RISK = as.character(AutoIntPlot$RISK)
AutoIntPlot$RISK[AutoIntPlot$RISK == "-1"] = 'other'
AutoIntPlot$RISK[AutoIntPlot$RISK == "0"] = 'other'
AutoIntPlot$RISK[AutoIntPlot$RISK == "2"] = 'other'
AutoIntPlot$RISK[AutoIntPlot$RISK == '3'] = 'other'
AutoIntPlot$RISK[AutoIntPlot$RISK == "-2"] = 'other'

AutoIntPlot$MAKE = as.character(AutoIntPlot$MAKE)
AutoIntPlot$MAKE[AutoIntPlot$MAKE == 'chevrolet'] = 'other'
AutoIntPlot$MAKE[AutoIntPlot$MAKE == 'dodge'] = 'other'
AutoIntPlot$MAKE[AutoIntPlot$MAKE == 'peugot'] = 'other'
AutoIntPlot$MAKE[AutoIntPlot$MAKE == 'plymouth'] = 'other'
AutoIntPlot$MAKE[AutoIntPlot$MAKE == 'subaru'] = 'other'
AutoIntPlot$MAKE[AutoIntPlot$MAKE == 'toyota'] = 'other'
AutoIntPlot$MAKE[AutoIntPlot$MAKE == 'volkswagen'] = 'other'
AutoIntPlot$MAKE[AutoIntPlot$MAKE == 'audi'] = 'other'

AutoIntPlot$CYL = as.character(AutoIntPlot$CYL)
AutoIntPlot$CYL[AutoIntPlot$CYL == 'six'] = 'other'
AutoIntPlot$CYL[AutoIntPlot$CYL == 'three'] = 'other'

AutoIntPlot$DOOR = as.character(AutoIntPlot$DOOR)

AutoIntPlot$DRIVE = as.character(AutoIntPlot$DRIVE)
AutoIntPlot$DRIVE = ifelse(AutoIntPlot$DRIVE == 'fwd', 'fwd', 'other')

AutoIntPlot$FUELSYS = as.character(AutoIntPlot$FUELSYS)
AutoIntPlot$FUELSYS = ifelse(AutoIntPlot$FUELSYS == 'mpfi', 'mpfi', 'other')

AutoIntPlot$ASPIRATION = as.character(AutoIntPlot$ASPIRATION)

AutoIntPlot$BODY = as.character(AutoIntPlot$BODY)
AutoIntPlot$BODY = ifelse(AutoIntPlot$BODY == 'hardtop', 'hardtop', 'other')

#Interaction plot between qualitative variables
#RISK & MAKE
agg = aggregate(PRICE ~ RISK + MAKE, AutoIntPlot, mean)
interaction.plot(agg$MAKE, agg$RISK, agg$PRICE, type="b", pch=c(17,18), lty=1, lwd=2, col=rainbow(2), legend = TRUE, las=2, main="Interaction Plot", ann=FALSE)
mtext("MAKE", side = 1, line = 1, at = 13, cex = 1.5, col="BLUE")
mtext("Mean of Price", side=2, line=3, at=20000, cex=1.5, col="BLUE")
mtext("Interaction Plot", side=3, line=1.5, at=6.5, cex=2.5)

#RISK & FUELSYS
agg = aggregate(PRICE ~ RISK + FUELSYS, AutoIntPlot, mean)
interaction.plot(agg$FUELSYS, agg$RISK, agg$PRICE, type="b", pch=c(17,18), lty=1, lwd=2, col=rainbow(2), legend = TRUE, las=2, main="Interaction Plot", ann=FALSE, )
mtext("FUELSYS", side = 1, line = 1, at = 2.2, cex = 1.5, col="BLUE")
mtext("Mean of Price", side=2, line=3, at=12000, cex=1.5, col="BLUE")
mtext("Interaction Plot", side=3, line=1.5, at=1.5, cex=2.5)

#RISK & ASPRIATION
agg = aggregate(PRICE ~ RISK + ASPIRATION, AutoIntPlot, mean)
interaction.plot(agg$ASPIRATION, agg$RISK, agg$PRICE, type="b", pch=c(17,18), lty=1, lwd=2, col=rainbow(2), legend = TRUE, las=2, main="Interaction Plot", ann=FALSE, )
mtext("ASPIRATION", side = 1, line = 1, at = 2.2, cex = 1.5, col="BLUE")
mtext("Mean of Price", side=2, line=3, at=12000, cex=1.5, col="BLUE")
mtext("Interaction Plot", side=3, line=1.5, at=1.5, cex=2.5)

#RISK & DRIVE
agg = aggregate(PRICE ~ RISK + DRIVE, AutoIntPlot, mean)
interaction.plot(agg$DRIVE, agg$RISK, agg$PRICE, type="b", pch=c(17,18), lty=1, lwd=2, col=rainbow(2), legend = TRUE, las=2, main="Interaction Plot", ann=FALSE, )
mtext("DRIVE", side = 1, line = 1, at = 2.2, cex = 1.5, col="BLUE")
mtext("Mean of Price", side=2, line=3, at=12000, cex=1.5, col="BLUE")
mtext("Interaction Plot", side=3, line=1.5, at=1.5, cex=2.5)

#RISK & CYL
agg = aggregate(PRICE ~ RISK + CYL, AutoIntPlot, mean)
interaction.plot(agg$CYL, agg$RISK, agg$PRICE, type="b", pch=c(17,18), lty=1, lwd=2, col=rainbow(2), legend = TRUE, las=2, main="Interaction Plot", ann=FALSE, )
mtext("CYL", side = 1, line = 1, at = 4.5, cex = 1.5, col="BLUE")
mtext("Mean of Price", side=2, line=3, at=20000, cex=1.5, col="BLUE")
mtext("Interaction Plot", side=3, line=1.5, at=2.7, cex=2.5)

#RISK & DOOR
agg = aggregate(PRICE ~ RISK + DOOR, AutoIntPlot, mean)
interaction.plot(agg$DOOR, agg$RISK, agg$PRICE, type="b", pch=c(17,18), lty=1, lwd=2, col=rainbow(2), legend = TRUE, las=2, main="Interaction Plot", ann=FALSE, )
mtext("DOOR", side = 1, line = 3.2, at = 2.2, cex = 1.5, col="BLUE")
mtext("Mean of Price", side=2, line=3, at=10000, cex=1.5, col="BLUE")
mtext("Interaction Plot", side=3, line=1.5, at=1.5, cex=2.5)

#RISK & BODY
agg = aggregate(PRICE ~ RISK + BODY, AutoIntPlot, mean)
interaction.plot(agg$BODY, agg$RISK, agg$PRICE, type="b", pch=c(17,18), lty=1, lwd=2, col=rainbow(2), legend = TRUE, las=2, main="Interaction Plot", ann=FALSE, )
mtext("BODY", side = 1, line = 1, at = 2.2, cex = 1.5, col="BLUE")
mtext("Mean of Price", side=2, line=3, at=10000, cex=1.5, col="BLUE")
mtext("Interaction Plot", side=3, line=1.5, at=1.5, cex=2.5)

#FUELSYS & MAKE
agg = aggregate(PRICE ~ FUELSYS + MAKE, AutoIntPlot, mean)
interaction.plot(agg$MAKE, agg$FUELSYS, agg$PRICE, type="b", pch=c(17,18), lty=1, lwd=2, col=rainbow(2), legend = TRUE, las=2, main="Interaction Plot", ann=FALSE, )
mtext("MAKE", side = 1, line = 1, at = 13, cex = 1.5, col="BLUE")
mtext("Mean of Price", side=2, line=3, at=22000, cex=1.5, col="BLUE")
mtext("Interaction Plot", side=3, line=1.5, at=6.5, cex=2.5)

#FUELSYS & ASPIRATION
agg = aggregate(PRICE ~ FUELSYS + ASPIRATION, AutoIntPlot, mean)
interaction.plot(agg$ASPIRATION, agg$FUELSYS, agg$PRICE, type="b", pch=c(17,18), lty=1, lwd=2, col=rainbow(2), legend = TRUE, las=2, main="Interaction Plot", ann=FALSE, )
mtext("ASPIRATION", side = 1, line = 1, at = 2.2, cex = 1.5, col="BLUE")
mtext("Mean of Price", side=2, line=3, at=12000, cex=1.5, col="BLUE")
mtext("Interaction Plot", side=3, line=1.5, at=1.5, cex=2.5)

#FUELSYS & CYL
agg = aggregate(PRICE ~ FUELSYS + CYL, AutoIntPlot, mean)
interaction.plot(agg$CYL, agg$FUELSYS, agg$PRICE, type="b", pch=c(17,18), lty=1, lwd=2, col=rainbow(2), legend = TRUE, las=2, main="Interaction Plot", ann=FALSE, )
mtext("CYL", side = 1, line = 1, at = 4.5, cex = 1.5, col="BLUE")
mtext("Mean of Price", side=2, line=3, at=22000, cex=1.5, col="BLUE")
mtext("Interaction Plot", side=3, line=1.5, at=2.7, cex=2.5)

#FUELSYS & DRIVE
agg = aggregate(PRICE ~ FUELSYS + DRIVE, AutoIntPlot, mean)
interaction.plot(agg$DRIVE, agg$FUELSYS, agg$PRICE, type="b", pch=c(17,18), lty=1, lwd=2, col=rainbow(2), legend = TRUE, las=2, main="Interaction Plot", ann=FALSE, )
mtext("DRIVE", side = 1, line = 1, at = 2.2, cex = 1.5, col="BLUE")
mtext("Mean of Price", side=2, line=3, at=12000, cex=1.5, col="BLUE")
mtext("Interaction Plot", side=3, line=1.5, at=1.5, cex=2.5)

#FUELSYS & DOOR
agg = aggregate(PRICE ~ FUELSYS + DOOR, AutoIntPlot, mean)
interaction.plot(agg$DOOR, agg$FUELSYS, agg$PRICE, type="b", pch=c(17,18), lty=1, lwd=2, col=rainbow(2), legend = TRUE, las=2, main="Interaction Plot", ann=FALSE, )
mtext("DOOR", side = 1, line = 1, at = 2.2, cex = 1.5, col="BLUE")
mtext("Mean of Price", side=2, line=3, at=12000, cex=1.5, col="BLUE")
mtext("Interaction Plot", side=3, line=1.5, at=1.5, cex=2.5)

#FUELSYS & BODY
agg = aggregate(PRICE ~ FUELSYS + BODY, AutoIntPlot, mean)
interaction.plot(agg$BODY, agg$FUELSYS, agg$PRICE, type="b", pch=c(17,18), lty=1, lwd=2, col=rainbow(2), legend = TRUE, las=2, main="Interaction Plot", ann=FALSE, )
mtext("BODY", side = 1, line = 1, at = 2.2, cex = 1.5, col="BLUE")
mtext("Mean of Price", side=2, line=3, at=14000, cex=1.5, col="BLUE")
mtext("Interaction Plot", side=3, line=1.5, at=1.5, cex=2.5)

#ASPIRATION & MAKE
agg = aggregate(PRICE ~ ASPIRATION + MAKE, AutoIntPlot, mean)
interaction.plot(agg$MAKE, agg$ASPIRATION, agg$PRICE, type="b", pch=c(17,18), lty=1, lwd=2, col=rainbow(2), legend = TRUE, las=2, main="Interaction Plot", ann=FALSE, )
mtext("MAKE", side = 1, line = 1, at = 13, cex = 1.5, col="BLUE")
mtext("Mean of Price", side=2, line=3, at=22000, cex=1.5, col="BLUE")
mtext("Interaction Plot", side=3, line=1.5, at=2.7, cex=2.5)

#ASPIRATION & CYL
agg = aggregate(PRICE ~ ASPIRATION + CYL, AutoIntPlot, mean)
interaction.plot(agg$CYL, agg$ASPIRATION, agg$PRICE, type="b", pch=c(17,18), lty=1, lwd=2, col=rainbow(2), legend = TRUE, las=2, main="Interaction Plot", ann=FALSE, )
mtext("CYL", side = 1, line = 1, at = 4.5, cex = 1.5, col="BLUE")
mtext("Mean of Price", side=2, line=3, at=22000, cex=1.5, col="BLUE")
mtext("Interaction Plot", side=3, line=1.5, at=2.7, cex=2.5)

#ASPIRATION & DRIVE
agg = aggregate(PRICE ~ ASPIRATION + DRIVE, AutoIntPlot, mean)
interaction.plot(agg$DRIVE, agg$ASPIRATION, agg$PRICE, type="b", pch=c(17,18), lty=1, lwd=2, col=rainbow(2), legend = TRUE, las=2, main="Interaction Plot", ann=FALSE, )
mtext("DRIVE", side = 1, line = 1, at = 2.2, cex = 1.5, col="BLUE")
mtext("Mean of Price", side=2, line=3, at=14000, cex=1.5, col="BLUE")
mtext("Interaction Plot", side=3, line=1.5, at=1.5, cex=2.5)

#ASPIRATIOn & DOOR
agg = aggregate(PRICE ~ ASPIRATION + DOOR, AutoIntPlot, mean)
interaction.plot(agg$DOOR, agg$ASPIRATION, agg$PRICE, type="b", pch=c(17,18), lty=1, lwd=2, col=rainbow(2), legend = TRUE, las=2, main="Interaction Plot", ann=FALSE, )
mtext("DOOR", side = 1, line = 1, at = 2.2, cex = 1.5, col="BLUE")
mtext("Mean of Price", side=2, line=3, at=14000, cex=1.5, col="BLUE")
mtext("Interaction Plot", side=3, line=1.5, at=1.5, cex=2.5)

#ASPIRATION & BODY
agg = aggregate(PRICE ~ ASPIRATION + BODY, AutoIntPlot, mean)
interaction.plot(agg$BODY, agg$ASPIRATION, agg$PRICE, type="b", pch=c(17,18), lty=1, lwd=2, col=rainbow(2), legend = TRUE, las=2, main="Interaction Plot", ann=FALSE, )
mtext("BODY", side = 1, line = 1, at = 2.2, cex = 1.5, col="BLUE")
mtext("Mean of Price", side=2, line=3, at=18000, cex=1.5, col="BLUE")
mtext("Interaction Plot", side=3, line=1.5, at=1.5, cex=2.5)

#DRIVE & MAKE
agg = aggregate(PRICE ~ DRIVE + MAKE, AutoIntPlot, mean)
interaction.plot(agg$MAKE, agg$DRIVE, agg$PRICE, type="b", pch=c(17,18), lty=1, lwd=2, col=rainbow(2), legend = TRUE, las=2, main="Interaction Plot", ann=FALSE, )
mtext("MAKE", side = 1, line = 1, at = 13, cex = 1.5, col="BLUE")
mtext("Mean of Price", side=2, line=3, at=20000, cex=1.5, col="BLUE")
mtext("Interaction Plot", side=3, line=1.5, at=6.5, cex=2.5)

#DRIVE & CYL
agg = aggregate(PRICE ~ DRIVE + CYL, AutoIntPlot, mean)
interaction.plot(agg$CYL, agg$DRIVE, agg$PRICE, type="b", pch=c(17,18), lty=1, lwd=2, col=rainbow(2), legend = TRUE, las=2, main="Interaction Plot", ann=FALSE, )
mtext("CYL", side = 1, line = 1, at = 4.5, cex = 1.5, col="BLUE")
mtext("Mean of Price", side=2, line=3, at=22000, cex=1.5, col="BLUE")
mtext("Interaction Plot", side=3, line=1.5, at=2.8, cex=2.5)

#DRIVE & DOOR
agg = aggregate(PRICE ~ DRIVE + DOOR, AutoIntPlot, mean)
interaction.plot(agg$DOOR, agg$DRIVE, agg$PRICE, type="b", pch=c(17,18), lty=1, lwd=2, col=rainbow(2), legend = TRUE, las=2, main="Interaction Plot", ann=FALSE, )
mtext("DOOR", side = 1, line = 1, at = 2.2, cex = 1.5, col="BLUE")
mtext("Mean of Price", side=2, line=3, at=12000, cex=1.5, col="BLUE")
mtext("Interaction Plot", side=3, line=1.5, at=1.5, cex=2.5)

#DRIVE & BODY
agg = aggregate(PRICE ~ DRIVE + BODY, AutoIntPlot, mean)
interaction.plot(agg$BODY, agg$DRIVE, agg$PRICE, type="b", pch=c(17,18), lty=1, lwd=2, col=rainbow(2), legend = TRUE, las=2, main="Interaction Plot", ann=FALSE, )
mtext("BODY", side = 1, line = 1, at = 2.2, cex = 1.5, col="BLUE")
mtext("Mean of Price", side=2, line=3, at=12000, cex=1.5, col="BLUE")
mtext("Interaction Plot", side=3, line=1.5, at=1.5, cex=2.5)

#CYL & MAKE
agg = aggregate(PRICE ~ CYL + MAKE, AutoIntPlot, mean)
interaction.plot(agg$MAKE, agg$CYL, agg$PRICE, type="b", pch=c(15,16,17,18), lty=1, lwd=2, col=rainbow(4), legend = TRUE, las=2, main="Interaction Plot", ann=FALSE, )
mtext("MAKE", side = 1, line = 1, at = 13, cex = 1.5, col="BLUE")
mtext("Mean of Price", side=2, line=3, at=22000, cex=1.5, col="BLUE")
mtext("Interaction Plot", side=3, line=1.5, at=6, cex=2.5)

#CYL & DOOR
agg = aggregate(PRICE ~ CYL + DOOR, AutoIntPlot, mean)
interaction.plot(agg$DOOR, agg$CYL, agg$PRICE, type="b", pch=c(15,16,17,18), lty=1, lwd=2, col=rainbow(4), legend = TRUE, las=2, main="Interaction Plot", ann=FALSE, )
mtext("DOOR", side = 1, line = 1, at = 2.2, cex = 1.5, col="BLUE")
mtext("Mean of Price", side=2, line=3, at=22000, cex=1.5, col="BLUE")
mtext("Interaction Plot", side=3, line=1.5, at=1.5, cex=2.5)

#CYL & BODY
agg = aggregate(PRICE ~ CYL + BODY, AutoIntPlot, mean)
interaction.plot(agg$BODY, agg$CYL, agg$PRICE, type="b", pch=c(15,16,17,18), lty=1, lwd=2, col=rainbow(4), legend = TRUE, las=2, main="Interaction Plot", ann=FALSE, )
mtext("BODY", side = 1, line = 1, at = 2.2, cex = 1.5, col="BLUE")
mtext("Mean of Price", side=2, line=3, at=22000, cex=1.5, col="BLUE")
mtext("Interaction Plot", side=3, line=1.5, at=1.5, cex=2.5)

#DOOR & MAKE
agg = aggregate(PRICE ~ DOOR + MAKE, AutoIntPlot, mean)
interaction.plot(agg$MAKE, agg$DOOR, agg$PRICE, type="b", pch=c(17,18), lty=1, lwd=2, col=rainbow(2), legend = TRUE, las=2, main="Interaction Plot", ann=FALSE, )
mtext("MAKE", side = 1, line = 1, at = 13, cex = 1.5, col="BLUE")
mtext("Mean of Pirce", side=2, line=3, at=20000, cex=1.5, col="BLUE")
mtext("Interaction Plot", side=3, line=1.5, at=6.5, cex=2.5)

#DOOR & BODY
agg = aggregate(PRICE ~ DOOR + BODY, AutoIntPlot, mean)
interaction.plot(agg$BODY, agg$DOOR, agg$PRICE, type="b", pch=c(17,18), lty=1, lwd=2, col=rainbow(2), legend = TRUE, las=2, main="Interaction Plot", ann=FALSE, )
mtext("BODY", side = 1, line = 1, at = 2.2, cex = 1.5, col="BLUE")
mtext("Mean of Pirce", side=2, line=3, at=11500, cex=1.5, col="BLUE")
mtext("Interaction Plot", side=3, line=1.5, at=1.5, cex=2.5)

#BODY & MAKE
agg = aggregate(PRICE ~ BODY + MAKE, AutoIntPlot, mean)
interaction.plot(agg$MAKE, agg$BODY, agg$PRICE, type="b", pch=c(17,18), lty=1, lwd=2, col=rainbow(2), legend = TRUE, las=2, main="Interaction Plot", ann=FALSE, )
mtext("MAKE", side = 1, line = 1, at = 13, cex = 1.5, col="BLUE")
mtext("Mean of Pirce", side=2, line=3, at=20000, cex=1.5, col="BLUE")
mtext("Interaction Plot", side=3, line=1.5, at=6.5, cex=2.5)


#In order to determine the interaction terms between qualitative variables and quantitative variables, 
#we construct the scatterplot of PRICE and a individual quantitative variables. 
#We mark the points with different colors and sizes according to the value of a qualitative variable. 
#If there is an interaction term between a qualitative variable and a quantitative variable, 
#different sets of points should construct lines with different shape.

#Scatterplot of HEIGHT and PRICE.
plot(PRICE ~ HEIGHT, data = AutoIntPlot, subset = RISK == '1', pch = 15, cex=0.6, xlim=c(46, 61), ylim = c(500, 45500), main="Scatterplot of HEIGHT VS. PRICE")
points(PRICE ~ HEIGHT, data = AutoIntPlot, subset = RISK == 'other', pch = 16, col = 'RED', cex = 0.6)
legend(60, 45500, c("1", "other"), pch = c(15, 16), col = c("black", "red"), title = "RISK")

plot(PRICE ~ HEIGHT, data = AutoIntPlot, subset = FUELSYS == 'mpfi', pch = 15, cex=0.6, xlim=c(46, 61), ylim = c(500, 45500), main="Scatterplot of HEIGHT VS. PRICE")
points(PRICE ~ HEIGHT, data = AutoIntPlot, subset = FUELSYS == 'other', pch = 16, col = 'RED', cex = 0.6)
legend(60, 45500, c("mpfi", "other"), pch = c(15, 16), col = c("black", "red"), title = "FUELSYS")

plot(PRICE ~ HEIGHT, data = AutoIntPlot, subset = ASPIRATION == 'turbo', pch = 15, cex=0.5, xlim=c(46, 61), ylim = c(500, 45500), main="Scatterplot of HEIGHT VS. PRICE")
points(PRICE ~ HEIGHT, data = AutoIntPlot, subset = ASPIRATION == 'std', pch = 16, col = 'RED', cex = 0.5)
legend(60, 45500, c("turbo", "std"), pch = c(15, 16), col = c("black", "red"), title = "ASPIRATION")

plot(PRICE ~ HEIGHT, data = AutoIntPlot, subset = CYL == 'four', pch = 15, xlim=c(46, 61), ylim = c(500, 45500), main="Scatterplot of HEIGHT VS. PRICE", cex=0.6)
points(PRICE ~ HEIGHT, data = AutoIntPlot, subset = CYL == 'five', pch = 16, col = 'RED')
points(PRICE ~ HEIGHT, data = AutoIntPlot, subset = CYL == 'eight', pch = 17, col = 'BLUE')
points(PRICE ~ HEIGHT, data = AutoIntPlot, subset = CYL == 'other', pch = 18, col = 'GREEN')
legend(60, 45500, c("four", "five", "eight", "other"), pch = c(15, 16, 17, 18), col = c("black", "red", "blue", "green"), title = "CYL")

plot(PRICE ~ HEIGHT, data = AutoIntPlot, subset = DRIVE == 'fwd', pch = 15, cex=0.6, xlim=c(46, 61), ylim = c(500, 45500), main="Scatterplot of HEIGHT VS. PRICE")
points(PRICE ~ HEIGHT, data = AutoIntPlot, subset = DRIVE == 'other', pch = 16, col = 'RED', cex = 0.7)
legend(60, 45500, c("fwd", "other"), pch = c(15, 16), col = c("black", "red"), title = "DRIVE")

plot(PRICE ~ HEIGHT, data = AutoIntPlot, subset = DOOR == 'four', pch = 15, cex=0.6, xlim=c(46, 61), ylim = c(500, 45500), main="Scatterplot of HEIGHT VS. PRICE")
points(PRICE ~ HEIGHT, data = AutoIntPlot, subset = DOOR == 'two', pch = 16, col = 'RED', cex = 0.7)
legend(60, 45500, c("four", "two"), pch = c(15, 16), col = c("black", "red"), title = "DOOR")

plot(PRICE ~ HEIGHT, data = AutoIntPlot, subset = BODY == 'hardtop', pch = 15, cex=0.7, xlim=c(46, 61), ylim = c(500, 45500), main="Scatterplot of HEIGHT VS. PRICE")
points(PRICE ~ HEIGHT, data = AutoIntPlot, subset = BODY == 'other', pch = 16, col = 'RED', cex = 0.7)
legend(60, 45500, c("hardtop", "other"), pch = c(15, 16), col = c("black", "red"), title = "BODY")

#Scatterplot of NORMLOSS and PRICE.
plot(PRICE ~ NORMLOSS, data = AutoIntPlot, subset = RISK == '1', pch = 15, cex=0.6, xlim=c(60, 260), ylim = c(500, 45500), main="Scatterplot of NORMLOSS VS. PRICE")
points(PRICE ~ NORMLOSS, data = AutoIntPlot, subset = RISK == 'other', pch = 16, col = 'RED', cex = 0.7)
legend(245, 45500, c("1", "other"), pch = c(15, 16), col = c("black", "red"), title = "RISK")

plot(PRICE ~ NORMLOSS, data = AutoIntPlot, subset = FUELSYS == 'mpfi', pch = 15, cex=0.6, xlim=c(60, 260), ylim = c(500, 45500), main="Scatterplot of NORMLOSS VS. PRICE")
points(PRICE ~ NORMLOSS, data = AutoIntPlot, subset = FUELSYS == 'other', pch = 16, col = 'RED', cex = 0.7)
legend(245, 45500, c("mpfi", "other"), pch = c(15, 16), col = c("black", "red"), title = "FUELSYS")

plot(PRICE ~ NORMLOSS, data = AutoIntPlot, subset = ASPIRATION == 'turbo', pch = 15, cex=0.6, xlim=c(60, 260), ylim = c(500, 45500), main="Scatterplot of NORMLOSS VS. PRICE")
points(PRICE ~ NORMLOSS, data = AutoIntPlot, subset = ASPIRATION == 'std', pch = 16, col = 'RED', cex = 0.7)
legend(245, 45500, c("turbo", "std"), pch = c(15, 16), col = c("black", "red"), title = "ASPIRATION")

plot(PRICE ~ NORMLOSS, data = AutoIntPlot, subset = CYL == 'four', pch = 15, cex=0.6, xlim=c(60, 260), ylim = c(500, 45500), main="Scatterplot of NORMLOSS VS. PRICE")
points(PRICE ~ NORMLOSS, data = AutoIntPlot, subset = CYL == 'five', pch = 16, col = 'RED')
points(PRICE ~ NORMLOSS, data = AutoIntPlot, subset = CYL == 'eight', pch = 17, col = 'BLUE')
points(PRICE ~ NORMLOSS, data = AutoIntPlot, subset = CYL == 'other', pch = 18, col = 'GREEN')
legend(245, 45500, c("four", "five", "eight", "other"), pch = c(15, 16, 17, 18), col = c("black", "red", "blue", "green"), title = "CYL")

plot(PRICE ~ NORMLOSS, data = AutoIntPlot, subset = DRIVE == 'fwd', pch = 15, cex=0.6, xlim=c(60, 260), ylim = c(500, 45500), main="Scatterplot of NORMLOSS VS. PRICE")
points(PRICE ~ NORMLOSS, data = AutoIntPlot, subset = DRIVE == 'other', pch = 16, col = 'RED', cex = 0.7)
legend(245, 45500, c("fwd", "other"), pch = c(15, 16), col = c("black", "red"), title = "DRIVE")

plot(PRICE ~ NORMLOSS, data = AutoIntPlot, subset = DOOR == 'four', pch = 15, cex=0.6, xlim=c(60, 260), ylim = c(500, 45500), main="Scatterplot of NORMLOSS VS. PRICE")
points(PRICE ~ NORMLOSS, data = AutoIntPlot, subset = DOOR == 'two', pch = 16, col = 'RED', cex = 0.7)
legend(245, 45500, c("four", "two"), pch = c(15, 16), col = c("black", "red"), title = "DOOR")

plot(PRICE ~ NORMLOSS, data = AutoIntPlot, subset = BODY == 'hardtop', pch = 15, cex=0.8, xlim=c(60, 260), ylim = c(500, 45500), main="Scatterplot of NORMLOSS VS. PRICE")
points(PRICE ~ NORMLOSS, data = AutoIntPlot, subset = BODY == 'other', pch = 16, col = 'RED', cex = 0.7)
legend(245, 45500, c("hardtop", "other"), pch = c(15, 16), col = c("black", "red"), title = "BODY")

#Scatterplot of WHEEL and PRICE.
plot(PRICE ~ WHEEL, data = AutoIntPlot, subset = RISK == '1', pch = 15, cex=0.6, xlim=c(86, 122), ylim = c(500, 45500), main="Scatterplot of WHEEL VS. PRICE")
points(PRICE ~ WHEEL, data = AutoIntPlot, subset = RISK == 'other', pch = 16, col = 'RED', cex = 0.7)
legend(120, 45500, c("1", "other"), pch = c(15, 16), col = c("black", "red"), title = "RISK")

plot(PRICE ~ WHEEL, data = AutoIntPlot, subset = FUELSYS == 'mpfi', pch = 15, cex=0.6, xlim=c(86, 122), ylim = c(500, 45500), main="Scatterplot of WHEEL VS. PRICE")
points(PRICE ~ WHEEL, data = AutoIntPlot, subset = FUELSYS == 'other', pch = 16, col = 'RED', cex = 0.7)
legend(120, 45500, c("mpfi", "other"), pch = c(15, 16), col = c("black", "red"), title = "FUELSYS")

plot(PRICE ~ WHEEL, data = AutoIntPlot, subset = ASPIRATION == 'turbo', pch = 15, cex=0.6, xlim=c(86, 122), ylim = c(500, 45500), main="Scatterplot of WHEEL VS. PRICE")
points(PRICE ~ WHEEL, data = AutoIntPlot, subset = ASPIRATION == 'std', pch = 16, col = 'RED', cex = 0.7)
legend(118, 45500, c("turbo", "std"), pch = c(15, 16), col = c("black", "red"), title = "ASPIRATION")

plot(PRICE ~ WHEEL, data = AutoIntPlot, subset = CYL == 'four', pch = 15, cex=0.6, xlim=c(86, 122), ylim = c(500, 45500), main="Scatterplot of WHEEL VS. PRICE")
points(PRICE ~ WHEEL, data = AutoIntPlot, subset = CYL == 'five', pch = 16, col = 'RED')
points(PRICE ~ WHEEL, data = AutoIntPlot, subset = CYL == 'eight', pch = 17, col = 'BLUE')
points(PRICE ~ WHEEL, data = AutoIntPlot, subset = CYL == 'other', pch = 18, col = 'GREEN')
legend(120, 45500, c("four", "five", "eight", "other"), pch = c(15, 16, 17, 18), col = c("black", "red", "blue", "green"), title = "CYL")

plot(PRICE ~ WHEEL, data = AutoIntPlot, subset = DRIVE == 'fwd', pch = 15, cex=0.6, xlim=c(86, 122), ylim = c(500, 45500), main="Scatterplot of WHEEL VS. PRICE")
points(PRICE ~ WHEEL, data = AutoIntPlot, subset = DRIVE == 'other', pch = 16, col = 'RED', cex = 0.7)
legend(120, 45500, c("fwd", "other"), pch = c(15, 16), col = c("black", "red"), title = "DRIVE")

plot(PRICE ~ WHEEL, data = AutoIntPlot, subset = DOOR == 'four', pch = 15, cex=0.6, xlim=c(86, 122), ylim = c(500, 45500), main="Scatterplot of WHEEL VS. PRICE")
points(PRICE ~ WHEEL, data = AutoIntPlot, subset = DOOR == 'two', pch = 16, col = 'RED', cex = 0.7)
legend(120, 45500, c("four", "two"), pch = c(15, 16), col = c("black", "red"), title = "DOOR")

plot(PRICE ~ WHEEL, data = AutoIntPlot, subset = BODY == 'hardtop', pch = 15, cex=0.7, xlim=c(86, 122), ylim = c(500, 45500), main="Scatterplot of WHEEL VS. PRICE")
points(PRICE ~ WHEEL, data = AutoIntPlot, subset = BODY == 'other', pch = 16, col = 'RED', cex = 0.7)
legend(119, 45500, c("hardtop", "other"), pch = c(15, 16), col = c("black", "red"), title = "BODY")

#Scatterplot of CC and PRICE.
plot(PRICE ~ CC, data = AutoIntPlot, subset = RISK == '1', pch = 15, cex=0.6, xlim=c(60, 330), ylim = c(500, 45500), main="Scatterplot of CC VS. PRICE")
points(PRICE ~ CC, data = AutoIntPlot, subset = RISK == 'other', pch = 16, col = 'RED', cex = 0.7)
legend(315, 45500, c("1", "other"), pch = c(15, 16), col = c("black", "red"), title = "RISK")

plot(PRICE ~ CC, data = AutoIntPlot, subset = FUELSYS == 'mpfi', pch = 15, cex=0.6, xlim=c(60, 330), ylim = c(500, 45500), main="Scatterplot of CC VS. PRICE")
points(PRICE ~ CC, data = AutoIntPlot, subset = FUELSYS == 'other', pch = 16, col = 'RED', cex = 0.6)
legend(315, 45500, c("mpfi", "other"), pch = c(15, 16), col = c("black", "red"), title = "FUELSYS")

plot(PRICE ~ CC, data = AutoIntPlot, subset = ASPIRATION == 'turbo', pch = 15, cex=0.7, xlim=c(60, 330), ylim = c(500, 45500), main="Scatterplot of CC VS. PRICE")
points(PRICE ~ CC, data = AutoIntPlot, subset = ASPIRATION == 'std', pch = 16, col = 'RED', cex = 0.6)
legend(310, 45500, c("turbo", "std"), pch = c(15, 16), col = c("black", "red"), title = "ASPIRATION")

plot(PRICE ~ CC, data = AutoIntPlot, subset = CYL == 'four', pch = 15, cex =0.6, xlim=c(60, 330), ylim = c(500, 45500), main="Scatterplot of CC VS. PRICE")
points(PRICE ~ CC, data = AutoIntPlot, subset = CYL == 'five', pch = 16, col = 'RED')
points(PRICE ~ CC, data = AutoIntPlot, subset = CYL == 'eight', pch = 17, col = 'BLUE')
points(PRICE ~ CC, data = AutoIntPlot, subset = CYL == 'other', pch = 18, col = 'GREEN')
legend(315, 45500, c("four", "five", "eight", "other"), pch = c(15, 16, 17, 18), col = c("black", "red", "blue", "green"), title = "CYL")

plot(PRICE ~ CC, data = AutoIntPlot, subset = DRIVE == 'fwd', pch = 15, cex=0.6, xlim=c(60, 330), ylim = c(500, 45500), main="Scatterplot of CC VS. PRICE")
points(PRICE ~ CC, data = AutoIntPlot, subset = DRIVE == 'other', pch = 16, col = 'RED', cex = 0.7)
legend(315, 45500, c("fwd", "other"), pch = c(15, 16), col = c("black", "red"), title = "DRIVE")

plot(PRICE ~ CC, data = AutoIntPlot, subset = DOOR == 'four', pch = 15, cex=0.6, xlim=c(60, 330), ylim = c(500, 45500), main="Scatterplot of CC VS. PRICE")
points(PRICE ~ CC, data = AutoIntPlot, subset = DOOR == 'two', pch = 16, col = 'RED', cex = 0.7)
legend(315, 45500, c("four", "two"), pch = c(15, 16), col = c("black", "red"), title = "DOOR")

plot(PRICE ~ CC, data = AutoIntPlot, subset = BODY == 'hardtop', pch = 15, cex=0.7, xlim=c(60, 330), ylim = c(500, 45500), main="Scatterplot of CC VS. PRICE")
points(PRICE ~ CC, data = AutoIntPlot, subset = BODY == 'other', pch = 16, col = 'RED', cex = 0.7)
legend(314, 45500, c("hardtop", "other"), pch = c(15, 16), col = c("black", "red"), title = "BODY")

#Based on the scatterplot above, there is no interaction term 
#between any qualitative variables and quantitative variables.
#The interaction terms between any quantitative variables and the quadratic terms 
#will be directly considered in the beginning regression model.


#---------------------------------------------------------------------------------------------------#
#Now we are going to use these terms to build the model and do regression.

#Make a dataset to be modeled
AutoModel <- AutoWithDummy[,c("RISK_1","FUELSYS_mpfi", "ASPIRATION", "DRIVE_fwd", "DOOR", "BODY_hdtp", "CYL_4", "CYL_5", "CYL_8", "MAKE_merc", "MAKE_bmw", "MAKE_porsche", "MAKE_saab", "MAKE_jag", "MAKE_volvo", "MAKE_mazda", "MAKE_honda", "MAKE_nissan", "MAKE_mit", "CC", "WHEEL", "NORMLOSS", "HEIGHT")]
AutoModel$RISK_1_FUELSYS_mpfi = AutoModel$RISK_1 * AutoModel$FUELSYS_mpfi
AutoModel$RISK_1_ASPIRATION = AutoModel$RISK_1 * AutoModel$ASPIRATION
AutoModel$RISK_1_DRIVE_fwd = AutoModel$RISK_1 * AutoModel$DRIVE_fwd
AutoModel$FUELSYS_mpfi_ASPIRATION = AutoModel$FUELSYS_mpfi * AutoModel$ASPIRATION
AutoModel$FUELSYS_mpfi_CYL_4 = AutoModel$FUELSYS_mpfi * AutoModel$CYL_4
AutoModel$FUELSYS_mpfi_CYL_5 = AutoModel$FUELSYS_mpfi * AutoModel$CYL_5
AutoModel$FUELSYS_mpfi_CYL_8 = AutoModel$FUELSYS_mpfi * AutoModel$CYL_8
AutoModel$FUELSYS_mpfi_DRIVE_fwd = AutoModel$FUELSYS_mpfi * AutoModel$DRIVE_fwd
AutoModel$FUELSYS_mpfi_BODY_hdtp = AutoModel$FUELSYS_mpfi * AutoModel$BODY_hdtp
AutoModel$ASPIRATION_DOOR = AutoModel$ASPIRATION * AutoModel$DOOR
AutoModel$ASPIRATION_BODY_hdtp = AutoModel$ASPIRATION * AutoModel$BODY_hdtp
AutoModel$DOOR_CYL_4 = AutoModel$DOOR * AutoModel$CYL_4
AutoModel$DOOR_CYL_5 = AutoModel$DOOR * AutoModel$CYL_5
AutoModel$DOOR_CYL_8 = AutoModel$DOOR * AutoModel$CYL_8
AutoModel$BODY_hdtp_CYL_4 = AutoModel$BODY_hdtp * AutoModel$CYL_4
AutoModel$BODY_hdtp_CYL_5 = AutoModel$BODY_hdtp * AutoModel$CYL_5
AutoModel$BODY_hdtp_CYL_8 = AutoModel$BODY_hdtp * AutoModel$CYL_8
AutoModel$DRIVE_fwd_BODY_hdtp = AutoModel$DRIVE_fwd * AutoModel$BODY_hdtp
AutoModel$CC_SQU = AutoModel$CC * AutoModel$CC
AutoModel$WHEEL_SQU = AutoModel$WHEEL * AutoModel$WHEEL
AutoModel$NORMLOSS_SQU = AutoModel$NORMLOSS * AutoModel$NORMLOSS
AutoModel$HEIGHT_SQU = AutoModel$HEIGHT * AutoModel$HEIGHT
AutoModel$CC_WHEEL = AutoModel$CC * AutoModel$WHEEL
AutoModel$CC_NORMLOSS = AutoModel$CC * AutoModel$NORMLOSS
AutoModel$CC_HEIGHT = AutoModel$CC * AutoModel$HEIGHT
AutoModel$WHEEL_NORMLOSS = AutoModel$WHEEL * AutoModel$NORMLOSS
AutoModel$WHEEL_HEIGHT = AutoModel$WHEEL * AutoModel$HEIGHT
AutoModel$NORMLOSS_HEIGHT = AutoModel$NORMLOSS * AutoModel$HEIGHT
AutoModel$PRICE = AutoWithDummy$PRICE

#Model Regression
reg_model1 = lm(PRICE ~ ., data = AutoModel)
summary(reg_model1)

#In the reg_model1, global F statistic is significant and adjusted r-square is 0.9479, 
#showing the model is statistically useful. 
#However the p-values of higher order terms and interaction terms between quantitative variables 
#are all larger than 0.05. We remove these terms and construct reg_model2. 

reg_model2 = lm(PRICE ~ RISK_1 + FUELSYS_mpfi + ASPIRATION + DRIVE_fwd + DOOR + BODY_hdtp + CYL_4 + CYL_5 + CYL_8 + MAKE_bmw + MAKE_honda + MAKE_jag + MAKE_mazda+ MAKE_merc + MAKE_mit + MAKE_nissan + MAKE_porsche + MAKE_saab + MAKE_volvo + CC + WHEEL + NORMLOSS + HEIGHT + RISK_1_FUELSYS_mpfi + RISK_1_ASPIRATION + RISK_1_DRIVE_fwd + FUELSYS_mpfi_ASPIRATION + FUELSYS_mpfi_CYL_4 + FUELSYS_mpfi_CYL_5 + FUELSYS_mpfi_CYL_8 + FUELSYS_mpfi_DRIVE_fwd + FUELSYS_mpfi_BODY_hdtp + ASPIRATION_DOOR + ASPIRATION_BODY_hdtp + DOOR_CYL_4 + DOOR_CYL_5 + DOOR_CYL_8 + BODY_hdtp_CYL_4 + BODY_hdtp_CYL_5 + BODY_hdtp_CYL_8 + DRIVE_fwd_BODY_hdtp, data = AutoModel)
summary(reg_model2)

#In the reg_model2, global F statistic is significant and adjusted r-square is 0.9474, 
#showing the model is statistically useful. 
#However some model coefficients of the interaction terms between qualitative variables 
#are not avaiable, and some p-values of interaction terms between qualitative variables 
#are larger than 0.2. We remove these terms and construct reg_model3.

reg_model3 = lm(PRICE ~ RISK_1 + FUELSYS_mpfi + ASPIRATION + DRIVE_fwd + DOOR + BODY_hdtp + CYL_4 + CYL_5 + CYL_8 + MAKE_bmw + MAKE_honda + MAKE_jag + MAKE_mazda + MAKE_merc + MAKE_mit + MAKE_nissan + MAKE_porsche + MAKE_saab + MAKE_volvo + CC + WHEEL + NORMLOSS + HEIGHT + FUELSYS_mpfi_ASPIRATION + FUELSYS_mpfi_BODY_hdtp, data = AutoModel)
summary(reg_model3)

#In the reg_model3, global F statistic is significant and adjusted r-square is 0.9492, 
#showing the model is statistically useful. 
#But the p-value of the interaction term between FUELSYS_mpfi and BODY_hdtp is larger than 0.1. 
#We remove this term and construct reg_model4.

reg_model4 = lm(PRICE ~ RISK_1 + FUELSYS_mpfi + ASPIRATION + DRIVE_fwd + DOOR + BODY_hdtp + CYL_4 + CYL_5 + CYL_8 + MAKE_bmw + MAKE_honda + MAKE_jag + MAKE_mazda + MAKE_merc + MAKE_mit + MAKE_nissan + MAKE_porsche + MAKE_saab + MAKE_volvo + CC + WHEEL + NORMLOSS + HEIGHT + FUELSYS_mpfi_BODY_hdtp, data = AutoModel)
summary(reg_model4)

#In the reg_model4, global F statistic is significant and adjusted r-square is 0.9487, 
#showing the model is statistically useful. But the p-value of Make_mit is larger than 0.1. 
#We remove this term and construct reg_model5. 
#Noting that the p-value of BODY_hdtp is also larger than 0.1, 
#however, there is a interaction term between BODY_hdtp and FUELSYS_mpfi in the model. 
#Therefore we leave BODY_hdtp in the model.

reg_model5 = lm(PRICE ~ RISK_1 + FUELSYS_mpfi + ASPIRATION + DRIVE_fwd + DOOR + BODY_hdtp + CYL_4 + CYL_5 + CYL_8 + MAKE_bmw + MAKE_honda + MAKE_jag + MAKE_mazda + MAKE_merc + MAKE_nissan + MAKE_porsche + MAKE_saab + MAKE_volvo + CC + WHEEL + NORMLOSS + HEIGHT + FUELSYS_mpfi_BODY_hdtp, data = AutoModel)
summary(reg_model5)

#In the reg_model5, global F statistic is significant and adjusted r-square is 0.948, 
#showing the model is statistically useful. 
#Noting that the p-value of BODY_hdtp is still larger than 0.05, 
#we try to remove the interaction term and do F-test to compare nested models.

reg_model6 = lm(PRICE ~ RISK_1 + FUELSYS_mpfi + ASPIRATION + DRIVE_fwd + DOOR + BODY_hdtp +CYL_4 + CYL_5 + CYL_8 + MAKE_bmw + MAKE_honda + MAKE_jag + MAKE_mazda + MAKE_merc + MAKE_nissan + MAKE_porsche + MAKE_saab + MAKE_volvo + CC + WHEEL + NORMLOSS + HEIGHT, data = AutoModel)
summary(reg_model6)
anova(reg_model5, reg_model6)

#Since the F-test is significant, we conclude that the interaction term do affect the dependent variable. 
#Therefore, we use reg_model5 as the best regression model. 
#The adjusted R-square is 0.948 and the standard error is 1340.

#---------------------------------------------------------------------------------------------------#
#We check assumptions by residual analysis.

#To check the assumption that the mean of random error is zero, we construct partial residual plots.
crPlots(reg_model5)
#In the partial residual plots of quantitative variables, 
#we find that the points are around the red dashed lines without distinctive patterns. 
#And in the partial residual plots of qualitative variables, 
#we find that regarding each value of qualitative variables, the points are randomly distributed around this value. 
#Therefore, we conclude that the model satisfy the assumption that the mean of random error is zero.

#To check the assumption that the error term have constant variance for all levels of the independent variables, we plot the scatterplot of residuals and price.
residuals = resid(reg_model5)
par(mfrow = c(1,1))
plot(AutoWithDummy$PRICE, residuals, xlab = "PRICE", main = "Scatterplot of Price VS. residuals")
abline(a=0,b=0, col='red',lty=2, lwd=2)
#In the residuals plot, we find that the points are randomly located around 0. 
#Therefore, we conclude that the model satisfy the assumption of homoscedasticity.

#To check the assumption that the error term is normally distributed, we construct the histogram plot, the density plot and normal probability plot of residuals,
hist(residuals, breaks = seq(-4000, 6000, 500))
plot(density(residuals), xlab="residuals", main ="Density plot of residuals")
qqnorm(residuals, xlab='Normal Quantiles', ylab='Residuals Quantiles')
qqline(residuals, col='red')
#In the plots above, we conclude that the model satisfy the assumption of normal distribution.

#To check the assumption that the redisuals are uncorrelated, we run Durbin-Wastson test.
durbinWatsonTest(reg_model5)
#In the Durbin-Wastson test, we find that the statistic is 1.83 and the p-value is 0.038. 
#Therefore, we conclude that the model satisfy the assumption that residuals are uncorrelated.


#---------------------------------------------------------------------------------------------------#
#Check the outliers
outlierTest(reg_model5)
qqPlot(rstudent(reg_model5), distribution = 't', df=159, main='QQ Plot', ylab='Studentized Residuals')
#The outlier test shows that the point No.173 is outlier. 


#Since there is one outlier, we simply remove it and do model regression again on the rest of data.
AutoModel_WithoutOutliers = AutoModel[row.names(AutoModel)!='173',]
reg_model6 = lm(PRICE ~ RISK_1 + FUELSYS_mpfi + ASPIRATION + DRIVE_fwd + DOOR + BODY_hdtp + CYL_4 + CYL_5 + CYL_8 + MAKE_bmw + MAKE_honda + MAKE_jag + MAKE_mazda + MAKE_merc + MAKE_nissan + MAKE_porsche + MAKE_saab + MAKE_volvo + CC + WHEEL + NORMLOSS + HEIGHT + FUELSYS_mpfi_BODY_hdtp, data = AutoModel_WithoutOutliers)
summary(reg_model6)
CV=100*(1251/mean(AutoModel_WithoutOutliers$PRICE))
CV

#The Global F-statistics is 144.7 and the p-value is less than 2.2e-16, showing the model is statistically useful. 
#The adjusted R-square is 0.9546, showing that about 95.46% of dependent variable error can be explained by the model. 
#The standard error is 1251, showing about 95% of the observed prices lie within 2502 of their respective predicted price. 
#The C.V. is 10.97%, showing the value of s for the regression model is about 11% of the value of the sample mean price.
