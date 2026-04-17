#Cleaned version for Hacky
#read in packages
library(readxl)
library(ggplot2)
library(tidyverse)
library(lme4)
library(ggfortify)
library(geomorph) #this is for head morphology data that no one will be able to run but me, don't need to install

#read in excel files
salcolor <- read_xlsx("data/cinereus_data_2025.xlsx", sheet = "SalColor", na = "NA")
saldiet <- read_xlsx("data/cinereus_data_2025.xlsx", sheet = "Diet", na = "NA")

#Color variation ------------
#changing column names for easier typing
colnames(salcolor)[33] <- "X_avg"
colnames(salcolor)[35] <- "Y_avg"

#removing individuals with no color data collected
salcolor <- subset(salcolor, Label != "NA")

#nice plot sorted by each morph in XY color space
ggplot(salcolor, aes(X_avg, Y_avg, color= morph)) +
  geom_point() +
  labs(x = "X mean", y= "Y mean") +
  theme_classic() +
  theme(axis.title = element_text(size = 12, face = "bold"), axis.text = element_text(color= "black", size = 11), legend.position = "none") +
  scale_color_manual(values =c("darkslategrey", "red", "orange"))


#Head morphology ------------
#removing non-red striped and lead individuals from the dataset
redonly <- subset(salcolor, morph == "red")
redonly <- subset(redonly, head_width != "NA")
adultredonly <- subset(redonly, svl >= 35)

#reading in adult red TPS file (you cannot mess around with this data because it requires an additional download and didn't feel like it was worth it, sorry lol)
asmorph <- readland.tps("data/2025 TPS Files/FullDataset.TPS", specID = "ID")

#fixing the angle of the heads of each specimen. This generates a new landmark points based on the rotation. The 11th landmark (back of the head) is stabilized around 0 (every specimen has 0 for the landmark position).
adj.asmorph <- fixed.angle(asmorph, art.pt = 1, angle.pts.1 = 5, angle.pts.2 = 6, rot.pts = c(2,3,4,5))

#checking landmark placement with the adjusted angle points
asmorph_rot <- gpagen(adj.asmorph)

#plotting GPA with rotation adjustment
plot(asmorph_rot)

#PCA of landmark points
morph_PCA <- gm.prcomp(asmorph_rot$coords, scale=T)

#plotting my PCA- having a hard time interpreting this since this is using coordinates rather than distances between landmarks
morph_PCA_plot <- plot(morph_PCA, main= "PCA")

#getting loadings (haven't figured out how to add these to the plot)
asloading <- morph_PCA$rotation

#getting component scores for all individuals
as.data.frame(morph_PCA$x) #all principal components

#This is PC1 for all individuals
princomp1 <- as.data.frame(morph_PCA$x[,1])
princomp1 <- rownames_to_column(princomp1, var = "unique_id")
colnames(princomp1)[2] <- "PC1"

#adding PC1 scores for each individual to the main adult striped dataframe
#NAs for individuals when no landmarking was done
pcaronly <- left_join(adultredonly, princomp1, by= "unique_id")
head(pcaronly$PC1)

#removing NAs in PC1 column since it causes an error in model construction when using lmer
pca_asonly <- subset(pcaronly, PC1 != "NA")

#some models
#X_avg value: red-green color
mod1 <- lmer(pca_asonly$PC1 ~ pca_asonly$X_avg + pca_asonly$Y_avg + (1|pca_asonly$location), REML=F)
#Saturdation mean: how grey the color is (highly saturated is very red, low saturation more grey)
mod2 <- lmer(pca_asonly$PC1 ~ pca_asonly$Saturation_mean + (1|pca_asonly$location), REML = F)
#Luminance mean: how much white
mod3 <- lmer(pca_asonly$PC1 ~ pca_asonly$lumMean + (1|pca_asonly$location), REML = F)



#Diet ------------------

#removing columns/rows from saldiet dataframe and changing names
saldiet$date...1 <- NULL
saldiet$julian_date <- NULL
saldiet$`Highlight is GRFP selected samples` <- NULL
saldiet$Notes <- NULL
saldiet <- saldiet[-c(122:125), ]
colnames(saldiet)[1] <- "date"

#changing NAs to 0 in saldiet
saldiet[is.na(saldiet)] <- 0

#combining adult and larval columns to get a total for each family/order
total_coleoptera <- rowSums(saldiet[,12:13])
total_diptera <- rowSums(saldiet[,16:17])
total_hymenoptera <- rowSums(saldiet[,21:22])
saldiet$total_Coleoptera <- total_coleoptera
saldiet$total_Diptera <- total_diptera
saldiet$total_Hymenoptera <- total_hymenoptera

#reordering columns so total columns are after the proper groups
saldiet <- saldiet[, c(1:13,32,14:17,33,18:22,34,23:31)]

#making new dataframe that removes non-totaled columns and columns with only 0s
saldiet3 <- saldiet
saldiet3$Coleoptera <- NULL
saldiet3$Coleoptera_larva <- NULL
saldiet3$Diptera <- NULL
saldiet3$Diptera_larva <- NULL
saldiet3$Hymenoptera <- NULL
saldiet3$Hymenoptera_larva <- NULL
saldiet3$Dermaptera <- NULL
saldiet3$Orthoptera <- NULL

##giving each location a unique numeric ID for the biplot
saldiet4$location <- as.integer(factor(saldiet4$location, levels = unique(saldiet4$location)))

#PCA for diet across locations with total counts
diet.pca<-prcomp(saldiet4[,8:26],center=TRUE,scale.=TRUE)

#Plotting PCA with total counts- some bad outliers
autoplot(diet.pca, data= saldiet4, colour= "location", loadings= T, loadings.label= T, loadings.colour ="black", loadings.label.colour = 'black')

#making new dataframe with diet proportion per salamander to rerun PCA to potentially remove the outlier points
saldiettot <- saldiet4
saldiettot <- as.data.frame(saldiettot)
salsum <- rowSums(saldiettot[,8:26])

#adding total column to dataframe
saldiettot["total"] <- salsum

#proportions for each salamander across diet type
saldietprop <- saldiettot[,8:26]/rowSums(saldiettot[,8:26])
saldietprop[is.na(saldietprop)] <- 0

#making column with location and moving it to the front of the dataframe
saldietprop$location <- saldiet4$location
saldietprop <- saldietprop[, c(20, 1:19)]

#PCA with proportion data
propdiet.pca <- prcomp(saldietprop[,2:20], center = T, scale. = T)
autoplot(propdiet.pca, data= saldietprop, colour= "location", loadings= T, loadings.label= T, loadings.colour = 'black',loadings.label.colour = 'black')
autoplot(propdiet.pca, data= saldietprop, colour= "location")

#Currently having issues using the PCs since the length is different between my adult striped dataframe and my diet one since not all striped adults in my second time point underwent lavage- need to remove those rows
