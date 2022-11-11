library(tidyverse)
library(MASS)
library(corrgram)
library(caret)
library(ggord) #install_github('fawda123/ggord')

source("C:/Users/pablo/OneDrive/R/scripts/panel.signif.R")
source("C:/Users/pablo/OneDrive/R/scripts/panel.shadeNtext.R")



set.seed(030517)

#open data
morfo<-read.csv2("C:/Users/pablo/OneDrive/Artigos Publicados/Allito et al_ophiotrixtrindadencis/morfo_medidas.csv")

#summary
str(morfo)

#as factor
morfo$CS <- as.factor(morfo$CS)

#order the factor
morfo$CS <- factor(morfo$CS, levels = c("1", "2", "3", "4"))

#name for graphs
letra<-as.factor(substring(morfo$local, 1, 1))

#correlograma
corrgram(morfo[,-c(1:4)], type="data", lower.panel=panel.shadeNtext, 
         upper.panel=panel.signif, cor.method="pearson")
descrCor <-  cor(morfo[,-c(1:5)])
highlyCorDescr <- findCorrelation(descrCor, cutoff = .9)
highlyCorDescr #remove data with .9 correlation

############################################

cols <- c("1" = "#d01c8b", "2" = "#f1b6da", "3" = "#b8e186", "4" = "#4dac26")

ord_1 <- lda(CS ~ ., morfo[,-c(1:3)])
ord_1

ord.class <- predict(ord_1, dimen=2)$class
class.table <- table(morfo$CS, ord.class)
class.table
mosaicplot(class.table, color=T)

ggord(ord_1, morfo$CS, axes = c("1", "2"), ellipse = F,size = 6, cols=cols,  xlims = c(-12,12), ylims = c(-8,12)) +
  geom_text(label=letra, size=3) #eixo 1 e 2

ggord(ord_1, morfo$CS, axes = c("2", "3"), ellipse = F,size = 6, cols=cols) +
  geom_text(label=letra, size=3) #eixo 2 e 3

ggord(ord_1, morfo$CS, axes = c("1", "3"), ellipse = F,size = 6, cols=cols) +
  geom_text(label=letra, size=3) #eixo 1 e 3 


# acessando quanto acurado ? a predi??o do modelo 
# pocentagem de classifica??es corretas por categoria de CS
ct <- table(morfo$CS, predict(ord_1, morfo[,-c(1:4)])$class)
diag(prop.table(ct, 1))
# total de classifica??es corretas
sum(diag(prop.table(ct)))

##############


sumcr.mva1 <- manova(cbind(dd, rs, disc.spine, as, as2, rs_l, rs_w, os_l, os_w, ads_l, ads_w, dap_l, dap_w, vap1_l, vap1_w, vap2_l, vap2_w, od) ~ CS, data=morfo)
summary.aov(sumcr.mva1)
summary(sumcr.mva1, test="Wilks")

################
forcas<-data.frame(ord_1$scaling, var=row.names(ord_1$scaling))

ggplot(forcas, aes(x=reorder(var, -LD1), y=LD1, fill=LD1)) +
  geom_bar(stat = "identity", color="black")+
  scale_fill_distiller(palette = "Spectral", direction = 1)+
  theme_bw()+
  theme(legend.position="none", 
        axis.text.x = element_text(size = 11, colour = "black",angle = 45, hjust = 1),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank())

ggplot(forcas, aes(x=reorder(var, -LD2), y=LD2, fill=LD2)) +
  geom_bar(stat = "identity", color="black")+
  scale_fill_distiller(palette = "Spectral", direction = 1)+
  theme_bw()+
  theme(legend.position="none", 
        axis.text.x = element_text(size = 11, colour = "black",angle = 45, hjust = 1),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank())


# make predictions
predictions1 <- predict(ord_1, morfo[,-c(1:4)])$class
# summarize accuracy
tt<-table(predictions1, morfo$CS)
tt
sum(tt[row(tt) != col(tt)]) / sum(tt)



lda.values <- predict(ord_1, morfo[,-c(1:4)])
pred.lda<- data.frame(CS=morfo$CS,  CS_lda=lda.values$class, lda.values$x)

ggord(ord_1, morfo$CS, axes = c("1", "2"), ellipse = F,size = 6, cols=cols,  xlims = c(-12,12), ylims = c(-8,12)) +
  geom_text(label=letra, size=3) #eixo 1 e 2

ggsave("lda_ophitrix.pdf", device = "pdf", width = 25, height = 25, units = "cm", dpi = 600,useDingbats=FALSE)
#salva na pasta documentos

ggplot(pred.lda, aes(x=LD1, fill=CS)) + 
  geom_density()+
  scale_fill_manual(values = cols)+
  scale_x_continuous(limits = c(-12,12))+
  theme_bw()

ggsave("lda1density_ophiotrix.pdf", device = "pdf", width = 25, height = 5, units = "cm", dpi = 600,useDingbats=FALSE)


ggplot(pred.lda, aes(x=LD2, fill=CS)) + 
  geom_density()+
  scale_fill_manual(values = cols)+
  scale_x_continuous(limits = c(-8,12))+
  theme_bw()

ggsave("lda2density_ophiotrix.pdf", device = "pdf", width = 25, height = 5, units = "cm", dpi = 600,useDingbats=FALSE)

summary(aov(lda.values$x[,1]~ morfo$CS))
mod.HSD<-TukeyHSD(aov(lda.values$x[,1]~ morfo$CS))
mod.HSD
plot(mod.HSD)

summary(aov(lda.values$x[,2]~ morfo$CS))
mod.HSD2<-TukeyHSD(aov(lda.values$x[,2]~ morfo$CS))
mod.HSD2
plot(mod.HSD2)



