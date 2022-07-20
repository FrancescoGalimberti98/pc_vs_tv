library(showtext)
library(viridis)
library(grid)
library(ggtext)
library(ggrepel)
library(readxl)

font_add_google('Montserrat', family = 'Montserrat')
showtext_auto()

data <- read_excel("PC vs tv.xlsx")
data$Zona = factor(data$Zona, levels = c('Nord Est','Nord Ovest','Centro','Isole','Sud'), ordered = T)

beta = lm(pc_everyday ~ tv_everyday, data = data)$coefficients
cor(data$pc_everyday,data$tv_everyday)

# glm.data = data.frame(Regione = data$Regione,
#                       pc = data$pc_everyday*10,
#                       no_pc = 1000 - data$pc_everyday*10,
#                       tv = data$tv_everyday)
# y = as.matrix(data.frame(pc = glm.data$pc,no_pc = glm.data$no_pc))
# 
# fit.glm = glm(y ~ tv, data = glm.data, family = 'binomial')
# prev.glm = predict(fit.glm, newdata = data.frame(tv = seq(0,1,by=0.001)))
# plot(seq(0,1,by=0.001), prev.glm, type  = 'l')

ggplot(data, aes(y=pc_everyday, x = tv_everyday, color = Zona)) +
  geom_point(size=3,show.legend = F) +
  #ylim(28.2,41.9) +
  geom_text_repel(aes(label = Regione),show.legend = F,
                  family = 'Montserrat',fontface = "bold",size = 8, min.segment.length = 3,point.padding = unit(1,"lines")) +
  labs(y = "% uso quotidiano PC", x = "% uso quotidiano TV",family = 'Montserrat',fontface = "bold") +
  scale_color_viridis(begin = 0.25,end = 0.83,direction = 1,discrete = TRUE, option = "G") +
  theme_minimal(base_size=34, base_family = 'Montserrat') +
  theme(axis.line = element_line(color='grey85'), panel.grid.minor = element_blank(),
        axis.title=element_text(face="bold"), legend.text = element_text(face="bold"),
        axis.text = element_text(face="bold")) +
  #geom_hline(yintercept = mean(data$pc_everyday), col = 'grey40',lty = 2,lwd = 1.3) +
  #geom_vline(xintercept = mean(data$tv_everyday), col = 'grey40', lwd = 1.3, lty = 2) +
  geom_abline(slope = beta[2], intercept = beta[1], col = '#34cbb1',lty = 2,lwd = 1.4)

# ASSI INVERTITI
beta = lm(tv_everyday ~ pc_everyday, data = data)$coefficients

ggplot(data, aes(y=tv_everyday, x = pc_everyday, color = Zona)) +
 geom_point(size=3,show.legend = F) +
 #ylim(28.2,41.9) +
 geom_text_repel(aes(label = Regione),show.legend = F,
                 family = 'Montserrat',fontface = "bold",size = 8, min.segment.length = 3,point.padding = unit(1,"lines")) +
 labs(y = "% uso quotidiano TV", x = "% uso quotidiano PC",family = 'Montserrat',fontface = "bold") +
 scale_color_viridis(begin = 0.25,end = 0.83,direction = 1,discrete = TRUE, option = "G") +
 theme_minimal(base_size=34, base_family = 'Montserrat') +
 theme(axis.line = element_line(color='grey85'), panel.grid.minor = element_blank(),
       axis.title=element_text(face="bold"), legend.text = element_text(face="bold"),
       axis.text = element_text(face="bold")) +
 #geom_hline(yintercept = mean(data$pc_everyday), col = 'grey40',lty = 2,lwd = 1.3) +
 #geom_vline(xintercept = mean(data$tv_everyday), col = 'grey40', lwd = 1.3, lty = 2) +
 geom_abline(slope = beta[2], intercept = beta[1], col = '#34cbb1',lty = 2,lwd = 1.4)

# PER LEGENDA

ggplot(data, aes(y=pc_everyday, x = tv_everyday, color = Zona)) +
  geom_point(size=15,show.legend = TRUE) +
  geom_text_repel(aes(label = Regione),show.legend = FALSE,family = 'Montserrat',fontface = "bold",size = 2) +
  labs(y = "pc_everyday", x = "% tv_everyday",family = 'Montserrat',fontface = "bold") +
  scale_color_viridis(begin = 0.25,end = 0.83,direction = 1,discrete = TRUE, option = "G") +
  theme_minimal(base_size=25, base_family = 'Montserrat') +
  theme(axis.line = element_line(color='grey85'), panel.grid.minor = element_blank(),
        axis.title=element_text(face="bold"), legend.text = element_text(face="bold"),
        axis.text = element_text(face="bold"),legend.direction="horizontal", legend.position = 'bottom')
