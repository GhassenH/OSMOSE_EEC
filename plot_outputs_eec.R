################################################################################
# Plot Osmose outputs: Biomass, Catch, TL, Biomass distribution, Foodweb
################################################################################

library(tidyverse)
library(ggthemes)
library(reshape)
library(gganimate)
library(igraph)
library(ggnetwork)
library(RColorBrewer)


## Color Palette
colors <- c("antiquewhite", "antiquewhite4", "aquamarine", "aquamarine4", 
            "azure3", "azure4", "gold", "gold4", "bisque", "bisque4", "slateblue1", "blue4", 
            "brown1", "brown4", "cyan", "cyan4", "darkolivegreen1", "darkolivegreen4",
            "darkorange", "darkorange4", "darkorchid", "darkorchid4", "darksalmon","gray0",
            "darkseagreen", "darkseagreen4", "darkslategray1", "darkslategray4", "darkred", "deeppink", "deeppink4",
            "dodgerblue", "dodgerblue4", "chartreuse", "chartreuse4", "cornsilk3", "cornsilk4",
            "red2",  "lightpink", "lightpink4", "blue1", "blue2")


# =================================== Biomass, Catch, Size and Trophic level plots ==========================================

## Biomass outputs
data_B <- read.table("output/eec_biomass_Simu0.csv", sep = ",", dec = ".", skip = 1, header = T)
data_B <- data_B[-c(1:20),]
data_B_long <- pivot_longer(data_B, cols = 2:15 , names_to = "Species", values_to = "Biomass")
# data_B_long <- left_join(data_B_long, calib_data, by = "Species")

plot_biomass <- ggplot(data = data_B_long, aes(x=Time, y=Biomass)) +
  geom_line(color="dodgerblue") +
  facet_wrap(~ Species, ncol = 4, scales="free_y") +
  ylab("Biomass (t)") +
  xlab("Time (y)") +
  theme_bw() +
  theme(legend.position="bottom") +
  ggtitle("Biomass")
plot_biomass

## Catch outputs
data_C <- read.table("output/eec_yield_Simu0.csv", sep = ",", dec = ".", skip = 1, header = T)
data_C <- data_C[-c(1:20),]
data_C_long <- pivot_longer(data_C, cols = 2:15 , names_to = "Species", values_to = "Catch")
data_C_long <- left_join(data_C_long, calib_data, by = "Species")

plot_Catch <- ggplot(data = data_C_long, aes(x=Time, y=Catch)) +
  geom_line(color="dodgerblue") +
  # geom_line(aes(y=Landings, color="Landings"), linetype="dashed" ) +
  # scale_colour_manual("", breaks = c("Catch", "Landings"),
  #                     values = c("Catch"="black", "Landings"="dodgerblue"),
  #                     labels=c("OSMOSE catches", "Observed landings")) +
  facet_wrap(~ Species, ncol = 4, scales="free_y") +
  ylab("Catch (t)") +
  xlab("Time (y)") +
  theme_bw() +
  theme(legend.position="bottom") +
  ggtitle("Catch")
plot_Catch

## Size outputs
data_size <- read.table("output/SizeIndicators/eec_meanSize_Simu0.csv", sep = ",", dec = ".", skip = 1, header = T)
data_size <- data_size[-c(1:20),]
data_size_c <- read.table("output/SizeIndicators/eec_meanSizeCatch_Simu0.csv", sep = ",", dec = ".", skip = 1, header = T)
data_size_c <- data_size_c[-c(1:20),]
data_size$type <- "biomass"
data_size_c$type <- "catch"
data_size <- rbind(data_size,data_size_c)
data_size_long <- pivot_longer(data_size, cols = 2:15 , names_to = "Species", values_to = "Size")

plot_size <- ggplot(data = data_size_long, aes(x=Time, y=Size, color=type)) +
  geom_line()+
  facet_wrap(~ Species, ncol = 4, scales="free_y") +
  ylab("Size (cm)") +
  xlab("Time (y)") +
  theme_bw() +
  theme(legend.position="bottom", legend.title = element_blank()) +
  ggtitle("Mean size")
plot_size

## Mean Trophic level of catches
data_TLc <- read.table("output/Trophic/eec_meanTLCatch_Simu0.csv", sep = ",", dec = ".", skip = 1, header = T)
data_TLc_long <- pivot_longer(data_TLc, cols = 2:17 , names_to = "Species", values_to = "meanTLcatch")
data_TLc_long <- left_join(data_TLc_long, calib_data, by = "Species")


plot_TLc <- ggplot(data = data_TLc_long, aes(x=Time, y=meanTLcatch)) +
  geom_line(aes(color="meanTLcatch")) +
  geom_line(aes(y=TL, color="TL"), linetype="dashed" ) +
  scale_colour_manual("", breaks = c("meanTLcatch", "TL"),
                      values = c("meanTLcatch"="black", "TL"="dodgerblue"),
                      labels=c("Mean TL catch", "Observed TL")) +
  facet_wrap(~ Species, ncol = 4, scales="free_y") +
  ylab("Trophic level") +
  xlab("Time (y)") +
  theme_bw() +
  theme(legend.position="bottom") +
  ggtitle("Mean Trophic level of catches")
plot_TLc

## save plots
ggsave(plot_biomass, filename = "output/Plots/Biomass.png", units="cm", width = 25, height = 15, dpi = 800)
ggsave(plot_Catch, filename = "output/Plots/Catch.png", units="cm", width = 25, height = 15, dpi = 800)
ggsave(plot_TLc, filename = "output/Plots/TL_catch.png", units="cm", width = 25, height = 15, dpi = 800)
ggsave(plot_size, filename = "output/Plots/Mean_size.png", units="cm", width = 25, height = 15, dpi = 800)



# ===================diet matrix by age (adults and juvenile)=======================

## juvenile diet
diet <- read.table("output/Trophic/eec_dietMatrix_Simu0.csv", sep = ",", dec = ".", skip = 1, header = T)
diet <- subset(diet, Time==40)
diet <- diet[,-1]

# sp_names <-c("AtlanticHerring", "AtlanticMackerel", "Sandeel","EuropeanSprat", "NorwayPout", 
#              "EuropeanPlaice","CommonSole", "Saithe", "AtlanticCod","Haddock", "HorseMackerel", 
#              "Whiting", "CommonDab", "GreyGurnard", "Hake", "Shrimp")
sp_names <- c("lesserSpottedDogfish", "redMullet", "pouting", "whiting", "poorCod",
              "cod", "dragonet", "sole", "plaice", "horseMackerel", "mackerel", "herring", 
              "sardine", "squids")              

# diet_recruit <-diet[, c(1,seq(2, 33, by=2))] # selection of adults
# names(diet_recruit) <- c("Prey", sp_names)
diet_melt <- melt(diet, id.vars = c("Prey"))

ncolors <- 18
set.seed(3)
c <-colors()[sample(1:657, ncolors, replace = F)]
c[13] <- "navyblue"
c[37] <- "orange"
c[27] <- "gold"

diet_juv <- ggplot(diet_melt, aes(x=variable, y=value, fill=Prey))+
  geom_bar(stat = "identity")+
  ylab("Prey biomass (%)")+
  xlab("Predators")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12), 
        legend.position = "bottom")+
  scale_fill_manual(values = colors) +
  ggtitle("Osmose Diet composition (juvenile)")
diet_juv

## adults diet 
diet <- read.table("output/Trophic/ns_dietMatrix_Simu0.csv", sep = ",", dec = ".", skip = 1, header = T)
diet <- subset(diet, Time==40)
diet <- diet[,-1]

sp_names <-c("AtlanticHerring", "AtlanticMackerel", "Sandeel","EuropeanSprat", "NorwayPout", 
             "EuropeanPlaice","CommonSole", "Saithe", "AtlanticCod","Haddock", "HorseMackerel", 
             "Whiting", "CommonDab", "GreyGurnard", "Hake", "Shrimp")

diet_recruit <-diet[, c(seq(1, 34, by=2))] # selection of adults
names(diet_recruit) <- c("Prey", sp_names)
diet_recruit_melt <- melt(diet_recruit, id.vars = c("Prey"))

ncolors <- 40
set.seed(3)
c <-colors()[sample(1:657, ncolors, replace = F)]
c[13] <- "navyblue"
c[37] <- "orange"
c[27] <- "gold"

diet_adult <- ggplot(diet_recruit_melt, aes(x=variable, y=value, fill=Prey))+
  geom_bar(stat = "identity")+
  ylab("Prey biomass (%)")+
  xlab("Predators")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12), 
        legend.position = "bottom")+
  scale_fill_manual(values = colors) +
  ggtitle("Osmose Diet composition (adults)")
diet_adult

## save diet matrix
ggsave(diet_adult, filename = "output/Plots/Diet matrix adults.png", units="cm", width = 25, height = 20, dpi = 400)
ggsave(diet_juv, filename = "output/Plots/Diet matrix juveniles.png", units="cm", width = 25, height = 20, dpi = 400)


## animated diet matrix by age (adults and juvenile)
diet <- read.table("output/Trophic/ns_dietMatrix_Simu0.csv", sep = ",", dec = ".", skip = 1, header = T)
sp_names <-c("AtlanticHerring", "AtlanticMackerel", "Sandeel","EuropeanSprat", "NorwayPout", 
             "EuropeanPlaice","CommonSole", "Saithe", "AtlanticCod","Haddock", "HorseMackerel", 
             "Whiting", "CommonDab", "GreyGurnard", "Hake", "Shrimp")

diet_pre_recruit <-diet[, c(1,2, seq(3, 34, by=2))]
names(diet_pre_recruit) <- c("Time", "Prey", sp_names)
diet_pre_recruit$class <- "Juveniles" 

diet_recruit <-diet[, -c(seq(3, 34, by=2))]
names(diet_recruit) <- c("Time", "Prey", sp_names)
diet_recruit$class <- "Adults"

all_diet <- rbind(diet_recruit, diet_pre_recruit)
diet_all_melt <- melt(all_diet, id.vars = c("Time", "Prey", "class"))

ncolors <- 40
set.seed(3)
c <-colors()[sample(1:657, ncolors, replace = F)]
c[13] <- "navyblue"
c[37] <- "orange"

diet_plot_all_diet <- ggplot(diet_all_melt, aes(x=variable, y=value, fill=Prey))+
  geom_bar(stat = "identity")+
  ylab("Prey biomass (%)")+
  xlab("Predators")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12), 
        legend.position = "bottom")+
  facet_grid(~class) +
  scale_fill_manual(values = colors) +
  transition_states(Time) +
  ease_aes('linear') +
  ggtitle("Osmose Diet composition", subtitle = "Year {frame} of {nframes}")

animate(diet_plot_all_diet, renderer = gifski_renderer(), width = 1800, height = 1600, res = 200, nframes = length(unique(diet_all_melt$Time)))
anim_save("output/Plots/Diet matrix Juvenile_Adults animate.gif")







# =================================== network plot ==========================================
## data diet matrix
TL <- read.table("output/Trophic/ns_meanTL_Simu0.csv", sep = ",", dec = ".", skip = 1, header = T)
TL <- subset(TL, Time==40)
TL <-TL[,-1]

diet <- read.table("output/Trophic/ns_dietMatrix_Simu0.csv", sep = ",", dec = ".", skip = 1, header = T)
diet <- subset(diet, Time==40)
diet <-diet[,-1]

## data preparation : LINKS
diet_melt<-melt(diet)
links<-diet_melt[!(diet_melt$value==0),]
colnames(links)<-c("from", "to", "weight")
row.names(links)<-NULL

## data preparation : NODES
nodes<-diet[,1]
# group <- c(rep("Fish", 15), rep("Crustacean", 1), rep("Plankton", 5), rep("Benthic", 3))
group2 <- c(rep("Pelagic", 4), "Benthopelagic", rep("Benthic", 2), rep("Benthopelagic", 3),
            "Pelagic", "Benthopelagic", "Benthic", rep("Benthopelagic", 2), "Benthic",
            rep("Plankton", 5), rep("Benthos", 3))
nodes<-data.frame(id = nodes, group = group2)

net <- graph_from_data_frame(d = links, vertices = nodes, directed = F)
ggnetwork(net)

## plot ggnetwork
palette1 <- brewer.pal(n = length(unique(group2)), name = "Dark2")

# preparation of the layout
x_axis <- c(1, 2.5, 1.5, 3.5, 2.5, 2, 3.5, 3.5, 3, 1.5, 3, 4, 1.5, 2.5, 2.25, 2.5, 3, 2, 2, 3.5, 2.75, 1, 4, 2.5) 
layout_TL <- data.frame(x = x_axis , y=c(t(TL), c(1, 1, 2, 2, 2.3, 2, 2, 2)))
layout_TL <- as.matrix(layout_TL)
data_net <- ggnetwork(net, layout = layout_TL)

# plot
plot_network <- ggplot(data_net, aes(x = x, y = y, xend = xend, yend = yend, color=group)) +
  geom_edges(aes(alpha=weight), curvature = 0.1, ncp=5, show.legend = F) +
  geom_nodes(size = 4) +
  scale_color_brewer(palette = "Set2")+
  geom_nodetext_repel(aes(label = name), nudge_y = 0, nudge_x = 0) +
  # geom_segment(aes(x = 0, y = -0.25, xend = 1, yend = -0.25),
  # arrow = arrow(length = unit(0.25, "cm"), type = "closed", angle = 20), color = "grey60") +
  geom_segment(aes(x = -0.25, y = 0, xend = -0.25, yend = 1),
               arrow = arrow(length = unit(0.25, "cm"), type = "closed", angle = 20), color = "grey60") +
  annotate("text", label = "Trophic level", x = -0.28, y = 0.5, angle = 90, color = "grey40") +
  theme_blank(legend.title=element_blank()) 
plot_network

ggsave(plot_network, filename = "output/Plots/Foodweb.png", units="cm", width = 25, height = 15, dpi = 400)


# =================================== diet matrix plot ALL ages ==========================================

## 1- diet composition all ages 
## To use this code, osmose outputs should be aggregated (all sizes)
diet <- read.table("output/Trophic/ns_dietMatrix_Simu0.csv", sep = ",", dec = ".", skip = 1, header = T)
diet <- subset(diet, Time==40)
diet <-diet[,-1]

diet_melt <- melt(diet)

mypalette5<-c("#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628",  "#377EB8", "tomato2", 
              "#F781BF", "#66C2A5", "#FC8D62",
              "#8DA0CB", "lightskyblue1", "#A6D854", "#FFD92F", "#B3B3B3", "#E5C494",
              "green", "#E7298A", "blue","#E6AB02", "navyblue", "#D95F02", "purple", "orange")

diet_plot <- ggplot(diet_melt, aes(x=variable, y=value, fill=Prey))+
  geom_bar(stat = "identity")+
  ylab("Prey biomass (%)")+
  xlab("Predators")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12), 
        legend.position = "bottom")+
  scale_fill_manual(values = mypalette5)+
  ggtitle("Osmose Diet composition")
diet_plot

ggsave(diet_plot, filename = "output/Plots/Diet matrix.png", units="cm", width = 25, height = 20, dpi = 400)

## diet matrix animated
# To use this code, osmose outputs should be aggregated (all sizes)
diet_anim <- read.table("output/Trophic/ns_dietMatrix_Simu0.csv", sep = ",", dec = ".", skip = 1, header = T)
diet_melt <- melt(diet, id.vars = c("Time", "Prey"))

diet_plot_a <- ggplot(diet_melt, aes(x=variable, y=value, fill=Prey))+
  geom_bar(stat = "identity")+
  ylab("Prey biomass (%)")+
  xlab("Predators")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12))+
  scale_fill_manual(values = mypalette5) +
  transition_states(Time) +
  ease_aes('linear') +
  ggtitle("Osmose Diet composition", subtitle = "Year {frame} of {nframes}")

animate(diet_anim, renderer = gifski_renderer(), width = 1600, height = 1200, res = 200, nframes = length(unique(diet_melt$Time)))
anim_save("output/Plots/Diet matrix animate.gif")

