## This script subsets and plots the data for the total environmental forces on the Tetra Hedron and Arapaho barges.

library(ggplot2)
library(magrittr)
library(dplyr)


## Set the working directory and read the data
setwd("W:/Active Projects/1608 - Tetra Hedron Mooring Workability/05 Engineering/054 Analysis/Sorted Results/Plots/")
df <- read.csv("Hedron and Arapaho Forces.csv", header = TRUE, sep = ",")


## Hedron acceptance criteria
##
## FS of 2.5 = 202 MT load per anchor line
##
## FS of 2 = 253 MT load per anchor line
##
## FS of 1 = 506 MT load per anchor line
##
## Arapaho acceptance criteria
##
## FS of 2.5 = 122 MT load per anchor line
##
## FS of 2 = 153 MT load per anchor line
##
## FS of 1 = 306 MT load per anchor line


### Hedron Environmental Forces (Water Depth = 40ft) ###
## Subset the data
df.sub1 = df %>% filter(Barge.Name=="Hedron",
                        Water.Depth==40)
df.sub1$Current.Speed.f <- factor(df.sub1$Current.Speed,
                                  levels = sort(unique(df.sub1$Current.Speed)),
                                  labels = paste("Current Speed: ", sort(unique(df.sub1$Current.Speed)), " knots"))
df.sub1$Heading.f <- factor(df.sub1$Heading,
                            levels = sort(unique(df.sub1$Heading)),
                            labels = paste("Heading: ", sort(unique(df.sub1$Heading)), " degrees"))

g1 <- ggplot(data = df.sub1,
       aes(Wind.Speed, Total.Force, group=Wave.Height, color=Wave.Height)) + 
  geom_line(size = 0.5) + 
  geom_text(data = df.sub1 %>% filter(Wind.Speed==max(Wind.Speed)),
            aes(label=paste("Hs = ", Wave.Height, " ft"), y=Total.Force, x=Wind.Speed + 0.5), hjust=0, size=2) +
  theme_bw() + guides(color=FALSE) + facet_grid(Current.Speed.f ~ Heading.f) + 
  scale_x_continuous(limits = c(10,78), breaks = c(10,20,30,40,50,60,70)) +
  ggtitle("Tetra Hedron Environmental Forces (Water Depth=40ft)") + xlab("Wind Speed [knots]") + 
  ylab("Total Force [MT]") + geom_hline(yintercept = 506, color="red", size=0.5) + 
  geom_hline(yintercept = 202, color="lawngreen", size=0.5) + geom_hline(yintercept = 432, color="orange", size=0.5) +
  geom_hline(yintercept = 36, color="black", size=0.5) +
  annotate("text", x=10, y=550, label="Fos=1", size = 2, color="red") +
  annotate("text", x=11, y=475, label="Anchor Uplift", size = 2, color="orange") +
  annotate("text", x=10, y=245, label="FoS=1", size = 2, color="lawngreen") + 
  annotate("text", x=11, y=80, label="Anchor Drag", size = 2, color="black")

pdf("Hedron Environmental Forces (Water Depth = 40ft).pdf", width = 17, height = 11)
g1
dev.off()


### Hedron Environmental Forces (Water Depth = 80ft) ###
## Subset the data
df.sub2 = df %>% filter(Barge.Name=="Hedron",
                        Water.Depth==80)
df.sub2$Current.Speed.f <- factor(df.sub2$Current.Speed,
                                  levels = sort(unique(df.sub2$Current.Speed)),
                                  labels = paste("Current Speed: ", sort(unique(df.sub2$Current.Speed)), " knots"))
df.sub2$Heading.f <- factor(df.sub2$Heading,
                            levels = sort(unique(df.sub2$Heading)),
                            labels = paste("Heading: ", sort(unique(df.sub2$Heading)), " degrees"))

g2 <- ggplot(data = df.sub2,
             aes(Wind.Speed, Total.Force, group=Wave.Height, color=Wave.Height)) + 
  geom_line(size = 0.5) + 
  geom_text(data = df.sub2 %>% filter(Wind.Speed==max(Wind.Speed)),
            aes(label=paste("Hs = ", Wave.Height, " ft"), y=Total.Force, x=Wind.Speed + 0.5), hjust=0, size=2) +
  theme_bw() + guides(color=FALSE) + facet_grid(Current.Speed.f ~ Heading.f) + 
  scale_x_continuous(limits = c(10,78), breaks = c(10,20,30,40,50,60,70)) +
  ggtitle("Tetra Hedron Environmental Forces (Water Depth=80ft)") + xlab("Wind Speed [knots]") + 
  ylab("Total Force [MT]") + geom_hline(yintercept = 506, color="red", size=0.5) + 
  geom_hline(yintercept = 202, color="lawngreen", size=0.5) + geom_hline(yintercept = 243, color="orange", size=0.5) +
  geom_hline(yintercept = 36, color="black", size=0.5) +
  annotate("text", x=10, y=550, label="Fos=1", size = 2, color="red") +
  annotate("text", x=11, y=275, label="Anchor Uplift", size = 2, color="orange") +
  annotate("text", x=10, y=225, label="FoS=1", size = 2, color="lawngreen") + 
  annotate("text", x=11, y=80, label="Anchor Drag", size = 2, color="black")

pdf("Hedron Environmental Forces (Water Depth = 80ft).pdf", width = 17, height = 11)
g2
dev.off()


### Arapaho Environmental Forces (Water Depth = 40ft) ###
## Subset the data
df.sub3 = df %>% filter(Barge.Name=="Arapaho",
                        Water.Depth==40)
df.sub3$Current.Speed.f <- factor(df.sub3$Current.Speed,
                                  levels = sort(unique(df.sub3$Current.Speed)),
                                  labels = paste("Current Speed: ", sort(unique(df.sub3$Current.Speed)), " knots"))
df.sub3$Heading.f <- factor(df.sub3$Heading,
                            levels = sort(unique(df.sub3$Heading)),
                            labels = paste("Heading: ", sort(unique(df.sub3$Heading)), " degrees"))

g3 <- ggplot(data = df.sub3,
             aes(Wind.Speed, Total.Force, group=Wave.Height, color=Wave.Height)) + 
  geom_line(size = 0.5) + 
  geom_text(data = df.sub3 %>% filter(Wind.Speed==max(Wind.Speed)),
            aes(label=paste("Hs = ", Wave.Height, " ft"), y=Total.Force, x=Wind.Speed + 0.5), hjust=0, size=2) +
  theme_bw() + guides(color=FALSE) + facet_grid(Current.Speed.f ~ Heading.f) + 
  scale_x_continuous(limits = c(10,78), breaks = c(10,20,30,40,50,60,70)) +
  ggtitle("Tetra Arapaho Environmental Forces (Water Depth=40ft)") + xlab("Wind Speed [knots]") + 
  ylab("Total Force [MT]") + geom_hline(yintercept = 306, color="red", size=0.5) + 
  geom_hline(yintercept = 122, color="lawngreen", size=0.5) + geom_hline(yintercept = 432, color="orange", size=0.5) +
  geom_hline(yintercept = 36, color="black", size=0.5) +
  annotate("text", x=10, y=320, label="Fos=1", size = 2, color="red") +
  annotate("text", x=11, y=447, label="Anchor Uplift", size = 2, color="orange") +
  annotate("text", x=10, y=137, label="FoS=1", size = 2, color="lawngreen") + 
  annotate("text", x=11, y=51, label="Anchor Drag", size = 2, color="black")

pdf("Arapaho Environmental Forces (Water Depth = 40ft).pdf", width = 17, height = 11)
g3
dev.off()


### Hedron Environmental Forces (Water Depth = 80ft) ###
## Subset the data
df.sub4 = df %>% filter(Barge.Name=="Arapaho",
                        Water.Depth==80)
df.sub4$Current.Speed.f <- factor(df.sub4$Current.Speed,
                                  levels = sort(unique(df.sub4$Current.Speed)),
                                  labels = paste("Current Speed: ", sort(unique(df.sub4$Current.Speed)), " knots"))
df.sub4$Heading.f <- factor(df.sub4$Heading,
                            levels = sort(unique(df.sub4$Heading)),
                            labels = paste("Heading: ", sort(unique(df.sub4$Heading)), " degrees"))

g4 <- ggplot(data = df.sub4,
             aes(Wind.Speed, Total.Force, group=Wave.Height, color=Wave.Height)) + 
  geom_line(size = 0.5) + 
  geom_text(data = df.sub4 %>% filter(Wind.Speed==max(Wind.Speed)),
            aes(label=paste("Hs = ", Wave.Height, " ft"), y=Total.Force, x=Wind.Speed + 0.5), hjust=0, size=2) +
  theme_bw() + guides(color=FALSE) + facet_grid(Current.Speed.f ~ Heading.f) + 
  scale_x_continuous(limits = c(10,78), breaks = c(10,20,30,40,50,60,70)) +
  ggtitle("Tetra Arapaho Environmental Forces (Water Depth=80ft)") + xlab("Wind Speed [knots]") + 
  ylab("Total Force [MT]") + geom_hline(yintercept = 306, color="red", size=0.5) + 
  geom_hline(yintercept = 122, color="lawngreen", size=0.5) + geom_hline(yintercept = 243, color="orange", size=0.5) +
  geom_hline(yintercept = 36, color="black", size=0.5) +
  annotate("text", x=10, y=320, label="Fos=1", size = 2, color="red") +
  annotate("text", x=11, y=260, label="Anchor Uplift", size = 2, color="orange") +
  annotate("text", x=10, y=137, label="FoS=1", size = 2, color="lawngreen") + 
  annotate("text", x=11, y=51, label="Anchor Drag", size = 2, color="black")

pdf("Arapaho Environmental Forces (Water Depth = 80ft).pdf", width = 17, height = 11)
g4
dev.off()