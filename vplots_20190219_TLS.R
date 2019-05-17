require(ggplot2)
require(ggpubr)
require(plyr)
require(dplyr)
require(reshape2)


data <- read.csv("~/Dropbox/dissertation/perception/discrim/20180730_no_outliers_full_normed_subs_processed.csv", header=T)
data.full <- data

# keep only target subjects
subs <- c("ar_003","ar_004","ar_017","ar_027","ar_032","uk_026","uk_027","uk_030","uk_032")
data <- droplevels(data[data$subject %in% subs,])

# keep only unpredictable length vowels
data <- droplevels(data[data$unpred=="unpredictable",])

# subset only high front vowels
# data <- droplevels(data[data$vowel %in% c("i:", "i", "ɨ:", "ɨ", "e:", "e"),])

##### plot M \& D data for N Welsh #####
md <- read.csv("~/Dropbox/dissertation/perception/Monophthongs_anon.csv", header=T)

names(md) <- gsub("F?[12]?(dur)?", "", names(md))
names(md) <- gsub("_$", "", names(md))

md.dur <- md[,c(1,2,3,6,9,12,15,18,21,24,27,30,36,39)]
#md.dur$var <- "dur"
md.F1 <- md[,c(1,2,4,7,10,13,16,19,22,25,28,31,37,40)]
#md.F1$var <- "F1"
md.F2 <- md[,c(1,2,5,8,11,14,17,20,23,26,29,32,38,41)]
#md.F2$var <- "F2"

#############
md.dur <- reshape(md.dur, 
                  varying = c("i_long", "I_short", "e_long", "E_short", "a_long", "A_short", "o_long", "O_short", "u_long", "U_short", "Bar_lg", "Bar_sht"), 
                  v.names = "dur",
                  timevar = "vowel", 
                  times = c("i_long", "I_short", "e_long", "E_short", "a_long", "A_short", "o_long", "O_short", "u_long", "U_short", "Bar_lg", "Bar_sht"), 
                  new.row.names = 1:1000,
                  direction = "long")

md.F1 <- reshape(md.F1, 
                 varying = c("i_long", "I_short", "e_long", "E_short", "a_long", "A_short", "o_long", "O_short", "u_long", "U_short", "Bar_lg", "Bar_sht"), 
                 v.names = "F1",
                 timevar = "vowel", 
                 times = c("i_long", "I_short", "e_long", "E_short", "a_long", "A_short", "o_long", "O_short", "u_long", "U_short", "Bar_lg", "Bar_sht"), 
                 new.row.names = 1:1000,
                 direction = "long")

md.F2 <- reshape(md.F2, 
                 varying = c("i_long", "I_short", "e_long", "E_short", "a_long", "A_short", "o_long", "O_short", "u_long", "U_short", "Bar_lg", "Bar_sht"), 
                 v.names = "F2",
                 timevar = "vowel", 
                 times = c("i_long", "I_short", "e_long", "E_short", "a_long", "A_short", "o_long", "O_short", "u_long", "U_short", "Bar_lg", "Bar_sht"), 
                 new.row.names = 1:1000,
                 direction = "long")

md.long <- cbind(md.dur[,c(1,5,2,3,4)], md.F1[,c(4)], md.F2[,c(4)])
names(md.long) <- c("name", "id", "group", "vowel", "duration", "F1", "F2")
md.fp <- md.long

#md.fp <- cbind(md.long[,c(1,4)], rep("", nrow(md.long)), md.long[,c(6,7)], rep(NA, nrow(md.long)), rep(NA, nrow(md.long)), rep(NA, nrow(md.long)), rep(NA, nrow(md.long)))
#names(md.fp) <- c("speaker", "vowel", "context", "F1", "F2", "F3", "gl.F1", "gl.F2", "gl.F3")
md.fp$vowel <- rep(c("i:", "i", "e:", "e", "a:", "a", "o:", "o", "u:", "u", "ɨ:", "ɨ"), each=20)
md.fp$vowel <- as.factor(md.fp$vowel)
md.fp$vowel = factor(md.fp$vowel,levels(md.fp$vowel)[c(6,5,8,7,4,3,12,11,10,9,2,1)])



md.fp$length <- "short"
md.fp$length[grep(":", md.fp$vowel)] <- "long"

names(md.fp) <- c("subject", "id", "group", "vowel", "duration", "F1", "F2", "length")
md.fp$length <- as.factor(md.fp$length)
md.fp$group <- gsub("1", "NW", md.fp$group)
md.fp$group <- gsub("2", "SW", md.fp$group)
md.fp$group <- as.factor(md.fp$group)
md.fp$id <- as.factor(md.fp$id)
md.fp$f1_50 <- md.fp$F1
md.fp$f2_50 <- md.fp$F2

#md_plot_nw <- md_plot[grep("NW", md_plot$speaker),]
#md_plot_nw$context <- rep(c("long", "short"), each=10, length.out=120)
md.fp$vowel.fp <- gsub(":", "", md.fp$vowel)
md.fp$vowel.fp <- as.factor(md.fp$vowel.fp)
md.means <- md.fp %>%
  group_by(vowel, group) %>%
  summarise(F1 = mean(F1),
            F2 = mean(F2),
            f1_50 = mean(F1),
            f2_50 = mean(F2),
            duration = mean(duration))

# M&D F1-F2
ggplot(data=md.fp, aes(x=f2_50, y=f1_50, label=vowel)) +
  geom_point(alpha=0.25, aes(color=vowel), size=10, show.legend=FALSE) + 
  geom_text(data=md.means, size=10, show.legend=FALSE) +
  facet_wrap(~group) + 
  scale_color_discrete(guide=FALSE) +
  ylab("F1 (hz)") + xlab("F2 (hz)") +
  labs(lty="Vowel") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text=element_text(size=12),
        legend.text=element_text(size=10)) +
  scale_x_reverse(position = "top", limits = c(3000, 500)) +
  scale_y_reverse(position = "right", limits = c(1000, 200))

# M&D duration
ggplot(data=md.fp, aes(vowel, duration)) +
  #geom_boxplot()
  geom_boxplot(outlier.shape = NA, position="dodge", aes(color=length), show.legend=FALSE) +
  facet_wrap(~group) +
  ylab("Duration (ms)") + xlab("Vowel") +
  labs(lty="Vowel") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text=element_text(size=12),
        legend.text=element_text(size=10)) +
  scale_y_continuous(limits = c(50,250)) +
  ggtitle("Welsh vowel duration")
  

# ggplot(data=ws.fp[!is.na(ws.fp$syll_type2) & ws.fp$vowel %in% c("i:", "ɨ:", "e:", "i", "ɨ", "e"),], aes(vowel, duration)) +
#   #geom_boxplot()
#   geom_boxplot(outlier.shape = 1, position="dodge", aes(color=length), show.legend=FALSE) +
#   #facet_wrap(~unpred, nrow=1, scales="free_x") +
#   ylab("Duration (ms)") + xlab("Vowel") +
#   labs(lty="Vowel") +
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=12),
#         strip.text=element_text(size=12),
#         legend.text=element_text(size=10)) +
#   ggtitle("Welsh-Spanish vowel duration")


##### # plot WS data ##### 
ws.fp <- droplevels(data[data$group=="WS",])
ws.fp$vowel.fp <- gsub(":", "", ws.fp$vowel)

ws.fp$vowel = factor(ws.fp$vowel,levels(ws.fp$vowel)[c(6,5,8,7,4,3,12,11,10,9,2,1)])

ws.fp.2 <- ws.fp %>%
  group_by(subject, vowel) %>%
  summarize(f1_norm = mean(f1_norm),
            f2_norm = mean(f2_norm),
            duration = mean(duration),
            f1_50 = mean(f1_50),
            f2_50 = mean(f2_50))

ws.means <- ws.fp %>%
  group_by(vowel, syll_type2) %>%
  summarise(f1_norm = mean(f1_norm),
            f2_norm = mean(f2_norm),
            duration = mean(duration),
            f1_50 = mean(f1_50),
            f2_50 = mean(f2_50))

# dist <- function(x, y) {
#   sqrt((x-lead(x))^2+(y-lead(y))^2)}
# 
# ws.euc.dist <- ws.fp[ws.fp$vowel %in% c("i:", "i", "e:", "e"),] %>%
#   group_by(vowel) %>%
#   summarize(f1.n = mean(f1_norm),
#             f2.n = mean(f2_norm), 
#             euc.dist=dist(f1.n, f2.n))


# WS F1-F2

ggplot(data=ws.fp[!is.na(ws.fp$syll_type2),], aes(x=f2_norm, y=f1_norm, label=vowel)) +
  geom_point(alpha=0.25, aes(color=vowel), size=10, show.legend=FALSE) + 
  geom_text(data=ws.means, size=10, show.legend=FALSE) +
  #facet_wrap(~syll_type2) + 
  scale_color_discrete(guide=FALSE) +
  ylab("Normalized F1") + xlab("Normalized F2") +
  labs(lty="Vowel") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text=element_text(size=12),
        legend.text=element_text(size=10)) +
    scale_x_reverse(position = "top") +
    scale_y_reverse(position = "right")

# plot with one dot per subject per vowel - normalized data
ggplot(data=ws.fp.2, aes(x=f2_norm, y=f1_norm, label=vowel)) +
  geom_point(alpha=0.25, aes(color=vowel), size=10, show.legend=FALSE) + 
  geom_text(data=ws.means, size=10, show.legend=FALSE) +
  #facet_wrap(~syll_type2) + 
  scale_color_discrete(guide=FALSE) +
  ylab("Normalized F1") + xlab("Normalized F2") +
  labs(lty="Vowel") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text=element_text(size=12),
        legend.text=element_text(size=10)) +
  scale_x_reverse(position = "top") +
  scale_y_reverse(position = "right")

# plot with one dot per subject per vowel - non-normalized data
ggplot(data=ws.fp.2, aes(x=f2_50, y=f1_50, label=vowel)) +
  geom_point(alpha=0.25, aes(color=vowel), size=10, show.legend=FALSE) + 
  geom_text(data=ws.means, size=10, show.legend=FALSE) +
  #facet_wrap(~syll_type2) + 
  scale_color_discrete(guide=FALSE) +
  ylab("F1 (hz)") + xlab("F2 (hz)") +
  labs(lty="Vowel") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text=element_text(size=12),
        legend.text=element_text(size=10)) +
  scale_x_reverse(position = "top", limits = c(3000, 500)) +
  scale_y_reverse(position = "right", limits = c(1000, 200)) +
  ggtitle("Patagonian Welsh")

# only target vowels
# plot with one dot per subject per vowel - non-normalized data
ggplot(data=ws.fp.2[ws.fp.2$vowel %in% c("i:", "ɨ:", "e:", "i", "ɨ", "e"),], aes(x=f2_50, y=f1_50, label=vowel)) +
  geom_point(alpha=0.25, aes(color=vowel), size=10, show.legend=FALSE) + 
  geom_text(data=ws.means[ws.means$vowel %in% c("i:", "ɨ:", "e:", "i", "ɨ", "e"),], size=10, show.legend=FALSE) +
  #facet_wrap(~syll_type2) + 
  scale_color_discrete(guide=FALSE) +
  ylab("F1 (hz)") + xlab("F2 (hz)") +
  labs(lty="Vowel") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text=element_text(size=12),
        legend.text=element_text(size=10)) +
  scale_x_reverse(position = "top") + #, limits = c(3000, 500)) +
  scale_y_reverse(position = "right") + #, limits = c(1000, 200)) +
  ggtitle("Patagonian Welsh")

# WS duration
ggplot(data=ws.fp[!is.na(ws.fp$syll_type2) & ws.fp$vowel %in% c("i:", "ɨ:", "e:", "i", "ɨ", "e"),], aes(vowel, duration)) +
  #geom_boxplot()
  geom_boxplot(outlier.shape = 1, position="dodge", aes(color=length), show.legend=FALSE) +
  #facet_wrap(~unpred, nrow=1, scales="free_x") +
  ylab("Duration (ms)") + xlab("Vowel") +
  labs(lty="Vowel") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text=element_text(size=12),
        legend.text=element_text(size=10)) +
  ggtitle("Patagonian Welsh")


##### # plot WE data ##### 
we.fp <- droplevels(data[data$group=="WE" & data$vowel!="ə",])
we.fp$vowel.fp <- gsub(":", "", we.fp$vowel)

we.fp$vowel = factor(we.fp$vowel,levels(we.fp$vowel)[c(6,5,8,7,4,3,12,11,10,9,2,1)])

we.fp.2 <- we.fp %>%
  group_by(subject, vowel) %>%
  summarize(f1_norm = mean(f1_norm),
            f2_norm = mean(f2_norm),
            duration = mean(duration),
            f1_50 = mean(f1_50),
            f2_50 = mean(f2_50))

we.means <- we.fp %>%
  group_by(vowel, syll_type2) %>%
  summarise(f1_norm = mean(f1_norm),
            f2_norm = mean(f2_norm),
            duration = mean(duration),
            f1_50 = mean(f1_50),
            f2_50 = mean(f2_50))

#dist <- function(x, y) {
#  sqrt((x-lead(x))^2+(y-lead(y))^2)}

# we.euc.dist <- we.fp[we.fp$vowel %in% c("i:", "i", "e:", "e"),] %>%
#   group_by(vowel) %>%
#   summarize(f1.n = mean(f1_norm),
#             f2.n = mean(f2_norm), 
#             euc.dist=dist(f1.n, f2.n))




# WE F1-F2

ggplot(data=we.fp[!is.na(we.fp$syll_type2),], aes(x=f2_norm, y=f1_norm, label=vowel)) +
  geom_point(alpha=0.25, aes(color=vowel), size=10, show.legend=FALSE) + 
  geom_text(data=we.means, size=10, show.legend=FALSE) +
  #facet_wrap(~syll_type2) + 
  scale_color_discrete(guide=FALSE) +
  ylab("Normalized F1") + xlab("Normalized F2") +
  labs(lty="Vowel") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text=element_text(size=12),
        legend.text=element_text(size=10)) +
  scale_x_reverse(position = "top") +
  scale_y_reverse(position = "right")

# plot with one dot per subject per vowel - normalized data
ggplot(data=we.fp.2, aes(x=f2_norm, y=f1_norm, label=vowel)) +
  geom_point(alpha=0.25, aes(color=vowel), size=10, show.legend=FALSE) + 
  geom_text(data=we.means, size=10, show.legend=FALSE) +
  #facet_wrap(~syll_type2) + 
  scale_color_discrete(guide=FALSE) +
  ylab("Normalized F1") + xlab("Normalized F2") +
  labs(lty="Vowel") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text=element_text(size=12),
        legend.text=element_text(size=10)) +
  scale_x_reverse(position = "top") +
  scale_y_reverse(position = "right")

# plot with one dot per subject per vowel - non-normalized data
ggplot(data=we.fp.2, aes(x=f2_50, y=f1_50, label=vowel)) +
  geom_point(alpha=0.25, aes(color=vowel), size=10, show.legend=FALSE) + 
  geom_text(data=we.means, size=10, show.legend=FALSE) +
  #facet_wrap(~syll_type2) + 
  scale_color_discrete(guide=FALSE) +
  ylab("F1 (hz)") + xlab("F2 (hz)") +
  labs(lty="Vowel") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text=element_text(size=12),
        legend.text=element_text(size=10)) +
  scale_x_reverse(position = "top", limits = c(3000, 500)) +
  scale_y_reverse(position = "right", limits = c(1000, 200)) +
  ggtitle("Northern Welsh")

# only target vowels
# plot with one dot per subject per vowel - non-normalized data
ggplot(data=we.fp.2[we.fp.2$vowel %in% c("i:", "ɨ:", "e:", "i", "ɨ", "e"),], aes(x=f2_50, y=f1_50, label=vowel)) +
  geom_point(alpha=0.25, aes(color=vowel), size=10, show.legend=FALSE) + 
  geom_text(data=we.means[we.means$vowel %in% c("i:", "ɨ:", "e:", "i", "ɨ", "e"),], size=10, show.legend=FALSE) +
  #facet_wrap(~syll_type2) + 
  scale_color_discrete(guide=FALSE) +
  ylab("F1 (hz)") + xlab("F2 (hz)") +
  labs(lty="Vowel") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text=element_text(size=12),
        legend.text=element_text(size=10)) +
  scale_x_reverse(position = "top") + #, limits = c(3000, 500)) +
  scale_y_reverse(position = "right") + #, limits = c(1000, 200)) +
  ggtitle("Northern Welsh")

# WE duration
ggplot(data=we.fp[!is.na(we.fp$syll_type2) & we.fp$vowel %in% c("i:", "ɨ:", "e:", "i", "ɨ", "e"),], aes(vowel, duration)) +
  #geom_boxplot()
  geom_boxplot(outlier.shape = 1, position="dodge", aes(color=length), show.legend=FALSE) +
  #facet_wrap(~unpred, nrow=1, scales="free_x") +
  ylab("Duration (ms)") + xlab("Vowel") +
  labs(lty="Vowel") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text=element_text(size=12),
        legend.text=element_text(size=10)) +
  ggtitle("Northern Welsh")



##### calculate distances #####
points <- rbind(data.frame(group=rep("MD"), md.means[md.means$group=="NW" & md.means$vowel %in% c("i:", "ɨ:", "e:", "i", "ɨ", "e"),c("vowel", "f1_50", "f2_50")]),
                 data.frame(group=rep("WE"),  we.means[we.means$vowel %in% c("i:", "ɨ:", "e:", "i", "ɨ", "e"),c("vowel", "f1_50", "f2_50")]),
                 data.frame(group=rep("WS"), ws.means[ws.means$vowel %in% c("i:", "ɨ:", "e:", "i", "ɨ", "e"),c("vowel", "f1_50", "f2_50")]))

points$comp <- paste(points$group, points$vowel, sep="_")

points1 <- points
points2 <- points

cr.dist <- crossdist(points1$f1_50, points1$f2_50, points2$f1_50, points2$f2_50)
rownames(cr.dist) <- points$comp
colnames(cr.dist) <- points$comp

write.csv(cr.dist, "~/Dropbox/dissertation/TLS2019/crossdistances.csv", row.names = T, col.names = T)


cr.dist1 <- melt(cr.dist)
  #data.frame(point1 = rownames(cr.dist), point2 = colnames(cr.dist), distance = diag(cr.dist))
cr.dist1$group1 <- str_extract(cr.dist1$Var1, "^[A-Z]{2}")
cr.dist1$group1 <- as.factor(cr.dist1$group1)
cr.dist1$group2 <- str_extract(cr.dist1$Var2, "^[A-Z]{2}")
cr.dist1$group2 <- as.factor(cr.dist1$group2)

cr.dist1$comparison <- paste(str_extract(cr.dist1$Var1, ".{1}:?$"), str_extract(cr.dist1$Var2, ".{1}:?$"), sep="-")
cr.dist1$comparison <- as.factor(cr.dist1$comparison)

cr.dist2 <- droplevels(cr.dist1[cr.dist1$group1==cr.dist1$group2,])

cr.dist2$comparison <- gsub("e-e:","e:-e", cr.dist2$comparison)
cr.dist2$comparison <- gsub("i-i:","i:-i", cr.dist2$comparison)
cr.dist2$comparison <- gsub("ɨ-i","i-ɨ", cr.dist2$comparison)
cr.dist2$comparison <- gsub("ɨ-ɨ:","ɨ:-ɨ", cr.dist2$comparison)
cr.dist2$comparison <- gsub("ɨ:-i:","i:-ɨ:", cr.dist2$comparison)


cr.dist3 <- droplevels(cr.dist2[cr.dist2$comparison %in% c("i:-ɨ:", "i-ɨ", "i:-e:", "i-e", "i:-i", "ɨ:-ɨ", "e:-e"),])
cr.dist3$comparison <- as.factor(cr.dist3$comparison)
cr.dist3$comparison = factor(cr.dist3$comparison,levels(cr.dist3$comparison)[c(6,3,4,2,5,7,1)])

# plot BETWEEN vowel pair comparisons
ggplot(data=cr.dist3[cr.dist3$comparison %in% c("i:-ɨ:", "i-ɨ", "i:-e:", "i-e"),], aes()) +
  geom_col(aes(x=group1, y=value, fill=comparison), position="dodge") +
  ylab("Euclidean distance (hz)") + xlab("") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text=element_text(size=12),
        legend.text=element_text(size=12)) +
  scale_x_discrete(labels=c("MD 2011", "Nor. Welsh", "Pat. Welsh")) +
  ggtitle("Distance across vowel pairs")


# plot WITHIN vowel pair comparisons
ggplot(data=cr.dist3[cr.dist3$comparison %in% c("i:-i", "ɨ:-ɨ", "e:-e"),], aes()) +
  geom_col(aes(x=group1, y=value, fill=comparison), position="dodge") +
  ylab("Euclidean distance (hz)") + xlab("") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text=element_text(size=12),
        legend.text=element_text(size=12)) +
  scale_x_discrete(labels=c("MD 2011", "Nor. Welsh", "Pat. Welsh")) +
  ggtitle("Distance within vowel pairs")


