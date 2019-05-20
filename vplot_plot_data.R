# These are the R packages you need to run this code correctly.
require(ggplot2)
require(ggpubr)
require(plyr)
require(dplyr)
require(reshape2)

# If you're running this after running vplots_read_data.R, you will already have a dataframe called 'data' in your environment



# Assuming we want to plot a F1 ~ F2 vowel space, we first need to tell ggplot2 what data to look at
ggplot(data=data)

# We also need to tell it what factors it should pay attention to
ggplot(data=data, aes(x=F2, y=F1, label=vowel))

# If you run that code, you get a plot, but no data appears
ggplot(data=data, aes(x=F2, y=F1)) +
  geom_point()

# Now we have data, but no information about what vowels it's from
ggplot(data=data, aes(x=F2, y=F1)) +
  geom_point(aes(color=vowel))

# Something doesn't look quite right - /u/ is in the bottom left, but if we want a traditional vowel plot, it should be in the top right
# ggplot(data=data, aes(x=F2, y=F1)) +
#   geom_point(aes(color=vowel)) +
#   scale_x_reverse() +
#   scale_y_reverse()
# 
# # Now our data looks like it's in the right place (we've reversed the x and y axes). If we want the axis labels to be on the top and right, we just have to add position modifiers to the scale_ commands.
# ggplot(data=data, aes(x=F2, y=F1)) +
#   geom_point(aes(color=vowel)) +
#   scale_x_reverse(position = "top") +
#   scale_y_reverse(position = "right")

# We can also indicate vowel identity using shape instead of color.
ggplot(data=data, aes(x=F2, y=F1)) +
  geom_point(aes(shape=vowel))

# We can also plot the vowel symbols, using geom_text() instead of geom_point()
ggplot(data=data, aes(x=F2, y=F1)) +
  geom_text(aes(label=vowel))

# You can combine geom_text() and color
ggplot(data=data, aes(x=F2, y=F1)) +
  geom_text(aes(label=vowel, color=vowel))

# You may want to include both raw and summarized data. To summarize your data, you can use the reshape2 packages (for pipes %>%) and dplyr
# This command produces a new dataframe (actually a tibble, don't worry about that) with one value for combination of vowel and v_length
# It summarizes across speakers and across any other variables that aren't included in the group_by() command
# Here, I only calculate means, but you can make many other summary calculations if desired.
data.means <- data %>%
  group_by(vowel) %>%
  summarise(F1 = mean(F1),
            F2 = mean(F2))

# Once you have that summarized, data you can add it to the plot containing the unsummarized data
ggplot(data=data, aes(x=F2, y=F1)) +
  geom_text(aes(label=vowel, color=vowel)) +
  geom_text(data=data.means, aes(label=vowel), size=12)

# You can vary the transparency of layers of data by setting alpha to a value between 0 and 1. The smaller alpha is, the more transparent the result.
ggplot(data=data, aes(x=F2, y=F1)) +
  geom_text(aes(label=vowel, color=vowel)) +
  geom_text(data=data.means, alpha=0.5, aes(label=vowel), size=12)

# Here are some other useful ggplot commands.

# To hide a legend that you don't want, try adding show.legend=FALSE to the geom_ call.
ggplot(data=data, aes(x=vowel, y=duration)) + geom_boxplot(aes(color=v_length))
ggplot(data=data, aes(x=vowel, y=duration)) + geom_boxplot(aes(color=v_length), show.legend=FALSE)

# To change the font size of axis and legend text, use the theme() command
ggplot(data=data, aes(x=vowel, y=duration)) + 
  geom_boxplot(aes(color=v_length)) +
  theme(axis.text=element_text(size=24),
      axis.title=element_text(size=24),
      strip.text=element_text(size=24),
      legend.title=element_text(size=24),
      legend.text=element_text(size=24))

# To change the default names of the x and y axes add
ggplot(data=data, aes(x=vowel, y=duration)) + 
  geom_boxplot(aes(color=v_length)) +
  ylab("Duration (ms)") + 
  xlab("VOWELS")

# To specify the limits of the axes, add limits, to the call to scale_x_()
ggplot(data=data, aes(x=vowel, y=duration)) + 
  geom_boxplot(aes(color=v_length)) +
  scale_y_continuous(limits = c(0,500))

# Summarize data by calculating mean values for each vowel produced by each speaker
data.summary <- data %>%
  group_by(subject, vowel, v_length) %>%
  summarize(f1_norm = mean(f1_norm),
            f2_norm = mean(f2_norm),
            duration = mean(duration),
            f0_50 = mean(f0_50),
            F1 = mean(F1),
            F2 = mean(F2),
            f3_50 = mean(f3_50))

# Summarize data by calculating mean values for each vowel (across speakers) at each level of the 'v_length' variable
data.means <- data %>%
  group_by(vowel, v_length) %>%
  summarise(f1_norm = mean(f1_norm),
            f2_norm = mean(f2_norm),
            duration = mean(duration),
            f0_50 = mean(f0_50),
            F1 = mean(F1),
            F2 = mean(F2),
            f3_50 = mean(f3_50))


# Vowel duration
ggplot(data=data.summary, aes(vowel, duration)) +
  geom_boxplot(outlier.shape = 1, position="dodge", aes(color=v_length), show.legend=FALSE) +
  ylab("Duration (ms)") + xlab("Vowel") +
  labs(lty="Vowel") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text=element_text(size=12),
        legend.text=element_text(size=10)) +
  ggtitle("Vowel duration")

# plot with one dot per subject per vowel - normalized data
ggplot(data=data.summary, aes(x=f2_norm, y=f1_norm, label=vowel)) +
  geom_point(alpha=0.25, aes(color=vowel), size=10, show.legend=FALSE) + 
  geom_text(data=data.means, size=10, show.legend=FALSE) +
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
ggplot(data=data.summary, aes(x=F2, y=F1, label=vowel)) +
  geom_point(alpha=0.25, aes(color=vowel, shape=subject), size=10, show.legend=FALSE) + 
  facet_wrap(.~subject)
  geom_text(data=data.means, size=10, show.legend=FALSE) +
  scale_color_discrete(guide=FALSE) +
  ylab("F1 (hz)") + xlab("F2 (hz)") +
  labs(lty="Vowel") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text=element_text(size=12),
        legend.text=element_text(size=10)) +
  scale_x_reverse(position = "top", limits = c(3000, 500)) +
  scale_y_reverse(position = "right", limits = c(1000, 200))

##### # plot WS data ##### 
ws.fp <- droplevels(data[data$group=="WS",])
ws.fp$vowel.fp <- gsub(":", "", ws.fp$vowel)

ws.fp$vowel = factor(ws.fp$vowel,levels(ws.fp$vowel)[c(6,5,8,7,4,3,12,11,10,9,2,1)])

ws.fp.2 <- ws.fp %>%
  group_by(subject, vowel) %>%
  summarize(f1_norm = mean(f1_norm),
            f2_norm = mean(f2_norm),
            duration = mean(duration),
            F1 = mean(F1),
            F2 = mean(F2))

ws.means <- ws.fp %>%
  group_by(vowel, syll_type2) %>%
  summarise(f1_norm = mean(f1_norm),
            f2_norm = mean(f2_norm),
            duration = mean(duration),
            F1 = mean(F1),
            F2 = mean(F2))

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
ggplot(data=ws.fp.2, aes(x=F2, y=F1, label=vowel)) +
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
ggplot(data=ws.fp.2[ws.fp.2$vowel %in% c("i:", "ɨ:", "e:", "i", "ɨ", "e"),], aes(x=F2, y=F1, label=vowel)) +
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




##### # plot WE data ##### 
we.fp <- droplevels(data[data$group=="WE" & data$vowel!="ə",])
we.fp$vowel.fp <- gsub(":", "", we.fp$vowel)

we.fp$vowel = factor(we.fp$vowel,levels(we.fp$vowel)[c(6,5,8,7,4,3,12,11,10,9,2,1)])

we.fp.2 <- we.fp %>%
  group_by(subject, vowel) %>%
  summarize(f1_norm = mean(f1_norm),
            f2_norm = mean(f2_norm),
            duration = mean(duration),
            F1 = mean(F1),
            F2 = mean(F2))

we.means <- we.fp %>%
  group_by(vowel, syll_type2) %>%
  summarise(f1_norm = mean(f1_norm),
            f2_norm = mean(f2_norm),
            duration = mean(duration),
            F1 = mean(F1),
            F2 = mean(F2))

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
ggplot(data=we.fp.2, aes(x=F2, y=F1, label=vowel)) +
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
ggplot(data=we.fp.2[we.fp.2$vowel %in% c("i:", "ɨ:", "e:", "i", "ɨ", "e"),], aes(x=F2, y=F1, label=vowel)) +
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
  geom_boxplot(outlier.shape = 1, position="dodge", aes(color=v_length), show.legend=FALSE) +
  #facet_wrap(~unpred, nrow=1, scales="free_x") +
  ylab("Duration (ms)") + xlab("Vowel") +
  labs(lty="Vowel") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text=element_text(size=12),
        legend.text=element_text(size=10)) +
  ggtitle("Northern Welsh")



##### calculate distances #####
points <- rbind(data.frame(group=rep("MD"), md.means[md.means$group=="NW" & md.means$vowel %in% c("i:", "ɨ:", "e:", "i", "ɨ", "e"),c("vowel", "F1", "F2")]),
                data.frame(group=rep("WE"),  we.means[we.means$vowel %in% c("i:", "ɨ:", "e:", "i", "ɨ", "e"),c("vowel", "F1", "F2")]),
                data.frame(group=rep("WS"), ws.means[ws.means$vowel %in% c("i:", "ɨ:", "e:", "i", "ɨ", "e"),c("vowel", "F1", "F2")]))

points$comp <- paste(points$group, points$vowel, sep="_")

points1 <- points
points2 <- points

cr.dist <- crossdist(points1$F1, points1$F2, points2$F1, points2$F2)
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


