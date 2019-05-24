# These are the R packages you need to run this code correctly.
require(ggplot2)
require(ggpubr)
require(plyr)
require(dplyr)
require(reshape2)
require(RColorBrewer)

# If you're running this after running vplots_read_data.R, you will already have a dataframe called 'data' in your environment

# Assuming we want to plot a F1 ~ F2 vowel space, we first need to tell ggplot2 what data to look at
ggplot(data=data)

# We also need to tell it what factors it should pay attention to
ggplot(data=data, aes(x=F2, y=F1, label=vowel))

# If you run that code, you get a plot, but no data appears
ggplot(data=data, aes(x=F2, y=F1)) +
  geom_point()

# Now we have data, but no information about what vowels it's from
# You can specify that `color` should be used to indicate levels of the vowel factor
ggplot(data=data, aes(x=F2, y=F1)) +
  geom_point(aes(color=vowel))

# If your vowel data is non-normalized F1 and F2 values,
# you may notice that this plot does not match up with a traditional vowel plot (ex: /u/ should be in the top right)
# The way to fix this (if necessary) is to reverse the x and y axes
# ggplot(data=data, aes(x=F2, y=F1)) +
#   geom_point(aes(color=vowel)) +
#   scale_x_reverse() +
#   scale_y_reverse()
# 
# You can also re-position the axes so that the x-axis is on the top and the y-axis is on the right
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
            F2 = mean(F2),
            duration = mean(duration))

# Once you have that summarized, data you can add it to the plot containing the unsummarized data
ggplot(data=data, aes(x=F2, y=F1)) +
  geom_text(aes(label=vowel, color=vowel)) +
  geom_text(data=data.means, aes(label=vowel), size=12)

# You can vary the transparency of layers of data by setting alpha to a value between 0 and 1. The smaller alpha is, the more transparent the result.
ggplot(data=data, aes(x=F2, y=F1)) +
  geom_text(aes(label=vowel, color=vowel)) +
  geom_text(data=data.means, alpha=0.5, aes(label=vowel), size=12)

# You can use the non-summarized data to create ellipses to represent the distribution of vowel groups
ggplot(data=data, aes(x=F2, y=F1)) +
  stat_ellipse(linetype=2, geom="polygon", alpha=0.25, aes(fill=vowel)) +
  geom_point(alpha=0.25, aes()) +
  geom_text(data=data.means, aes(label=vowel), size=12)

# The plots above show two continuous variables (F1, F2) and each level of a categorical variable (vowel)
# If you only have one continuous variable (such as duration) and one categorical variable, you can create other types of plots

ggplot(data=data, aes(x=vowel, y=duration)) +
  geom_boxplot()

# You can represent additional categorical variables (here, vowel length) in several different ways.
# You can show them all on the same plot using color
ggplot(data=data, aes(x=vowel, y=duration, color=v_length)) +
  geom_boxplot()
# You can create a plot for each level of the variable using 
ggplot(data=data, aes(x=vowel, y=duration, color=v_length)) +
  geom_boxplot() +
  facet_grid(.~v_length)
ggplot(data=data, aes(x=vowel, y=duration, color=v_length)) +
  geom_boxplot() +
  facet_grid(vowel~v_length)
  
# Here are some other useful ggplot commands.

# To change the theme of the plot, you can add a theme_() command
ggplot(data=data, aes(x=F2, y=F1)) +
  geom_point(aes(color=vowel)) +
  #theme_bw() 
  #theme_light()
  theme_void()

# You can also modify the colors of the plot using RColorBrewer
ggplot(data=data, aes(x=F2, y=F1,color=vowel)) +
  geom_point() +
  theme_bw() +
  scale_fill_brewer(palette="Dark2", aesthetics = c("color"))

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

# To specify the limits of the axes, add limits to the call to scale_x_continuous()
# NB: scale commands have 3 parts, scale_, axis identifier (x, y), and type of axis (continuous, discrete, log, etc.)
ggplot(data=data, aes(x=vowel, y=duration)) + 
  geom_boxplot(aes(color=v_length)) +
  scale_y_continuous(limits = c(0,500))

# You can specify the title of the plot using ggtitle()
ggplot(data=data, aes(x=vowel, y=duration, color=v_length)) +
  geom_boxplot() +
  ggtitle("Vowel duration")

