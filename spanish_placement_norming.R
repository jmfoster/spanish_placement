require(data.table)
require(ggplot2)
require(MASS)

working_dir_filepath = '~/workspace/assett/spanish'
setwd(working_dir_filepath)

pdf('./plots.pdf', width=12, height=11)

theme_set(theme_grey(base_size = 18)) 
d = read.csv('data/data_cleaned.csv', header=T)
d = data.table(d)

# Set pass/fail cutoff
d$pass = 0
d[d$course_grade>=70, 'pass'] = 1
d$pass = as.factor(d$pass)


ftable(xtabs(~ course_level + pass, data=d))
xtabs(~ course_level, data=d)

d$placement_exam_taken = substr(d$Placement_Exam_Taken, 1, 12)
xtabs(~ course_level + placement_exam_taken, data=d)

# exclude 2110 Section 6
d = d[d$Sections != 'SPAN 2110-006',]
nrow(d)

# exclude SPN 3000
d = d[d$course_level != 'SPAN 3000',]
nrow(d)

# store course_level as ordered factor
d$course_level = as.ordered(d$course_level)#, levels=)


# boxplots of placement_score by course_level and pass/fail
ggplot(d, aes(x = pass, y = placement_score)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5, width=.1) +
  #geom_point() +
  facet_grid(~ course_level, margins = FALSE) #+
#theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


# exclude outliers
#boxplot(placement_score ~ course_level, data=d)
#boxplot(placement_score ~ course_level + pass, data=d)
#quartiles = d[, list(q1=quantile(placement_score, .25), q3=quantile(placement_score, .75), IQR=quantile(placement_score, .75)-quantile(placement_score, .25)), by=c('course_level')]
quartiles = d[, list(q1=quantile(placement_score, .25), q3=quantile(placement_score, .75), IQR=quantile(placement_score, .75)-quantile(placement_score, .25)), by=c('course_level', 'pass')]
quartiles$min = quartiles$q1 - 1.5*quartiles$IQR
quartiles$max = quartiles$q3 + 1.5*quartiles$IQR
quartiles
#d.outliers = merge(d, quartiles, by='course_level')
d.outliers = merge(d, quartiles, by=c('course_level', 'pass'))
nrow(d.outliers)
#d.outliers = d.outliers[(d.outliers$placement_score > d.outliers$min & d.outliers$placement_score < d.outliers$max), ]
d.outliers = d.outliers[(d.outliers$placement_score < d.outliers$max), ]
nrow(d.outliers)
d = d.outliers[ , setdiff(names(d.outliers), c('min', 'max')), with=FALSE]
#d = d.outliers
nrow(d)
ggplot(d, aes(x = pass, y = placement_score)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5, width=.1) +
  #geom_point() +
  facet_grid(~ course_level, margins = FALSE) #+
#theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
#boxplot(placement_score ~ course_level, data=d.outliers)

# Summary Stats
summary(d)
hist(d$placement_score)
hist(d$course_grade)
#plot(d$course_grade, d$placement_score)
#abline(lm(d$placement_score ~ d$course_grade))

mean_scores_by_level = d[,list(mean_placement_score=mean(placement_score)), by=c('course_level')]
mean_scores_by_level$level = 1:nlevels(mean_scores_by_level$course_level)
mean_scores_by_level
plot(mean_scores_by_level$level, mean_scores_by_level$mean_placement_score)
abline(lm(mean_scores_by_level$mean_placement_score ~ mean_scores_by_level$level))
plot(d$course_level, d$placement_score)
#abline(lm(d$course_grade ~ d$course_level))
#abline(h=polr_cuts$placement_score)

ftable(xtabs(~ course_level + pass, data=d))
xtabs(~ course_level, data=d)

# boxplots of placement_score by course_level and section
ggplot(d, aes(x = as.factor(section), y = placement_score)) +
  geom_boxplot(size = .75) +
  #geom_jitter(alpha = .5, width=.1) +
  #geom_point() +
  facet_grid(~ course_level, margins = FALSE) #+
#theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# boxplots of course_grade by course_level and section
ggplot(d, aes(x = as.factor(section), y = course_grade)) +
  geom_boxplot(size = .75) +
  #geom_jitter(alpha = .5, width=.1) +
  #geom_point() +
  facet_grid(~ course_level, margins = FALSE) #+
#theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


# lineplots of placement_score by course_level and course grade
ggplot(d, aes(x = course_grade, y = placement_score)) +
  geom_point() +
  #geom_smooth(method='lm', se=F) +
  geom_smooth(method='lm', se=T) +
  facet_grid(~ course_level, margins = FALSE) #+
#theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# combined lineplots
ggplot(d, aes(x = course_grade, y = placement_score, color=course_level)) +
  geom_point() +
  geom_smooth(method='lm', se=F) 
  #facet_grid(~ course_level, margins = FALSE) #+
#theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# stacked histograms
ggplot(d, aes(placement_score)) + 
  geom_histogram(aes(fill=course_level))


# standard error and error bars
placement_scores_by_course = d[d$pass==1, list(mean=mean(placement_score), sd=sd(placement_score), n=length(placement_score)), by=c('course_level')]
placement_scores_by_course
placement_scores_by_course$se = placement_scores_by_course$sd / sqrt(placement_scores_by_course$n)
dodge <- position_dodge(width = 0.9)
limits <- aes(ymax = placement_scores_by_course$mean + placement_scores_by_course$se,
              ymin = placement_scores_by_course$mean - placement_scores_by_course$se)
p <- ggplot(data = placement_scores_by_course, aes(x = course_level, y = mean, fill = course_level))
p + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) +
  ggtitle("Mean Placement Score by Current Course Level", subtitle="(passing course grades only)") +
  theme(legend.position = "none") + 
  #geom_hline(yintercept=c(18.5, 22.5, 27.5, 35.5))
  #geom_hline(yintercept=c(20, 25, 35))
  #theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        #axis.title.x=element_blank())

course_levels = levels(d$course_level)
borderline_cut_scores = rep(0, length(course_levels))
d$borderline_cut_score = 0
d$placement_level = 0
for(i in 1:length(course_levels)){
  #print(course_levels[i])
  d.level = d[d$course_level==course_levels[i],]
  plot(d.level$course_grade, d.level$placement_score, main=course_levels[i])
  abline(lm(d.level$placement_score ~ d.level$course_grade))
  
  # Borderline method
  d.borderline = d.level[(d.level$course_grade>=60 & d.level$course_grade<=80),]
  borderline_cut_scores[i] = as.integer(median(d.borderline$placement_score))
  print(paste(course_levels[i], borderline_cut_scores[i]))
  d[d$course_level==course_levels[i], 'borderline_cut_score'] = borderline_cut_scores[i]
  
  # set target placement_level based on combo of course_level and course_grade
  d[(d$course_level==course_levels[i] & d$pass==0), 'placement_level'] = i
  d[(d$course_level==course_levels[i] & d$pass==1), 'placement_level'] = i+1
}

d$placement_level = as.ordered(d$placement_level)
d$placement_level
unique(d[,c('course_level', 'pass', 'placement_level')])

d[, list(mean_placement_score=mean(placement_score)), by=c('course_level')]
d[, list(mean_placement_score=mean(placement_score)), by=c('course_level', 'pass')]
d[, list(mean_placement_score=mean(placement_score)), by=c('placement_level')]


# overlapping histograms [all]
d.level = d[d$course_level==course_levels[1],]
hist(d.level$placement_score, col=rgb(1,0,0,0.5),xlim=c(0,60), ylim=c(0,20), main="Overlapping Histogram", xlab="Variable")
for(i in 2:length(course_levels)){
  d.level = d[d$course_level==course_levels[i],]
  hist(d.level$placement_score, col=rgb(i/length(levels(d$course_level)),0,1,0.5), add=T)
}
box()

cut_scores = c(18.5, 22.5, 27.5, 35.5)
cut_scores

# overlapping histograms [pairwise]
for(i in 1:(length(course_levels)-1)){
  d.level = d[d$course_level==course_levels[i] & d$pass==1,]
  hist(d.level$placement_score, freq=FALSE, breaks=seq(0,60,3), col=rgb(1,0,0,0.5),xlim=c(0,60), ylim=c(0,.1), main=paste(course_levels[i], 'vs', course_levels[i+1]), xlab="Placement Score")
  d.level = d[d$course_level==course_levels[i+1] & d$pass==1,]
  hist(d.level$placement_score, freq=FALSE, breaks=seq(0,60,3), col=rgb(0,0,1,0.5), add=T)
  box()
  abline(v=cut_scores[i])
}

# cut scores (eyeballing): 20, 25, 35 
# cut scores (eyeballing): 20, 25, 35       [normalized]
# cut scores (eyeballing): 20, 25, 35       [normalized and excluding outliers]

placement_levels = levels(d$placement_level)
# overlapping histograms [pairwise]
for(i in 1:(length(placement_levels)-1)){
  d.level = d[d$placement_level==placement_levels[i],]
  hist(d.level$placement_score, right=T, freq=FALSE, breaks=seq(0,60,3), col=rgb(1,0,0,0.5),xlim=c(0,60), ylim=c(0,.1), main=paste("Placement Level", placement_levels[i], 'vs', placement_levels[i+1]), xlab="Placement Score")
  d.level = d[d$placement_level==placement_levels[i+1],]
  hist(d.level$placement_score, right=T, freq=FALSE, breaks=seq(0,60,3), col=rgb(0,0,1,0.5), add=T)
  box()
  abline(v=cut_scores[i])
}

# cut scores (eyeballing): 20, 20, 35, 35


# boxplots of placement_score by placement_level
ggplot(d, aes(x = placement_level, y = placement_score)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5, width=.1) 
  #geom_point() +
  #facet_grid(~ course_level, margins = FALSE) #+
#theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# standard error and error bars
placement_scores_by_course = d[, list(mean=mean(placement_score), sd=sd(placement_score), n=length(placement_score)), by=c('placement_level')]
placement_scores_by_course
print(placement_scores_by_course, digits=3)
placement_scores_by_course$se = placement_scores_by_course$sd / sqrt(placement_scores_by_course$n)
dodge <- position_dodge(width = 0.9)
limits <- aes(ymax = placement_scores_by_course$mean + placement_scores_by_course$se,
              ymin = placement_scores_by_course$mean - placement_scores_by_course$se)
p <- ggplot(data = placement_scores_by_course, aes(x = placement_level, y = mean, fill = placement_level))
p + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) +
  theme(legend.position = "none") +
  ggtitle("Mean Placement Score by Placement Level") +
  geom_hline(yintercept=c(18.5, 22.5, 27.5, 35.5))
  #geom_hline(yintercept=c(15, 22.5, 27, 35))
#theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
#axis.title.x=element_blank())
#abline(h=c(20, 25, 35))

# PLACEMENT LEVEL
## fit ordered logit model and store results 'm'
m <- polr(placement_level ~ placement_score, data = d, Hess=TRUE)
s = summary(m)
s

# COURSE LEVEL
## fit ordered logit model and store results 'm'
m <- polr(course_level ~ placement_score, data = d, Hess=TRUE)
s = summary(m)
s

placement_score = seq(from=0, to=60, by=1)
preds = predict(m, data.frame(placement_score))
predicted_cutoffs = data.frame(placement_score, preds)
predicted_cutoffs
polr_cuts = predicted_cutoffs[!duplicated(predicted_cutoffs$preds), ]
polr_cuts

polr_cuts$placement_score
borderline_cut_scores

#predicted polr cut scores
plot(placement_score, preds)
abline(v=polr_cuts$placement_score)

#actual data with polr cut scores
plot(d$placement_score, d$placement_level)
abline(v=polr_cuts$placement_score)

# actual data with borderline cut scores
plot(d$placement_score, d$placement_level)
abline(v=borderline_cut_scores)

# actual data with contrasting groups cut scores
plot(d$placement_score, d$placement_level)
abline(v=c(20, 25, 35))


# actual data w/ current course_level (not placement_level)
plot(d$placement_score, d$course_level)
abline(v=polr_cuts$placement_score)

# actual data w/ current course_level (not placement_level)
plot(d$placement_score, d$course_level)
abline(v=borderline_cut_scores)
borderline_cut_scores

dev.off() # save plots to pdf file.

##############  Scratch
# 
# # https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
# 
# 
# 
# # plot polr model predictions
# # https://stats.stackexchange.com/questions/89474/interpretation-of-ordinal-logistic-regression
# scores = seq(from=0, to=60, by=1)
# xbeta = scores * s$coefficients[1]
# logistic_cdf <- function(x) {
#   return( 1/(1+exp(-x) ) )
# }
# 
# s$zeta
# s$coefficients[1]
# 
# #http://doingbayesiandataanalysis.blogspot.com/2014/11/ordinal-probit-regression-transforming.html
# polrToOrdScale = function( polrObject ) {
#   polrThresh = polrObject$zeta
#   polrSlopes = polrObject$coefficients
#   polrInter = 0.0
#   polrSigma = 1.0
#   K = length(polrThresh) + 1  # K is number of ordinal levels
#   sigmaMult = unname( (K-2)/(polrThresh[K-1]-polrThresh[1]) )
#   inter = unname( 1.5 - ( sigmaMult*polrThresh[1] ) )
#   respThresh = sigmaMult*polrThresh + inter
#   respSigma = sigmaMult*polrSigma
#   respB0 = sigmaMult*polrInter + inter
#   respSlopes = sigmaMult*polrSlopes
#   return( list( sigma=respSigma , 
#                 b0=respB0 , 
#                 coefficients=respSlopes , 
#                 zeta=respThresh ) )
# }
# 
# polrToOrdScale(m)
