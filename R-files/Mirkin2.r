library(e1071)
set.seed(123)
online.news.popularity <- read.csv(file="sample_OnlineNewsPopularity.csv",head=TRUE,sep=",")

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}



feature <- online.news.popularity$shares
log_feature <- log(feature)
plot(density(feature))
plot(density(log_feature))
boxplot(feature)
boxplot(log_feature) # heavy tails

mean(log_feature) # 7.453988
median(log_feature) # 7.244228
skewness(log_feature)  # 1.08718 > 0, so, right skewed, so mean > median > mode
Mode(log_feature) # 7.003065


####################################################################################################
##########################             BOOTSTAP FOR MEAN          ##################################
####################################################################################################

generate_bs_data <- function(nrows, ncols, data) {
  n = length(data)
  bootstrap_matrix_index <- sample(1:n, size = nrows * ncols, replace = TRUE)
  bootstrap_matrix <- matrix(sapply(bootstrap_matrix_index, function(index){data[index]}), nrow = nrows, ncol = ncols)
}

get_pivot_bs_ci <- function(nrows, ncols, data, foo = mean, alpha = 0.05) {
  bs_data <- generate_bs_data(nrows = nrows, ncols = ncols, data = data)
  bs_sample_of_means <- apply(bs_data, MARGIN = 1, foo)
  bs_mean <- mean(bs_sample_of_means)
  bs_sd <- sd(bs_sample_of_means)
  ci.lvl <- -qnorm(alpha)
  q1.pivot <- bs_mean - ci.lvl * bs_sd
  q2.pivot <- bs_mean + ci.lvl * bs_sd
  c(q1.pivot, q2.pivot)
}

get_nonpivot_bs_ci <- function(nrows, ncols, data, foo = mean, alpha = 0.05) {
  bs_data <- generate_bs_data(nrows = nrows, ncols = ncols, data = data)
  bs_sample_of_means <- apply(bs_data, MARGIN = 1, foo)
  bs_sample_of_means <- sort(bs_sample_of_means)
  q1 <- bs_sample_of_means[alpha/2*N]
  q2 <- bs_sample_of_means[N - alpha/2*N]
  c(q1,q2)
}

get_statistics_ci_mean <- function(data, alpha = 0.05) {
  sample_mean <- mean(data)
  sample_sd <- sd(data)
  ci_lvl <- -qnorm(alpha)
  q1 <- sample_mean - ci_lvl * sample_sd
  q2 <- sample_mean + ci_lvl * sample_sd
  c(q1,q2)
}


n = length(log_feature)
N = 2000 # number of bootstrap samples
m = 200 # bootstrap sample size
alpha = 0.05
pivot_ci <- get_pivot_bs_ci(N ,m, log_feature, alpha)
npivot_ci <- get_nonpivot_bs_ci(N ,m, log_feature, alpha)
stat_ci <- get_statistics_ci_mean(log_feature, alpha)



list(Pivot = pivot_ci, NonPivot = npivot_ci,
     Statistics = stat_ci, RealMean = mean(log_feature))



####################################################################################################
##########################            BOOTSTAP FOR MEDIAN         ##################################
####################################################################################################

# http://sites.stat.psu.edu/~babu/mypdfpap/1984AS12.pdf

pivot_ci_median <- get_pivot_bs_ci(N ,m, log_feature, foo = median, alpha)
npivot_ci_median <- get_nonpivot_bs_ci(N ,m, log_feature, foo = median, alpha)

median(log_feature)
pivot_ci_median
npivot_ci_median

####################################################################################################
##########################            BOOTSTAP FOR MODE          ##################################
####################################################################################################


pivot_ci_mode <- get_pivot_bs_ci(N ,m, log_feature, foo = Mode, alpha)
npivot_ci_mode <- get_nonpivot_bs_ci(N ,m, log_feature, foo = Mode, alpha)

Mode(log_feature)
pivot_ci_median
npivot_ci_median


####################################################################################################
##########################                 GROUPPING              ##################################
####################################################################################################

weekend.indices <- as.logical(online.news.popularity$weekday_is_sunday + online.news.popularity$weekday_is_saturday)
weekend_feature <- log_feature[weekend.indices]
workday_feature <- log_feature[!weekend.indices]

length(weekend_feature)
length(workday_feature)
mean(weekend_feature)
mean(workday_feature)

N = 2000
m = 200
alpha = 0.05

pivot_ci_weekend <- get_pivot_bs_ci(N, m, weekend_feature, alpha)
npivot_ci_weekend <- get_nonpivot_bs_ci(N, m, weekend_feature, alpha)
pivot_ci_workday <- get_pivot_bs_ci(N, m, workday_feature, alpha)
npivot_ci_workday <- get_nonpivot_bs_ci(N, m, workday_feature, alpha)


pivot_ci_weekend
pivot_ci_workday

npivot_ci_weekend
npivot_ci_workday













