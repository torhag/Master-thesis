# Set working directory
setwd("N:/durable/Students/Corona research project/Rådata og metadata")
# Read in FFT dataset
fft <- data.table::fread("FFT.dat", header = FALSE, na.strings = "-999")
# Retain used variables only
fft <- fft[, c(1, 4, 8:10, 15:16, 19:20, 26, 35, 41, 50, 56, 62, 68, 74:75)]
# Assign names to columns
names(fft) <- c(
    "agecoh", "age", "immi1", "immi2", "female",
    "prior", "insti", "foster", "psyhlth",
    "innsum", "innylss", "outsum", "outylss",
    "sum6", "sum12", "sum18", "chgsum", "chgyls"
)

#########################
# Study imperfect cases #
#########################

# Retain only complete cases
fft_complete <- fft[complete.cases(fft)]
# How many cases contain missing data?
(n_missing <- nrow(fft) - nrow(fft_complete))
# Percentage of missing cases relative to total
(percent_missing <- n_missing / nrow(fft) * 100)

#################################################
# ANOVA for group differences at admission (T0) #
#################################################

# Turn binary variables into factors
fft$agecoh <- as.factor(fft$agecoh)
# Create a placeholder array
temp <- data.frame(matrix(NA, nrow = 10, ncol = 4))
# Assign column names
names(temp) <- c("df_1", "df_2", "F-stat", "p-value")
# Assign row names
row.names(temp) <- c(
    "innsum", "innylss",
    "age", "female", "immi1", "immi2",
    "prior", "insti", "foster", "psyhlth"
)

# Outcome variables
temp[1, 1] <- unlist(summary(aov(innsum ~ agecoh, data = fft)))[1]
temp[1, 2] <- unlist(summary(aov(innsum ~ agecoh, data = fft)))[2]
temp[1, 3] <- unlist(summary(aov(innsum ~ agecoh, data = fft)))[7]
temp[1, 4] <- unlist(summary(aov(innsum ~ agecoh, data = fft)))[9]

temp[2, 1] <- unlist(summary(aov(innylss ~ agecoh, data = fft)))[1]
temp[2, 2] <- unlist(summary(aov(innylss ~ agecoh, data = fft)))[2]
temp[2, 3] <- unlist(summary(aov(innylss ~ agecoh, data = fft)))[7]
temp[2, 4] <- unlist(summary(aov(innylss ~ agecoh, data = fft)))[9]

# Demographics
temp[3, 1] <- unlist(summary(aov(age ~ agecoh, data = fft)))[1]
temp[3, 2] <- unlist(summary(aov(age ~ agecoh, data = fft)))[2]
temp[3, 3] <- unlist(summary(aov(age ~ agecoh, data = fft)))[7]
temp[3, 4] <- unlist(summary(aov(age ~ agecoh, data = fft)))[9]

temp[4, 1] <- unlist(summary(aov(female ~ agecoh, data = fft)))[1]
temp[4, 2] <- unlist(summary(aov(female ~ agecoh, data = fft)))[2]
temp[4, 3] <- unlist(summary(aov(female ~ agecoh, data = fft)))[7]
temp[4, 4] <- unlist(summary(aov(female ~ agecoh, data = fft)))[9]

temp[5, 1] <- unlist(summary(aov(immi1 ~ agecoh, data = fft)))[1]
temp[5, 2] <- unlist(summary(aov(immi1 ~ agecoh, data = fft)))[2]
temp[5, 3] <- unlist(summary(aov(immi1 ~ agecoh, data = fft)))[7]
temp[5, 4] <- unlist(summary(aov(immi1 ~ agecoh, data = fft)))[9]

temp[6, 1] <- unlist(summary(aov(immi2 ~ agecoh, data = fft)))[1]
temp[6, 2] <- unlist(summary(aov(immi2 ~ agecoh, data = fft)))[2]
temp[6, 3] <- unlist(summary(aov(immi2 ~ agecoh, data = fft)))[7]
temp[6, 4] <- unlist(summary(aov(immi2 ~ agecoh, data = fft)))[9]

# Treatment variables
temp[7, 1] <- unlist(summary(aov(prior ~ agecoh, data = fft)))[1]
temp[7, 2] <- unlist(summary(aov(prior ~ agecoh, data = fft)))[2]
temp[7, 3] <- unlist(summary(aov(prior ~ agecoh, data = fft)))[7]
temp[7, 4] <- unlist(summary(aov(prior ~ agecoh, data = fft)))[9]

temp[8, 1] <- unlist(summary(aov(insti ~ agecoh, data = fft)))[1]
temp[8, 2] <- unlist(summary(aov(insti ~ agecoh, data = fft)))[2]
temp[8, 3] <- unlist(summary(aov(insti ~ agecoh, data = fft)))[7]
temp[8, 4] <- unlist(summary(aov(insti ~ agecoh, data = fft)))[9]

temp[9, 1] <- unlist(summary(aov(foster ~ agecoh, data = fft)))[1]
temp[9, 2] <- unlist(summary(aov(foster ~ agecoh, data = fft)))[2]
temp[9, 3] <- unlist(summary(aov(foster ~ agecoh, data = fft)))[7]
temp[9, 4] <- unlist(summary(aov(foster ~ agecoh, data = fft)))[9]

temp[10, 1] <- unlist(summary(aov(psyhlth ~ agecoh, data = fft)))[1]
temp[10, 2] <- unlist(summary(aov(psyhlth ~ agecoh, data = fft)))[2]
temp[10, 3] <- unlist(summary(aov(psyhlth ~ agecoh, data = fft)))[7]
temp[10, 4] <- unlist(summary(aov(psyhlth ~ agecoh, data = fft)))[9]

# Round F-statistics and p-values
temp[, 3] <- round(temp[, 3], 2)
temp[, 4] <- round(temp[, 4], 3)
# Display summary table
temp

###############################
# Plot effectiveness measures #
###############################

# Frequency tables for behavioural chagnes
table(unlist(fft$chgsum)) # Full sample
table(unlist(fft$chgsum[which(fft$agecoh == 1)])) # Before
table(unlist(fft$chgsum[which(fft$agecoh == 2)])) # During
table(unlist(fft$chgsum[which(fft$agecoh == 3)])) # After

# Frequency tables for risk reduction
table(unlist(fft$chgyls)) # Full sample
table(unlist(fft$chgyls[which(fft$agecoh == 1)])) # Before
table(unlist(fft$chgyls[which(fft$agecoh == 2)])) # During
table(unlist(fft$chgyls[which(fft$agecoh == 3)])) # After

# Activate package for mimicking distribution by moments
library(PearsonDS)

pdf("FFT_effectiveness.pdf", paper = "a4")
# Reset canvas to 3-row by 2-column. Plot top-down, then left-right
par(mfcol = c(3, 2))
# Ret common scales
sum_common_x <- c(-3, 5)
sum_common_break <- 8
x_seq <- seq(min(sum_common_x), max(sum_common_x), 0.1)

hist(fft$chgsum[which(fft$agecoh == 1)],
    xlim = sum_common_x,
    breaks = sum_common_break,
    freq = FALSE,
    xlab = "Before", main = "FFT Increase in National Outcome Goals"
)
# Red zero line
abline(v = 0, col = "red")
# Blue distribution curve
# Superimpose on histogram
par(new = TRUE)
# Extract "before"
before <- fft$chgsum[which(fft$agecoh == 1)]
# Discard NAs in "before"
before <- before[!is.na(before)]
plot(
    # Series of x
    x_seq,
    # Series of y
    dpearson(
        x_seq,
        moments = empMoments(before)
    ),
    xlab = "", ylab = "", axes = FALSE, # Turn off labels and axes
    type = "l", col = "blue" # Line style and colour
)

hist(fft$chgsum[which(fft$agecoh == 2)],
    xlim = sum_common_x,
    breaks = sum_common_break, freq = FALSE,
    xlab = "During", main = ""
)
abline(v = 0, col = "red")
par(new = TRUE)
during <- fft$chgsum[which(fft$agecoh == 2)]
during <- during[!is.na(during)]
plot(
    x_seq,
    dpearson(
        x_seq,
        moments = empMoments(during)
    ),
    xlab = "", ylab = "",
    type = "l", col = "blue", axes = FALSE
)

hist(fft$chgsum[which(fft$agecoh == 3)],
    xlim = sum_common_x,
    breaks = sum_common_break, freq = FALSE,
    xlab = "After", main = ""
)
abline(v = 0, col = "red")
par(new = TRUE)
after <- fft$chgsum[which(fft$agecoh == 3)]
after <- after[!is.na(after)]
plot(
    x_seq,
    dpearson(
        x_seq,
        moments = empMoments(after)
    ),
    xlab = "", ylab = "",
    type = "l", col = "blue", axes = FALSE
)

yls_common_x <- c(-10, 30)
yls_common_break <- 20
x_seq <- seq(min(yls_common_x), max(yls_common_x), 0.1)

hist(fft$chgyls[which(fft$agecoh == 1)],
    xlim = yls_common_x,
    breaks = yls_common_break, freq = FALSE,
    xlab = "Before", main = "FFT Reduction in Risk Levels"
)
abline(v = 0, col = "red")
par(new = TRUE)
before <- fft$chgyls[which(fft$agecoh == 1)]
before <- before[!is.na(before)]
plot(
    x_seq,
    dpearson(
        x_seq,
        moments = empMoments(before)
    ),
    xlab = "", ylab = "",
    type = "l", col = "blue", axes = FALSE
)
hist(fft$chgyls[which(fft$agecoh == 2)],
    xlim = yls_common_x,
    breaks = yls_common_break, freq = FALSE,
    xlab = "During", main = ""
)
abline(v = 0, col = "red")
par(new = TRUE)
during <- fft$chgyls[which(fft$agecoh == 2)]
during <- during[!is.na(during)]
plot(
    x_seq,
    dpearson(
        x_seq,
        moments = empMoments(during)
    ),
    xlab = "", ylab = "",
    type = "l", col = "blue", axes = FALSE
)
hist(fft$chgyls[which(fft$agecoh == 3)],
    xlim = yls_common_x,
    breaks = yls_common_break, freq = FALSE,
    xlab = "After", main = ""
)
abline(v = 0, col = "red")
par(new = TRUE)
after <- fft$chgyls[which(fft$agecoh == 3)]
after <- after[!is.na(after)]
plot(
    x_seq,
    dpearson(
        x_seq,
        moments = empMoments(after)
    ),
    xlab = "", ylab = "",
    type = "l", col = "blue", axes = FALSE
)
# Restore canvas
par(mfcol = c(1, 1))
dev.off()
