# Callin Switzer
# 15 Dec 2016
# update 16 Dec
# update 9 April 2018
# Processing the sounds of mating bees

# Make an oscillogram in R (show time vs. sound)
# 
# Make spectrogram (shows frequency vs. time)
# 
# Make a zoomed-in view of a portion of the sound


# install packages
ipak <- function(pkg){
     new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
     if(length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
     sapply(pkg, require, character.only = TRUE)
}

packages <- c("tuneR", "seewave")
ipak(packages)

setwd("~/Documents/GitRepos/BeeSoundAnalysis_UofA")


# set the audio player -- mac specific 
setWavPlayer("afplay")


wavFle = "~/Dropbox/dataAnalysisForOthers/SoundAnalysisforSteve/SLB_cooler5.wav"

# load sound
w1 <- readWave(wavFle)

# view oscillogram chunks for each channel (left and right)
oscillo(w1@right, f = w1@samp.rate, from = 0, to = 2)
oscillo(w1@left, f = w1@samp.rate, from = 1, to = 2)

# listen to sound (plays in only one headphone, by default)
listen(w1)

# convert to mono by combining left and right
w2 <- mono(w1, which = "both")


# view oscillogram of mono wave
oscillo(w2, from = 1, to = 2)

op = par()
op$mai

# redo fig 2

# view spectrogram of a small portion of the recording
# make figure 1
tiff("~/Dropbox/dataAnalysisForOthers/SoundAnalysisforSteve/Fig2_largeFonts.tiff",width = 8, height = 5, units = 'in', res = 1200)


sp1 <- spectro(cutw(w2, from = 1, to = 2, f = w2@samp.rate), f = w2@samp.rate, dB = "D",
               wl = 512, cexaxis = 1.2, cexlab = 1.2, scalecexlab = 1.2, oma = c(1,1,1,1),
               wn = "hanning", 
               ovlp = 10,
               osc = TRUE,
               palette = colorRampPalette(c("grey70", "black")))
# annotate
mtext("*", at = c(0.048, 0.248, 0.45, 0.66, 0.883), cex = 1.7, line = -2.5)
mtext("†", at = c(0.135, 0.34, 0.39, 0.55, 0.76, 0.815, 0.99), cex = 0.9, line = -1.8)

dev.off()



## _____________________________________

# redo fig 3

tiff("~/Dropbox/dataAnalysisForOthers/SoundAnalysisforSteve/Fig3_largeFonts.tiff",width = 6.5, height = 8, units = 'in', res = 1200)
{
aa = cutw(w2, from = 1, to = 2, f = w2@samp.rate)
layout(matrix(c(1,1,2), nrow = 3, ncol = 1, byrow = TRUE))
par(mai = c(0,0,0,0))
plot(y = aa / max(aa), x = seq(0, 1, length.out = length(aa)), type = 'l', bty = "n", ylim = c(-4.5, 1), 
     yaxt = "n", xlab = "", ylab = "", ann = FALSE, xaxt = "n"
     )
# add scale
arrows(x0 = 0.8, y0 = -1.1, x1 = 1, y1 = -1.1, lwd = 1.2, length = 0.05, code = 3)
text(0.9, -1.15, "0.2 s", adj = c(0.5, 1.3), cex = 1.5)

# add line segments
x0 = (1.435 - 1) 
x1 = (1.515 - 1) 
y0 = -0.3
y1 = -0.9
segments(x0, y0, x1 = x0, y1, lwd = 1.2)
segments(x0 = x1, y0, x1 = x1, y1, lwd = 1.2)

# add arrows
arrows(x0, y0 = y1, x1 = 0, y1 = y1 - 1, lwd = 1.2, length = 0.05)
arrows(x0 = x1, y0 = y1, x1 = 1, y1 = y1 - 1, lwd = 1.2, length = 0.05)

# add next layer
bb = cutw(w2, from = x0 + 1, to = x1 + 1, f = w2@samp.rate)
lines(bb/ max(bb) - 3,  x = seq(0, 1, length.out = length(bb)))

# add scale
arrows(x0 = 0.8, y0 = -4.5+0.3, x1 = 1, y1 = -4.5 + 0.3, lwd = 1.2, length = 0.05, code = 3)
text(0.9, -4.55+0.3, "0.014 s", adj = c(0.5, 1.3), cex = 1.5)

# add letters
text('A', x= 0, y = 1, cex = 2)
text('B', x= 0, y = -1.6, cex = 2)


# plot spectrum
spp <- spec((bb/max(bb)), f = w2@samp.rate, scaled = TRUE, fftw = TRUE, PSD = FALSE, wl = 1024, main = "FFT spectrum of zoomed section", plot = FALSE, norm = FALSE)
par(mai = c(0.6,0.7,0,0))

spectrum = data.frame(unlist(spp))
xx <- c(spectrum$x * 1000, rev(spectrum$x * 1000))
ref = max(spectrum$y)
# calculate dB, with 0 being the max
dB = 20* log10(spectrum$y / ref)
yy <- c(rep(min(dB), length(dB)), rev(dB))

plot(xx, yy, type = 'l', ylab = "Relative amplitude (dB)", xlab = "Frequency (Hz)", 
     bty = "n", log = "x", ylim = c(-50, 0), cex.axis = 1.5, cex.lab = 1.5, xlim = c(100, 22050))
polygon(x = xx, y = yy, col='black', border=NA)

# add text
text(round(xx[yy == max(yy)]), y = 0, labels = paste(round(xx[yy == max(yy)]), "Hz", sep  = " "), adj = c(-0.2, 0.5), cex = 1.5)

mtext('C', at = c(51, 15), cex = 2 * 2/3)



}
dev.off()



tiff("~/Dropbox/dataAnalysisForOthers/SoundAnalysisforSteve/Fig3_no_dB_largeFonts.tiff",width = 6.5, height = 8, units = 'in', res = 1200)
{
     aa = cutw(w2, from = 1, to = 2, f = w2@samp.rate)
     layout(matrix(c(1,1,2), nrow = 3, ncol = 1, byrow = TRUE))
     par(mai = c(0,0,0,0))
     plot(y = aa / max(aa), x = seq(0, 1, length.out = length(aa)), type = 'l', bty = "n", ylim = c(-4.5, 1), 
          yaxt = "n", xlab = "", ylab = "", ann = FALSE, xaxt = "n"
     )
     # add scale
     arrows(x0 = 0.8, y0 = -1.1, x1 = 1, y1 = -1.1, lwd = 1.2, length = 0.05, code = 3)
     text(0.9, -1.15, "0.2 s", adj = c(0.5, 1.3), cex = 1.5)
     
     # add line segments
     x0 = (1.435 - 1) 
     x1 = (1.515 - 1) 
     y0 = -0.3
     y1 = -0.9
     segments(x0, y0, x1 = x0, y1, lwd = 1.2)
     segments(x0 = x1, y0, x1 = x1, y1, lwd = 1.2)
     
     # add arrows
     arrows(x0, y0 = y1, x1 = 0, y1 = y1 - 1, lwd = 1.2, length = 0.05)
     arrows(x0 = x1, y0 = y1, x1 = 1, y1 = y1 - 1, lwd = 1.2, length = 0.05)
     
     # add next layer
     bb = cutw(w2, from = x0 + 1, to = x1 + 1, f = w2@samp.rate)
     lines(bb/ max(bb) - 3,  x = seq(0, 1, length.out = length(bb)))
     
     # add scale
     arrows(x0 = 0.8, y0 = -4.5+0.3, x1 = 1, y1 = -4.5 + 0.3, lwd = 1.2, length = 0.05, code = 3)
     text(0.9, -4.55+0.3, "0.014 s", adj = c(0.5, 1.3), cex = 1.5)
     
     # add letters
     text('A', x= 0, y = 1, cex = 2)
     text('B', x= 0, y = -1.6, cex = 2)
     
     
     # plot spectrum
     spp <- spec((bb/max(bb)), f = w2@samp.rate, scaled = TRUE, fftw = TRUE, PSD = FALSE, wl = 1024, main = "FFT spectrum of zoomed section", plot = FALSE, norm = FALSE)
     par(mai = c(0.6,0.7,0,0))
     
     spectrum = data.frame(unlist(spp))
     xx <- c(spectrum$x * 1000, rev(spectrum$x * 1000))
     ref = max(spectrum$y)
     # calculate dB, with 0 being the max
     dB = spectrum$y/max(spectrum$y)
     yy <- c(rep(min(dB), length(dB)), rev(dB))
     
     yy = yy[xx >= 99]
     xx = xx[xx>=99]
     
     plot(xx, yy, type = 'l', ylab = "Relative amplitude", xlab = "Frequency (Hz)", 
          bty = "n", log = "x", cex.lab=1.5, cex.axis = 1.5, xlim = c(100, 22050), yaxt="n")
     axis(side = 2, at = c(0,1), cex.axis = 1.5, cex.lab = 1.5)
     polygon(x = xx, y = yy, col='black', border=NA)
     
     # add text
     text(round(xx[yy == max(yy)]), y = 1, labels = paste(round(xx[yy == max(yy)]), "Hz", sep  = " "), adj = c(-0.2, 0.5), cex = 1.5)
     
     mtext('C', at = c(51, 15), cex = 2 * 2/3)
     
     
     
}
dev.off()



# find max 
xx[which.max(yy)]



xx2 <- xx[xx > 600 & xx < 1000]
yy2 <- yy[xx > 600 & xx < 1000]

xx2[which.max(yy2)]

points(xx2[which.max(yy2)], yy2[which.max(yy2)], col = 'red')
points(xx[which.max(yy)], yy[which.max(yy)], col = 'red')
xx[which.max(yy)]

