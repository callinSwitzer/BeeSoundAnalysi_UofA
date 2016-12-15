# Callin Switzer
# 15 Dec 2016
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

packages <- c("tuneR", "ggplot2", "compiler", "manipulate", "shiny", "seewave", "viridis")
ipak(packages)

setwd("~/Documents/GitRepos/BeeSoundAnalysis_UofA")


# set the audio player -- mac specific 
setWavPlayer("afplay")


wavFle = "/Users/callinswitzer/Dropbox/dataAnalysisForOthers/SoundAnalysisforSteve/SLB_pair1_cut.wav"

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
oscillo(w2, from = 0, to = 2)


# view spectrogram of a small portion of the recording
# make figure 1
pdf("SpectrogamAndOscillogram.pdf", width = 8, height = 5)
sp1 <- spectro(cutw(w2, from = 1, to = 2, f = w2@samp.rate), f = w2@samp.rate, 
               wl = 512,
               wn = "hanning", 
               ovlp = 50,
               osc = TRUE,
               palette = colorRampPalette(c("white", "black")))
dev.off()



# same thing, but can't put oscillogram below
vv <- ggspectro(cutw(w2, from = 1, to = 3, f = w2@samp.rate), f = w2@samp.rate) + 
     stat_contour(geom="polygon", aes(fill=..level..), bins=30) + 
     scale_fill_continuous(name="Amplitude\n(dB)\n", limits=c(-30,0),
                           na.value="transparent", low="white", high="black") + 
     theme_classic()
vv

## fft and oscillogram
## make figure 2
pdf("WaveAndFFT.pdf", width = 8, height = 6)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
oscillo(cutw(w2, from = 1, to = 2, f = w2@samp.rate), f = w2@samp.rate, title = 'Oscillogram of 1 second of sound')
rect(0.11, -10000000,  0.18, 1000000, border = FALSE, col = rgb(0,0,0, 0.5) )


w3 <- cutw(w2, from = 1.11, to = 1.18, f = w2@samp.rate)
oscillo(w3, f = w2@samp.rate, title = 'Zoomed section of sound from above')
rect(0, -10000000,  0.07, 1000000, border = FALSE, col = rgb(0,0,0, 0.5) )

listen(w3, f = w2@samp.rate)

# plot spectrum
freqRangeOfInterest <- c(0, 1)

spp <- spec(w3, f = w2@samp.rate, PSD = TRUE, flim = freqRangeOfInterest, wl = 512, main = "FFT spectrum of zoomed section")

# filter to less than 400 Hz and greater than 195Hz
sppFilt <- spp[spp[,1] < freqRangeOfInterest[2] & spp[,1] > freqRangeOfInterest[1],] 

# identify peaks in this spectrum
peaks <- which(diff(sign(diff(sppFilt[,2])))==-2)+1
sppPks <- sppFilt[peaks,]
# show top 5 highest peaks
(tp5 <- sppPks[order(sppPks[,2], decreasing = T)[1:5],])
points(x = tp5[1,1], y = tp5[1,2], col = 'red')
text(x = tp5[1,1], y = tp5[1,2], labels = paste(round(tp5[1,1],digits = 3)*1000, "Hz"), adj = -.3)
dev.off()

# listen to the sine waves at those frequencies for comparison
# listen(r, f = f, from = ret[[1]], to = ret[[2]] )
pkNum <- 1
listen(sine(tp5[pkNum,1]*1000, duration = 10000))
listen(w3, f = w2@samp.rate)


#####  here is where I'm starting
# to try to automatically detect sounds in the recording, and 
# then I'll be able to take an "average" fft for all of the buzzes

# get times in recording when there are sounds


str(sp1)
length(sp1$freq)
dim(sp1$amp)
length(sp1$time)
length(sp1$amp)

css <- colSums((sp1$amp))
length(css)
par(mfrow = c(2,1))
plot(css, type = 'l')
oscillo(cutw(w2, from = 1, to = 2, f = w2@samp.rate), f = w2@samp.rate)
plot(cutw(w2, from = 1, to = 2, f = w2@samp.rate), type = 'l')
lines(css + 10000, x = sp1$time, type = 'l', col = 'red')
abline(h = )

amps <- t(abs(sp1$amp))
dim(amps)
ncol(amps)
amps[10:20, 60:70] <- 1000
image(amps)
summary(amps)

sounds <- colSums(amps[, 50:100])
plot(sounds, type = 'l')

image(t(sp1$amp))


