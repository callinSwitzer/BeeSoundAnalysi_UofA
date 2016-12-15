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
sp1 <- spectro(cutw(w2, from = 1, to = 2, f = w2@samp.rate), f = w2@samp.rate, ovlp = 90,
               osc = TRUE,
               palette = colorRampPalette(c("white", "black")))



vv <- ggspectro(cutw(w2, from = 1, to = 3, f = w2@samp.rate), f = w2@samp.rate) + 
     stat_contour(geom="polygon", aes(fill=..level..), bins=30) + 
     scale_fill_continuous(name="Amplitude\n(dB)\n", limits=c(-30,0),
                           na.value="transparent", low="white", high="black") + 
     theme_classic()
vv


fund(w2, fmax = 1000, ylim = c(0, 1))



ggplot(sp1, aes(x = time, y = freq, color = amp)) + 
     geom_point()
