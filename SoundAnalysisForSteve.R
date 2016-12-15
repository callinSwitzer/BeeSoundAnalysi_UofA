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

packages <- c("tuneR", "ggplot2", "compiler", "manipulate", "shiny", "seewave", "plyr", "reshape2")
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
w4 <- cutw(w2, from = 0, to = 5, f = w2@samp.rate)
sp2 <- spectro(w4, f = w2@samp.rate, plot = FALSE,
               wl = 512,
               wn = "hanning", 
               ovlp = 50,
               osc = TRUE,
               palette = colorRampPalette(c("white", "black")))
str(sp2)
length(sp2$freq)
dim(sp2$amp)
length(sp2$time)
length(sp2$amp)

css <- scale(colSums((sp2$amp[50:256, ])), scale = TRUE) # sums up noises above 4 kHz
css <- css - min(css)
length(css)
par(mfrow = c(2,1))
plot(css, x = sp2$time, type = 'l')
abline(h = 1, col = 'red')
plot(css > 1, x = sp2$time, type = 'l')

# find times
ff <- rle(as.numeric(css > 1))

ndf <- data.frame(css, css > 1, sp2$time, startStop = NA)
ndf$startStop[cumsum(ff$lengths)] <- "event"
ndf$startStop[nrow(ndf)] <- NA
ndf$ststp <- sapply(1:nrow(ndf), function(x){
     if(is.na(ndf$startStop[x])) {NA}
     else if(ndf$startStop[x] == "event" & ndf$css...1[x] == TRUE) {"stop"}
     else if(ndf$startStop[x] == "event" & ndf$css...1[x] == FALSE) {"start"}
          })

stopInd <- which(ndf$ststp == "stop")
# ndf$ststp[stopInd + 1] <- "stop"
# ndf$ststp[stopInd] <- NA
ndf <- ndf[!is.na(ndf$ststp),c(3,5) ]


{if(ndf$ststp[1] == "stop"){
     ndf <- ndf[-1, ]
}}

{if(ndf$ststp[nrow(ndf)] == "start"){
     ndf <- ndf[-nrow(ndf), ]
}}

dev.off()
plot(css, x = sp2$time, type = 'l')

rect(xleft = ndf$sp2.time[ndf$ststp == "start"], ybottom = -1,
     xright = ndf$sp2.time[ndf$ststp == "stop"], ytop = 5, col = rgb(0,0,0, 0.5) 
     )
abline(v = ndf$sp2.time[ndf$ststp == "start"], col = 'green')
abline(v = ndf$sp2.time[ndf$ststp == "stop"], col = 'red')


ndf$event = rep(1:(nrow(ndf)/ 2), each =2)

ndf2 <- dcast(ndf,  event~ststp, value.var="sp2.time")
ndf2$leng <- ndf2$stop - ndf2$start
ndf2
hist(ndf2$leng)

ndf2$longbuzz <- ndf2$leng > 0.05

lb_short <- ndf2[ndf2$longbuzz,]

ndf3 <- melt(ndf2, 
             id.vars = c("event", "longbuzz"), 
             measure.vars = c("start", "stop"))

ndf3 <- ndf3[order(ndf3$event), ]

longBuzz <- ndf3[ndf3$longbuzz == TRUE,]

shortClick <-  ndf3[ndf3$longbuzz == FALSE,]


plot(y = scale(w4, scale = TRUE, center = FALSE), xlab = "time (s)", ylab = "Amplitude",
     x = seq(0, to = length(w4) / w2@samp.rate, length.out = length(w4)), 
     type = 'l')
rect(xleft = longBuzz$value[longBuzz$variable == "start"], ybottom = -10, border = FALSE,
     xright = longBuzz$value[longBuzz$variable == "stop"], ytop = 10, col = rgb(0,0,0, 0.5) 
)


# calculate fft for all long buzzes


# freqs
dev.off()
pdf("figLongBuzz_fullSpectrum.pdf", width = 100, height = 10)
layout(matrix(c(rep(1, nrow(lb_short)), 2:(nrow(lb_short) + 1)), 2, ncol = nrow(lb_short), byrow = TRUE))
plot(y = scale(w4, scale = TRUE, center = FALSE), xlab = "time (s)", ylab = "Amplitude",
     x = seq(0, to = length(w4) / w2@samp.rate, length.out = length(w4)), 
     type = 'l')
rect(xleft = longBuzz$value[longBuzz$variable == "start"], ybottom = -10, border = FALSE,
     xright = longBuzz$value[longBuzz$variable == "stop"], ytop = 10, col = rgb(0,0,0, 0.5) 
)


lb_short$freqs <- NA
for(ii in 1:nrow(lb_short)){
     w3 <- cutw(w2, from = lb_short$start[ii], to =  lb_short$stop[ii], f = w2@samp.rate)
     # oscillo(w3, f = w2@samp.rate, title = 'Zoomed section of sound from above')
     # 
      # listen(w3, f = w2@samp.rate)
    
     # plot spectrum
     freqRangeOfInterest <- c(0, 20)
     
     spp <- spec(w3, f = w2@samp.rate, PSD = TRUE, flim = freqRangeOfInterest, wl = 512)
     
     # filter to less than 400 Hz and greater than 195Hz
     sppFilt <- spp[spp[,1] < freqRangeOfInterest[2] & spp[,1] > freqRangeOfInterest[1],] 
     
     # identify peaks in this spectrum
     peaks <- which(diff(sign(diff(sppFilt[,2])))==-2)+1
     sppPks <- sppFilt[peaks,]
     # show top 5 highest peaks
     (tp5 <- sppPks[order(sppPks[,2], decreasing = T)[1:5],])
     points(x = tp5[,1], y = tp5[,2], col = 'red')
     text(x = tp5[,1], y = tp5[,2], labels = paste(round(tp5[,1],digits = 3)*1000, "Hz"), adj = -.3)
     lb_short$freqs[ii] <- tp5[1,1]
     # Sys.sleep(0.1)
     
}
dev.off()
hist(lb_short$freqs)
mean(lb_short$freqs)
listen(sine(mean(lb_short$freqs)*1000))




# freqs
dev.off()
pdf("figLongBuzz_ReducedSpectrum.pdf", width = 100, height = 10)
layout(matrix(c(rep(1, nrow(lb_short)), 2:(nrow(lb_short) + 1)), 2, ncol = nrow(lb_short), byrow = TRUE))
plot(y = scale(w4, scale = TRUE, center = FALSE), xlab = "time (s)", ylab = "Amplitude",
     x = seq(0, to = length(w4) / w2@samp.rate, length.out = length(w4)), 
     type = 'l')
rect(xleft = longBuzz$value[longBuzz$variable == "start"], ybottom = -10, border = FALSE,
     xright = longBuzz$value[longBuzz$variable == "stop"], ytop = 10, col = rgb(0,0,0, 0.5) 
)


lb_short$freqs <- NA
for(ii in 1:nrow(lb_short)){
     w3 <- cutw(w2, from = lb_short$start[ii], to =  lb_short$stop[ii], f = w2@samp.rate)
     # oscillo(w3, f = w2@samp.rate, title = 'Zoomed section of sound from above')
     # 
     # listen(w3, f = w2@samp.rate)
     
     # plot spectrum
     freqRangeOfInterest <- c(0.2, 1)
     
     spp <- spec(w3, f = w2@samp.rate, PSD = TRUE, flim = freqRangeOfInterest, wl = 512)
     
     # filter to less than 400 Hz and greater than 195Hz
     sppFilt <- spp[spp[,1] < freqRangeOfInterest[2] & spp[,1] > freqRangeOfInterest[1],] 
     
     # identify peaks in this spectrum
     peaks <- which(diff(sign(diff(sppFilt[,2])))==-2)+1
     sppPks <- sppFilt[peaks,]
     # show top 5 highest peaks
     (tp5 <- sppPks[order(sppPks[,2], decreasing = T)[1:5],])
     points(x = tp5[,1], y = tp5[,2], col = 'red')
     text(x = tp5[,1], y = tp5[,2], labels = paste(round(tp5[,1],digits = 3)*1000, "Hz"), adj = -.3)
     lb_short$freqs[ii] <- tp5[1,1]
     # Sys.sleep(0.1)
     
}
dev.off()
hist(lb_short$freqs)
mean(lb_short$freqs)
listen(sine(mean(lb_short$freqs)*1000))



##################################
## SHORT CLICKS
plot(y = scale(w4, scale = TRUE, center = FALSE), xlab = "time (s)", ylab = "Amplitude",
     x = seq(0, to = length(w4) / w2@samp.rate, length.out = length(w4)), 
     type = 'l')
rect(xleft = shortClick$value[shortClick$variable == "start"], ybottom = -10, border = FALSE,
     xright = shortClick$value[shortClick$variable == "stop"], ytop = 10, col = rgb(0,0,0, 0.5) 
)


# calculate fft for all short clicks
sc_short <-  ndf2[!ndf2$longbuzz,]

# freqs

dev.off()
pdf("fig_ShortClicks.pdf", width = 100, height = 10)
layout(matrix(c(rep(1, nrow(sc_short)), 2:(nrow(sc_short) + 1)), 2, ncol = nrow(sc_short), byrow = TRUE))
plot(y = scale(w4, scale = TRUE, center = FALSE), xlab = "time (s)", ylab = "Amplitude",
     x = seq(0, to = length(w4) / w2@samp.rate, length.out = length(w4)), 
     type = 'l')
rect(xleft = shortClick$value[shortClick$variable == "start"], ybottom = -10, border = FALSE,
     xright = shortClick$value[shortClick$variable == "stop"], ytop = 10, col = rgb(0,0,0, 0.5) 
)


sc_short$freqs <- NA
for(ii in 1:nrow(sc_short)){
     w3 <- cutw(w2, from = sc_short$start[ii], to =  sc_short$stop[ii], f = w2@samp.rate)
     # oscillo(w3, f = w2@samp.rate, title = 'Zoomed section of sound from above')

     # listen(w3, f = w2@samp.rate)
     
     # plot spectrum
     freqRangeOfInterest <- c(0, 20)
     
     spp <- spec(w3, f = w2@samp.rate, PSD = TRUE, flim = freqRangeOfInterest, wl = 512)
     
     # filter to less than 400 Hz and greater than 195Hz
     sppFilt <- spp[spp[,1] < freqRangeOfInterest[2] & spp[,1] > freqRangeOfInterest[1],] 
     
     # identify peaks in this spectrum
     peaks <- which(diff(sign(diff(sppFilt[,2])))==-2)+1
     sppPks <- sppFilt[peaks,]
     # show top 5 highest peaks
     (tp5 <- sppPks[order(sppPks[,2], decreasing = T)[1:5],])
     points(x = tp5[,1], y = tp5[,2], col = 'red')
     text(x = tp5[,1], y = tp5[,2], labels = paste(round(tp5[,1],digits = 3)*1000, "Hz"), adj = -.3)
     sc_short$freqs[ii] <- tp5[1,1]
     #Sys.sleep(0.1)
     
}
dev.off()
