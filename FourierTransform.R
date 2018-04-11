
library(seewave)

pdf("/Users/cswitzer/Dropbox/dataAnalysisForOthers/SoundAnalysisforSteve/FourierExample.pdf", width = 6.5, height = 8)

sampFreq = 44100

time_s = seq(0, 0.1, length.out = sampFreq / 10)
y_val = 4 * sin(2 * pi * time_s * 200) + 2*sin(2*pi * time_s * 2500) + 1*sin(2*pi * time_s * 5500)

par(mfrow= c(4,1))

plot(time_s, y_val, type = "l", main = "wave for analysis", bty = 'n')



# plot spectrum
# note multiply y-val by 2 to get correct amplitude in spectrum
spp <- spec(y_val*2, f = sampFreq, scaled = TRUE, fftw = TRUE, PSD = FALSE, wl = 1024, main = "FFT spectrum", plot = FALSE, norm = FALSE)

spectrum = data.frame(unlist(spp))

linearSpectrum = spectrum

xx <- c(linearSpectrum$x * 1000, rev(linearSpectrum$x * 1000)) #  # convert from kHz to Hz
# calculate dB, with 0 being the max
dB = linearSpectrum$y
yy <- c(rep(min(dB), length(dB)), rev(dB))

plot(xx[xx > 99], yy[xx > 99], type = 'l', ylab = "Absolute amplitude", xlab = "Frequency (Hz)", bty = "n", log = "x", cex.lab=1.1, xlim = c(100, 22050))
polygon(x = xx, y = yy, col='black', border=NA)
abline(h = c(4,2,1), lty = 2)


plot(xx[xx > 99], yy[xx > 99] / max(yy[xx > 99]), type = 'l', ylab = "Relative amplitude (linear scale)", xlab = "Frequency (Hz)", bty = "n", log = "x", cex.lab=1.1, xlim = c(100, 22050))
polygon(x = xx, y = yy / max(yy[xx > 99]), col='black', border=NA)
abline(h = c(4,2,1)/4, lty = 2)


xx <- c(spectrum$x * 1000, rev(spectrum$x * 1000))
ref = max(spectrum$y)
# calculate dB, with 0 being the max
dB = 20* log10(spectrum$y / ref)
yy <- c(rep(min(dB), length(dB)), rev(dB))

plot(xx, yy, type = 'l', ylab = "Relative amplitude (dB)", xlab = "Frequency (Hz)", bty = "n", log = "x", ylim = c(-20, 0), cex.lab=1.1, xlim = c(100, 22050))
polygon(x = xx, y = yy, col='black', border=NA)
abline(h = c(0, -6, -12), lty = 2)

dev.off()
