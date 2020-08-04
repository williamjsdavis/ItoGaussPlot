# ItoGaussPlot
An example of the calculation and displaying of Ito-Gauss plots. Calculations are a measure of the variability of linear trends <img src="https://render.githubusercontent.com/render/math?math=B"> in a time-series observation, as a function of windowing length, <img src="https://render.githubusercontent.com/render/math?math=w">.

<img src="https://render.githubusercontent.com/render/math?math=x(t)=A+Bt">
![equation](https://latex.codecogs.com/gif.latex?x(t)=A&plus;Bt)
<img src="https://latex.codecogs.com/gif.latex?x(t)=A&plus;Bt" /> 

For more details, see:

- [Variability of Millennial‐Scale Trends in the Geomagnetic Axial Dipole, Buffett et al. (2019)](https://doi.org/10.1029/2019GL085909)
- [Decadal global temperature variability increases strongly with climate sensitivity, Nijsse et al. (2019)](https://doi.org/10.1038/s41558-019-0527-4)

Import libraries for calculations and getting data.

```{r}
library(ItoGaussPlot)
library(rmatio)
```

For this example, we will consider the Ornstein-Ulhenbeck process, 

<img src="https://render.githubusercontent.com/render/math?math=dx_t=-\gamma x_t dt + \sqrt{D}dW_t">

where <img src="https://render.githubusercontent.com/render/math?math=x_t"> is the amplitude as a function of time, <img src="https://render.githubusercontent.com/render/math?math=\gamma"> is the slope of the drift term, <img src="https://render.githubusercontent.com/render/math?math=\sqrt{D}"> is the amplitude of the noise, and <img src="https://render.githubusercontent.com/render/math?math=W_t"> is a [Wiener process](https://en.wikipedia.org/wiki/Wiener_process). A previously calculated realization of this process (calculated using the [Euler-Maruyama method](https://en.wikipedia.org/wiki/Euler–Maruyama_method)) is loaded.

```{r}
data <- read.mat('tX_g1D1e0.dat')
time <- data$tX[1,]
amplitude <- data$tX[2,]
dataVars <- list(gamma=1.0, D=1.0, dt=0.001)
dataVars$Nt <- ceiling(tail(time, n=1)/dataVars$dt)

plot(time, amplitude, type = 'l',
     main='Ornstein-Uhlenbeck integration',
     xlab='Time', ylab='Amplitude')
```
<img src="https://user-images.githubusercontent.com/38541020/89318856-37247100-d634-11ea-9ae3-88d89558f145.jpg" width="800" height="auto"/>

Calculating the variability of trends as a function of windowing length is done with the `stdWindowSlopeCast()` function. The time vector, amplitude vector, and window length(s) are arguments.

```{r}
windows <- 10^seq(-1,2,len=20)
slopeStds <- stdWindowSlopeCast(time, amplitude, windows)
```

For the Ornstein-Ulhenbeck process, a closed form expression of slope variability can be derived.

<img src="https://render.githubusercontent.com/render/math?math=\sigma_B(w) = \sqrt{24\gamma D f(\gamma w)}">

where <img src="https://render.githubusercontent.com/render/math?math=f(x)"> is the function

<img src="https://render.githubusercontent.com/render/math?math=f(x) = x^{-3}-3x^{-4}+12x^{-6}-e^{-x}\Big(3x^{-4}+12x^{-5}+12x^{-6}\Big).">

```{r}
windowsTheory <- 10^seq(-1,2,len=50)
slopeStdsTheory <- OUanalytic(windowsTheory, dataVars$gamma, dataVars$D)
slopeStdsTheoryLimit <- OUanalyticSmallW(windowsTheory, dataVars$gamma, dataVars$D)
```

Below is a plot comparing the numerical calculations and the theoretical values.

```{r}
plot(windowsTheory, slopeStdsTheory, 
     log='xy', type = 'l',
     main='Standard deviation of slope by windowing',
     xlab='Window length, w', ylab=expression('Std. of slope, σ'[b]))
lines(windowsTheory, slopeStdsTheoryLimit, lty=2)
points(windows, slopeStds, col="red", pch=16)
legend("topright", legend=c("Theory", "Small w limit", "Calculation"),
       col=c("black", "black", "red"), lty=c(1,3,0), pch=c(0,0,16), 
       pt.cex=c(0,0,1), lwd=c(1.5,1.5,1), cex=0.8)
```

<img src="https://user-images.githubusercontent.com/38541020/89319063-7c48a300-d634-11ea-9073-81cb3d20c723.jpg" width="800" height="auto"/>

## Uneven time-steps
The package uses a different algorthm for unevenly spaces time-series.

```{r}
data <- read.mat('tX_g1D1e0NU.dat')
timeNU <- data$tX[1,]
amplitudeNU <- data$tX[2,]
dataVarsNU <- list(gamma=1.0, D=1.0, dt=0.001)
dataVarsNU$Nt <- ceiling(tail(time, n=1)/dataVars$dt)

par(mfrow=c(1,2))    # set the plotting area into a 1*2 array
plot(timeNU, amplitudeNU, type = 'l',
     main='Ornstein-Uhlenbeck integration',
     xlab='Time', ylab='Amplitude')
hist(diff(timeNU),
     main='Histogram of time-steps',
     xlab='Time-step', ylab='Counts')
```

<img src="https://user-images.githubusercontent.com/38541020/89319152-971b1780-d634-11ea-867b-8179dabf3def.jpg" width="800" height="auto"/>

The `stdWindowSlopeCast()` function is used again, but theuneven timesteps in the time vector cause a different method to be called.

```{r}
slopeStdsNU <- stdWindowSlopeCast(timeNU, amplitudeNU, windows)
```

Below is a plot comparing the numerical calculations and the theoretical values.

```{r}
plot(windowsTheory, slopeStdsTheory, 
     log='xy', type = 'l',
     main='Standard deviation of slope by windowing',
     xlab='Window length, w', ylab=expression('Std. of slope, σ'[b]))
lines(windowsTheory, slopeStdsTheoryLimit, lty=2)
points(windows, slopeStdsNU, col="red", pch=16)
legend("topright", legend=c("Theory", "Small w limit", "Calculation"),
       col=c("black", "black", "red"), lty=c(1,3,0), pch=c(0,0,16), 
       pt.cex=c(0,0,1), lwd=c(1.5,1.5,1), cex=0.8)
```

<img src="https://user-images.githubusercontent.com/38541020/89319326-cfbaf100-d634-11ea-8cef-5942ce5d6f5f.jpg" width="800" height="auto"/>
