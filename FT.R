#####################
# Fourier transform #
#####################

# Time series
Ts<-runif(n=100,min=-5,max=5)

# Forward Fourier transform
N=length(Ts)

FT<-numeric()
for(n in 1:N){
  A=numeric()
  for(k in 1:N){
    A[k]=Ts[k]/N*exp(-2*pi*1i*(n-1)*(k-1)/N)
  }
  FT[n]=sum(A)*N
}

# Forward Fast Fourier transform
FFT<-fft(Ts)

# Comparison of FT and FFT
plot(Re(FT),Re(FFT))
plot(Im(FT),Im(FFT))

View(data.frame(FFT,FT))

# Inverse Fourier transform

IFT<-numeric()
for(n in 1:N){
  A=numeric()
  for(k in 1:N){
    A[k]=FT[k]/N*exp(2*pi*1i*(n-1)*(k-1)/N)
  }
  IFT[n]=Re(sum(A))
}

plot(Ts,IFT)

# Inverse Fast Fourier transform
IFFT<-Re(fft(FFT,inverse=TRUE)/N)

plot(Ts,IFFT)

plot(IFT,IFFT)