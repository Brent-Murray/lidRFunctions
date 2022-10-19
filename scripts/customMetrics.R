# Custom Metrics Function
my_metrics <- function(Z, Intensity, ReturnNumber, NumberOfReturns, Classification){
  
  # Get Returns
  firstReturn <- ReturnNumber == 1L # First Returns
  intermediateReturn <- (ReturnNumber > 1L & ReturnNumber < NumberOfReturns) # Intermediate Returns
  lastReturn <- ReturnNumber == NumberOfReturns # Last Returns
  singleReturn <- NumberOfReturns == 1L # Single Returns
  firstManyReturn <- (NumberOfReturns > 1L & ReturnNumber == 1L) # First of Many Returns
  lastManyReturn <- (NumberOfReturns > 1L & ReturnNumber == NumberOfReturns) # Last of Many Returns
  firstAbove05 <- (ReturnNumber == 1L & Z > 0.5) # First Returns Above 0.5m
  firstAbove1 <- (ReturnNumber == 1L & Z > 1) # First Returns Above 1m
  firstAbove2 <- (ReturnNumber == 1L & Z > 2) # First Returns Above 2m
  firstAbove3 <- (ReturnNumber == 1L & Z > 3) # First Returns Above 3m
  firstAbove4 <- (ReturnNumber == 1L & Z > 4) # First Returns Above 4m
  firstAbove5 <- (ReturnNumber == 1L & Z > 5) # First Returns Above 5m
  firstAbove10 <- (ReturnNumber == 1L & Z > 10) # First Returns Above 10m
  firstAbove15 <- (ReturnNumber == 1L & Z > 15) # First Returns Above 15m
  firstAbove20 <- (ReturnNumber == 1L & Z > 20) # First Returns Above 20m
  firstAbove25 <- (ReturnNumber == 1L & Z > 25) # First Returns Abvoe 25m
  lastAbove1 <- (ReturnNumber == NumberOfReturns & Z > 1) # Last Returns Above 1m
  groundReturn <- Classification == LASGROUND # Ground Classified Returns
  firstbtw05 <- (ReturnNumber == 1L & Z > 0 & Z < 5) # First Returns Between 0m and 5m
  firstbtw510 <- (ReturnNumber == 1L & Z > 5 & Z < 10) # First Returns Between 5m and 10m
  firstbtw1015 <- (ReturnNumber == 1L & Z > 10 & Z < 15) # First Returns Between 10m and 15m
  firstbtw1520 <- (ReturnNumber == 1L & Z > 15 & Z < 20) # First Returns Between 15m and 20m
  firstbtw2025 <- (ReturnNumber == 1L & Z > 20 & Z < 25) # First Returns Between 20m and 25m
  firstbtw2530 <- (ReturnNumber == 1L & Z > 25 & Z < 30) # First Returns Between 25m and 30m
  firstbtw3035 <- (ReturnNumber == 1L & Z > 30 & Z < 35) # First Returns Between 30m and 35m
  firstbtw3540 <- (ReturnNumber == 1L & Z > 35 & Z < 40) # First Returns Between 35m and 40m
  
  # Height of Returns
  zfirst <- Z[firstReturn]# Height of First Returns
  zintermediate <- Z[intermediateReturn] # Height of Intermediate Returns
  zlast <- Z[lastReturn] # Height of Last Returns
  zFirstMany <- Z[firstManyReturn] # Height of First Of Many Returns
  zLastMany <- Z[lastManyReturn] # Height of Last of Many Returns
  
  # Intensity of Returns
  ifirst <- Intensity[firstReturn] # Intensity of First Returns
  iintermediate <- Intensity[intermediateReturn] # Intensity of Intermediate Returns
  iFirstMany <- Intensity[firstManyReturn] # Intensity of First of Many Returns
  iLastMany <- Intensity[lastManyReturn] # Intensity of Last of Many Returns
  iLastAbove1 <- Intensity[lastAbove1] # Intensity of Last Returns Above 1m
  iFirstAbove1 <- Intensity[firstAbove1] # Intensity of First Returns Above 1m
  iGround <- Intensity[groundReturn] # Intensity of Ground Classified Points
  
  
  
  # List of Metrics
  metrics <- list(
    # Metrics to Output
    zmax1th = if (is.null(as.double(max(zfirst))) || is.infinite(as.double(max(zfirst)))) 0 else as.double(max(zfirst)), # Max Height of First Returns
    zmean1th = if (is.null(as.double(mean(zfirst)))) 0 else as.double(mean(zfirst)), # Mean Height of First Returns
    zsd1th = if (is.null(sd(zfirst))) 0 else sd(zfirst), # Standard Deviation of Height of First Returns
    imax1th = if (is.null(as.double(max(ifirst))) || is.infinite(as.double(max(zfirst)))) 0 else as.double(max(ifirst)), # Max Intensity of First Returns
    imean1th = if (is.null(as.double(mean(ifirst)))) 0 else as.double(mean(ifirst)), # Mean Intensity of First Returns
    zmeanint = if (is.null(as.double(mean(zintermediate)))) 0 else as.double(mean(zintermediate)), # Mean Height of Intermediate Returns
    zsdint = if (is.null(sd(zintermediate))) 0 else sd(zintermediate), # Standard Deviation of Height of Intermediate Returns
    imeanint =  if (is.null(mean(iintermediate))) 0 else mean(iintermediate), # Mean Intensity of Intermediate Returns
    zq991th = if (is.null(quantile(zfirst, 0.99)[[1]])) 0 else quantile(zfirst, 0.99)[[1]], # Height of First Returns - 99th Percentile
    zq951th = if (is.null(quantile(zfirst, 0.95)[[1]])) 0 else quantile(zfirst, 0.95)[[1]], # Height of First Returns - 95th Percentile
    zq901th = if (is.null(quantile(zfirst, 0.90)[[1]])) 0 else quantile(zfirst, 0.90)[[1]], # Height of First Returns - 90th Percentile
    zq801th = if (is.null(quantile(zfirst, 0.80)[[1]])) 0 else quantile(zfirst, 0.80)[[1]], # Height of First Returns - 80th Percentile
    zq701th = if (is.null(quantile(zfirst, 0.70)[[1]])) 0 else quantile(zfirst, 0.70)[[1]], # Height of First Returns - 70th Percentile
    zq601th = if (is.null(quantile(zfirst, 0.60)[[1]])) 0 else quantile(zfirst, 0.60)[[1]], # Height of First Returns - 60th Percentile
    zq501th = if (is.null(quantile(zfirst, 0.50)[[1]])) 0 else quantile(zfirst, 0.50)[[1]], # Height of First Returns - 50th Percentile
    zq401th = if (is.null(quantile(zfirst, 0.40)[[1]])) 0 else quantile(zfirst, 0.40)[[1]], # Height of First Returns - 40th Percentile
    zq301th = if (is.null(quantile(zfirst, 0.30)[[1]])) 0 else quantile(zfirst, 0.30)[[1]], # Height of First Returns - 30th Percentile
    zq201th = if (is.null(quantile(zfirst, 0.20)[[1]])) 0 else quantile(zfirst, 0.20)[[1]], # Height of First Returns - 20th Percentile
    zq99last = if (is.null(quantile(zlast, 0.99)[[1]])) 0 else quantile(zlast, 0.99)[[1]], # Height of Last Returns - 99th Percentile
    zq98last = if (is.null(quantile(zlast, 0.95)[[1]])) 0 else quantile(zlast, 0.95)[[1]], # Height of Last Returns - 95th Percentile
    zq90last = if (is.null(quantile(zlast, 0.90)[[1]])) 0 else quantile(zlast, 0.90)[[1]], # Height of Last Returns - 90th Percentile
    zq80last = if (is.null(quantile(zlast, 0.80)[[1]])) 0 else quantile(zlast, 0.80)[[1]], # Height of Last Returns - 80th Percentile
    zq70last = if (is.null(quantile(zlast, 0.70)[[1]])) 0 else quantile(zlast, 0.70)[[1]], # Height of Last Returns - 70th Percentile
    zq60last = if (is.null(quantile(zlast, 0.60)[[1]])) 0 else quantile(zlast, 0.60)[[1]], # Height of Last Returns - 60th Percentile
    zq50last = if (is.null(quantile(zlast, 0.50)[[1]])) 0 else quantile(zlast, 0.50)[[1]], # Height of Last Returns - 50th Percentile
    zq40last = if (is.null(quantile(zlast, 0.40)[[1]])) 0 else quantile(zlast, 0.40)[[1]], # Height of Last Returns - 40th Percentile
    zq30last = if (is.null(quantile(zlast, 0.30)[[1]])) 0 else quantile(zlast, 0.30)[[1]], # Height of Last Returns - 30th Percentile
    zq20last = if (is.null(quantile(zlast, 0.20)[[1]])) 0 else quantile(zlast, 0.20)[[1]], # Height of Last Returns - 20th Percentile
    iq99 = if (is.null(quantile(Intensity, 0.99)[[1]])) 0 else quantile(Intensity, 0.99)[[1]], # Intensity - 99th Percentile
    iq95 = if (is.null(quantile(Intensity, 0.95)[[1]])) 0 else quantile(Intensity, 0.95)[[1]], # Intensity - 95th Percentile
    iq90 = if (is.null(quantile(Intensity, 0.90)[[1]])) 0 else quantile(Intensity, 0.90)[[1]], # Intensity - 90th Percentile
    iq80 = if (is.null(quantile(Intensity, 0.80)[[1]])) 0 else quantile(Intensity, 0.80)[[1]], # Intensity - 80th Percentile
    iq70 = if (is.null(quantile(Intensity, 0.70)[[1]])) 0 else quantile(Intensity, 0.70)[[1]], # Intensity - 70th Percentile
    iq60 = if (is.null(quantile(Intensity, 0.60)[[1]])) 0 else quantile(Intensity, 0.60)[[1]], # Intensity - 60th Percentile
    iq50 = if (is.null(quantile(Intensity, 0.50)[[1]])) 0 else quantile(Intensity, 0.50)[[1]], # Intensity - 50th Percentile
    iq40 = if (is.null(quantile(Intensity, 0.40)[[1]])) 0 else quantile(Intensity, 0.40)[[1]], # Intensity - 40th Percentile
    iq30 = if (is.null(quantile(Intensity, 0.30)[[1]])) 0 else quantile(Intensity, 0.30)[[1]], # Intensity - 30th Percentile
    iq20 = if (is.null(quantile(Intensity, 0.20)[[1]])) 0 else quantile(Intensity, 0.20)[[1]], # Intensity - 20th Percentile
    iq10 = if (is.null(quantile(Intensity, 0.10)[[1]])) 0 else quantile(Intensity, 0.10)[[1]], # Intensity - 10th Percentile
    iq5 = if (is.null(quantile(Intensity, 0.05)[[1]])) 0 else quantile(Intensity, 0.05)[[1]], # Intensity - 5th Percentile
    iq1 = if (is.null(quantile(Intensity, 0.01)[[1]])) 0 else quantile(Intensity, 0.01)[[1]], # Intensity - 1st Percentile
    p1thabove05 = if (is.null((sum(firstAbove05)/sum(firstReturn)))) 0 else (sum(firstAbove05)/sum(firstReturn)), # Proportion of First Returns Above 0.5m
    p1thabove1 = if (is.null((sum(firstAbove1)/sum(firstReturn)))) 0 else (sum(firstAbove1)/sum(firstReturn)), # Proportion of First Returns Above 1m
    zmean1thabove1 = if (is.null(as.double(mean(firstAbove1)))) 0 else as.double(mean(firstAbove1)), # Mean Height of 1st Returns Above 1m
    zsd1thabove1 = if (is.null(sd(firstAbove1))) 0 else sd(firstAbove1), # Standard Deviation of Heights of First Returns Above 1m
    plastabove1 = if (is.null((sum(lastAbove1)/sum(lastReturn)))) 0 else (sum(lastAbove1)/sum(lastReturn)), # Proportion of Last Returns Above 1m
    zmeanlastabove1 = if (is.null(as.double(mean(lastAbove1)))) 0 else as.double(mean(lastAbove1)), # Mean Height of Last Returns Above 1m
    zsdlastabove1 = if (is.null(sd(lastAbove1))) 0 else sd(lastAbove1), # Standard Deviation of Heights of Last Returns Above 1m
    p1thabove2 = if (is.null((sum(firstAbove2)/sum(firstReturn)))) 0 else (sum(firstAbove2)/sum(firstReturn)), # Proportion of First Returns Above 2m
    p1thabove3 = if (is.null((sum(firstAbove3)/sum(firstReturn)))) 0 else (sum(firstAbove3)/sum(firstReturn)), # Proportion of First Returns Above 3m
    p1thabove4 = if (is.null((sum(firstAbove4)/sum(firstReturn)))) 0 else (sum(firstAbove4)/sum(firstReturn)), # Proportion of First Returns Above 4m
    p1thabove5 = if (is.null((sum(firstAbove5)/sum(firstReturn)))) 0 else (sum(firstAbove5)/sum(firstReturn)), # Proportion of First Returns Above 5m
    p1thabove10 = if (is.null((sum(firstAbove10)/sum(firstReturn)))) 0 else (sum(firstAbove10)/sum(firstReturn)), # Proportion of First Returns Above 10m
    p1thabove15 = if (is.null((sum(firstAbove15)/sum(firstReturn)))) 0 else (sum(firstAbove15)/sum(firstReturn)), # Proportion of First Returns Above 15m
    p1thabove20 = if (is.null((sum(firstAbove20)/sum(firstReturn)))) 0 else (sum(firstAbove20)/sum(firstReturn)), # Proportion of First Returns Above 20m
    p1thabove25 = if (is.null((sum(firstAbove25)/sum(firstReturn)))) 0 else (sum(firstAbove25)/sum(firstReturn)), # Proportion of First Returns Above 25m
    p1thbtw05 = if (is.null((sum(firstbtw05)/sum(firstReturn)))) 0 else (sum(firstbtw05)/sum(firstReturn)), # Proportion of First Returns Between 0m and 5m
    p1thbtw510 = if (is.null((sum(firstbtw510)/sum(firstReturn)))) 0 else (sum(firstbtw510)/sum(firstReturn)), # Proportion of First Returns Between 5m and 10m
    p1thbtw1015 = if (is.null((sum(firstbtw1015)/sum(firstReturn)))) 0 else (sum(firstbtw1015)/sum(firstReturn)), # Proportion of First Returns Between 10m and 15m
    p1thbtw1520 = if (is.null((sum(firstbtw1520)/sum(firstReturn)))) 0 else (sum(firstbtw1520)/sum(firstReturn)), # Proportion of First Returns Between 15m and 20m
    p1thbtw2025 = if (is.null((sum(firstbtw2025)/sum(firstReturn)))) 0 else (sum(firstbtw2025)/sum(firstReturn)), # Proportion of First Returns Between 20m and 25m
    p1thbtw2530 = if (is.null((sum(firstbtw2530)/sum(firstReturn)))) 0 else (sum(firstbtw2530)/sum(firstReturn)), # Proportion of First Returns Between 25m and 30m
    p1thbtw3035 = if (is.null((sum(firstbtw3035)/sum(firstReturn)))) 0 else (sum(firstbtw3035)/sum(firstReturn)), # Proportion of First Returns Between 30m and 35m
    p1thbtw3540 = if (is.null((sum(firstbtw3540)/sum(firstReturn)))) 0 else (sum(firstbtw3540)/sum(firstReturn)), # Proportion of First Returns Between 35m and 40m
    psingle = if (is.null((sum(singleReturn)/sum(NumberOfReturns)))) 0 else (sum(singleReturn)/sum(NumberOfReturns)), # Proportion of Single Returns
    zmean1thmany = if (is.null(as.double(mean(zFirstMany)))) 0 else as.double(mean(zFirstMany)), # Mean Height of First of Many Returns
    zsd1thmany = if (is.null(sd(zFirstMany))) 0 else sd(zFirstMany), # Standard Deviation of Height of First of Many Returns
    p1thmany = if (is.null((sum(firstManyReturn)/sum(NumberOfReturns)))) 0 else (sum(firstManyReturn)/sum(NumberOfReturns)), # Proportion of First of Many Returns
    imean1thmany = if (is.null(as.double(mean(iFirstMany)))) 0 else as.double(mean(iFirstMany)), # Mean Intensity of First of Many Returns
    pint = if (is.null((sum(intermediateReturn)/sum(NumberOfReturns)))) 0 else (sum(intermediateReturn)/sum(NumberOfReturns)), # Proportion of Intermediate Returns
    zmeanlastmany = if (is.null(as.double(mean(zLastMany)))) 0 else as.double(mean(zLastMany)), # Mean Height of Last of Many Returns
    zsdlastmany = if (is.null(sd(zLastMany))) 0 else sd(zLastMany), # Standard Deviation of Height of Last of Many Returns
    plastmany = if (is.null((sum(lastManyReturn)/sum(NumberOfReturns)))) 0 else (sum(lastManyReturn)/sum(NumberOfReturns)), # Proportion of Last of Many Returns
    imeanlastmany = if (is.null(as.double(mean(iLastMany)))) 0 else as.double(mean(iLastMany)), # Mean Intensity of Last of Many Returns
    imean1thabove1 = if (is.null(as.double(mean(iFirstAbove1)))) 0 else as.double(mean(iFirstAbove1)), # Mean Intensity of First Returns Above 1m
    isdlastabove1 = if (is.null(as.double(mean(iLastAbove1)))) 0 else as.double(mean(iLastAbove1)), # Standard Deviation of Intensity of Last Returns Above 1m
    imeanground = if (is.null(mean(iGround))) 0 else mean(iGround), # Mean Intensity of Ground Classified Returns
    imaxground = if (is.null(as.double(max(iGround))) || is.infinite(as.double(max(iGround)))) 0 else as.double(max(iGround)), # Max Intensity of Ground Classified Returns
    iminground = if (is.null(as.double(min(iGround))) || is.infinite(as.double(min(iGround)))) 0 else as.double(min(iGround)) # Min Intensity of Ground Classified Returns
  )
  #print(metrics)
  return(metrics)
}