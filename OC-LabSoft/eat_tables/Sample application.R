# generates Table withe applied volume
appli_Table<-function(step){
  
  #getting infos
  table = step$table
  band_length = table[table[,1] == "Band length [mm]",2]
  dist_x = table[table[,1] == "First application position X [mm]",2]
  dist_x = dist_x-band_length/2
  dist_y = table[table[,1] == "application position Y [mm]",2]
  nbr_band = table[table[,1] == "Number of bands",2]
  dev_dir = table[table[,1] == "Development direction (X: 0, Y: 1)",2] ## 0 for X, 1 for Y
  plate_y = table[table[,1] == "Plate Y [mm]",2]
  plate_x = table[table[,1] == "Plate X [mm]",2]  
  
  ## deal with plate dimension
  dist_x = dist_x + 50-plate_x/2
  dist_y = dist_y + 50-plate_y/2

  
  
  ## deal with evolution in nbr of band
  if(is.null(step$appli_table)){## create original table
    appli_table = data.frame(Repeat = rep(1,nbr_band),I = rep(10,nbr_band),Use = rep(T,nbr_band))
  }else if(nrow(step$appli_table) < nbr_band){## modify table length
    appli_table = rbind(
      step$appli_table,
      data.frame(Repeat = rep(1,nbr_band),I = rep(10,nbr_band),Use = rep(T,nbr_band))
    )[seq(nbr_band),]
  }else if(nrow(step$appli_table) > (nbr_band)){## modify table length
    appli_table = step$appli_table[seq(nbr_band),]
  }
  else {appli_table=step$appli_table}
  rownames(appli_table) = seq(nrow(appli_table))
  
  return(appli_table)
}

# generates gcode for the Application  
gcode<-function(step){  
  #getting infos
  table = step$table
  band_length = table[table[,1] == "Band length [mm]",2]
  dist_x = table[table[,1] == "First application position X [mm]",2]
  dist_x = dist_x-band_length/2
  dist_y = table[table[,1] == "application position Y [mm]",2]
  gap = table[table[,1] == "Track distance [mm]",2]
  gap=gap-band_length
  speed = table[table[,1] == "Speed [mm/s]",2]
  path=table[table[,1] == "Number of paths",2]
  L=table[table[,1] == "Pulse delay [µs] (<20)",2]
  W=table[table[,1] == "Delay between path [s]",2]
  nozzle = table[table[,1] == "Used Nozzle",2]
  dev_dir = table[table[,1] == "Development direction (X: 0, Y: 1)",2] ## 0 for X, 1 for Y


  ## empty array
  inche = 25.4 # mm/inche
  dpi = 96
  reso = inche/dpi
  S = rep(0,12)
  for(i in seq(12)){if(i %in% as.numeric(nozzle)){S[i] = 1}};
  S=sum(2^(which(S== 1)-1))
  shift = round((1 - nozzle)*reso,3)
  dist_y = dist_y + shift
  
  SA_table=step$appli_table
  
  # nozzle_Y = 1 ## use only one nozzle
  # plate_x=100
  
  
  start_gcode = c("G28 X0; home X axis",
                  "G28 Y0; home Y axis",
                  "G21 ; set units to millimeters",
                  "G90 ; use absolute coordinates",
                  paste0("G1 F",60*speed," ; set speed in mm per min for the movement"),
                  paste0("G1 X",dist_y," ; go in X position") 
  )
  end_gcode = c("G28 X0; home X axis",
                "G28 Y0; home Y axis",
                "M84     ; disable motors")
  ## previously function a_to_gcode_X_fix
  
  ## begin gcode
  ## iterate
  gcode = c(start_gcode,unlist(lapply(seq(path),function(k){
    gcode = c()
    for(band in seq(nrow(SA_table))){
      Y_coord = round(seq(from = dist_y+(band-1)*(gap+band_length),by=reso,length.out = ceiling(band_length/reso)),3)
      Y_coord = Y_coord + shift
      if(SA_table$Use[band]){
        I = SA_table$I[band]
        for(Repeat in seq(SA_table$Repeat[band])){
          for(i in Y_coord){ # X loop, need modulo
            gcode = c(gcode,paste0("G1 Y",i," ; go in position - path: ",k))
            gcode = c(gcode,"M400 ; Wait for current moves to finish ")
            gcode = c(gcode,paste0("M700 P0 I",I," L",L," S",S," ; Fire"))
          }
        }
      }
    }
    if(W != 0){gcode=c(gcode,paste0("G4 S",W,"; wait in seconds"))}
    gcode
  })))
  gcode = c(gcode,end_gcode)
}

# generates the information plot
plot_step<-function(step) {
  #getting infos
  table = step$table
  band_length = table[table[,1] == "Band length [mm]",2]
  dist_x = table[table[,1] == "First application position X [mm]",2]
  dist_x = dist_x-band_length/2
  gap = table[table[,1] == "Track distance [mm]",2]
  gap=gap-band_length
  dist_y = table[table[,1] == "application position Y [mm]",2]
  dev_dir = table[table[,1] == "Development direction (X: 0, Y: 1)",2] ## 0 for X, 1 for Y
  plate_y = table[table[,1] == "Plate Y [mm]",2]
  plate_x = table[table[,1] == "Plate X [mm]",2]    
  nozzle = table[table[,1] == "Used Nozzle",2]
  path=table[table[,1] == "Number of paths",2]
  
  ## deal with plate dimension
  dist_x = dist_x + 50-plate_x/2
  dist_y = dist_y + 50-plate_y/2

  inche = 25.4 # mm/inche
  dpi = 96
  reso = inche/dpi
  S = rep(0,12)
  for(i in seq(12)){if(i %in% as.numeric(nozzle)){S[i] = 1}};
  S=sum(2^(which(S == 1)-1))
  shift = round((1 - nozzle)*reso,3)
  
  SA_table=step$appli_table
    plot(c(1,100),c(1,100),type="n",xaxt = 'n',xlim=c(0,100),ylim=c(100,0),xlab="",ylab="Application direction (X) ")
    axis(3)
    mtext("Migration direction (Y)", side=3, line=3)
    for(band in seq(nrow(SA_table))){
      if(SA_table$Use[band]){
          segments(x0 = dist_y,
                   y0 = dist_x+shift+(band-1)*(gap+band_length),
                   y1 = dist_x+shift+(band-1)*(gap+band_length)+band_length)
      }
    }
    symbols(x=50,y=50,add = T,inches = F,rectangles = rbind(c(plate_y,plate_x)),lty=2)


    step$info = c(paste0("Band ",seq(nrow(SA_table)),": ", SA_table$I*Drop_vol*ceiling(band_length/reso)*SA_table$Repeat*path/1000," µL used\n")
  )
  return(step)
}