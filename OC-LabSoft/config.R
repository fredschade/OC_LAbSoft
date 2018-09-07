## config.R file for OC_manager

## change here for the python version
# use_python("/usr/bin/python3.5", required = T)

board = F ## set to T for development purpose if you are not connected to the arduino
# pulse_delay_secu = T ## delay max of 20 microseconds for inkjet pulse delay

Drop_vol = 0.15 # in nL, use to calculate volume in Methods

# resolution nozzles
inche = 25.4 # mm/inche
dpi = 96 # resolution of the Hp cartdrige (datasheet), dpi=number/inch
reso = round(inche/dpi,3) # distance of one nozzle to the next one

Visu_roi = "0.25,0.2,0.6,0.6"
