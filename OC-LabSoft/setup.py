#to send a file of gcode to the printer
# import sys
# 
# sys

# print(pwd)

import atexit
import os
import sys
sys.path.append(os.getcwd())
# sys.path.append("/usr/local/lib/python3.5")
import time

from printrun.printcore import printcore
from printrun import gcoder
p=printcore()


def connect_board(port):
  p.connect(port,115200)
  p._listen_until_online()
  print(p.printer)

def close_connections():
  p.disconnect()
  time.sleep(0.1)
  print(p.printer)

def send_gcode(file):
  print(p.printer)
  # time.sleep(1)
  gcode=[i.strip() for i in open(file)] # or pass in your own array of gcode lines instead of reading from a file
  gcode = gcoder.LightGCode(gcode)
  p.startprint(gcode)
  while p.printing:
    time.sleep(0.1)


def send_cmd(cmd):
  p.send_now(cmd)

def cancelprint():
  print("stop print")
  p.cancelprint()




atexit.register(close_connections)
