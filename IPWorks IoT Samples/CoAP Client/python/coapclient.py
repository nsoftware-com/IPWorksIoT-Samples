# 
# IPWorks IoT 2024 Python Edition - Sample Project
# 
# This sample project demonstrates the usage of IPWorks IoT in a 
# simple, straightforward way. It is not intended to be a complete 
# application. Error handling and other checks are simplified for clarity.
# 
# www.nsoftware.com/ipworksiot
# 
# This code is subject to the terms and conditions specified in the 
# corresponding product license agreement which outlines the authorized 
# usage and restrictions.
# 

import sys
import string
from ipworksiot import *

input = sys.hexversion < 0x03000000 and raw_input or input


def ensureArg(args, prompt, index):
    if len(args) <= index:
        while len(args) <= index:
            args.append(None)
        args[index] = input(prompt)
    elif args[index] is None:
        args[index] = input(prompt)


coap = CoAP()

def fire_error(e):
  print("OnError: [" + str(e.error_code) + "] " + e.description)

def fire_log(e):
  print("Log: [" + str(e.log_level) +"] " + e.message)

def fire_notification(e):
  if (e.is_latest):
    print("OnNotification: Recieved notification about recourse at "  + e.uri + ": [" + str(coap.get_response_code()) +
          "] " + coap.get_response_data().decode("utf-8"))
  else:
    print("OnNotification: Received out-of-date notification about resource at " + e.uri + "; ignoring.")
    
def fire_request_complete(e):
  if e.error_code == 0:
    print("Request for " + e.uri + " completed successfully: [" + str(coap.get_response_code()) + "] " + str(coap.get_response_data().decode("utf-8")))
  elif e.error_code == 709: # Indicates non-2.xx response code returned by server.
    print("Request for " + e.uri + " completed unsuccessfully: [" + str(coap.get_response_code()) + "] " + str(coap.get_response_data().decode("utf-8")))
  else:
    print("Request for " + e.uri + " failed with the following error: [" + str(e.error_code) + "] " + e.error_description)
    
def print_prompt():
  return input("\nWould you like to:\n" +
                "\t1. Get Data\n" +
                "\t2. Post Data\n" +
                "\t3. Put Node\n" +
                "\t4. Delete Node\n" +
                "\t5. Start Observation\n" +
                "\t6. Stop Observation\n" +
                "\t7. Quit\n? ")

def get_uri(host, port, default_uri):
  temp = input("URI (default: "+default_uri+")? ")
  if temp == "":
    return "coap://" + host + ":" + str(port) + "/" + default_uri
  return "coap://" + host + ":" + str(port) + "/" + temp.strip()

coap.on_error = fire_error
coap.on_log = fire_log
coap.on_notification = fire_notification
coap.on_request_complete = fire_request_complete
host = "localhost"
port = 5683

temp = input("Please enter the address for the CoAP Server the client will connect to (default: localhost)? ")
if temp != "":
  host = temp
temp = input("Please enter the port for the client to connect on (default: 5683)? ")
if temp != "":
  port = int(temp)
  
try:
  response = print_prompt()
  while response != "7":
    if response == "1": # Get Data
      uri = get_uri(host, port, "animals/domestic/cat")
      coap.get(uri)
      
    elif response == "2": # Post Data
      coap.set_request_content_format(0)
      uri = get_uri(host, port, "animals/domestic/cat")
      temp = input("Request Body (default: MEOW!!)? ")
      if temp == "":
        coap.set_request_data("MEOW!!")
      else:
        coap.set_request_data(temp)
      coap.post(uri)
      
    elif response == "3": # Put Node
      coap.set_request_content_format(0)
      uri = get_uri(host, port, "animals/domestic/hamster")
      temp = input("Request Body (default: Squeak!)? ")
      if temp == "":
        coap.set_request_data("Squeak!")
      else:
        coap.set_request_data(temp)
      coap.put(uri)
      
    elif response == "4": # Delete Node
      coap.set_request_content_format(0)
      uri = get_uri(host, port, "animals/domestic/dog")
      coap.delete(uri)
      
    elif response == "5": # Start Observation
      coap.set_request_content_format(0)
      uri = get_uri(host, port, "animals/domestic/cat")
      print("Starting observation of " + uri + ".")
      coap.start_observing(uri)
      
    elif response == "6": # Stop Observation
      coap.set_request_content_format(0)
      uri = get_uri(host, port, "animals/domestic/cat")
      print("Stopping observation of " + uri + ".")
      coap.stop_observing(uri)
      
    else:
      print("Unrecognized Command")
    response = print_prompt()
          
except IPWorksIoTError as error:
  print("Error creating sender link: %i - %s" % (error.code, error.message))
  exit()

