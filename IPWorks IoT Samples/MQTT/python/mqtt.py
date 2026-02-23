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


mqtt = MQTT()
msgReceived = False

def fire_message_in(e):
  print("Incoming message from <%s>: %s\n" % (e.topic, e.message.decode("UTF-8")))
  global msgReceived
  msgReceived = True

def fire_subscribed(e):
  print("Subscribed to %s.\n", e.topic_filter)

def get_input(default):
  result = input()
  if result == "":
    result = default
  return result


print("******************************************************************************\n")
print("* This demo shows how to use the MQTT component to publish and subscribe to  *\n")
print("* topics. The demo makes use of a publicly available test server.            *\n")
print("******************************************************************************\n\n")

mqtt.on_message_in = fire_message_in
mqtt.on_subscribed = fire_subscribed

mqtt.set_client_id("testClient")
mqtt.connect_to("test.mosquitto.org", 1883)

print("Please enter a topic to subscribe and publish to [nsoftware_test]:")
topic = get_input("nsoftware_test")
mqtt.subscribe(topic, 1)

print("Please enter a message to send [Hello MQTT!]:")
message = get_input("Hello MQTT!")
mqtt.publish_message(topic, 1, message)

while not msgReceived:
  mqtt.do_events()

print("Press any key to continue.")
input()


