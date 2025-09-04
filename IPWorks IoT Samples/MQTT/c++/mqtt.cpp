/*
 * IPWorks IoT 2024 C++ Edition - Sample Project
 *
 * This sample project demonstrates the usage of IPWorks IoT in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/ipworksiot
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "../../include/ipworksiot.h"

bool msgReceived = false;

#define LINE_LEN 256
char input[LINE_LEN + 1];
const char* prompt(const char* prompt, const char* defaultVal) {
  printf("%s [%s]: ", prompt, defaultVal);
  fgets(input, LINE_LEN, stdin);
  input[strlen(input) - 1] = '\0';
  if (strlen(input) == 0) {
    strncpy(input, defaultVal, LINE_LEN);
    input[LINE_LEN] = '\0';
  }
  return input;
}

class MyMQTT : public MQTT
{
  // Fires when a message is received
  virtual int FireMessageIn(MQTTMessageInEventParams *e) {
    printf("Incoming Message from <%s>: %s\n", e->Topic, e->Message);
    msgReceived = true;
    return 0;
  }

  // Fires when done subscribing to a topic
  virtual int FireSubscribed(MQTTSubscribedEventParams *e) {
    printf("Subscribed to %s.\n", e->TopicFilter);
    return 0;
  }
};

int main()
{

  printf("******************************************************************************\n");
  printf("* This demo shows how to use the MQTT component to publish and subscribe to  *\n");
  printf("* topics. The demo makes use of a publicly available test server.            *\n");
  printf("******************************************************************************\n\n");

  MyMQTT mqtt;

  // Configure settings
  mqtt.SetClientId("testClient");
  mqtt.ConnectTo("test.mosquitto.org", 1883);

  // Prompt for a topic
  char topic[LINE_LEN];
  strncpy(topic, prompt("Please enter a topic to subscribe and publish to", "nsoftware_test"), LINE_LEN);

  // Subscribe to the topic with max QOS 1 (At least once delivery)
  mqtt.Subscribe(topic, 1);

  // Prompt for a message
  char message[LINE_LEN];
  strncpy(message, prompt("Please enter a message to send", "Hello MQTT!"), LINE_LEN);

  // Publish the message with QOS 1 (At least once delivery)
  mqtt.PublishMessage(topic, 1, message);

  // Wait until the message has been received from the subscription
  while (!msgReceived) {
    mqtt.DoEvents();
  }

  printf("Press any key to continue.");
  fgetc(stdin);
  return 0;
}



