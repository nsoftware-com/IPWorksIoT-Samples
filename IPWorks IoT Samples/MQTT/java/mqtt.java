/*
 * IPWorks IoT 2024 Java Edition - Sample Project
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

import java.io.*;
import ipworksiot.*;

public class mqtt extends ConsoleDemo{

    public static void main(String[] args) {
        System.out.println("*******************************************************");
        System.out.println("* This is a demo for the IP*Works IoT MQTT Class.     *");
        System.out.println("* It prompts the user for a topic and message, then   *");
        System.out.println("* subscribes to that topic and publishes the message. *");
        System.out.println("* The demo uses a publicly available test server.     *");
        System.out.println("*******************************************************");
        MQTT mqtt = new MQTT();
        try {
            // Set up event handlers
            mqtt.addMQTTEventListener(new MQTTEventListener() {
                @Override
                public void connected(MQTTConnectedEvent mqttConnectedEvent) {

                }

                @Override
                public void connectionStatus(MQTTConnectionStatusEvent mqttConnectionStatusEvent) {

                }

                @Override
                public void disconnected(MQTTDisconnectedEvent mqttDisconnectedEvent) {

                }

                @Override
                public void error(MQTTErrorEvent mqttErrorEvent) {

                }

                @Override
                public void log(MQTTLogEvent mqttLogEvent) {

                }

                @Override
                public void messageAck(MQTTMessageAckEvent mqttMessageAckEvent) {

                }

                @Override
                public void messageIn(MQTTMessageInEvent mqttMessageInEvent) {
                    // Fires whenever a message is received
                    System.out.println(new String(mqttMessageInEvent.message)); // Print the contents of the message.
                }

                @Override
                public void messageOut(MQTTMessageOutEvent mqttMessageOutEvent) {

                }

                @Override
                public void readyToSend(MQTTReadyToSendEvent mqttReadyToSendEvent) {

                }

                @Override
                public void SSLServerAuthentication(MQTTSSLServerAuthenticationEvent mqttSSLServerAuthenticationEvent) {

                }

                @Override
                public void SSLStatus(MQTTSSLStatusEvent mqttSSLStatusEvent) {

                }

                @Override
                public void subscribed(MQTTSubscribedEvent mqttSubscribedEvent) {

                }

                @Override
                public void unsubscribed(MQTTUnsubscribedEvent mqttUnsubscribedEvent) {

                }
            });

            // Connect to the test server
            mqtt.connectTo("test.mosquitto.org", 1883);

            // Get a topic and subscribe to it
            String topic = prompt("Please enter a topic to subscribe to", ":", "nsoftware/test");

            // Subscribe at QoS Level 2
            mqtt.subscribe(topic, 2);

            Thread.sleep(1000); // Wait for a retained message

            // Get a message to post and post it to the topic
            String message = prompt("Please enter a message to post", ":", "Hello World!");
            // Publish the message at QoS level 2
            mqtt.publishMessage(topic, 2, message);

            Thread.sleep(1000); // Wait for the message to be published and returned
        } catch (Exception e) {
            displayError(e);
        }
    }
}

class ConsoleDemo {
  private static BufferedReader bf = new BufferedReader(new InputStreamReader(System.in));

  static String input() {
    try {
      return bf.readLine();
    } catch (IOException ioe) {
      return "";
    }
  }
  static char read() {
    return input().charAt(0);
  }

  static String prompt(String label) {
    return prompt(label, ":");
  }
  static String prompt(String label, String punctuation) {
    System.out.print(label + punctuation + " ");
    return input();
  }
  static String prompt(String label, String punctuation, String defaultVal) {
      System.out.print(label + " [" + defaultVal + "]" + punctuation + " ");
      String response = input();
      if (response.equals(""))
        return defaultVal;
      else
        return response;
  }

  static char ask(String label) {
    return ask(label, "?");
  }
  static char ask(String label, String punctuation) {
    return ask(label, punctuation, "(y/n)");
  }
  static char ask(String label, String punctuation, String answers) {
    System.out.print(label + punctuation + " " + answers + " ");
    return Character.toLowerCase(read());
  }

  static void displayError(Exception e) {
    System.out.print("Error");
    if (e instanceof IPWorksIoTException) {
      System.out.print(" (" + ((IPWorksIoTException) e).getCode() + ")");
    }
    System.out.println(": " + e.getMessage());
    e.printStackTrace();
  }

  /**
   * Takes a list of switch arguments or name-value arguments and turns it into a map.
   */
  static java.util.Map<String, String> parseArgs(String[] args) {
    java.util.Map<String, String> map = new java.util.HashMap<String, String>();
    
    for (int i = 0; i < args.length; i++) {
      // Add a key to the map for each argument.
      if (args[i].startsWith("-")) {
        // If the next argument does NOT start with a "-" then it is a value.
        if (i + 1 < args.length && !args[i + 1].startsWith("-")) {
          // Save the value and skip the next entry in the list of arguments.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), args[i + 1]);
          i++;
        } else {
          // If the next argument starts with a "-", then we assume the current one is a switch.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), "");
        }
      } else {
        // If the argument does not start with a "-", store the argument based on the index.
        map.put(Integer.toString(i), args[i].toLowerCase());
      }
    }
    return map;
  }
}




