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

public class mqttbroker extends ConsoleDemo{

    private static MQTTBroker broker;

    public static void main(String[] args) {
        try {
            System.out.println("******************************************************************************************************");
            System.out.println("* This demo shows how to set up an MQTT broker on your computer. By default, the server will operate *");
            System.out.println("* in plaintext. If SSL is desired, use the addBrokerListener method to add secure listeners.         *");
            System.out.println("******************************************************************************************************\n");
            broker = new MQTTBroker();

            //add listeners
            broker.addMQTTBrokerEventListener(new DefaultMQTTBrokerEventListener() {
                @Override
                public void error(MQTTBrokerErrorEvent e) {
                    System.out.println("OnError: [" + e.errorCode + "] " + e.description);
                }

                @Override
                public void log(MQTTBrokerLogEvent e) {
                    System.out.println("Log: [" + e.logLevel + "] " + e.message);
                }

                @Override
                public void SSLClientAuthentication(MQTTBrokerSSLClientAuthenticationEvent e) {
                    e.accept = true;
                }

                @Override
                public void messageSent(MQTTBrokerMessageSentEvent e) {
                    System.out.println("Client [" + e.connectionId + "] has received a message!");
                    System.out.println("ID: " + e.messageId);
                    System.out.println("Client ID: " + e.clientId);
                }

                @Override
                public void messageReceived(MQTTBrokerMessageReceivedEvent e) {
                    System.out.println("Client [" + e.connectionId + "] has sent a message!");
                    System.out.println("ID: " + e.messageId);
                    System.out.println("Client ID: " + e.clientId);
                }

                @Override
                public void unsubscribe(MQTTBrokerUnsubscribeEvent e) {
                    System.out.println("Client [" + e.connectionId + "] has unsubscribed to the " + e.topicFilter + "topic!");
                    System.out.println("ID: " + e.clientId);
                }

                @Override
                public void subscribe(MQTTBrokerSubscribeEvent e) {
                    System.out.println("Client [" + e.connectionId + "] has subscribed to topic: " + e.topicFilter);
                    System.out.println("ID: " + e.clientId + "\nRequested QoS: " + e.requestedQoS);
                }

                @Override
                public void disconnected(MQTTBrokerDisconnectedEvent e) {
                    System.out.println("Client [" + e.connectionId + "] has disconnected with status: " + e.statusCode + "/" + e.description);
                }

                @Override
                public void sessionRemoved(MQTTBrokerSessionRemovedEvent e) {
                    System.out.println("Client with ID " + e.clientId + " has removed the session!");
                }

                @Override
                public void sessionRequest(MQTTBrokerSessionRequestEvent e) {
                    System.out.println("Client [" + e.connectionId + "] has requested a session!\nClient ID is: " + e.clientId);
                    //here you can decide if you want to grant the client a session
                    e.accept = true;
                }

                @Override
                public void connectionRequest(MQTTBrokerConnectionRequestEvent e) {
                    System.out.println("Client [" + e.address + ":" + e.port + "] has requested a connection!");
                    //here you can decide if you want to establish a connection with the client
                    e.accept = true;
                }

                @Override
                public void connected(MQTTBrokerConnectedEvent e) {
                    System.out.println("Client [" + e.connectionId + "] has connected with status: " + e.statusCode + "/" + e.description);
                }
            });

            broker.addBrokerListener("localhost", Integer.parseInt(prompt("Port", ":", "1883")), 0, 0, null, null, null);

            broker.startListening();

            System.out.println("Server listening on port " + broker.getListeners().item(0).getLocalHost() + ".");
            System.out.println("Press Q to exit.\r\n\r\n");

            while (true) {
                if (System.in.available() > 0) {
                    if (String.valueOf(read()).equalsIgnoreCase("Q")) {
                        System.out.println("Broker shutting down. Goodbye!");
                        broker.shutdown();
                    }
                }
            }

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




