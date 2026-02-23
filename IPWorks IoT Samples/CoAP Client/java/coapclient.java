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
import java.util.Arrays;

import ipworksiot.*;

public class coapclient extends ConsoleDemo{

    public static void main(String[] args) {
        System.out.println("*****************************************************************");
        System.out.println("* This demo shows how to use the CoAP component in client mode. *");
        System.out.println("* It allows for get, post, put and delete requests to be sent   *");
        System.out.println("* to a CoAP server, as well as observing a specific resource at *");
        System.out.println("* a specified URI.                                              *");
        System.out.println("*****************************************************************\n");
        CoAP coap = new CoAP();

        try {
            //add listeners
            coap.addCoAPEventListener(new CoAPEventListener() {
                @Override
                public void error(CoAPErrorEvent e) {
                    System.out.println("OnError: [" + e.errorCode + "] " + e.description);
                }

                @Override
                public void log(CoAPLogEvent e) {
                    System.out.println("Log: [" + e.logLevel + "] " + e.message);
                }

                @Override
                public void notification(CoAPNotificationEvent e) {
                    if (e.isLatest){
                        System.out.println("OnNotification: Received notification about resource at " + e.URI + ": [" +
                                coap.getResponseCode() + "] " + new String(coap.getResponseData()));
                    }
                    else{
                        System.out.println("OnNotification: Received out-of-date notification about resource at " + e.URI + "; ignoring.");
                    }
                }

                @Override
                public void register(CoAPRegisterEvent coapRegisterEvent) {
                    // TODO Auto-generated method stub
                }

                @Override
                public void request(CoAPRequestEvent coapRequestEvent) {
                    // TODO Auto-generated method stub
                }

                @Override
                public void requestComplete(CoAPRequestCompleteEvent e) {
                    if (e.errorCode == 0)
                        System.out.println("Request for " + e.URI + " completed successfully: [" +
                                coap.getResponseCode() + "] " + new String(coap.getResponseData()));
                    else if (e.errorCode == 709) // Indicates non-2.xx response code returned by server.
                        System.out.println("Request for " + e.URI + " completed unsuccessfully: [" +
                                coap.getResponseCode() + "] " + new String(coap.getResponseData()));
                    else
                        System.out.println("Request for " + e.URI + " failed with the following error: (" +
                                e.errorCode + ") " + e.errorDescription);
                }

                @Override
                public void responseComplete(CoAPResponseCompleteEvent e) {
                    // TODO Auto-generated method stub
                }

                @Override
                public void unregistered(CoAPUnregisteredEvent coapUnregisteredEvent) {
                    // TODO Auto-generated method stub
                }
            });
            String host = "localhost";
            int port = 5683;

            if (args.length == 2) {
                host = args[0];
                port = Integer.parseInt(args[1]);
            }
            else{
                host = prompt("Please enter the address of the CoAP server",":","localhost");
                port = Integer.parseInt(prompt("Please enter the server port",":","5683"));
            }

            char response = printPrompt();
            while(response!='7') {
                if(response=='1') { // Get Data
                    try {
                        String uri = getURI(host, port, "animals/domestic/cat");
                        coap.get(uri);
                    }catch (Exception e) {
                        displayError(e);
                    }
                }
                else if(response=='2') { // Post Data
                    try {
                        coap.setRequestContentFormat(0);
                        String uri = getURI(host, port, "animals/domestic/cat");
                        String requestBody = prompt("Request Body",":","Meow");
                        coap.setRequestData(requestBody);
                        coap.post(uri);
                    }catch (Exception e) {
                        displayError(e);
                    }
                }
                else if(response=='3') { // Put Node
                    try {
                        coap.setRequestContentFormat(0);
                        String uri = getURI(host, port, "animals/domestic/hamster");
                        String requestBody = prompt("Request Body",":","Squeak");
                        coap.setRequestData(requestBody);
                        coap.put(uri);
                    }catch (Exception e) {
                        displayError(e);
                    }
                }
                else if(response=='4') { // Delete Node
                    try {
                        coap.setRequestContentFormat(0);
                        String uri = getURI(host, port, "animals/domestic/dog");
                        coap.delete(uri);
                    }catch (Exception e) {
                        displayError(e);
                    }
                }
                else if(response=='5') { // Start Observation. Also notifies the client about the current state of the node
                    try {
                        coap.setRequestContentFormat(0);
                        String uri = getURI(host, port, "animals/domestic/cat");
                        System.out.println("Starting observation of " + uri + ".");
                        coap.startObserving(uri);
                    }catch (Exception e) {
                        displayError(e);
                    }
                }
                else if(response=='6') { // Stop Observation
                    try {
                        coap.setRequestContentFormat(0);
                        String uri = getURI(host, port, "animals/domestic/cat");
                        System.out.println("Stopping observation of " + uri + ".");
                        coap.stopObserving(uri);
                    }catch (Exception e) {
                        displayError(e);
                    }
                }
                else{
                    System.out.print("\nUnrecognized Command\n");
                }
                response = printPrompt();
            }

        } catch (Exception e) {
            displayError(e);
        }
    }

    public static char printPrompt(){
        return ask("Would you like to:\n" +
                        "\t1. Get Data\n" +
                        "\t2. Post Data\n" +
                        "\t3. Put Node\n" +
                        "\t4. Delete Node\n" +
                        "\t5. Start Observation\n" +
                        "\t6. Stop Observation\n" +
                        "\t7. Quit\n",
                "?", "");
    }

    public static String getURI(String host, int port, String defaultURI){
        String path = prompt("URI",":",defaultURI);
        return "coap://" + host + ":" + port + "/" + path.trim();
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




