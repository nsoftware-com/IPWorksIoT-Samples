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

public class amqp extends ConsoleDemo{

    public static void main(String[] args) {
        System.out.println("*******************************************************");
        System.out.println("* This is a demo for the IP*Works IoT AMQP Class.     *");
        System.out.println("* It allows simple message sending and receiving.     *");
        System.out.println("*******************************************************");
        AMQP amqp = new AMQP();
        
        
        try {
        //add listeners
        	amqp.addAMQPEventListener(new AMQPEventListener() {
    			
    			@Override
    			public void messageOutcome(AMQPMessageOutcomeEvent arg0) {
    				// TODO Auto-generated method stub
    				
    			}
    			
    			@Override
    			public void messageOut(AMQPMessageOutEvent arg0) {
    				// TODO Auto-generated method stub
    				
    			}
    			
    			@Override
    			public void messageIn(AMQPMessageInEvent arg0) {
    				System.out.println("\nMessage Received: "+amqp.getReceivedMessage().getValue());
    				
    			}
    			
    			@Override
    			public void log(AMQPLogEvent arg0) {
    				// TODO Auto-generated method stub
    				
    			}
    			
    			@Override
    			public void linkReadyToSend(AMQPLinkReadyToSendEvent arg0) {
    				// TODO Auto-generated method stub
    				
    			}
    			
    			@Override
    			public void error(AMQPErrorEvent arg0) {
    				// TODO Auto-generated method stub
    				
    			}
    			
    			@Override
    			public void disconnected(AMQPDisconnectedEvent arg0) {
    				if(arg0.statusCode!=0) {
						System.out.println("Error disconnecting: "+arg0.statusCode+" - "+arg0.description);
					}
    				System.exit(0);
    			}
    			
    			@Override
    			public void connectionStatus(AMQPConnectionStatusEvent arg0) {
    				// TODO Auto-generated method stub
    				
    			}
    			
    			@Override
    			public void connected(AMQPConnectedEvent arg0) {
    				System.out.println("Connected.");
    				
    			}
    			
    			@Override
    			public void SSLStatus(AMQPSSLStatusEvent arg0) {
    				// TODO Auto-generated method stub
    				
    			}
    			
    			@Override
    			public void SSLServerAuthentication(AMQPSSLServerAuthenticationEvent arg0) {
    				arg0.accept=true;
    				
    			}
    		});
        	
        //configure settings
        
        //set container id, ssl enabled and remote host
        amqp.setContainerId("ContainerId");
        boolean ssl = Boolean.valueOf(ask("Use SSL","?")=='y');
        amqp.setSSLEnabled(ssl);
        amqp.setRemoteHost(prompt("Remote Host"));
        
        //prompt for remote port
        String defPort = "5672";
        if(ssl) {
        	defPort = "5671"; 
    	}
        amqp.setRemotePort(Integer.valueOf(prompt("Remote Port",":",defPort)));
        
        //prompt for user and password
        amqp.setUser(prompt("User"));
        amqp.setPassword(prompt("Password"));
        
        //connect to host
        amqp.connect();
        
        //prompt for automatic or retrieve flow
        char flow = ' ';
        while(flow!='a'&&flow!='f') {
    		flow = ask("Choose (a)utomatic or (f)etch flow",":","a/f");
        }
        if(flow=='f') {
        	amqp.setReceiveMode(AMQP.rmRetrieve);
            amqp.setRetrieveTimeout(5);
        }
        
        //set session id and create sender link with a unique link name and name of target node (at most once flow by default, settled = true)
        amqp.createSession("SessionId");
        amqp.createSenderLink("SessionId", "SenderLinkName", "TargetName");
        
        //create receiver link with existing session ID, unique link name, and name of the target node        
        amqp.createReceiverLink("SessionId", "ReceiverLinkName", "TargetName");
        
        char response = ask("\nWould you like to (s)end message, (f)etch message, or (q)uit","?","s/f/q");
        while(response!='q') {
        	if(response=='s') {
        		amqp.resetMessage();
        		amqp.getMessage().setValueType(17); //string
        		amqp.getMessage().setValue(prompt("Enter message"));
        		amqp.sendMessage("SenderLinkName");
        		System.out.println("Message sent.\n");
        	} else if(response=='f') {
        		System.out.println("Retrieveing message...");
        		try {
        		amqp.retrieveMessage("ReceiverLinkName");
        		} catch(IPWorksIoTException e) {
        			if(e.getCode()==201) {
        				System.out.println("Timeout. No message received.");
        			} else {
        				displayError(e);
        			}
        		}
        	}
        	response = ask("Would you like to (s)end message, (f)etch message, or (q)uit","?","s/f/q");
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




