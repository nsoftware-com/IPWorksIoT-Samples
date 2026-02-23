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
import java.nio.charset.StandardCharsets;
import java.util.*;

import ipworksiot.*;

public class coapserver extends ConsoleDemo{

    private static TreeNode root;
    private static HashMap<String, List<String>> observedNodes;
    private static CoAP coap;

    private static class TreeNode {
        public String tag, text, path;
        public TreeNode parent;
        public List<TreeNode> children;

        TreeNode(String tag, String text){
            this.tag = tag;
            this.text = text;
            this.path = text;
            this.parent = null;
            this.children = new ArrayList<TreeNode>();
        }

        public void add(TreeNode node){
            node.parent = this;
            update(node);
            this.children.add(node);
        }

        public void remove(TreeNode node){
            for (TreeNode child : node.children){
                child.delete();
            }
            children.remove(node);
        }

        public void delete(){
            for (TreeNode child : this.children) {
                child.delete();
            }
        }

        public void update(TreeNode node){
            node.path = this.path + "/" + node.path;
            for (TreeNode child : node.children){
                update(child);
            }
        }
    }

    public static void main(String[] args) {
        System.out.println("*******************************************************************");
        System.out.println("* This demo shows how to use the CoAP component in server mode.   *");
        System.out.println("* It allows for get, post, put and delete requests to be          *");
        System.out.println("* received from a CoAP client, as well as observing a specific    *");
        System.out.println("* resource at a client-specified URI.                             *");
        System.out.println("* By default, data is stored in a tree with the following format: *");
        System.out.println("*                                                                 *");
        System.out.println("* <root>                                                          *");
        System.out.println("* +-- animals                                                     *");
        System.out.println("*     +--domestic                                                 *");
        System.out.println("*     |   +-- dog                                                 *");
        System.out.println("*     |   +-- cat                                                 *");
        System.out.println("*     |   +-- parakeet                                            *");
        System.out.println("*     +-- farm                                                    *");
        System.out.println("*     |   +-- cow                                                 *");
        System.out.println("*     |   +-- pig                                                 *");
        System.out.println("*     |   +-- sheep                                               *");
        System.out.println("*     +-- wild                                                    *");
        System.out.println("*         +-- lion                                                *");
        System.out.println("*         +-- tiger                                               *");
        System.out.println("*         +-- bear                                                *");
        System.out.println("*******************************************************************\n");
        coap = new CoAP();

        TreeNode treeNode22 = new TreeNode("Woof!", "dog");
        TreeNode treeNode23 = new TreeNode("Meow!", "cat");
        TreeNode treeNode24 = new TreeNode("Chirp!", "parakeet");
        TreeNode treeNode25 = new TreeNode("Domestic Animal Noises", "domestic");
        treeNode25.add(treeNode22);
        treeNode25.add(treeNode23);
        treeNode25.add(treeNode24);

        TreeNode treeNode26 = new TreeNode("Moo!", "cow");
        TreeNode treeNode27 = new TreeNode("Oink!", "pig");
        TreeNode treeNode28 = new TreeNode("Baa!", "sheep");
        TreeNode treeNode29 = new TreeNode("Farm Animal Noises", "farm");
        treeNode29.add(treeNode26);
        treeNode29.add(treeNode27);
        treeNode29.add(treeNode28);

        TreeNode treeNode30 = new TreeNode("Roar!", "lion");
        TreeNode treeNode31 = new TreeNode("*growl*", "tiger");
        TreeNode treeNode32 = new TreeNode("Oh My!", "bear");
        TreeNode treeNode33 = new TreeNode("Wild Animal Noises", "wild");
        treeNode33.add(treeNode30);
        treeNode33.add(treeNode31);
        treeNode33.add(treeNode32);

        TreeNode treeNode34 = new TreeNode("Root Node for animal noises", "animals");
        treeNode34.add(treeNode25);
        treeNode34.add(treeNode29);
        treeNode34.add(treeNode33);

        root = new TreeNode("Hello World!", "<root>");
        root.add(treeNode34);
        observedNodes = new HashMap<String, List<String>>();
        try {
        //add listeners
            coap.addCoAPEventListener(new DefaultCoAPEventListener() {
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
                public void register(CoAPRegisterEvent e) {
                    try {
                        System.out.println("OnRegister: Client " + e.remoteHost + ":" + e.remotePort + " attempting to register as an observer for " +
                                e.URI + "...");
                        String uriPath = e.URI.substring(e.URI.indexOf("://") + 3);
                        uriPath = uriPath.substring(uriPath.indexOf('/') + 1);
                        int qStartIdx = uriPath.indexOf('?');
                        String uriQuery = qStartIdx > 0 ? uriPath.substring(qStartIdx + 1) : null;
                        if (qStartIdx > 0) uriPath = uriPath.substring(0, qStartIdx);

                        String nodePath = URIPathToNodePath(uriPath);
                        TreeNode node = FindNode(nodePath);
                        if (node == null) {
                            coap.setResponseContentFormat(-1);
                            coap.setResponseData("");
                            coap.setResponseCode("4.04"); // "Not Found".
                            System.out.println("OnRegister: No such node found.");
                        } else {
                            coap.setResponseContentFormat(0); // "text/plain; charset=utf-8"
                            coap.setResponseData(GetNodeData(nodePath, uriQuery));
                            coap.setResponseCode("2.05"); // "Content".
                            if (IsObservableNode(nodePath)) {
                                List<String> obsURIs = observedNodes.get(nodePath) != null ? (List<String>) observedNodes.get(nodePath) : new ArrayList<String>();
                                obsURIs.add(e.URI);
                                observedNodes.put(nodePath, obsURIs);
                                e.accept = true;
                                System.out.println("OnRegister: Node found, registration accepted.");
                            } else {
                                // It's handled just like a GET request, e.Accept is false by default.
                                System.out.println("OnRegister: Node found, but observation not allowed, handling like normal GET request.");
                            }
                        }
                    } catch (Exception exception) {
                        displayError(exception);
                    }
                }

                @Override
                public void request(CoAPRequestEvent e) {
                    try{
                        String method = RequestMethodToString(e.method);
                        System.out.println("OnRequest: " + method + " request received from " + e.remoteHost + ":" + e.remotePort + " (URIHost=" + e.URIHost +
                                " URIPort=" + e.URIPort + " URIPath=" + e.URIPath + " URIQuery=" + e.URIQuery + " Id=" + e.requestId + ")...");

                        // Quick sanity check: query params are only allowed when GETing /other/query.
                        if (e.URIQuery != null && !e.URIQuery.isEmpty() || !method.equals("GET")) {
                            coap.setResponseContentFormat(-1);
                            coap.setResponseData("Query strings are not supported for the specified request method and/or resource."); // Diagnostic payload.
                            coap.setResponseCode("4.00"); // "Bad Request".
                            System.out.println("OnRegister: Node found, but query parameters not allowed.");
                        }

                        String nodePath = URIPathToNodePath(e.URIPath);
                        TreeNode node = FindNode(nodePath);
                        switch (method) {
                            case "GET":
                                if (node == null) {
                                    coap.setResponseContentFormat(-1);
                                    coap.setResponseData("");
                                    coap.setResponseCode("4.04"); // "Not Found".
                                    System.out.println("OnRequest: No such node found.");
                                } else {
                                    coap.setResponseContentFormat(0); // "text/plain; charset=utf-8"
                                    coap.setResponseData(GetNodeData(nodePath, e.URIQuery));
                                    coap.setResponseCode("2.05"); // "Content".
                                    System.out.println("OnRequest: Node found, returning data.");
                                }
                                return;
                            case "POST":
                            case "PUT":
                                if (IsReadOnlyNode(nodePath)) break;
                                if (coap.getRequestContentFormat() != 0) {
                                    coap.setResponseContentFormat(-1);
                                    coap.setResponseData("Request's Content-Format must be \"text/plain; charset=utf-8\"."); // Diagnostic payload.
                                    coap.setResponseCode("4.00"); // "Bad Request".
                                    System.out.println("OnRequest: Request's Content-Format is invalid (must be \"text/plain; charset=utf-8\").");
                                    return;
                                }
                                if (node == null) {
                                    TreeNode parent = FindParentNode(nodePath);
                                    if (parent == null) {
                                        coap.setResponseContentFormat(-1);
                                        coap.setResponseData("One or more parent nodes are missing."); // Diagnostic payload.
                                        coap.setResponseCode("4.00"); // "Bad Request".
                                        System.out.println("OnRequest: One or more parent nodes are missing.");
                                    } else {
                                        String newNodeName = nodePath.substring(nodePath.lastIndexOf('/') + 1);
                                        node = new TreeNode(new String(coap.getRequestData(), StandardCharsets.UTF_8), newNodeName);
                                        parent.add(node);

                                        coap.setResponseContentFormat(-1);
                                        coap.setResponseData("");
                                        coap.setResponseCode("2.01"); // "Created".
                                        System.out.println("OnRequest: Node created at " + nodePath + ".");
                                    }
                                } else {
                                    UpdateNode(node, new String(coap.getRequestData(), StandardCharsets.UTF_8));
                                    coap.setResponseContentFormat(-1);
                                    coap.setResponseData("");
                                    coap.setResponseCode("2.04"); // "Changed".
                                }
                                return;
                            case "DELETE":
                                if (node != null) {
                                    if (IsReadOnlyNode(nodePath)) break;
                                    DeleteNode(node);
                                }
                                coap.setResponseContentFormat(-1);
                                coap.setResponseData("");
                                coap.setResponseCode("2.02"); // "Deleted".
                                System.out.println("OnRequest: Node deleted (or did not exist in the first place).");
                                return;
                        }
                        // Default response: Request method not allowed.
                        coap.setResponseContentFormat(-1);
                        coap.setResponseData("The specified request method is not supported for this resource."); // Diagnostic payload.
                        coap.setResponseCode("4.05"); // "Method not allowed."
                        System.out.println("OnRequest: Method not allowed.");
                    } catch (Exception exception) {
                        displayError(exception);
                    }

                }

                @Override
                public void requestComplete(CoAPRequestCompleteEvent e) {
                    // TODO Auto-generated method stub
                }

                @Override
                public void responseComplete(CoAPResponseCompleteEvent e) {
                    System.out.println("OnResponseComplete: Response sent for request Id " + e.requestId + ".");
                }

                @Override
                public void unregistered(CoAPUnregisteredEvent e) {
                    System.out.println("OnUnregistered: Client " + e.remoteHost + ":" + e.remotePort + " is no longer observing " + e.URI + ".");
                    String uriPath = e.URI.substring(e.URI.indexOf("://") + 3);
                    uriPath = uriPath.substring(uriPath.indexOf('/') + 1);
                    int qStartIdx = uriPath.indexOf('?');
                    if (qStartIdx > 0) uriPath = uriPath.substring(0, qStartIdx);
                    String nodePath = URIPathToNodePath(uriPath);

                    List<String> obsURIs = null;
                    obsURIs = observedNodes.get(nodePath);
                    if (obsURIs != null) {
                        obsURIs.remove(e.URI);
                        if (obsURIs.size() == 0) observedNodes.remove(nodePath);
                    }
                }
            });
            int port = 5683;
            if (args.length == 1) {
                port = Integer.parseInt(args[0]);
            }
            else{
                port = Integer.parseInt(prompt("Please enter the port to listen on",":","5683"));
            }

            coap.setLocalPort(port);
            coap.setListening(true);
            System.out.println("\nServer is listening on port '" + coap.getLocalPort() + "'. Enter 'q' to quit.");

            char response = ' ';
            while(true) {
                response = read();
                if (response != 'q'){
                    System.out.println("\nUnrecognized Command. Enter 'q' to quit.");
                }
                else{
                    break;
                }
                System.out.println("\nServer is listening. Enter 'q' to quit.");
            }
        } catch (Exception e) {
            displayError(e);
        }
    }

    private static boolean IsReadOnlyNode(String nodePath) {
        return nodePath.equals("<root>");
    }

    private static TreeNode FindNode(String nodePath) {
        return FindNode(nodePath, root);
    }

    private static TreeNode FindNode(String nodePath, TreeNode curr) {
        for (TreeNode node : curr.children) {
            if (node.path.equals(nodePath)) return node;
            if (nodePath.startsWith(node.path + "/")) return FindNode(nodePath, node);
        }
        return null;
    }

    private static void UpdateNode(TreeNode node, String newData) {
        node.tag = newData;
        notifyIfNodeObserved(node, false);
    }

    private static void DeleteNode(TreeNode node) {
        notifyIfChildrenObserved(node);
        notifyIfNodeObserved(node, true);
        node.parent.children.remove(node);
        node.delete();
    }

    private static boolean IsObservableNode(String nodePath) {
        return !nodePath.equals("<root>") && !nodePath.equals("<root>/system") && !nodePath.equals("<root>/other") && !nodePath.startsWith("<root>/other/");
    }

    private static TreeNode FindParentNode(String nodePath) throws Exception {
        int index = nodePath.lastIndexOf('/');
        if (index < 0) throw new Exception("Invalid node path \"" + nodePath + "\" passed to FindParentNode()!");
        return FindNode(nodePath.substring(0, index));
    }

    private static String URIPathToNodePath(String uriPath) {
        return uriPath.isEmpty() ? "<root>" : "<root>/" + uriPath;
    }

    private static String GetNodeData(String nodePath, String uriQuery) {
        TreeNode node = FindNode(nodePath);
        if (node == null) return null;
        return (String) node.tag;
    }

    private static void notifyIfNodeObserved(TreeNode node, boolean deleted) {
        try {
            if (!observedNodes.containsKey(node.path)) return;
            List<String> observedURIs = observedNodes.get(node.path);

            if (!deleted) {
                coap.setResponseContentFormat(0); // "text/plain; charset=utf-8"
                coap.setResponseData(node.tag);
                coap.setResponseCode("2.05"); // "Content".
            } else {
                coap.setRequestContentFormat(-1);
                coap.setResponseData("");
                coap.setResponseCode("4.04"); // "Not Found".
            }

            sendNotification(observedURIs);
            if (deleted) observedNodes.remove(node.path);
        } catch (Exception e) {
            displayError(e);
        }
    }

    // Unlike NotifyIfNodeObserved, this method is only ever called to notify about deletions.
    private static void notifyIfChildrenObserved(TreeNode node) {
        try {
            coap.setRequestContentFormat(-1);
            coap.setResponseData("");
            coap.setResponseCode("4.04"); // "Not Found".

            for (TreeNode child : node.children) {
                if (observedNodes.containsKey(child.path)) {
                    sendNotification(observedNodes.get(child.path));
                    observedNodes.remove(child.path);
                    notifyIfChildrenObserved(child);
                }
            }
        } catch (Exception e) {
            displayError(e);
        }
    }

    private static void sendNotification(List<String> uris) {
        try {
            // uris can contain duplicates, so we'll keep track of the URIs we've notified.
            HashSet<String> notifiedURIs = new HashSet<String>();
            for (String uri : uris) {
                // Only call coap.SendNotification() if we haven't already notified the current URI.
                if (notifiedURIs.add(uri)) coap.sendNotification(uri);
            }
        } catch (Exception e) {
            displayError(e);
        }
    }

    private static String RequestMethodToString(int methodCode) {
        switch (methodCode) {
            case 1: return "GET";
            case 2: return "POST";
            case 3: return "PUT";
            case 4: return "DELETE";
            default: return "<other>";
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




