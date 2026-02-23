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
observed_nodes = {}

class TreeNode:
  def __init__(self, tag, text):
    self.tag = tag
    self.text = text
    self.path = text
    self.parent = None
    self.children = []

  def add(self, node):
    node.parent = self
    self.update(node)
    self.children.append(node)

  def remove(self, node):
    for child in node.children:
      child.delete()
    self.children.remove(node)

  def delete(self):
    for child in self.children:
      child.delete()

  def update(self, node):
    node.path = self.path + "/" + node.path
    for child in node.children:
      self.update(child)

treeNode22 = TreeNode("Woof!", "dog")
treeNode23 = TreeNode("Meow!", "cat")
treeNode24 = TreeNode("Chirp!", "parakeet")
treeNode25 = TreeNode("Domestic Animal Noises", "domestic")
treeNode25.add(treeNode22)
treeNode25.add(treeNode23)
treeNode25.add(treeNode24)

treeNode26 = TreeNode("Moo!", "cow")
treeNode27 = TreeNode("Oink!", "pig")
treeNode28 = TreeNode("Baa!", "sheep")
treeNode29 = TreeNode("Farm Animal Noises", "farm")
treeNode29.add(treeNode26)
treeNode29.add(treeNode27)
treeNode29.add(treeNode28)

treeNode30 = TreeNode("Roar!", "lion")
treeNode31 = TreeNode("*growl*", "tiger")
treeNode32 = TreeNode("Oh My!", "bear")
treeNode33 = TreeNode("Wild Animal Noises", "wild")
treeNode33.add(treeNode30)
treeNode33.add(treeNode31)
treeNode33.add(treeNode32)

treeNode34 = TreeNode("Root Node for animal noises", "animals")
treeNode34.add(treeNode25)
treeNode34.add(treeNode29)
treeNode34.add(treeNode33)

root = TreeNode("Hello World!", "<root>")
root.add(treeNode34)

def fire_error(e):
  print("OnError: [" + str(e.error_code) + "] " + e.description)

def fire_log(e):
  print("Log: [" + str(e.log_level) +"] " + e.message)

def fire_notification(e):
  if (e.isLatest):
    print("OnNotification: Recieved notification about recourse at "  + e.uri + ": [" + coap.get_response_code() +
          "] " + coap.get_response_data())
  else:
    print("OnNotification: Received out-of-date notification about resource at " + e.uri + "; ignoring.")

def fire_register(e):
  try:
    print("OnRegister: Client " + e.remote_host + ":" + str(e.remote_port) + " attempting to register as an observer for " + e.uri + "...")
    uripath = e.uri[(e.uri.index("://")+3):]
    uripath = uripath[(uripath.index("/")+1):]
    nodepath = uripath_to_nodepath(uripath)
    uriquery = ""
    if '?' in uripath:
      q_start_idx = uripath.index('?')
      if q_start_idx > 0 :
        uriquery = uripath[q_start_idx + 1:]
    node = find_node(nodepath)

    if node == None:
      coap.set_response_content_format(-1)
      coap.set_response_data("")
      coap.set_response_code("4.04")
      print("OnRegister: No such node found.")
    else:
      coap.set_response_content_format(0)
      coap.set_response_data(get_node_data(nodepath, uriquery))
      coap.set_response_code("2.05")
      if (is_observable_node(nodepath)):
        if nodepath in observed_nodes:
          obsURIs = observed_nodes[nodepath]
        else:
          obsURIs = []
        obsURIs.append(e.uri)
        observed_nodes[nodepath] = obsURIs
        e.accept = True
        print("OnRegister: Node found, registration accepted.")
      else:
        #It's handled just like a GET request, e.Accept is false by default.
        print("OnRegister: Node found, but observation not allowed, handling like normal GET request.");
  except IPWorksIoTError as error:
    print("Error creating sender link: %i - %s" % (error.code, error.message))
    exit()

def fire_request(e):
  try:
    method = request_method_to_string(e.method)
    print("OnRequest: " + method + " request received from " + e.remote_host + ":" + str(e.remote_port) + " (URIHost=" + e.uri_host +
          " URIPort=" + str(e.uri_port) + " URIPath=" + e.uri_path + " URIQuery=" + e.uri_query + " Id=" + e.request_id + ")...")

    nodepath = uripath_to_nodepath(e.uri_path)
    node = find_node(nodepath)

    if method == "GET":
      if (node == None):
        coap.set_response_content_format(-1)
        coap.set_response_data("")
        coap.set_response_code("4.04") # "Not Found".
        print("OnRequest: No such node found.")
      else:
        coap.set_response_content_format(0)
        coap.set_response_data(get_node_data(nodepath, e.uri_query))
        coap.set_response_code("2.05") # "Content".
        print("OnRequest: Node found, returning data.")
      return
    elif method == "POST" or method == "PUT":
      if is_read_only_node(nodepath):
        pass
      elif coap.get_request_content_format() != 0:
        coap.set_response_content_format(-1)
        coap.set_response_data("Request's Content-Format must be \"text/plain; charset=utf-8\".") # Diagnostic payload.
        coap.set_response_code("4.00") # "Bad Request".
        print("OnRequest: Request's Content-Format is invalid (must be \"text/plain; charset=utf-8\").")
        return
      elif node == None:
        parent = find_parent_node(nodepath)
        if (parent == None):
          coap.set_response_content_format(-1)
          coap.set_response_data("One or more parent nodes are missing")  # Diagnostic payload.
          coap.set_response_code("4.00") # "Bad Request".
          print("OnRequest: One or more parent nodes are missing.")
        else:
          new_node_name = nodepath[nodepath.rfind('/') + 1:]
          node = TreeNode(coap.get_request_data(), new_node_name)
          parent.add(node)

          coap.set_response_content_format(-1)
          coap.set_response_data("")
          coap.set_response_code("2.01")  # "Created".
          print("OnRequest: Node created at " + nodepath + ".")
      else:
        update_node(node, coap.get_request_data())
        coap.set_response_content_format(-1)
        coap.set_response_data("")
        coap.set_response_code("2.04")  # "Changed".
      return
    if method == "DELETE":
      if node != None:
        if is_read_only_node(nodepath):
          coap.set_response_content_format(-1)
          coap.set_response_data(
            "The specified request method is not supported for this resource.")  # Diagnostic payload.
          coap.set_response_code("4.05")  # "Method not allowed."
          print("OnRequest: Method not allowed.")
          return
        delete_node(node)
      coap.set_response_content_format(-1)
      coap.set_response_data("")
      coap.set_response_code("2.02")  # "Deleted".
      print("OnRequest: Node deleted (or did not exist in the first place).")
      return
    # Default response: Request method not allowed.
    coap.set_response_content_format(-1)
    coap.set_response_data("The specified request method is not supported for this resource.") # Diagnostic payload.
    coap.set_response_code("4.05") # "Method not allowed."
    print("OnRequest: Method not allowed.")
  except IPWorksIoTError as error:
    print("Error creating sender link: %i - %s" % (error.code, error.message))
    exit()

def fire_request_complete(e):
  if e.errorCode == 0:
    print("Request for " + e.uri + " completed successfully: [" + coap.get_response_code() + "] " + coap.get_responsedata())
  elif e.errorCode == 709:
    print("Request for " + e.uri + " completed unsuccessfully: [" + coap.get_response_code() + "] " + coap.get_responsedata())
  else:
    print("Request for " + e.uri + " failed with the following error: (" + e.error_code + ") " + e.error_description)

def fire_response_complete(e):
  print("OnResponseComplete: Response sent for request Id " + e.request_id + ".")

def fire_unregistered(e):
  print("OnUnregistered: Client " + e.remote_host + ":" + str(e.remote_port) + " is no longer observing " + e.uri + ".")
  uripath = e.uri[(e.uri.index("://") + 3):]
  uripath = uripath[(uripath.index("/") + 1):]
  nodepath = uripath_to_nodepath(uripath)
  uriquery = ""
  if '?' in uripath:
    q_start_idx = uripath.index('?')
    if q_start_idx > 0:
      uriquery = uripath[q_start_idx + 1:]
  node = find_node(nodepath)
  nodepath = uripath_to_nodepath(uripath)

  obsURIs = None
  obsURIs = observed_nodes[nodepath]
  if obsURIs != None:
    obsURIs.remove(e.uri)
    if len(obsURIs) == 0:
      observed_nodes.pop(nodepath)

def is_read_only_node(nodepath):
  return nodepath == "<root>"

def find_node(nodepath):
  return find_node_(nodepath, root)

def find_node_(nodepath, curr):
  for node in curr.children:
    if node.path == nodepath:
      return node
    if nodepath.startswith(node.path + "/"):
      return find_node_(nodepath, node)
  return None

def update_node(node, new_data):
  node.tag = new_data
  notify_if_node_observed(node, False)

def delete_node(node):
  notify_if_children_observed(node)
  notify_if_node_observed(node, True)
  node.parent.children.remove(node)
  node.delete()

def is_observable_node(nodepath):
  return nodepath != "<root>"

def find_parent_node(nodepath):
  index = nodepath.rfind('/')
  if (index < 0):
    print("Invalid node path \"" + nodepath + "\" passed to find_parent_node()!")
    exit()
  return find_node(nodepath[0:index])

def uripath_to_nodepath(uripath):
  if uripath == "":
    return "<root>"
  else:
    return "<root>/" + uripath

def get_node_data(nodepath, uri_query):
  node = find_node(nodepath)
  if (node == None):
    return None
  return node.tag

def notify_if_node_observed(node, deleted):
  try:
    if not node.path in observed_nodes:
      return
    observed_URIs = observed_nodes[node.path]

    if not deleted:
      coap.set_response_content_format(0) # "text/plain; charset=utf-8"
      coap.set_response_data(node.tag)
      coap.set_response_code("2.05") # "Content".
    else:
      coap.set_response_content_format(-1)
      coap.set_response_data("")
      coap.set_response_code("4.04")

    send_notification(observed_URIs)
    if deleted:
      observed_node.remove(node.path)
  except IPWorksIoTError as error:
    print("Error creating sender link: %i - %s" % (error.code, error.message))
    exit()

# Unlike NotifyIfNodeObserved, this method is only ever called to notify about deletions.
def notify_if_children_observed(node):
  try:
    coap.set_response_content_format(-1)
    coap.set_response_data("")
    coap.set_response_code("4.04") # "Not Found"

    for child in node.children:
      if child.path in observed_nodes:
        send_notification(observed_nodes[child.path])
        observed_nodes.remove(child.path)
        notify_if_children_observed(child)
  except IPWorksIoTError as error:
    print("Error creating sender link: %i - %s" % (error.code, error.message))
    exit()

def send_notification(uris):
  try:
    notified_uris = []
    for uri in uris:
      if not uri in notified_uris:
        notified_uris.append(uri)
        coap.send_notification(uri)
  except IPWorksIoTError as error:
    print("Error creating sender link: %i - %s" % (error.code, error.message))
    exit()

def request_method_to_string(code):
  if code == 1:
    return "GET"
  elif code == 2:
    return "POST"
  elif code == 3:
    return "PUT"
  if code == 4:
    return "DELETE"
  else:
    return "<other>"


print("*******************************************************************")
print("* This demo shows how to use the CoAP component in server mode.   *")
print("* It allows for get, post, put and delete requests to be          *")
print("* received from a CoAP client, as well as observing a specific    *")
print("* resource at a client-specified URI.                             *")
print("* By default, data is stored in a tree with the following format: *")
print("*                                                                 *")
print("* <root>                                                          *")
print("* +-- animals                                                     *")
print("*     +--domestic                                                 *")
print("*     |   +-- dog                                                 *")
print("*     |   +-- cat                                                 *")
print("*     |   +-- parakeet                                            *")
print("*     +-- farm                                                    *")
print("*     |   +-- cow                                                 *")
print("*     |   +-- pig                                                 *")
print("*     |   +-- sheep                                               *")
print("*     +-- wild                                                    *")
print("*         +-- lion                                                *")
print("*         +-- tiger                                               *")
print("*         +-- bear                                                *")
print("*******************************************************************\n")

port = input("Please enter the port on which the server will listen (default: 5683)?")
if (port != ""):
  port = int(port)
else:
  port = 5683
host = "localhost"
port = 5683 #TODO: Remove This

coap.on_error = fire_error
coap.on_log = fire_log
coap.on_notification = fire_notification
coap.on_register = fire_register
coap.on_request = fire_request
coap.on_request_complete = fire_request_complete
coap.on_response_complete = fire_response_complete
coap.on_unregistered = fire_unregistered

coap.set_local_port(port)
coap.set_local_host(host)
coap.set_listening(True)

print ("\nServer is listening at " + coap.get_local_host() + ":" + str(coap.get_local_port()) + ". Enter Ctrl-C to quit.\n")
while True:
  coap.do_events()





