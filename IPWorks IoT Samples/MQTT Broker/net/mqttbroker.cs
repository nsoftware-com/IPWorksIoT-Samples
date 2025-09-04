/*
 * IPWorks IoT 2024 .NET Edition - Sample Project
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
 * 
 */

﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using nsoftware.IPWorksIoT;

class MQTTBrokerDemo : ConsoleDemo
{
  private static Dictionary<string, string> myArgs;
  private static MQTTBroker broker;
  private static int port = 1883;
  private static byte[] sslCertStore = null;
  private static string sslCertPass = null;

  static void Main(string[] args)
  {
    if (args.Length > 0)
    {
      try
      {
        myArgs = ParseArgs(args);

        if (myArgs.ContainsKey("help")) throw new Exception("Info");

        if (myArgs.ContainsKey("cert") || myArgs.ContainsKey("pass"))
        {
          if (myArgs.ContainsKey("cert") && myArgs.ContainsKey("pass"))
          {
            sslCertStore = Encoding.UTF8.GetBytes(myArgs["cert"]);
            sslCertPass = myArgs["pass"];
            port = 8883;
          }
          else throw new Exception("If the 'cert' argument is passed the 'pass' argument should be passed as well!");
        }
        
        if (myArgs.ContainsKey("port")) port = int.Parse(myArgs["port"]);
      }
      catch (Exception ex)
      {
        Console.WriteLine(ex.Message + "\n");
        Console.WriteLine("usage: mqttbroker [/port portNr] [/cert certStore] [/pass certStorePassword] /help\n");
        Console.WriteLine("   help                   prints this message");
        Console.WriteLine("   portNr                 the port to listen on (optional, default 1883)");
        Console.WriteLine("   certStore              the path of the certificate (optional)");
        Console.WriteLine("   certStorePassword      the password of the certificate (should be included if the 'cert' argument is passed)");
        Console.WriteLine("\nExample mqttbroker /port 2222 /cert path/to/certStore /pass password");

        return;
      }
    }
    
    broker = new MQTTBroker();

    broker.OnConnectionRequest += Broker_OnConnectionRequest;
    broker.OnConnected += Broker_OnConnected;
    broker.OnDisconnected += Broker_OnDisconnected;
    broker.OnSessionRequest += Broker_OnSessionRequest;
    broker.OnSessionRemoved += Broker_OnSessionRemoved;
    broker.OnSubscribe += Broker_OnSubscribe;
    broker.OnUnsubscribe += Broker_OnUnsubscribe;
    broker.OnMessageReceived += Broker_OnMessageReceived;
    broker.OnMessageSent += Broker_OnMessageSent;
    broker.OnSSLClientAuthentication += Broker_OnSSLClientAuthentication;

    Task task = new Task(() => { while (true) broker.DoEvents(); });

    try
    {
      if (sslCertStore == null || sslCertPass == null)
        broker.AddBrokerListener("127.0.0.1", port, (int)MQTTBrokerProtocols.mbpTCP, (int)CertStoreTypes.cstUser, null, null, null);
      else
        broker.AddBrokerListener("127.0.0.1", port, (int)MQTTBrokerProtocols.mbpTLS, (int)CertStoreTypes.cstAuto, sslCertStore, sslCertPass, "*");

      broker.StartListening();
      task.Start();

      Console.WriteLine("MQTT Broker started on port " + port + "...");
      Console.WriteLine("Type \"?\" for a list of commands.");
      Console.Write("mqttbroker> ");
      
      string command;
      string[] arguments;

      while (true)
      {
        command = Console.ReadLine();
        arguments = command.Split();

        if (arguments[0].Equals("?") || arguments[0].Equals("help"))
        {
          Console.WriteLine("Commands: ");
          Console.WriteLine("  ?                            display the list of valid commands");
          Console.WriteLine("  help                         display the list of valid commands");
          Console.WriteLine("  users                        list all currently connected users");
          Console.WriteLine("  sessions                     list all currently active sessions");
          Console.WriteLine("  disconnect <id>              disconnect client by connection id");
          Console.WriteLine("  quit                         exit the application");
        }
        else if (arguments[0].Equals("quit") || arguments[0].Equals("exit"))
        {
          broker.Shutdown();
          Console.WriteLine("Broker has stopped!");
          break;
        }
        else if (arguments[0].Equals("users"))
        {
          foreach (MQTTBrokerConnection conn in broker.Connections.Values)
            Console.WriteLine(conn.ConnectionId);
        }
        else if (arguments[0].Equals("sessions"))
        {
          foreach (MQTTBrokerSession session in broker.Sessions)
            Console.WriteLine("Client with ID " + session.ClientId + " is subscribed to " + session.SubTopicFilters + "! Qos: " + session.SubGrantedQoS);
        }
        else if (arguments[0].Equals("disconnect"))
        {
          if (arguments.Length > 1) broker.Disconnect(arguments[1]);
        }
        else if (arguments[0].Equals(""))
        {
          // Do nothing.
        }
        else Console.WriteLine("Invalid command.");
        
        Console.Write("mqttbroker> ");
      }
    }
    catch (Exception ex) 
    { 
      Console.WriteLine("Error: " + ex.Message);
      broker.Shutdown();
      Console.WriteLine("press any key to quit!");
      Console.Read();
    }
  }

  #region Events
  private static void Broker_OnSSLClientAuthentication(object sender, MQTTBrokerSSLClientAuthenticationEventArgs e)
  {
    // Here you can decide if you want to authenticate the client via TLS/SSL
    e.Accept = true;
  }

  private static void Broker_OnMessageSent(object sender, MQTTBrokerMessageSentEventArgs e)
  {
    Console.WriteLine("Client [" + e.ConnectionId + "] has received a message!");
    Console.WriteLine("ID: " + e.MessageId);
    Console.WriteLine("Client ID: " + e.ClientId);
  }

  private static void Broker_OnMessageReceived(object sender, MQTTBrokerMessageReceivedEventArgs e)
  {
    Console.WriteLine("Client [" + e.ConnectionId + "] has sent a message!");
    Console.WriteLine("ID: " + e.MessageId);
    Console.WriteLine("Client ID: " + e.ClientId);
  }

  private static void Broker_OnUnsubscribe(object sender, MQTTBrokerUnsubscribeEventArgs e)
  {
    Console.WriteLine("Client [" + e.ConnectionId + "] has unsubscribed to the " + e.TopicFilter + "topic!");
    Console.WriteLine("ID: " + e.ClientId);
  }

  private static void Broker_OnSubscribe(object sender, MQTTBrokerSubscribeEventArgs e)
  {
    Console.WriteLine("Client [" + e.ConnectionId + "] has subscribed to topic: " + e.TopicFilter);
    Console.WriteLine("ID: " + e.ClientId + "\nRequested QoS: " + e.RequestedQoS);
  }

  private static void Broker_OnDisconnected(object sender, MQTTBrokerDisconnectedEventArgs e)
  {
    Console.WriteLine("Client [" + e.ConnectionId + "] has disconnected with status: " + e.StatusCode + "/" + e.Description);
  }

  private static void Broker_OnSessionRemoved(object sender, MQTTBrokerSessionRemovedEventArgs e)
  {
    Console.WriteLine("Client with ID " + e.ClientId + " has removed the session!");
  }

  private static void Broker_OnSessionRequest(object sender, MQTTBrokerSessionRequestEventArgs e)
  {
    Console.WriteLine("Client [" + e.ConnectionId + "] has requested a session!\nClient ID is: " + e.ClientId);
    // Here you can decide if you want to grant the client a session
    e.Accept = true;
  }

  private static void Broker_OnConnectionRequest(object sender, MQTTBrokerConnectionRequestEventArgs e)
  {
    Console.WriteLine("Client [" + e.Address + ":" + e.Port + "] has requested a connection!");
    // Here you can decide if you want to establish a connection with the client
    e.Accept = true;
  }

  private static void Broker_OnConnected(object sender, MQTTBrokerConnectedEventArgs e)
  {
    Console.WriteLine("Client [" + e.ConnectionId + "] has connected with status: " + e.StatusCode + "/" + e.Description);
  }
  #endregion
}




class ConsoleDemo
{
  /// <summary>
  /// Takes a list of switch arguments or name-value arguments and turns it into a dictionary.
  /// </summary>
  public static System.Collections.Generic.Dictionary<string, string> ParseArgs(string[] args)
  {
    System.Collections.Generic.Dictionary<string, string> dict = new System.Collections.Generic.Dictionary<string, string>();

    for (int i = 0; i < args.Length; i++)
    {
      // Add a key to the dictionary for each argument.
      if (args[i].StartsWith("/"))
      {
        // If the next argument does NOT start with a "/", then it is a value.
        if (i + 1 < args.Length && !args[i + 1].StartsWith("/"))
        {
          // Save the value and skip the next entry in the list of arguments.
          dict.Add(args[i].ToLower().TrimStart('/'), args[i + 1]);
          i++;
        }
        else
        {
          // If the next argument starts with a "/", then we assume the current one is a switch.
          dict.Add(args[i].ToLower().TrimStart('/'), "");
        }
      }
      else
      {
        // If the argument does not start with a "/", store the argument based on the index.
        dict.Add(i.ToString(), args[i].ToLower());
      }
    }
    return dict;
  }
  /// <summary>
  /// Asks for user input interactively and returns the string response.
  /// </summary>
  public static string Prompt(string prompt, string defaultVal)
  {
    Console.Write(prompt + (defaultVal.Length > 0 ? " [" + defaultVal + "]": "") + ": ");
    string val = Console.ReadLine();
    if (val.Length == 0) val = defaultVal;
    return val;
  }
}