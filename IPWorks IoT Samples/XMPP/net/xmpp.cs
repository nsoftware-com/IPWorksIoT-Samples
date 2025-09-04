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
using nsoftware.IPWorksIoT;

class xmppDemo
{
  private static XMPP xmpp = new nsoftware.IPWorksIoT.XMPP();
  private static bool messageReceived = false;

  static void Main(string[] args)
  {
    if (args.Length < 6)
    {
      Console.WriteLine("usage: xmpp /s server /u username /p password\n");
      Console.WriteLine("  server      the name or address of the XMPP server");
      Console.WriteLine("  username    the username used to authenticate to the XMPP server");
      Console.WriteLine("  password    the password used to authenticate to the XMPP server");
      Console.WriteLine("\nExample: xmpp /s talk.google.com /u myusername /p mypassword");
    }
    else
    {
      xmpp.AuthMethods = "SASL/PLAIN";
      xmpp.OnSSLServerAuthentication += xmpp_OnSSLServerAuthentication;
      xmpp.OnConnected += xmpp_OnConnected;
      xmpp.OnDisconnected += xmpp_OnDisconnected;
      xmpp.OnMessageIn += xmpp_OnMessageIn;

      try
      {
        System.Collections.Generic.Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);

        xmpp.IMServer = myArgs["s"];
        xmpp.User = myArgs["u"];
        xmpp.Password = myArgs["p"];

        Console.WriteLine("Connecting...");
        xmpp.Connect();

        int buddyCount = xmpp.Buddies.Count;
        Console.WriteLine("Buddy list:");

        if (buddyCount > 0)
        {
          for (int i = 0; i < buddyCount; i++)
          {
            Console.WriteLine(i + 1 + ") " + xmpp.Buddies[i].Id);
          }

          Console.Write("Select a buddy: ");
          int buddy = int.Parse(Console.ReadLine());

          Console.Write("Message to send to buddy: ");
          xmpp.MessageText = Console.ReadLine();

          xmpp.SendMessage(xmpp.Buddies[buddy - 1].Id);

          Console.WriteLine("Receiving responses...");
          while (!messageReceived)
          {
            xmpp.DoEvents();
          }

          Console.WriteLine("Disconnecting...");
          xmpp.Disconnect();
        }
      }
      catch (Exception ex)
      {
        Console.WriteLine(ex.Message);
      }
    }
  }

  #region "Events"

  private static void xmpp_OnSSLServerAuthentication(object sender, XMPPSSLServerAuthenticationEventArgs e)
  {
    if (e.Accept) return;
    Console.Write("Server provided the following certificate:\nIssuer: " + e.CertIssuer + "\nSubject: " + e.CertSubject + "\n");
    Console.Write("The following problems have been determined for this certificate: " + e.Status + "\n");
    Console.Write("Would you like to continue anyways? [y/n] ");
    if (Console.Read() == 'y') e.Accept = true;
  }

  private static void xmpp_OnConnected(object sender, XMPPConnectedEventArgs e)
  {
    Console.WriteLine("Welcome! Connection established!");
  }

  private static void xmpp_OnDisconnected(object sender, XMPPDisconnectedEventArgs e)
  {
    Console.WriteLine("Disconnected");
  }

  private static void xmpp_OnMessageIn(object sender, XMPPMessageInEventArgs e)
  {
    Console.WriteLine(e.From + " said: " + e.MessageText);
    messageReceived = true;
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