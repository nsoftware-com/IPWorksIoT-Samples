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

public class AMQPDemo
{
  private static AMQP amqp = new AMQP();
  private static bool msgReceived = false;

  static void Main(string[] args)
  {
    Console.WriteLine("*******************************************************************************\n");
    Console.WriteLine("* This demo shows how to use the AMQP component to send and receive messages. *\n");
    Console.WriteLine("*******************************************************************************\n\n");

    amqp.OnMessageIn += FireMessageIn;
    amqp.OnSSLServerAuthentication += FireSSLServerAuthentication;

    amqp.ContainerId = "ContainerId";

    var use_ssl = GetInput("Use SSL? (y/n)");
    while (use_ssl != "y" && use_ssl != "n")
    {
      use_ssl = GetInput("Use SSL? Please type 'y' or 'n'");
    }
    amqp.SSLEnabled = use_ssl == "y";

    var remote_host = GetInput("Remote Host", "localhost");
    var remote_port = GetInput("Remote Port", amqp.SSLEnabled ? "5671" : "5672");

    amqp.User = GetInput("User");
    amqp.Password = GetInput("Password");

    try
    {
      amqp.ConnectTo(remote_host, int.Parse(remote_port));
    }
    catch (IPWorksIoTException error)
    {
      Console.WriteLine($"Error connecting: {error.Code} - {error.Message}");
      Environment.Exit(0);
    }

    amqp.CreateSession("SessionId");

    try
    {
      amqp.CreateSenderLink("SessionId", "SenderLinkName", "TargetName");
    }
    catch (IPWorksIoTException error)
    {
      Console.WriteLine($"Error creating sender link: {error.Code} - {error.Message}");
      Environment.Exit(0);
    }

    amqp.ReceiveMode = AMQPReceiveModes.rmRetrieve;
    amqp.RetrieveTimeout = 5;

    try
    {
      amqp.CreateReceiverLink("SessionId", "ReceiverLinkName", "TargetName");
    }
    catch (IPWorksIoTException error)
    {
      Console.WriteLine($"Error creating receiver link: {error.Code} - {error.Message}");
      Environment.Exit(0);
    }

    var command = "";
    while (command != "q")
    {
      if (command == "s")
      {
        amqp.ResetMessage();
        amqp.Message.ValueType = AMQPValueTypes.mvtString;
        amqp.Message.Value = GetInput("Enter message to send", "Hello!");

        try
        {
          amqp.SendMessage("SenderLinkName");
        }
        catch (IPWorksIoTException error)
        {
          Console.WriteLine($"Error sending message: {error.Code} - {error.Message}");
          Environment.Exit(0);
        }
        Console.WriteLine("Message sent");
      }
      else if (command == "f")
      {
        Console.WriteLine("Retrieveing message...");
        try
        {
          amqp.RetrieveMessage("ReceiverLinkName");
        }
        catch (IPWorksIoTException error)
        {
          if (error.Code == 201)
          {
            Console.WriteLine("Timeout - no message received");
          }
          else
          {
            Console.WriteLine($"Error sending message: {error.Code} - {error.Message}");
            Environment.Exit(0);
          }
        }
      }
      command = GetInput("Choose send message (s), retrieve message (f), or quit (q)", "q");
    }
  }

  private static void FireMessageIn(object sender, AMQPMessageInEventArgs e)
  {
    Console.WriteLine($"Incoming Message from <{e.LinkName}>: {amqp.ReceivedMessage.Value}\n");
    msgReceived = true;
  }

  private static void FireSSLServerAuthentication(object sender, AMQPSSLServerAuthenticationEventArgs e)
  {
    e.Accept = true;
  }


  private static string GetInput(string prompt, string defaultVal = "")
  {
    Console.Write($"{prompt}{(defaultVal != "" ? $"[{defaultVal}]" : "")}: ");
    var result = Console.ReadLine();
    if (string.IsNullOrEmpty(result))
    {
      result = defaultVal;
    }
    return result;
  }
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