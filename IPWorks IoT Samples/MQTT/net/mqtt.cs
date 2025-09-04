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

﻿
using System;
using nsoftware.IPWorksIoT;

class Program
{
  private static MQTT mqtt;
  private static string topic;
  private static bool topicUnset = true;

  static void Main(string[] args)
  {
    mqtt = new MQTT();
    mqtt.OnMessageIn += MQTT_OnMessageIn;

    string host = "test.mosquitto.org";
    int port = 1883;
    topic = "nsoftware/test";
    int qos = 1;

    try
    {
      Console.WriteLine($"Connecting to {host}");
      mqtt.ConnectTo(host, port);
      Console.WriteLine($"Connected.\n Enter topic to subscribe and publish to [default: nsoftware/test]. Type 'quit' to quit.");
      Prompt();

      string input;
      while ((input = Console.ReadLine()) != null)
      {
        if (input.ToLower() == "quit")
        {
          Console.WriteLine("Quitting");
          break;
        }
        else if (topicUnset)
        {
          if (!string.IsNullOrEmpty(input))
          {
            topic = input;
          }

          topicUnset = false;

          try
          {
            mqtt.Subscribe(topic, qos);
            Console.WriteLine($"Subscribed to {topic}.\n Enter messages to send. Type 'quit' to quit.");
          }
          catch (Exception e)
          {
            Console.WriteLine(e.Message);
          }
        }
        else
        {
          try
          {
            mqtt.PublishMessage(topic, qos, input);
          }
          catch (Exception e)
          {
            Console.WriteLine(e.Message);
          }
        }

        Prompt();
      }
    }
    catch (Exception e)
    {
      Console.WriteLine(e.Message);
    }
  }

  private static void MQTT_OnMessageIn(object sender, MQTTMessageInEventArgs e)
  {
    Console.WriteLine($"Message from {e.Topic} at QoS {e.QOS}: \"{e.Message}\"");
  }

  private static void Prompt()
  {
    Console.Write("mqtt> ");
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