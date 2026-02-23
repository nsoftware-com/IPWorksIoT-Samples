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

class jsonDemo
{
  private static JSON json = new nsoftware.IPWorksIoT.JSON();

  static void Main(string[] args)
  {
    json.OnStartDocument += json_OnStartDocument;
    json.OnEndDocument += json_OnEndDocument;
    json.OnStartElement += json_OnStartElement;
    json.OnEndElement += json_OnEndElement;
    json.OnCharacters += json_OnCharacters;

    try
    {
      json.InputFile = "..\\..\\..\\books.json";
      json.Parse();

      json.XPath = "/json/store/books";
      int bookCount = json.XChildren.Count;

      for (int i = 1; i <= bookCount; i++)
      {
        Console.WriteLine("\nBook #" + i);

        json.XPath = "/json/store/books/[" + i + "]";
        int propCount = json.XChildren.Count;

        for (int j = 1; j <= propCount; j++)
        {
          json.XPath = "/json/store/books/[" + i + "]/[" + j + "]";
          Console.WriteLine(json.XElement + ": " + json.XText);
        }
      }
    }
    catch (Exception ex)
    {
      Console.WriteLine(ex.Message);
    }
  }

  #region "Events"

  private static void json_OnStartDocument(object sender, JSONStartDocumentEventArgs e)
  {
    Console.WriteLine("Started parsing file");
  }

  private static void json_OnEndDocument(object sender, JSONEndDocumentEventArgs e)
  {
    Console.WriteLine("Finished parsing file");
  }

  private static void json_OnStartElement(object sender, JSONStartElementEventArgs e)
  {
    Console.WriteLine("Element " + e.Element + " started");
  }

  private static void json_OnEndElement(object sender, JSONEndElementEventArgs e)
  {
    Console.WriteLine("Element " + e.Element + " ended");
  }

  private static void json_OnCharacters(object sender, JSONCharactersEventArgs e)
  {
    Console.WriteLine(e.Text);
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