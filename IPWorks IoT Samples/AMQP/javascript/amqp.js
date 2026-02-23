/*
 * IPWorks IoT 2024 JavaScript Edition - Sample Project
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
 
const readline = require("readline");
const ipworksiot = require("@nsoftware/ipworksiot");

if(!ipworksiot) {
  console.error("Cannot find ipworksiot.");
  process.exit(1);
}
let rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

main();

async function main() {
  console.log("***************************************************************");
  console.log("* This demo shows how to send and receive messages using      *");
  console.log("* the AMQP component.                                         *");
  console.log("***************************************************************");

  const amqp = new ipworksiot.amqp();
  let messageReceived = false;

  // Fires when a message is received
  amqp.on('MessageIn', async (e) => {
    console.log(`Message received: ${await amqp.getReceivedMessage().getValue()}`);
    messageReceived = true;
  })
  .on('SSLServerAuthentication', (e) => ( e.accept = true))
  .on('ConnectionStatus', (e) => (console.log(`Connection status update: ${e.connectionEvent}`)));

  let argv = process.argv;
  if (argv.length < 4) {

    console.log("Usage: node amqp.js [option]");
    console.log("  Options: ");
    console.log("    --send        send a message");
    console.log("    --retrieve    retrieve a message");
    console.log("    -u            the user identifier to use for login to server (default is none)");
    console.log("    -p            the password to log in (default is none)");
    console.log("    -rp, --port   the remote port to connect to on the remote host (default is 5672, or 5671 for ssl)");
    console.log("    -s, --ssl     sets ssl enabled to true (default is false)");
    console.log("    -f            [only used when sending] sets receive mode to retrieve while sending messages (default is automatic)");
    console.log("    -m            [only used when sending] the message to send (default is 'Hello, AMQP!')");
    console.log("    -h, --host    domain name or IP address of the remote host")
    console.log("Examples: node amqp.js --send -h <server_address> -u username -p password -m \"Hello, AMQP\""); //send with defaults: port 5672, no ssl, automatic mode
    console.log("          node amqp.js --send -h <server_address> -u username -p password -rp 5670 -s -f -m \"Hello, AMQP\""); //send with options: port 5670, ssl enabled, retrieve mode
    console.log("          node amqp.js --retrieve -h <server_address> -u username -p password"); //retrieve with defaults: port 5672, no ssl
    console.log("          node amqp.js --retrieve -h <server_address> -u username -p password -rp 5670 -s"); //retrieve with options: port 5670, ssl enabled
    process.exit();
  }

  //configure settings
  let message = "Hello, AMQP!";
  let remotePort = 5672;
  let action = "";
  let host = "";
  for (i = 0; i < argv.length; i++) {
    if (argv[i].startsWith("-")) {
      if (argv[i] === "-u") amqp.setUser(argv[i + 1]); // args[i+1] corresponds to the value of argument [i]
      if (argv[i] === "-p") amqp.setPassword(argv[i + 1]);
      if (argv[i] === "-rp" || argv[i] === "--port") {
        remotePort = parseInt(argv[i + 1]);
        amqp.setRemotePort(remotePort);
      }
      if (argv[i] === "-s" || argv[i] === "--ssl") amqp.setSSLEnabled(true);
      if (argv[i] === "-f") amqp.setReceiveMode(1); //retrieve mode = 1, default is automatic
      if (argv[i] === "-m") message = argv[i + 1];
      if (argv[i] === "--send") action = "send";
      if (argv[i] === "--retrieve") action = "retrieve";
      if (argv[i] === "-h" || argv[i] === "--host") host = argv[i+1];
    }
  }

  if (action.length === 0) {
    console.log("No action specified.");
    process.exit();
  }
  if (host.length === 0) {
    console.log("No server specified.");
    process.exit();
  }

  try {
    amqp.setContainerId("ContainerId"); //set unique containerid
    await amqp.connectTo(host, remotePort);
    await amqp.createSession("SessionName"); //create session with session name
    await amqp.createSenderLink("SessionName", "SenderLinkName", "TargetName"); //create sender link
    if (action === "retrieve") { //if action is retrieve
      amqp.setReceiveMode(1); //retrieve mode = 1, default is automatic
    }
    await amqp.createReceiverLink("SessionName", "ReceiverLinkName", "TargetName");
  } catch (error) {
    console.log(`Error connecting: ${error.message}`);
    process.exit();
  }

  if (action === "send") { //if action is send
    //send message
    try {
      amqp.setMessage(new ipworksiot.AMQPMessage());
      amqp.getMessage().setValueType(17); //string type
      await amqp.getMessage().setValue(message);
      await amqp.sendMessage("SenderLinkName"); //send message using same sender link
      console.log("Message sent.");
      if (amqp.getReceiveMode() === 0) { //if receive mode is auto
        while (!messageReceived) { //wait until message received and prints
          await amqp.doEvents();
        }
      }
    } catch (error) {
      console.log(`Error sending message: ${error.message}`);
    }
    await amqp.disconnect();
    process.exit();
  } else {
    //retrieve message
    try {
      amqp.setRetrieveTimeout(5); //set timeout to 5 seconds
      console.log("Retrieveing message...");
      await amqp.retrieveMessage("ReceiverLinkName"); //retrieve message using same receiver link
    } catch (error) {
      console.log(`Error retrieveing message: ${error.message}`);
    }
    await amqp.disconnect();
    process.exit();
  }
}


function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}]${punctuation} `);
}
