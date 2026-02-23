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
  console.log("* This demo shows how to publish or subscribe to an MQTT      *");
  console.log("* topic. By default, test.mosquitto.org is used with a        *");
  console.log("* topic of nsoftware/test.                                    *");
  console.log("***************************************************************");

  const mqtt = new ipworksiot.mqtt();

  // Fires when a message is received
  mqtt.on('MessageIn', (e) => {
    console.log(`Message from ${e.topic} at QoS ${e.QOS}: \n${e.message.toString()}`);
  });

  let argv = process.argv;
  if (argv.length < 3) {
    console.log("Usage: node mqtt.js [options]");
    console.log("  Options: ");
    console.log("    --publish     publish a message");
    console.log("    --subscribe   subscribe to messages");
    console.log("    -t --topic    the topic to subscribe or publish to (default is test/nsoftware");
    console.log("    -m --message  [only used when publishing] the message to send");
    console.log("    -h --host     remote host (default is test.mosquitto.org)");
    console.log("    -p --port     remote port (default is 1883)");

    console.log("Examples: node amqp.js --publish -m \"Hello, MQTT!\""); // publish message to the default (nsoftware/test) topic
    console.log("          node amqp.js --publish -t \"nsoftware/test123\" -m \"Hello, MQTT!\""); // publish message to nsoftware/test123 topic
    console.log("          node amqp.js --subscribe"); // subscribe to the default (nsoftware/test) topic
    console.log("          node amqp.js --subscribe -t \"nsoftware/test123\""); // subscribe to nsoftware/test123 topic
    process.exit();
  }

  // Connection settings with default topic.
  // test.mosquitto.org is a publically available test server for MQTT.
  let host = 'test.mosquitto.org';
  let port = 1883;
  let topic = 'nsoftware/test';
  let qos = 1;
  let action = "";
  let message = "";

  for (i = 0; i < argv.length; i++) {
    if (argv[i].startsWith("-")) {
      if (argv[i] === "--publish") action = "publish";
      if (argv[i] === "--subscribe") action = "subscribe";
      if (argv[i] === "-t" || argv[i] === "--topic") topic = argv[i+1];
      if (argv[i] === "-h" || argv[i] === "--host") host = argv[i+1];
      if (argv[i] === "-p" || argv[i] === "--port") port = parseInt(argv[i+1]);
      if (argv[i] === "-m" || argv[i] === "--message") message = argv[i+1];
    }
  }

  // check if an action has been specified
  if (action.length === 0) {
    console.log("No action specified.");
    process.exit();
  }
  // if publishing, we need a message
  if (action === "publish" && message.length === 0) {
    console.log("No message specified to publish.");
    process.exit();
  }

  try {
    console.log('Connecting to ' + host);
    await mqtt.connectTo(host, port);
    console.log('Connected.');

    if (action === "subscribe") {
      await mqtt.subscribe(topic, qos);
      console.log("Subscribed. Press Ctrl-C to exit.");
      // don't exit - messages will be displayed a they are received
    }
    if (action === "publish") {
      await mqtt.publishMessage(topic, qos, message);
      console.log("Message '" + message + "' published to topic: " + topic);
      await mqtt.disconnect();
      process.exit();
    }
  } catch (e) {
    console.log("Error: " + e.message);
    process.exit();
  }
}

function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}]${punctuation} `);
}
