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

let mqtt = new ipworksiot.mqtt();
// Fires when a message is received
mqtt.on('MessageIn', (e) => {
	console.log(`Message from ${e.topic} at QoS ${e.QOS}: \n${e.message.toString()}`);
	mqttprompt();
});
function mqttprompt() {
	process.stdout.write('mqtt> ');
}

main().catch(e => console.log(e))
async function main() {



	// Connection settings with default topic.
	// test.mosquitto.org is a publically available test server for MQTT.
	let host = 'test.mosquitto.org';
	let port = 1883;
	let topic = 'nsoftware/test';
	let qos = 1;
	let topicUnset = true;

	// Event to read a line of input
	rl.on('line', async (line) => {
		if (line.toLowerCase() === 'quit') {
			console.log('Quitting');
			process.exit();
		}
		// Set the topic if it hasn't been set yet
		else if (topicUnset) {
			// Use the default if they enter a blank line
			if (line !== '') {
				topic = line;
			}
			topicUnset = false;
			// Subscribe to the topic, then prompt for a message
			await mqtt.subscribe(topic, qos, function (e) {
				if (err) {
					console.log(e.description);
					return;
				}
			}).catch(e => console.log(e))
			console.log(`Subscribed to ${topic}. \r\nEnter messages to send. Send \'quit\' to quit.`);
			mqttprompt();
		}
		else {
			// Publish a message to the topic and then prompt again
			await mqtt.publishMessage(topic, qos, line, function (e) {
				if (err) {
					console.log(e.description);
					return;
				}
				mqttprompt();
			}).catch(e => console.log(e))
		}
	});



	// Connect to the host and then prompt for a topic
	console.log('Connecting to ' + host);
	await mqtt.connectTo(host, port, function (e) {
		if (err) {
			console.log(e.description);
			return;
		}
	}).catch(e => console.log(e))
	console.log('Connected.\r\nEnter topic to subscribe and publish to [default: nsoftware/test]. Send \'quit\' to quit.');
	mqttprompt();
}




function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}]${punctuation} `);
}
