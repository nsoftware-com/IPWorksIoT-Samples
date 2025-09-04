<?php
/*
 * IPWorks IoT 2024 PHP Edition - Sample Project
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
require_once('../include/ipworksiot_mqtt.php');
require_once('../include/ipworksiot_const.php');
?>
<?php
global $receivedMessage;
$receivedMessage = false;
class MyMQTT extends IPWorksIoT_MQTT
{
  // Fires when a message is received
  function fireMessageIn($param) {
    global $receivedMessage;
    $receivedMessage = true;
    echo "Message from " . $param['topic'] . " at QoS " . $param['qos'] . ": \"" . $param['message'] . "\"\n";
  }
}

function input($prompt) {
  echo $prompt;
  $handle = fopen("php://stdin", "r");
  $data = trim(fgets($handle));
  fclose($handle);
  return $data;
}

$mqtt = new MyMQTT();

$host = "test.mosquitto.org";
$port = 1883;
$topic = "nsoftware/test";
$qos = 1;

try {
  echo "Connecting to " . $host . "\n";
  $mqtt->doConnectTo($host, $port);
  echo "Connected.\n";

  $input = input("Enter topic to subscribe and publish to [default: nsoftware/test]. Type 'quit' to quit.\n");
  if (strtolower($input) == "quit") return;
  if (strlen($input) > 0) $topic = $input;
  
  $mqtt->doSubscribe($topic, $qos);
  echo "Subscribed to " . $topic . ".\nEnter messages to send. Type 'quit' to quit.\n";
  
  while (true) {
    $input = input("mqtt> ");
    if (strtolower($input) == "quit") {
      echo "Quitting\n";
      return;
    } elseif (strlen($input) > 0) {
      // publish the message and wait for an incoming message
      $receivedMessage = false;
      $mqtt->doPublishMessage($topic, $qos, $input);
      while (!$receivedMessage) {
        $mqtt->doEvents();
      }
    }
  }

} catch (Exception $ex) {
  echo "Error: " . $ex->getMessage() . "\n";
}
?>