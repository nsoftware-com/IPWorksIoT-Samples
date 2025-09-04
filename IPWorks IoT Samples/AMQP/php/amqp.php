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
require_once('../include/ipworksiot_amqp.php');
require_once('../include/ipworksiot_const.php');
?>
<?php
global $amqp;
class MyAMQP extends IPWorksIoT_AMQP
{
  function fireSSLServerAuthentication($param) {
    $param['accept'] = true;
    return $param;
  }
  function fireMessageIn($param) {
    global $amqp;
    echo "Incoming message from <" . $param['linkname'] . ">: " . $amqp->getReceivedMessageValue() . "\n";
  }
}

function input($prompt) {
  echo $prompt;
  $handle = fopen("php://stdin", "r");
  $data = trim(fgets($handle));
  fclose($handle);
  return $data;
}

try {
  $amqp = new MyAMQP();
  $amqp->setContainerId("ContainerId");

  if ($argc < 4) {
    echo "Usage: php amqp.php [options] server user password -port port -u user -p password -ssl\n\n";
    echo "    -ssl       whether to use SSL\n";
    echo "    -port      the port of the AMQP server\n";
    echo "    server     ...\n";
    echo "    user       ...\n";
    echo "    password   ...\n";
    echo "Example: php amqp.php -port 5671 -ssl 127.0.0.1 username password\n\n";
    return;
  } else {
    $amqp->setRemoteHost($argv[$argc-3]);
    $amqp->setUser($argv[$argc-2]);
    $amqp->setPassword($argv[$argc-1]);
  }

  for ($i = 0; $i < $argc; $i++) {
    if (str_starts_with($argv[$i], "-")) {
      if ($argv[$i] == "-ssl") { $amqp->setSSLEnabled(true); }
      if ($argv[$i] == "-port") { $amqp->setRemotePort($argv[$i + 1]);}
    }
  }
  try {
    $amqp->doConnect();
  } catch (Exception $ex) {
    echo "Error connecting: " . $ex->getMessage() . "\n";
    return;
  }

  $amqp->doCreateSession("SessionId");

  try {
    $amqp->doCreateSenderLink("SessionId", "SenderLinkName", "TargetName");
  } catch (Exception $ex) {
    echo "Error creating sender link: " . $ex->getMessage() . "\n";
    return;
  }

  $amqp->setReceiveMode(1); // Retrieve
  $amqp->setRetrieveTimeout(5);

  try {
    $amqp->doCreateReceiverLink("SessionId", "ReceiverLinkName", "TargetName");
  } catch (Exception $ex) {
    echo "Error creating receiver link: " . $ex->getMessage() . "\n";
    return;
  }

  $command = "";
  while ($command != "q") {
    if ($command == "s") {
      $amqp->doResetMessage();
      $amqp->setMessageValueType(17); // string
      $amqp->setMessageValue(input("Enter message to send: "));
      try {
        $amqp->doSendMessage("SenderLinkName");
      } catch (Exception $ex) {
        "Error sending message: " . $ex->getMessage . "\n";
        return;
      }
      echo "Message sent!\n";
    } elseif ($command == "f") {
      echo "Fetching message...\n";
      try {
        $amqp->doRetrieveMessage("ReceiverLinkName");
      } catch (Exception $ex) {
        if (str_starts_with($ex->getMessage(), "201")) {
          echo "Timeout - no message received.\n";
        } else {
          echo "Error fetching message: " . $ex->getMessage() . "\n";
          return;
        }
      }
    }
    $command = input("Choose send message (s), fetch message (f), or quit (q): ");
  }
} catch (Exception $ex) {
  echo "Error: " . $ex->getMessage() . "\n";
}
?>