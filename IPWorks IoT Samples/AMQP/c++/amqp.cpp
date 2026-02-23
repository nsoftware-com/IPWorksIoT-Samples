/*
 * IPWorks IoT 2024 C++ Edition - Sample Project
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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "../../include/ipworksiot.h"

bool messageReceived = false;
bool linkReadyToSend = false;

#define LINE_LEN 256
char input[LINE_LEN + 1];
const char* prompt(const char* prompt) {
	printf("%s: ", prompt);
	fgets(input, LINE_LEN, stdin);
	input[strlen(input) - 1] = '\0';
	if (strlen(input) == 0) {
		strncpy(input, "", LINE_LEN);
		input[LINE_LEN] = '\0';
	}
	return input;
}
const char* prompt(const char* prompt, const char* defaultVal) {
	printf("%s [%s]: ", prompt, defaultVal);
	fgets(input, LINE_LEN, stdin);
	input[strlen(input) - 1] = '\0';
	if (strlen(input) == 0) {
		strncpy(input, defaultVal, LINE_LEN);
		input[LINE_LEN] = '\0';
	}
	return input;
}

class MyAMQP : public AMQP
{
	virtual int FireSSLServerAuthentication(AMQPSSLServerAuthenticationEventParams *e) {
		//accept all
		e->Accept = true;
		return 0;
	}

	virtual int FireMessageIn(AMQPMessageInEventParams *e) {
		messageReceived = true;
		return 0;
	}
};

int main()
{
	MyAMQP amqp;
	
	//configure settings

	//set container id
	amqp.SetContainerId("ContainerId");

	//ask if ssl should be enabled
	char ssl[LINE_LEN];
	strncpy(ssl, prompt("Use SSL? (y/n)"), LINE_LEN);
	while (strcmp(ssl, "y") != 0 && strcmp(ssl, "n") != 0) {
		strncpy(ssl, prompt("Use SSL? Please type 'y' or 'n'"), LINE_LEN);
	}

	//prompt for remote host
	char host[LINE_LEN];
	strncpy(host, prompt("Remote Host"), LINE_LEN);

	//prompt for port, check if ssl enabled, if ssl enabled default is 5672, otherwise default is 5671
	char defPort[] = "5672";
	if (strcmp(ssl, "y") == 0) {
		strncpy(defPort, "5671", LINE_LEN);
		amqp.SetSSLEnabled(true);
	}
	else {
		amqp.SetSSLEnabled(false);
	}
	char port[LINE_LEN];
	strncpy(port, prompt("Remote Port", defPort), LINE_LEN);

	//prompt for user and password
	amqp.SetUser(prompt("User"));
	amqp.SetPassword(prompt("Password"));
	
	//connect using host and port information
	int ret_code = amqp.ConnectTo(host, atoi(port));
	if (ret_code)
	{
		printf("Error connecting: %i - %s\n", ret_code, amqp.GetLastError());
		printf("Press any key to continue.");
		fgetc(stdin);
		return 0;
	}

	//set session id and create sender link with a unique link name and name of target node (at most once flow by default, settled = true)
	amqp.CreateSession("SessionId");
	ret_code = amqp.CreateSenderLink("SessionId", "SenderLinkName", "TargetName");
	if (ret_code)
	{
		printf("Error creating sender link: %i - %s\n", ret_code, amqp.GetLastError());
		printf("Press any key to continue.");
		fgetc(stdin);
		return 0;
	}
	
	//create receiver link (retrieve mode) with existing session ID, unique link name, and name of the target node
	amqp.SetReceiveMode(RM_RETRIEVE);
	amqp.SetRetrieveTimeout(5);
	ret_code = amqp.CreateReceiverLink("SessionId", "ReceiverLinkName", "TargetName");
	if (ret_code)
	{
		printf("Error creating receiver link: %i - %s\n", ret_code, amqp.GetLastError());
		printf("Press any key to continue.");
		fgetc(stdin);
		return 0;
	}

	char resp[LINE_LEN];
	strncpy(resp, prompt("Choose send message (s), retrieve message (f) or quit (q)", "q"), LINE_LEN);
	while (strcmp(resp, "q") != 0) {
		if (strcmp(resp, "s") == 0) {
			//prompt for string message and send
			amqp.ResetMessage();
			amqp.SetMessageValueType(MVT_STRING);
			amqp.SetMessageValue(prompt("Enter message to send", "Hello!"));
			int ret_code = amqp.SendMessage("SenderLinkName");
			if (ret_code)
			{
				printf("Error sending: %i - %s\n", ret_code, amqp.GetLastError());
				printf("Press any key to continue.");
				fgetc(stdin);
				return 0;
			}
			printf("Message sent\n");

		}
		else if (strcmp(resp, "f") == 0) {
			//Retrieve message (class will block until message received or timeout)
			printf("Retrieveing message...\n");
			int ret_code = amqp.RetrieveMessage("ReceiverLinkName");

			if (ret_code == 201) {
				//Retrieve message timed out (set to 5 seconds)
				printf("Timeout - no message received\n");
			} else if(ret_code) {
				//Other error
				printf("Error retrieveing message: %i - %s\n", ret_code, amqp.GetLastError());
				printf("Press any key to continue.");
				fgetc(stdin);
				return 0;
			}
			//If retrieve method returns with message received
			if (messageReceived) {
				printf("Message received: %s\n", amqp.GetReceivedMessageValue());
				messageReceived = false;
				amqp.ResetMessage();
			}
		}
		strncpy(resp, prompt("Choose send message (s), retrieve message (f) or quit (q)", "q"), LINE_LEN);
	}

  return 0;
}

