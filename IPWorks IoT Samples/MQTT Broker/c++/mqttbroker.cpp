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
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include "../../include/ipworksiot.h"
#define LINE_LEN 80

class MyMQTTBroker : public MQTTBroker
{
	int FireSSLClientAuthentication(MQTTBrokerSSLClientAuthenticationEventParams *e)
	{
		//here you can decide if you want to authenticate the client via TLS/SSL
		e->Accept = true;
		return 0;
	}

	int FireMessageSent(MQTTBrokerMessageSentEventParams *e)
	{
		printf("Client [%s] has received a message!\n", e->ConnectionId);
		printf("ID: %s\n", e->MessageId);
		printf("Client ID: %s\n", e->ClientId);
		return 0;
	}

	int FireMessageReceived(MQTTBrokerMessageReceivedEventParams *e)
	{
		printf("Client [%s] has sent a message!\n",e->ConnectionId);
		printf("ID: %s\n", e->MessageId);
		printf("Client ID: %s\n",e->ClientId);
		return 0;
	}

	int FireUnsubscribe(MQTTBrokerUnsubscribeEventParams *e)
	{
		printf("Client [%s] has unsubscribed to the %s topic!\n",e->ConnectionId,e->TopicFilter);
		printf("ID: %s\n", e->ClientId);
		return 0;
	}

	int FireSubscribe(MQTTBrokerSubscribeEventParams *e)
	{
		printf("Client [%s] has subscribed to topic: %s\n",e->ConnectionId, e->TopicFilter);
		printf("ID: %s\nRequested QoS: %s\n", e->ClientId, e->RequestedQoS);
		return 0;
	}

	int FireDisconnected(MQTTBrokerDisconnectedEventParams *e)
	{
		printf("Client [%s] has disconnected with status: %s\nDescription: %s\n", e->Description,e->ConnectionId,e->StatusCode, e->Description);
		return 0;
	}

	int FireSessionRemoved(MQTTBrokerSessionRemovedEventParams *e)
	{
		printf("Client with ID %s has removed the session!\n",e->ClientId);
		return 0;
	}

	int FireSessionRequest(MQTTBrokerSessionRequestEventParams *e)
	{
		printf("Client [%s] has requested a session!\nClient ID is: %s\n", e->ClientId,e->ConnecitonId, e->ClientId);
		//here you can decide if you want to grant the client a session
		e->Accept = true;
		return 0;
	}

	int FireConnectionRequest(MQTTBrokerConnectionRequestEventParams *e)
	{
		printf("Client [%s:%s] has requested a connection!\n",e->Address, e->Port);
		//here you can decide if you want to establish a connection with the client
		e->Accept = true;
		return 0;
	}

	int FireConnected(MQTTBrokerConnectedEventParams *e)
	{
		printf("Client [%s] has connected with status: %s\nDescription: %s\n", e->Description,e->ConnectionId,e->StatusCode, e->Description);
		return 0;
	}
};

int main(int argc, char* argv[])
{
	MyMQTTBroker broker;

	char buffer[LINE_LEN];

	printf("*****************************************************************\n");
	printf("* This demo shows how to set up an echo server using broker. *\n");
	printf("*****************************************************************\n");

	int ret_code = broker.AddBrokerListener("localhost", 1883, 0, 0, null, null, null);
	if (ret_code)
	{
		printf("Error: %i - %s\n", ret_code, broker.GetLastError());
		goto done;
	}
	ret_code = broker.StartListening();

	if (ret_code)
	{
		printf("Error: %i - %s\n", ret_code, broker.GetLastError());
		goto done;
	}

	printf("Listening...\n");

	while (true)
	{
		broker.DoEvents();
	}

	done:
	if (broker.GetListening())
	{
		broker.StartListening();
	}

	printf("Exiting... (press enter)\n");
	getchar();

	return 0;
}




