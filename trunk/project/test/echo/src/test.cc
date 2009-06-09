/**
 * @file test.cc
 *
 * @date 8-Jun-2009
 * @author Jean-Lou Dupont
 */

#include <epapi.h>
#include <sys/time.h>
#include "logger.h"

int main() {

	DEBUG_LOG(LOG_INFO, "echo: START");

	PktHandler *ph = new PktHandler();
	MsgHandler *mh = new MsgHandler(ph);

	int result;
	mh->registerType(1, "echo", "l" );
	mh->registerType(2, "pong", "l" );

	Msg *m;
	long int myc = 0;
	while(1) {
		result = mh->rx(&m);
		if (result) {
			doLog(LOG_ERR, "RECEIVE ERROR, [%s]", mh->strerror());
			delete m;
			break;
		}
		long int counter;
		char format;
		result = m->getParam(0, &format, &counter);
		if (result) {
			doLog(LOG_ERR, "getParam, [%s]", m->strerror());
			delete m;
			break;
		}
		if (format!='l') {
			doLog(LOG_ERR, "bad format");
			delete m;
			break;
		}
		doLog(LOG_INFO, "rx, counter[%li]", counter);
		delete m;


		result = mh->send(2, myc);
		if (result) {
			doLog(LOG_ERR, "tx: error[%s]", mh->strerror());
			break;
		}
		//usleep(250*1000);

	}//while

	DEBUG_LOG(LOG_INFO, "echo: END");
}//
