/**
 * @file test.cc
 *
 * @date 8-Jun-2009
 * @author Jean-Lou Dupont
 */

#include <epapi.h>
#include "logger.h"

int main() {

	DEBUG_LOG(LOG_INFO, "echo: START");

	PktHandler *ph = new PktHandler();
	MsgHandler *mh = new MsgHandler(ph);

	int result;
	mh->registerType(1, "echo", "l");

	Msg *m;
	while(1) {
		result = mh->rx(&m);
		if (result) {
			doLog(LOG_ERR, "RECEIVE ERROR, [%s]", mh->strerror());
			break;
		}
		long int counter;
		char format;
		result = m->getParam(0, &format, &counter);
		if (result) {
			doLog(LOG_ERR, "getParam, [%s]", m->strerror());
			break;
		}
		if (format!='l') {
			doLog(LOG_ERR, "bad format");
			break;
		}
		doLog(LOG_INFO, "rx, counter[%li]", counter);
		delete m;

		result = mh->send(1, counter+1);
		if (result) {
			doLog(LOG_ERR, "tx: error[%s]", mh->strerror());
			break;
		}

	}//while

	DEBUG_LOG(LOG_INFO, "echo: END");
}//
