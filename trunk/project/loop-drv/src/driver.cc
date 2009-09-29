/**
 * @file driver.cc
 *
 * @date   2009-09-29
 * @author jldupont
 */

#include "epapi.h"


int main(int argc, char **argv) {

	TermHandler *ith = NULL;
  	PktHandler *ph  = new PktHandler();
	Pkt *p = NULL;
	int r;
	int last_error;


	do {
		if (NULL==p)
			p=new Pkt();

		r = ph->rx(&p);
		if (r) {
			last_error = ph->last_error;
			delete p;
			DBGLOG("Error, msg: %s", last_error);
			break;
		}

		// Start the RX side
		ith=new TermHandler(p);
		if (NULL==th) {
			DBGLOG("Error, msg: %s", last_error);
			last_error=EEPAPI_MALLOC;
			break;
		}




	} while (1) ;



	exit(last_error);
}//
