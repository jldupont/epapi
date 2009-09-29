/**
 * @file driver.cc
 *
 * @date   2009-09-29
 * @author jldupont
 */

#include "epapi.h"


int main(int argc, char **argv) {

  	PktHandler *ph = new PktHandler();
	Pkt *p = NULL;
	int r;


	do {
		if (NULL==pkt)
			p=new Pkt();

		r = ph->rx(&p);

	} while (1) ;

	if (r) {
		last_error = ph->last_error;
		delete p;
		return 1;
	}


	//Verify return code
	if (result) {
		//handle error
		printf("ERROR, message: %s", mh->strerror());
		// ...
	}


}//
