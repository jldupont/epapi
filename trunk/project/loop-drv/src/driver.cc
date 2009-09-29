/**
 * @file driver.cc
 *
 * @date   2009-09-29
 * @author jldupont
 */

#include "epapi.h"


int main(int argc, char **argv) {

  	PktHandler *ph  = new PktHandler();

	TermHandler *ith = NULL;
	TermHandler *oth = new TermHandler(ph);

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
		if (NULL==ith) {
			DBGLOG("Error, msg: %s", last_error);
			last_error=EEPAPI_MALLOC;
			break;
		}

		int result;
		TermStruct ts;
		oth->clean(&ts);
		// Loop through the received Term
		do {
			result=ith->iter( &ts );

			// we get an error of 'badtype' when the
			// end of the term is reached... I know this
			// is ugly but I couldn't find another way looking
			// through the Erlang erl_interface code...
			if (result) {
				last_error=ith->last_error;
				if (EEPAPI_BADTYPE!=last_error)
					break;
			}

			// got one term, stuff it in output side
			result=oth->append(&ts);
			if (result) {
				last_error=oth->last_error;
				break;
			}

			// reached the end... proceed to sending
			if (EEPAPI_BADTYPE==last_error) {
				result = 0;
			}

		} while(1);

		if (result) // error occured
			break;
		else {
			// send
			result=oth->send();
			if (result) {
				last_error=oth->last_error;
				break;
			}
		}


	} while (1) ;



	exit(last_error);
}//
