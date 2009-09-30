/**
 * @file driver.cc
 *
 * @date   2009-09-29
 * @author jldupont
 */

#include "epapi.h"


int main(int argc, char **argv) {

  	PktHandler *ph  = new PktHandler();

	TermHandler *ith = new TermHandler();
	TermHandler *oth = new TermHandler();

	TermStruct ts;

	Pkt *p = NULL;
	int r;
	int last_error;
	int result;

	do {
		if (NULL==p)
			p=new Pkt();
		else
			p->clean(); // recycle!

		r = ph->rx(&p);
		if (r) {
			last_error = ph->last_error;
			DBGLOG(LOG_ERR, "Error, msg: %s", ph->strerror());
			delete p;
			break;
		}

		DBGLOG(LOG_INFO, "epapi_loop_drv: got packet");

		// SETUP before iteration
		ith->clean(&ts);
		ith->init(p);

		Pkt *opkt = new Pkt(); //can't recycle those...
		oth->init(opkt);


		DBGLOG(LOG_INFO, "epapi_loop_drv: starting decode & adapt loop");
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
			if (!result)
				if (TERMTYPE_END==ts.type) {
					break;
					result=0;
				}


			if (!result) {
				DBGLOG(LOG_INFO, "epapi_loop_drv: appending");
				// got one term, stuff it in output side
				result=oth->append(&ts);
				if (result) {
					last_error=oth->last_error;
					DBGLOG(LOG_ERR, "epapi_loop_drv: append error, msg: %s", oth->strerror());
					break;
				}

				// NIL is sometimes used to terminate a term()
				// so it must be passed to 'append'
				if (TERMTYPE_NIL==ts.type)
					break;
			}

			//if (result &&(EEPAPI_BADTYPE==last_error)) {
			//	result = 0;
			//	break;
			//}

		} while(1);

		if (result) // error occured
			break;
		else {
			DBGLOG(LOG_INFO, "epapi_loop_drv: about to send back");
			// send
			result=ph->tx(opkt);
			delete opkt; // we won't be needed this regardless
			if (result) {
				last_error=oth->last_error;
				break;
			}
		}

		DBGLOG(LOG_INFO, "epapi_loop_drv: end of loop");

	} while (1) ;



	exit(last_error);
}//
