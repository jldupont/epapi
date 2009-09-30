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

	Pkt *ipkt = new Pkt();
	Pkt *opkt = new Pkt();

	int r;
	int last_error;
	int result;

	do {
		/*
		if (NULL==p)
			p=new Pkt();
		else
			p->clean(); // recycle!
		*/

		r = ph->rx(&ipkt);
		if (r) {
			last_error = ph->last_error;
			DBGLOG(LOG_ERR, "Error, msg: %s", ph->strerror());
			break;
		}

		DBGLOG(LOG_INFO, "epapi_loop_drv: got packet");

		// SETUP before iteration
		ith->clean(&ts);
		ith->init(ipkt);

		//Pkt *opkt = new Pkt(); //can't recycle those...
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
					break;
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
				// so it must be passed to 'append' *but* it nonetheless
				// terminates a term()
				if ((TERMTYPE_NIL==ts.type) || (TERMTYPE_END==ts.type))
					break;
			}


		} while(1);

		// make sure we are ready for next round... if any
		ipkt->clean();

		if (result) // error occured
			break;
		else {
			DBGLOG(LOG_INFO, "epapi_loop_drv: about to send back");
			// send
			result=ph->tx(opkt);
			opkt->clean(); //recycle

			//delete opkt; // we won't be needed this regardless
			if (result) {
				last_error=oth->last_error;
				break;
			}
		}

		DBGLOG(LOG_INFO, "epapi_loop_drv: end of loop");

	} while (1) ;


	delete ipkt;
	delete opkt;
	delete ph;
	delete ith;
	delete oth;
	exit(last_error);
}//
