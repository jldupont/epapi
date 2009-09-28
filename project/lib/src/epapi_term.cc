/**
 * @file epapi_term.cc
 *
 * @date   2009-09-28
 * @author jldupont
 */
#include "epapi.h"


Term::Term(void) {
	type=TERMTYPE_INVALID;
	size=0;
	data=NULL;
}

Term::~Term() {

	if (NULL!=data) {
		free(data);
	}
}

TermType
Term::getType(void) {
	return type;
}

void
Term::setType(TermType t) {
	type=t;
}

int
Term::getSize(void) {
	return size;
}

void
Term::setSize(int sz) {
	size=sz;
}

TermPointer
Term::getDataPtr(void) {
	return data;
}

void
Term::setDataPtr(TermPointer *ptr) {
	data=ptr;
}

// When sending
TermHandler::TermHandler(void) {
	ph=NULL;
}

// When receiving
TermHandler::TermHandler(PktHandler *packet) {
	ph=packet;
}

TermHandler::~TermHandler() {
}


int
TermHandler::send(void) {

	int result = ph->tx( ph );
	if (result) {

		// the PktHandler layer
		// will tell us what we need...
		last_error = p->last_error;
	}

	return result;
}

/**
 * Appends a term to the packet being currently assembled
 *
 * Atom:
 * 		append(TERMTYPE_ATOM, char *string)
 *
 * Tuple:
 *		append(TERMTYPE_TUPLE, int arity)
 *		append(TERMTYPE_xyz, ...)
 *		...
 *
 */
int
TermHandler::append(TermType type, ...) {

	ei_x_buff *b;

	// First time, let's prepare ourselves a packet
	if (NULL==ph) {
		ph=new Pkt();

		b= ph->getTxBuf();
		if (NULL==b) {
			delete ph;
			last_error = EEPAPI_MALLOC;
			return 1;
		}

		 if (ei_x_new_with_version(b)) {
			 last_error = EEPAPI_NEWEIBUF;
			 delete ph;
			 return 1;
		 }

	}//if


	b = ph->getTxBuf();
	if (NULL==b) {
		delete p;
		last_error = EEPAPI_MALLOC;
		return 1;
	}

	const char *string;
	int result, arity;

	long integer;
	unsigned long uinteger;
	long long linteger;
	unsigned long long ulinteger;
	double d;

	int index=0;
	va_list args;

	va_start(args, type);

	switch(type) {
	case TERMTYPE_START_LIST:
		 result=ei_x_encode_list_header(b, 1);
		break;

	case TERMTYPE_END_LIST:
		result=ei_x_encode_empty_list(b);
		break;

	case TERMTYPE_ATOM:
		string=va_arg(args, char *);
		result=ei_x_encode_atom(b, string);
		break;

	case TERMTYPE_TUPLE:
		arity=va_arg(args, int);
		result=ei_x_encode_tuple_header(b, arity);
		break;

	case TERMTYPE_FLOAT:
		d=va_arg(args, double);
		result=ei_x_encode_double(b, d);
		break;

	case TERMTYPE_LONG:
		integer=var_arg(args, long);
		result=ei_x_encode_long(b, integer);
		break;

	case TERMTYPE_ULONG:
		uinteger=var_arg(args, unsigned long);
		result=ei_x_encode_ulong(b, uinteger);
		break;

	case TERMTYPE_STRING:

	case TERMTYPE_BINARY:

	default:
		break;
	}//switch


}


