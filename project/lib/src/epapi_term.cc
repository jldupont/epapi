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
TermHandler::TermHandler(PktHandler *_ph) {
	ph=_ph;
	index=0;
}

// When receiving
TermHandler::TermHandler(PktHandler *_ph, Pkt *_p) {
	ph=_ph;
	p=_p;
	index=0;
}

TermHandler::~TermHandler() {
	if (NULL!=ph) {
		delete ph;
	}

	if (NULL!=p) {
		delete p;
	}
}


int
TermHandler::send(void) {

	if (NULL==ph) {
		last_error = EEPAPI_NULL;
		return 1;
	}

	int result = ph->tx( p );
	if (result) {

		// the PktHandler layer
		// will tell us what we need...
		last_error = ph->last_error;
	}

	return result;
}

/**
 * Appends a term to the packet being currently assembled
 *
 * String: must be NUL terminated
 *
 * Binary:
 * 		append(TERMTYPE_BINARY, long length, void *bin);
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
	if (NULL==p) {
		p=new Pkt();

		b= p->getTxBuf();
		if (NULL==b) {
			delete p;
			p=NULL;
			last_error = EEPAPI_MALLOC;
			return 1;
		}

		 if (ei_x_new_with_version(b)) {
			 last_error = EEPAPI_NEWEIBUF;
			 delete p;
			 p=NULL;
			 return 1;
		 }

	}//if


	b = p->getTxBuf();
	if (NULL==b) {
		delete p;
		last_error = EEPAPI_MALLOC;
		return 1;
	}

	void *bin;  long len;
	const char *string;
	int result, arity;

	long integer;
	unsigned long uinteger;
	long long linteger;
	unsigned long long ulinteger;
	double d;

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
		integer=va_arg(args, long);
		result=ei_x_encode_long(b, integer);
		break;

	case TERMTYPE_ULONG:
		uinteger=va_arg(args, unsigned long);
		result=ei_x_encode_ulong(b, uinteger);
		break;

	case TERMTYPE_LONGLONG:
		linteger=va_arg(args, long long);
		result=ei_x_encode_longlong(b, linteger);
		break;

	case TERMTYPE_ULONGLONG:
		ulinteger=va_arg(args, unsigned long long);
		result=ei_x_encode_ulonglong(b, ulinteger);
		break;

	case TERMTYPE_STRING:
		string=va_arg(args, char *);
		result=ei_x_encode_string(b, string);
		break;

	case TERMTYPE_BINARY:
		len=va_arg(args, long);
		bin=va_arg(args, void*);
		result=ei_x_encode_binary(b, bin, len);
		break;

	default:
		result=1;
		break;
	}//switch

	va_end(args);

	if (result) {
		last_error = EEPAPI_EIENCODE;
	}


	return result;
}//


/**
 * Iterates through the received packet and
 * extracts the elements of the term() 1by1
 *
 * The function returns the Term of type 'TERMTYPE_END'
 * when the end of the received term() is reached.
 *
 * It is the responsibility of the caller to dispose of
 * the returned Term through the TermPointer variable.
 */
int
TermHandler::iter(TermPointer **tptr) {

	if (NULL==p) {
		last_error=EEPAPI_NULL;
		return 1;
	}

	unsigned char *buf;

	buf=p->getBuf();

	int result;
	int version;

	// we are just starting... read the version header first
	if (0==index) {
		result=ei_decode_version((const char *)buf, &index, &version);
		if (result) {
			last_error=EEPAPI_EIDECODE;
			return 1;
		}
	}//if

	//we are at least past the 'version' field of the packet from hereon
	//we need to figure out which 'type' follows
	int type;
	int size;
	result=ei_get_type((const char *)buf, &index, &type, &size);
	if (result) {
		last_error=EEPAPI_EIDECODE;
		return 1;
	}



}//

