/**
 * @file epapi_term.cc
 *
 * @date   2009-09-28
 * @author jldupont
 *
 *
 * ERL_SMALL_INTEGER_EXT : 8  bits integer
 * ERL_INTEGER_EXT:        32 bits integer
 *
 * ERL_SMALL_BIG_EXT:      8  bits arity, bignum  -- might fit in 32/64 bits
 * ERL_LARGE_BIG_EXT:      32 bits arity, bignum  -- might fit in 32/64 bits
 *
 */
#include "epapi.h"

using namespace std;

const char *
TermHandler::term_strings[] = {
	"TERMTYPE_INVALID"
	,"TERMTYPE_END"
	,"TERMTYPE_START_LIST"
	,"TERMTYPE_END_LIST"
	,"TERMTYPE_START_TUPLE"
	,"TERMTYPE_ATOM"
	,"TERMTYPE_STRING"
	,"TERMTYPE_TUPLE"
	,"TERMTYPE_DOUBLE"
	,"TERMTYPE_LONG"
	,"TERMTYPE_ULONG"
	,"TERMTYPE_LONGLONG"
	,"TERMTYPE_ULONGLONG"
	,"TERMTYPE_BINARY"
	,"TERMTYPE_NIL"
};

const char *
TermHandler::termtype_tostring(TermType type) {

	if (type>((int)(sizeof(term_strings)/sizeof(int))))
		return NULL;

	return term_strings[type];
}


// When sending
TermHandler::TermHandler(PktHandler *_ph) {
	ph=_ph;
	index=0;
}

// When receiving
TermHandler::TermHandler(Pkt *_p) {
	ph=NULL;
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

void
TermHandler::destroy(TermStruct *ts) {
	if (NULL==ts) return;

	switch(ts->type) {
	case TERMTYPE_ATOM:
	case TERMTYPE_STRING:
	case TERMTYPE_BINARY:
		if (NULL!=ts->Value.string)
			free(ts->Value.string);
		break;
	default:
		break;
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

	case TERMTYPE_DOUBLE:
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
TermHandler::iter(TermStruct *ptr) {

	if ((NULL==p) || (NULL==ptr)){
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

	char *sptr;

	switch(type) {
	case ERL_SMALL_INTEGER_EXT:
	case ERL_INTEGER_EXT:
		result=ei_decode_long((const char *)buf, &index, &(ptr->Value.integer));
		ptr->type=TERMTYPE_LONG;

		if (result) {
			result=ei_decode_ulong((const char *)buf, &index, &(ptr->Value.uinteger));
			ptr->type=TERMTYPE_ULONG;
		}
		break;
	case ERL_LARGE_BIG_EXT:
	case ERL_SMALL_BIG_EXT:
		result=ei_decode_longlong((const char *)buf, &index, &(ptr->Value.linteger));
		ptr->type=TERMTYPE_LONGLONG;

		if (result) {
			result=ei_decode_ulonglong((const char *)buf, &index, &(ptr->Value.luinteger));
			ptr->type=TERMTYPE_ULONGLONG;
		}
		break;

	case ERL_ATOM_EXT:
		sptr=(char*) malloc(sizeof(char)*size+sizeof(char));
		if (NULL==sptr) {
			last_error=EEPAPI_MALLOC;
			return 1;
		}
		result=ei_decode_atom((const char *)buf, &index, sptr);
		ptr->Value.string=sptr;
		ptr->size=size;
		ptr->type=TERMTYPE_ATOM;
		break;

	case ERL_FLOAT_EXT:
		result=ei_decode_double((const char *)buf, &index, &(ptr->Value.afloat));
		ptr->type=TERMTYPE_DOUBLE;
		break;

	case ERL_SMALL_TUPLE_EXT:
	case ERL_LARGE_TUPLE_EXT:
		result=ei_decode_tuple_header((const char *)buf, &index, &size);
		ptr->size= (long) size;
		ptr->type=TERMTYPE_START_TUPLE;
		break;

	case ERL_NIL_EXT:
		(*ptr).type=TERMTYPE_NIL;
		break;

	case ERL_STRING_EXT:
		sptr=(char*) malloc(sizeof(char)*size+sizeof(char));
		if (NULL==sptr) {
			last_error=EEPAPI_MALLOC;
			return 1;
		}
		result=ei_decode_string((const char *)buf, &index, sptr);
		ptr->Value.string=sptr;
		ptr->size=size;
		ptr->type=TERMTYPE_STRING;
		break;

	case ERL_LIST_EXT:
		result=ei_decode_list_header((const char *)buf, &index, &size);
		ptr->size=(long) size;
		ptr->type=TERMTYPE_START_LIST;
		break;

	case ERL_BINARY_EXT:
		sptr=(char*) malloc(sizeof(char)*size+sizeof(char));
		if (NULL==sptr) {
			last_error=EEPAPI_MALLOC;
			return 1;
		}
		result=ei_decode_binary((const char *)buf, &index, sptr, &(ptr->size));
		ptr->Value.string=sptr;
		ptr->type=TERMTYPE_BINARY;
		break;



	// Unsupported types
	// ^^^^^^^^^^^^^^^^^
	case ERL_REFERENCE_EXT:
	case ERL_NEW_REFERENCE_EXT:
	case ERL_PORT_EXT:
	case ERL_PID_EXT:
	case ERL_NEW_FUN_EXT:
	case ERL_FUN_EXT:
	default:
		result=1;
		last_error=EEPAPI_BADTYPE;
		break;
	}//switch

	return result;
}//

