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
	p=NULL;
	index=0;
}

// When receiving
TermHandler::TermHandler(Pkt *_p) {
	ph=NULL;
	p=_p;
	index=0;
}

// Cleaning up 'ph' and 'p' is
// not our responsibility... we were just
// loaned those!
TermHandler::~TermHandler() {
}

void
TermHandler::initTx(Pkt *_p) {

	p=_p;
}

void
TermHandler::clean(TermStruct *ts) {
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
TermHandler::append(TermStruct *ts) {

	if (NULL==ts) {
		last_error=EEPAPI_NULL;
		return 1;
	}

	int result;
	ei_x_buff *b; //local shortcut

	if (NULL==p) {
		DBGLOG(LOG_INFO, "TermHandler::append - need a packet!");
		last_error=EEPAPI_NULL;
		return 1;
	}

	b = p->getTxBuf();
	if (NULL==b) {
		delete p;
		last_error = EEPAPI_MALLOC;
		return 1;
	}

	DBGLOG(LOG_INFO, "TermHandler::append: type: %i", ts->type);

	switch(ts->type) {

	case TERMTYPE_START_LIST:
		result=ei_x_encode_list_header(b, 1);
		break;

	case TERMTYPE_NIL:
	case TERMTYPE_END_LIST:
		result=ei_x_encode_empty_list(b);
		break;

	case TERMTYPE_ATOM:
		DBGLOG(LOG_INFO, "TermHandler::append: ATOM: %s", ts->Value.string);
		result=ei_x_encode_atom(b, (const char *)ts->Value.string);
		break;

	case TERMTYPE_START_TUPLE:
		DBGLOG(LOG_INFO, "TermHandler::append: TUPLE, size: %i", ts->size);
		result=ei_x_encode_tuple_header(b, (int) ts->size);
		break;

	case TERMTYPE_DOUBLE:
		result=ei_x_encode_double(b, ts->Value.afloat);
		break;

	case TERMTYPE_LONG:
		result=ei_x_encode_long(b, ts->Value.integer);
		break;

	case TERMTYPE_ULONG:
		result=ei_x_encode_ulong(b, ts->Value.uinteger);
		break;

	case TERMTYPE_LONGLONG:
		result=ei_x_encode_longlong(b, ts->Value.linteger);
		break;

	case TERMTYPE_ULONGLONG:
		result=ei_x_encode_ulonglong(b, ts->Value.luinteger);
		break;

	case TERMTYPE_STRING:
		result=ei_x_encode_string(b, (const char *) ts->Value.string);
		break;

	case TERMTYPE_BINARY:
		result=ei_x_encode_binary(b, ts->Value.string, ts->size);
		break;

	default:
		result=1;
		break;


	}//switch

	return result;
}


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
	if (NULL==buf) {
		last_error=EEPAPI_NULL;
		return 1;
	}

	int result;
	int version;

	// we are just starting... read the version header first
	if (0==index) {
		result=ei_decode_version((const char *)buf, &index, &version);
		if (result) {
			last_error=EEPAPI_EIDECODE;
			return 1;
		}

		DBGLOG(LOG_INFO, "TermHandler::iter: Version: %i", version);
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

	DBGLOG(LOG_INFO, "TermHandler::iter: type: %i", type);

	char *sptr;

	switch(type) {
	case ERL_SMALL_INTEGER_EXT:
	case ERL_INTEGER_EXT:
		DBGLOG(LOG_INFO, "TermHandler::iter: SMALL_INTEGER/INTEGER");
		result=ei_decode_long((const char *)buf, &index, &(ptr->Value.integer));
		ptr->type=TERMTYPE_LONG;

		if (result) {
			result=ei_decode_ulong((const char *)buf, &index, &(ptr->Value.uinteger));
			ptr->type=TERMTYPE_ULONG;
		}
		break;
	case ERL_LARGE_BIG_EXT:
	case ERL_SMALL_BIG_EXT:
		DBGLOG(LOG_INFO, "TermHandler::iter: BIG_EXT");
		result=ei_decode_longlong((const char *)buf, &index, &(ptr->Value.linteger));
		ptr->type=TERMTYPE_LONGLONG;

		if (result) {
			result=ei_decode_ulonglong((const char *)buf, &index, &(ptr->Value.luinteger));
			ptr->type=TERMTYPE_ULONGLONG;
		}
		break;

	case ERL_ATOM_EXT:
		DBGLOG(LOG_INFO, "TermHandler::iter: ATOM");
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
		DBGLOG(LOG_INFO, "TermHandler::iter: FLOAT");
		result=ei_decode_double((const char *)buf, &index, &(ptr->Value.afloat));
		ptr->type=TERMTYPE_DOUBLE;
		break;

	case ERL_SMALL_TUPLE_EXT:
	case ERL_LARGE_TUPLE_EXT:
		DBGLOG(LOG_INFO, "TermHandler::iter: TUPLE");
		result=ei_decode_tuple_header((const char *)buf, &index, &size);
		ptr->size= (long) size;
		ptr->type=TERMTYPE_START_TUPLE;
		break;

	case ERL_NIL_EXT:
		DBGLOG(LOG_INFO, "TermHandler::iter: NIL");
		ptr->type=TERMTYPE_NIL;
		result=0;
		break;

	case ERL_STRING_EXT:
		DBGLOG(LOG_INFO, "TermHandler::iter: STRING");
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
		DBGLOG(LOG_INFO, "TermHandler::iter: LIST");
		result=ei_decode_list_header((const char *)buf, &index, &size);
		ptr->size=(long) size;
		ptr->type=TERMTYPE_START_LIST;
		break;

	case ERL_BINARY_EXT:
		DBGLOG(LOG_INFO, "TermHandler::iter: BINARY");
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
		ptr->type=TERMTYPE_UNSUPPORTED;
		DBGLOG(LOG_ERR, "TermHandler::iter: UNSUPPORTED");
		result=1;
		last_error=EEPAPI_BADTYPE;
		break;
	}//switch

	DBGLOG(LOG_INFO, "TermHandler::iter: Result: %i", result);

	return result;
}//

