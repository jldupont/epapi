/*
 * epapi_term.h
 *
 *  Created on: 2009-09-28
 *      Author: jldupont
 */

#ifndef EPAPI_TERM_H_
#define EPAPI_TERM_H_

//dev support
#ifndef EPAPI_H_
	#include "epapi.h"
#endif


	typedef enum {
		TERMTYPE_INVALID = 0
		,TERMTYPE_END
		,TERMTYPE_START_LIST
		,TERMTYPE_END_LIST
		,TERMTYPE_START_TUPLE
		,TERMTYPE_ATOM
		,TERMTYPE_STRING
		,TERMTYPE_TUPLE
		,TERMTYPE_DOUBLE
		,TERMTYPE_LONG
		,TERMTYPE_ULONG
		,TERMTYPE_LONGLONG
		,TERMTYPE_ULONGLONG
		,TERMTYPE_BINARY
		,TERMTYPE_NIL
	} TermType;


	typedef struct _TermStruct {
		TermType type;
		long size; // only applicable to string
		union {
			unsigned long uinteger;
			long integer;
			long long linteger;
			unsigned long long luinteger;
			double afloat;
			void *string;
		} Value;
	} TermStruct;



	/**
	 * Term Handler
	 *
	 * This class serves as the main API for clients.
	 */
	class TermHandler: public epapiBase {

	public:
		static const char *term_strings[];

	protected:

		PktHandler *ph;
		Pkt *p;
		int index; // for the rx side

	public:
		/**
		 * Constructor used when sending
		 */
		TermHandler(PktHandler *ph);

		/**
		 * Constructor used when receiving
		 *
		 * @param p Pkt instance
		 */
		TermHandler(Pkt *p);

		/**
		 * Destructor
		 */
		~TermHandler();

		/**
		 * Frees a TermStruct
		 *
		 * @param ts TermStruct
		 */
		void destroy(TermStruct *ts);

		/**
		 * Returns a human readable string
		 * corresponding to a specific term type
		 *
		 * @param type TermType
		 * @return const char *
		 */
		const char *termtype_tostring(TermType type);

		/**
		 * Send the packet
		 *
		 * @return 0 SUCCESS
		 * @return 1 FAILURE
		 */
		int send(void);

		/**
		 * Appends an element to the packet
		 *
		 * @param type TermType
		 *
		 * @return 0 SUCCESS
		 * @return 1 FAILURE
		 */
		int append(TermType type, ...);

		/**
		 * Iteration interface - iterates over a packet
		 * and extracts all elements of the container term()
		 *
		 * @param ptr Pointer to TermStruct for receiving the term
		 *
		 * @return 0 SUCCESS
		 * @return 1 FAILURE
		 */
		int iter(TermStruct *ptr);

	};

#endif /* EPAPI_TERM_H_ */
