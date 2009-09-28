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
		,TERMTYPE_ATOM
		,TERMTYPE_TUPLE
		,TERMTYPE_FLOAT
		,TERMTYPE_LONG
		,TERMTYPE_ULONG
		,TERMTYPE_LONGLONG
		,TERMTYPE_ULONGLONG
		,TERMTYPE_STRING
		,TERMTYPE_BINARY
	} TermType;

	typedef void *TermPointer;


	class Term {

	protected:
		TermType type;
		int size;
		void *data;

	public:

		// for numbers e.g. long, float etc.
		Term(TermType type);

		// for string
		Term(TermType type, int size);

		Term(void);
		~Term();

		TermType getType(void);
		void     setType(TermType type);

		int  getSize(void);
		void setSize(int sz);

		TermPointer getDataPtr(void);
		void        setDataPtr(TermPointer *term_pointer);
	};


	/**
	 * Term Handler
	 *
	 * This class serves as the main API for clients.
	 */
	class TermHandler: public epapiBase {

	public:

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
		 * @param ph PktHandler instance
		 */
		TermHandler(PktHandler *ph, Pkt *p);

		/**
		 * Destructor
		 */
		~TermHandler();

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
		 * @param term_pointer Pointer to Term
		 *
		 * @return 0 SUCCESS
		 * @return 1 FAILURE
		 */
		int iter(TermPointer **term_pointer);

	};

#endif /* EPAPI_TERM_H_ */
