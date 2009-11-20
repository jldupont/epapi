#
# @author Jean-Lou Dupont
#
#
PRJ=epapi

ifndef DESTDIR
	DESTDIR=/usr/local
endif

all:
	@make -C lib        
	@make -C loop-drv   
	@make -C loop-test  

clean:
	@make -C lib       clean        
	@make -C loop-drv  clean 
	@make -C loop-test clean
	
install:
	
	@echo "> installing driver files in $(DESTDIR)/bin"
	@install -D $(CURDIR)/loop-drv/Debug/epapi_loop_drv_debug  $(DESTDIR)/bin/epapi_loop_drv_debug
	@install -D $(CURDIR)/loop-drv/Release/epapi_loop_drv      $(DESTDIR)/bin/epapi_loop_drv
	
	@echo "> installing lib files in $(DESTDIR)/lib"
	@install -D $(CURDIR)/lib/Debug/libepapi_debug.so   $(DESTDIR)/lib/libepapi_debug.so
	@install -D $(CURDIR)/lib/Release/libepapi.so       $(DESTDIR)/lib/libepapi.so

	@echo "> installing include files in $(DESTDIR)/include"
	@install -D $(CURDIR)/lib/include/epapi.h           $(DESTDIR)/include/epapi.h
	@install -D $(CURDIR)/lib/include/epapi_msg.h       $(DESTDIR)/include/epapi_msg.h
	@install -D $(CURDIR)/lib/include/epapi_pkt.h       $(DESTDIR)/include/epapi_pkt.h
	@install -D $(CURDIR)/lib/include/epapi_term.h      $(DESTDIR)/include/epapi_term.h
	
.PHONY: all clean install
