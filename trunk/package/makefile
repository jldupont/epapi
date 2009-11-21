#
# @author Jean-Lou Dupont
#
#
PRJ=epapi

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
	@install -D $(CURDIR)/loop-drv/Debug/epapi_loop_drv_debug  $(DESTDIR)/usr/bin/epapi_loop_drv_debug
	@install -D $(CURDIR)/loop-drv/Release/epapi_loop_drv      $(DESTDIR)/usr/bin/epapi_loop_drv
	
	@echo "> installing lib files in $(DESTDIR)/lib"
	@install -D $(CURDIR)/lib/Debug/libepapi_debug.so   $(DESTDIR)/usr/lib/libepapi_debug.so
	@install -D $(CURDIR)/lib/Release/libepapi.so       $(DESTDIR)/usr/lib/libepapi.so

	@echo "> installing include files in $(DESTDIR)/include"
	@install -D $(CURDIR)/lib/include/epapi.h           $(DESTDIR)/usr/include/epapi.h
	@install -D $(CURDIR)/lib/include/epapi_msg.h       $(DESTDIR)/usr/include/epapi_msg.h
	@install -D $(CURDIR)/lib/include/epapi_pkt.h       $(DESTDIR)/usr/include/epapi_pkt.h
	@install -D $(CURDIR)/lib/include/epapi_term.h      $(DESTDIR)/usr/include/epapi_term.h
	
.PHONY: all clean install
