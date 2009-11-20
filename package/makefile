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
	@echo "> copying driver files in /usr/bin"
	@cp loop-drv/Debug/epapi_loop_drv_debug  /usr/bin/epapi_loop_drv_debug
	@cp loop-drv/Release/epapi_loop_drv      /usr/bin/epapi_loop_drv
	
	@echo "> copying lib files in /usr/lib"
	@cp lib/Debug/libepapi_debug.so   /usr/lib/libepapi_debug.so
	@cp lib/Release/libepapi.so       /usr/lib/libepapi.so

	@echo "> copying include files in /usr/include"
	@cp lib/include/epapi.h           /usr/include/epapi.h
	@cp lib/include/epapi_msg.h       /usr/include/epapi_msg.h
	@cp lib/include/epapi_pkt.h       /usr/include/epapi_pkt.h
	@cp lib/include/epapi_term.h      /usr/include/epapi_term.h
	
.PHONY: all clean install
