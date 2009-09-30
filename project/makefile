#
# @author Jean-Lou Dupont
#
#
VERSION=`cat VERSION`
PRJ=epapi

all: 
	@echo "Version: ${VERSION}"
	@make -C lib
	@make -C loop-drv
	@make -C loop-test
	
doc:
	@echo "Version: ${VERSION}"
	@doxygen


#  /usr/lib/libepapi.so-VERSION
#  /usr/lib/libepapi_debug.so-VERSION
#  /usr/lib/erlang/lib/epapi/ebin/test.beam
#  /usr/lib/erlang/lib/epapi/src/test.erl
#  /usr/lib/pkgconfig/epapi.pc

deb:
	@echo "* Building .deb -- Version: ${VERSION}"
	
	@echo "> copying template from 'packages' to '/tmp'"
	@rm -rf /tmp/$(PRJ)
	@mkdir /tmp/$(PRJ)
	@cp -r packages/debian /tmp/$(PRJ)
	
	@echo "> cloning required files"
	@cp lib/Debug/libepapi_debug.so         /tmp/$(PRJ)/debian/usr/lib/libepapi_debug.so-$(VERSION)
	@cp lib/Release/libepapi.so             /tmp/$(PRJ)/debian/usr/lib/libepapi.so-$(VERSION)
	@cp loop-drv/Debug/epapi_loop_drv_debug /tmp/$(PRJ)/debian/usr/lib/erlang/lib/$(PRJ)/ebin/epapi_loop_drv_debug
	@cp loop-drv/Release/epapi_loop_drv     /tmp/$(PRJ)/debian/usr/lib/erlang/lib/$(PRJ)/ebin/epapi_loop_drv
	@cp loop-test/ebin/test.beam            /tmp/$(PRJ)/debian/usr/lib/erlang/lib/$(PRJ)/ebin/test.beam
	@cp loop-test/src/test.erl              /tmp/$(PRJ)/debian/usr/lib/erlang/lib/$(PRJ)/src/test.erl

	
