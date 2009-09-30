#
# @author Jean-Lou Dupont
#
#
VERSION=`cat VERSION`

all: 
	@echo "Version: ${VERSION}"
	@make -C lib
	@make -C loop-drv
	@make -C loop-test
	
doc:
	@echo "Version: ${VERSION}"
	@doxygen

	
