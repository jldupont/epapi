#
# @author Jean-Lou Dupont
#

MAKE=make
BUILDS=Release Debug
TARGETS=all clean

$(TARGETS):
	@for b in $(BUILDS) ; do $(MAKE) -C $$b $@ ; done

$(BUILDS):
	@for t in $(TARGETS) ; do $(MAKE) -C $@ $$t ; done

%:
	@for b in $(BUILDS) ; do $(MAKE) -C $$b $@ ; done
