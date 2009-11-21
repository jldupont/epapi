# @author Jean-Lou Dupont
# @title  EPAPI
#
VERSION:=`cat VERSION`
PRJ=epapi

DEFAULT_DISTRO=jaunty

ifeq ($(DIST),)
	DIST=${DEFAULT_DISTRO}
endif

clean:
	@echo "Version: ${VERSION}"
	@rm -r -f /tmp/$(PRJ)
	
orig:
	@echo "* Preparing for packaging -- Version: ${VERSION}, Distribution: ${DIST}"
	@echo "-------------------------"
	@echo ""
	@mkdir -p "/tmp/$(PRJ)"
	@mkdir -p "/tmp/$(PRJ)/$(PRJ)-$(VERSION)"	
	@echo "Copying package source tree"
	@rsync -r --exclude=*.svn* package/* "/tmp/$(PRJ)/$(PRJ)-$(VERSION)"
	@echo "Copying debian folder"
	@rsync -r --exclude=*.svn* packages/debian "/tmp/$(PRJ)/$(PRJ)-$(VERSION)"
	@echo "Copying makefile"
	@cp makefile "/tmp/$(PRJ)/makefile"
	@echo "Copying VERSION file"
	@cp VERSION "/tmp/$(PRJ)/VERSION"
	@echo "Adjusting debian/changelog to DIST"
	@cat packages/debian/changelog | sed "s/_DIST_/${DIST}/g" > "/tmp/${PRJ}/${PRJ}-${VERSION}/debian/changelog"
	@echo "** SUCCESS: folder ready: /tmp/$(PRJ)"
	@echo "*** DON'T FORGET TO UPDATE debian/changelog ***"

ppa:
	@echo "!!! Have you updated debian/changelog ?"
	@echo "Running 'debuild'"
	@cd "/tmp/$(PRJ)/$(PRJ)-$(VERSION)" && debuild -S
	
pb:
	@echo " ----------------------------- "
	@echo " RUNNING PBUILDER "
	@cd "/tmp/$(PRJ)/" && sudo DIST=${DIST} pbuilder build *.dsc

up:
	@echo "UPLOADING TO PPA"
	@cd "/tmp/$(PRJ)/" && dput ppa:jldupont/jldupont *.changes
	
doc:
	@echo "Version: ${VERSION}"
	@cd docs && doxygen
	@./.tools/adjust_version.py $(VERSION) $(DOCMAIN) $(DOCMAIN)	
	

.PHONY: orig ppa pb clean
	