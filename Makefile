ROSWELL_VERSION := v23.10.14.114
SBCL_VERSION := 2.5.10
TEST_FILE := tests/rbac-tests.lisp

install-roswell:
	@if ! which ros > /dev/null 2>&1; then \
		echo "Roswell not found. Installing..."; \
		curl -L $(ROSWELL_BASE_URL)/$(ROSWELL_VERSION)/roswell_$(subst v,,$(ROSWELL_VERSION))-1_amd64.deb --output roswell.deb; \
		sudo dpkg -i roswell.deb; \
		ros install sbcl-bin/$(SBCL_VERSION); \
		ros use sbcl-bin/$(SBCL_VERSION); \
		echo "Roswell installation complete."; \
	else \
		echo "Roswell already installed. Skipping..."; \
	fi
	touch $@

third-party-stuff:
	ros install cl-ppcre
	ros install postmodern
	ros install cl-fad
	ros install drakma
	ros install yason
	ros install fiveam
	ros install swank
	ros install hunchentoot
	ros install jose
	ros install babel
	ros install pythonic-string-reader
	ros install cl-csv

macnod-stuff:
	ros install macnod/dc-dlist
	ros install macnod/dc-ds
	ros install macnod/dc-eclectic
	ros install macnod/dc-time
	ros install macnod/p-log
	ros install macnod/rbac

dependencies: third-party-stuff macnod-stuff
