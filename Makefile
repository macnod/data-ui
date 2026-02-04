ROSWELL_VERSION := v23.10.14.114
SBCL_VERSION := 2.5.10
TEST_FILE := tests/data-ui-tests.lisp

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

install-dependencies:
	ros install postmodern
	ros install fiveam
	ros install cl-csv
	ros install trivial-utf-8
	ros install ironclad
	ros install cl-ppcre
	ros install hunchentoot
	ros install swank
	ros install spinneret
	ros install jose
	ros install lass
	ros install mgl-pax
	ros install cl-fad
	ros install macnod/dc-dlist/v1.0
	ros install macnod/dc-ds/v0.5
	ros install macnod/dc-time/v0.5
	ros install macnod/p-log/v0.9
	ros install macnod/rbac
	ros install macnod/dc-eclectic

test:
	tests/run tests

test-ci:
	tests/run tests

repl:
	tests/run repl

psql:
	tests/run psql

stop:
	tests/run stop

help:
	tests/run help

.PHONY: install-roswell install-dependencies test
.DEFAULT_GOAL := test
